// Crate Dependencies ---------------------------------------------------------
#[macro_use]
extern crate debug_stub_derive;


// Struct Tests ---------------------------------------------------------------
#[test]
fn test_struct_empty() {

    #[derive(DebugStub)]
    struct TestStruct;

    let s = TestStruct;

    assert_eq!(format!("{:?}", s), "TestStruct");
    assert_eq!(format!("{:#?}", s), "TestStruct");

}

#[test]
fn test_struct_compare_std_empty() {

    mod a {
        #[derive(DebugStub)]
        pub struct TestStruct;
    }

    mod b {
        #[derive(Debug)]
        pub struct TestStruct;
    }

    assert_eq!(
        format!("{:?}", a::TestStruct),
        format!("{:?}", b::TestStruct)
    );

    assert_eq!(
        format!("{:#?}", a::TestStruct),
        format!("{:#?}", b::TestStruct)
    );

}

#[test]
fn test_struct() {

    #[derive(Debug)]
    struct StructWithDebug {
        number: u64
    }

    struct StructWithoutDebug {
        string: String
    }

    #[derive(DebugStub)]
    struct TestStruct {
        value: bool,
        a: StructWithDebug,
        #[debug_stub="StructWithoutDebugReplaceValue"]
        b: StructWithoutDebug
    }

    let s = TestStruct {
        value: true,
        a: StructWithDebug {
            number: 42
        },
        b: StructWithoutDebug {
            string: "Hello World".to_string()
        }
    };

    assert_eq!(format!("{:?}", s), "TestStruct { value: true, a: StructWithDebug { number: 42 }, b: StructWithoutDebugReplaceValue }");
    assert_eq!(format!("{:#?}", s), r#"TestStruct {
    value: true,
    a: StructWithDebug {
        number: 42
    },
    b: StructWithoutDebugReplaceValue
}"#);

}

#[test]
fn test_struct_compare_std() {

    mod a {
        #[derive(Debug)]
        pub struct InternalStruct {
            pub a: bool
        }

        #[derive(DebugStub)]
        pub struct TestStruct {
            pub a: InternalStruct,
            pub b: u64
        }
    }

    mod b {
        #[derive(Debug)]
        pub struct InternalStruct {
            pub a: bool
        }

        #[derive(Debug)]
        pub struct TestStruct {
            pub a: InternalStruct,
            pub b: u64
        }
    }

    let struct_a = a::TestStruct {
        a: a::InternalStruct {
            a: true
        },
        b: 42
    };

    let struct_b = b::TestStruct {
        a: b::InternalStruct {
            a: true
        },
        b: 42
    };

    assert_eq!(
        format!("{:?}", struct_a),
        format!("{:?}", struct_b)
    );

    assert_eq!(
        format!("{:#?}", struct_a),
        format!("{:#?}", struct_b)
    );

}

#[test]
fn test_struct_with_type_parameters() {

    use std::fmt::{Debug, Display};

    #[derive(DebugStub)]
    struct TestStruct<T: Display + Debug> {
        t: T
    }

    assert_eq!(
        format!("{:?}", TestStruct {
            t: 42
        }),
        "TestStruct { t: 42 }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            t: 42
        }),
        r#"TestStruct {
    t: 42
}"#
    );

}

#[test]
fn test_struct_with_type_where_clause() {

    use std::fmt::{Debug, Display};

    #[derive(DebugStub)]
    struct TestStruct<T> where T: Display + Debug {
        t: T
    }

    assert_eq!(
        format!("{:?}", TestStruct {
            t: 42
        }),
        "TestStruct { t: 42 }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            t: 42
        }),
        r#"TestStruct {
    t: 42
}"#
    );

}

#[test]
fn test_struct_optional() {

    struct StructWithoutDebug;

    #[derive(DebugStub)]
    struct TestStruct {
        #[debug_stub(some="StructWithoutDebugReplaceValue")]
        s: Option<StructWithoutDebug>
    }

    assert_eq!(
        format!("{:?}", TestStruct {
            s: None
        }),
        "TestStruct { s: None }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            s: None
        }),
        r#"TestStruct {
    s: None
}"#
    );

    assert_eq!(
        format!("{:?}", TestStruct {
            s: Some(StructWithoutDebug)
        }),
        "TestStruct { s: StructWithoutDebugReplaceValue }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            s: Some(StructWithoutDebug)
        }),
        r#"TestStruct {
    s: StructWithoutDebugReplaceValue
}"#
    );

}

#[test]
fn test_struct_result() {

    struct StructWithoutDebug;
    struct ErrorWithoutDebug;

    #[derive(DebugStub)]
    struct TestStruct {
        #[debug_stub(ok="StructWithoutDebugReplaceValue", err="ErrorWithoutDebugReplaceValue")]
        s: Result<StructWithoutDebug, ErrorWithoutDebug>
    }

    assert_eq!(
        format!("{:?}", TestStruct {
            s: Err(ErrorWithoutDebug)
        }),
        "TestStruct { s: ErrorWithoutDebugReplaceValue }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            s: Err(ErrorWithoutDebug)
        }),
        r#"TestStruct {
    s: ErrorWithoutDebugReplaceValue
}"#
    );

    assert_eq!(
        format!("{:?}", TestStruct {
            s: Ok(StructWithoutDebug)
        }),
        "TestStruct { s: StructWithoutDebugReplaceValue }"
    );

    assert_eq!(
        format!("{:#?}", TestStruct {
            s: Ok(StructWithoutDebug)
        }),
        r#"TestStruct {
    s: StructWithoutDebugReplaceValue
}"#
    );

}

// Enum Tests -----------------------------------------------------------------

#[test]
fn test_enum_empty() {

    #[derive(DebugStub)]
    enum TestEnum {
        VariantC {},
        VariantD
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantC {}),
        "VariantC"
    );

    assert_eq!(
        format!("{:?}", TestEnum::VariantC {}),
        "VariantC"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantD),
        "VariantD"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantD),
        "VariantD"
    );

}

#[test]
fn test_enum_compare_std_empty() {

    mod a {
        #[derive(DebugStub)]
        pub enum TestEnum {
            VariantA,
            VariantB {}
        }
    }

    mod b {
        #[derive(Debug)]
        pub enum TestEnum {
            VariantA,
            VariantB {}
        }
    }

    assert_eq!(
        format!("{:?}", a::TestEnum::VariantA),
        format!("{:?}", b::TestEnum::VariantA)
    );

    assert_eq!(
        format!("{:#?}", a::TestEnum::VariantA),
        format!("{:#?}", b::TestEnum::VariantA)
    );

    assert_eq!(
        format!("{:?}", a::TestEnum::VariantB {}),
        format!("{:?}", b::TestEnum::VariantB {})
    );

    assert_eq!(
        format!("{:#?}", a::TestEnum::VariantB {}),
        format!("{:#?}", b::TestEnum::VariantB {})
    );

}


#[test]
fn test_enum() {

    #[derive(Debug)]
    struct StructWithDebug {
        number: u64
    }

    struct StructWithoutDebug {
        string: String
    }

    #[derive(DebugStub)]
    enum TestEnum {
        VariantA(
            StructWithDebug,
            #[debug_stub="StructWithoutDebugReplaceValue"]
            StructWithoutDebug,
            bool
        ),
        VariantB {
            a: StructWithDebug,
            #[debug_stub="StructWithoutDebugReplaceValue"]
            b: StructWithoutDebug,
            c: bool
        }
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(StructWithDebug {
            number: 42

        }, StructWithoutDebug {
            string: "Hello World".to_string()

        }, true)),

        "VariantA(StructWithDebug { number: 42 }, StructWithoutDebugReplaceValue, true)"
    );

    assert_eq!(
        format!("{:?}", TestEnum::VariantB {
            a: StructWithDebug {
                number: 42
            },
            b: StructWithoutDebug {
                string: "Hello World".to_string()
            },
            c: true

        }), "VariantB { a: StructWithDebug { number: 42 }, b: StructWithoutDebugReplaceValue, c: true }"
    );

}

#[test]
fn test_enum_compare_std() {

    mod a {

        #[derive(Debug)]
        pub struct InternalStruct {
            pub a: bool
        }

        #[derive(DebugStub)]
        pub enum TestEnum {
            VariantA(InternalStruct, bool, u64),
            VariantB {
                a: InternalStruct,
                b: bool,
                c: u64
            }
        }
    }

    mod b {

        #[derive(Debug)]
        pub struct InternalStruct {
            pub a: bool
        }

        #[derive(Debug)]
        pub enum TestEnum {
            VariantA(InternalStruct, bool, u64),
            VariantB {
                a: InternalStruct,
                b: bool,
                c: u64
            }
        }
    }

    let enum_a_a = a::TestEnum::VariantA(a::InternalStruct {
        a: true

    }, false, 42);

    let enum_a_b = a::TestEnum::VariantB {
        a: a::InternalStruct {
            a: true
        },
        b: false,
        c: 42
    };

    let enum_b_a = b::TestEnum::VariantA(b::InternalStruct {
        a: true

    }, false, 42);

    let enum_b_b = b::TestEnum::VariantB {
        a: b::InternalStruct {
            a: true
        },
        b: false,
        c: 42
    };

    assert_eq!(
        format!("{:?}", enum_a_a),
        format!("{:?}", enum_b_a)
    );

    assert_eq!(
        format!("{:#?}", enum_a_a),
        format!("{:#?}", enum_b_a)
    );

    assert_eq!(
        format!("{:?}", enum_a_b),
        format!("{:?}", enum_b_b)
    );

    assert_eq!(
        format!("{:#?}", enum_a_b),
        format!("{:#?}", enum_b_b)
    );

}

#[test]
fn test_enum_with_type_parameters() {

    use std::fmt::{Debug, Display};

    #[derive(DebugStub)]
    enum TestEnum<T: Display + Debug> {
        VariantA(T)
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(true)),
        "VariantA(true)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(true)),
        r#"VariantA(
    true
)"#
    );

}

#[test]
fn test_enum_with_type_where_clause() {

    use std::fmt::{Debug, Display};

    #[derive(DebugStub)]
    enum TestEnum<T> where T: Display + Debug {
        VariantA(T)
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(true)),
        "VariantA(true)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(true)),
        r#"VariantA(
    true
)"#
    );

}

#[test]
fn test_enum_optional() {

    struct StructWithoutDebug;

    #[derive(DebugStub)]
    enum TestEnum {
        VariantA(
            #[debug_stub(some="StructWithoutDebugReplaceValue")]
            Option<StructWithoutDebug>
        )
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(None)),
        "VariantA(None)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(None)),
        r#"VariantA(
    None
)"#
    );

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(Some(StructWithoutDebug))),
        "VariantA(StructWithoutDebugReplaceValue)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(Some(StructWithoutDebug))),
        r#"VariantA(
    StructWithoutDebugReplaceValue
)"#
    );

}

#[test]
fn test_enum_result() {

    struct StructWithoutDebug;
    struct ErrorWithoutDebug;

    #[derive(DebugStub)]
    enum TestEnum {
        VariantA(
            #[debug_stub(ok="StructWithoutDebugReplaceValue", err="ErrorWithoutDebugReplaceValue")]
            Result<StructWithoutDebug, ErrorWithoutDebug>
        )
    }

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(Err(ErrorWithoutDebug))),
        "VariantA(ErrorWithoutDebugReplaceValue)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(Err(ErrorWithoutDebug))),
        r#"VariantA(
    ErrorWithoutDebugReplaceValue
)"#
    );

    assert_eq!(
        format!("{:?}", TestEnum::VariantA(Ok(StructWithoutDebug))),
        "VariantA(StructWithoutDebugReplaceValue)"
    );

    assert_eq!(
        format!("{:#?}", TestEnum::VariantA(Ok(StructWithoutDebug))),
        r#"VariantA(
    StructWithoutDebugReplaceValue
)"#
    );

}

