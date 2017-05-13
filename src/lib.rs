//! This crate provides the `DebugStub` derive macro.
//!
//! The `DebugStub` derive macro can be used as a drop-in replacement for the
//! standard [`fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html)
//! when certain members of a struct or enum do not or cannot implement the
//! `fmt::Debug` trait themselves, but the containing struct or enum still wants
//! or needs to implement it.
//!
//! # Examples
//!
//! Using `DebugStub` with structs:
//!
//! ```
//! #[macro_use]
//! extern crate debug_stub_derive;
//!
//! # fn main() {
//! // A struct from an external crate which does not implement the `fmt::Debug`
//! // trait.
//! pub struct ExternalCrateStruct;
//!
//! // A struct in the current crate which wants to cleanly expose
//! // itself to the outside world with an implementation of `fmt::Debug`.
//! #[derive(DebugStub)]
//! pub struct PubStruct {
//!     a: bool,
//!     // Define a replacement debug serialization for the external struct.
//!     #[debug_stub="ReplacementValue"]
//!     b: ExternalCrateStruct
//! }
//!
//! assert_eq!(format!("{:?}", PubStruct {
//!     a: true,
//!     b: ExternalCrateStruct
//!
//! }), "PubStruct { a: true, b: ReplacementValue }");
//! # }
//! ```
//!
//! Using `DebugStub` with enums:
//!
//! ```
//! # #[macro_use]
//! # extern crate debug_stub_derive;
//! # fn main() {
//! pub struct ExternalCrateStruct;
//!
//! #[derive(DebugStub)]
//! pub enum PubEnum {
//!     VariantA(
//!         u64,
//!         #[debug_stub="ReplacementValue"]
//!         ExternalCrateStruct
//!     )
//! }
//!
//! assert_eq!(format!("{:?}", PubEnum::VariantA(
//!     42,
//!     ExternalCrateStruct
//!
//! )), "VariantA(42, ReplacementValue)");
//! # }
//! ```
//!
//! Using `DebugStub` with `Option` and `Result` types:
//!
//! ```
//! # #[macro_use]
//! # extern crate debug_stub_derive;
//! # fn main() {
//! pub struct ExternalCrateStruct;
//!
//! #[derive(DebugStub)]
//! pub struct PubStruct {
//!     #[debug_stub(some="ReplacementSomeValue")]
//!     a: Option<ExternalCrateStruct>,
//!     #[debug_stub(ok="ReplacementOkValue")]
//!     b: Result<ExternalCrateStruct, ()>
//! }
//!
//! assert_eq!(format!("{:?}", PubStruct {
//!     a: Some(ExternalCrateStruct),
//!     b: Ok(ExternalCrateStruct)
//!
//! }), "PubStruct { a: ReplacementSomeValue, b: ReplacementOkValue }");
//! # }
//! ```
#![deny(
    trivial_casts, trivial_numeric_casts,
    unsafe_code,
    unused_import_braces, unused_qualifications
)]


// Crate Dependencies ---------------------------------------------------------
extern crate proc_macro;
extern crate syn;
#[macro_use] extern crate quote;


// External Dependencies ------------------------------------------------------
use proc_macro::TokenStream;


/// Implementation of the `#[derive(debug_stub)]` derive macro.
#[proc_macro_derive(DebugStub, attributes(debug_stub))]
pub fn derive_debug_stub(input: TokenStream) -> TokenStream {
    let input = syn::parse_derive_input(&input.to_string()).unwrap();
    match expand_derive_serialize(&input) {
        Ok(expanded) => expanded.parse().unwrap(),
        Err(msg) => panic!(msg),
    }
}

fn expand_derive_serialize(ast: &syn::DeriveInput) -> Result<quote::Tokens, String> {
    match ast.body {
        syn::Body::Struct(ref data) => {
            Ok(implement_struct_debug(
                &ast.ident,
                &ast.generics,
                generate_field_tokens(data.fields())
            ))
        },
        syn::Body::Enum(ref variants) => {
            Ok(implement_enum_debug(
                &ast.ident,
                &ast.generics,
                variants.iter().map(|variant| {
                    generate_enum_variant_tokens(&ast.ident, variant)

                }).collect()
            ))
        }
    }
}

fn implement_struct_debug(
    ident: &syn::Ident,
    generics: &syn::Generics,
    tokens: Vec<quote::Tokens>

) -> quote::Tokens {

    let name = ident.to_string();
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let mut f = f.debug_struct(#name);
                #(#tokens);*;
                f.finish()
            }
        }
    }

}

fn implement_enum_debug(
    ident: &syn::Ident,
    generics: &syn::Generics,
    cases: Vec<quote::Tokens>

) -> quote::Tokens {

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote! {
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match *self {
                    #(#cases),*
                }
            }
        }
    }

}

fn generate_field_tokens(fields: &[syn::Field]) -> Vec<quote::Tokens> {
    fields.iter().map(|field| {
        if let Some(ref ident) = field.ident {
            let name = ident.to_string();
            let ident = syn::Ident::new(format!("self.{}", ident));
            if let Some(tokens) = extract_value_attr(&ident, &field.attrs, Some(&name)) {
                tokens

            } else {
                quote! {
                    f.field(#name, &#ident)
                }
            }

        } else {
            panic!("struct has unnamed fields")
        }

    }).collect()
}

fn generate_enum_variant_tokens(ident: &syn::Ident, variant: &syn::Variant) -> quote::Tokens {

    let variant_ident = &variant.ident;
    let variant_name = variant_ident.to_string();

    match variant.data {
        syn::VariantData::Struct(ref fields) => {

            let field_names: Vec<&syn::Ident> = fields.iter().map(|field| {
                field.ident.as_ref().unwrap()

            }).collect();

            let field_tokens: Vec<quote::Tokens> = fields.iter().zip(field_names.iter()).map(|(field, ident)| {
                let name = ident.to_string();
                if let Some(tokens) = extract_value_attr(&ident, &field.attrs, Some(&name)) {
                    tokens

                } else {
                    quote! {
                        f.field(#name, &#ident)
                    }
                }

            }).collect();

            // TODO dry with below
            quote! {
                #ident::#variant_ident{ #(ref #field_names),* } => {
                    let mut f = f.debug_struct(#variant_name);
                    #(#field_tokens);*;
                    f.finish()
                }
            }

        },
        syn::VariantData::Tuple(ref fields) => {

            let field_names: Vec<syn::Ident> = fields.iter().enumerate().map(|(index, _)| {
                syn::Ident::new(format!("tuple_{}", index))

            }).collect();

            let field_tokens: Vec<quote::Tokens> = fields.iter().zip(field_names.iter()).map(|(field, ident)| {
                // TODO if we never end up using the tuple name reduce it to _
                // TODO or simply add allow unused
                if let Some(tokens) = extract_value_attr(&ident, &field.attrs, None) {
                    tokens

                } else {
                    quote! {
                        f.field(&#ident)
                    }
                }

            }).collect();

            // TODO dry with above
            quote! {
                #ident::#variant_ident( #(ref #field_names),* ) => {
                    let mut f = f.debug_tuple(#variant_name);
                    #(#field_tokens);*;
                    f.finish()
                }
            }

        },
        syn::VariantData::Unit => quote! {
            #ident::#variant_ident => {
                f.debug_struct(#variant_name).finish()
            }
        }
    }

}

fn extract_value_attr(
    ident: &syn::Ident,
    attrs: &[syn::Attribute],
    name: Option<&str>

) -> Option<quote::Tokens> {

    for attr in attrs {
        if attr.value.name() == "debug_stub" {
            if let syn::MetaItem::NameValue(_, syn::Lit::Str(ref value, _)) = attr.value {
                // TODO must rename the outer capture variable into _ to avoid unused variable
                // warnings
                return Some(implement_replace_attr(name, value));

            } else if let syn::MetaItem::List(_, ref items) = attr.value {
                match extract_named_value_attrs(items) {
                    (None, None, Some(some)) => return Some(implement_some_attr(some, name, ident)),
                    (Some(ok), Some(err), None) => return Some(implement_result_attr(ok, err, name, ident)),
                    (Some(ok), None, None) => return Some(implement_ok_attr(ok, name, ident)),
                    (None, Some(err), None) => return Some(implement_err_attr(err, name, ident)),
                    _ => {}
                }
            }
        }
    }

    None

}

fn extract_named_value_attrs<'a>(items: &'a [syn::NestedMetaItem]) -> (Option<&'a str>, Option<&'a str>, Option<&'a str>) {

    let (mut ok, mut err, mut some) = (None, None, None);

    for item in items {
        if let &syn::NestedMetaItem::MetaItem(
            syn::MetaItem::NameValue(ref attr_name, syn::Lit::Str(ref value, _))

        ) = item {
            if attr_name == "some" {
                some = Some(value.as_str());

            } else if attr_name == "ok" {
                ok = Some(value.as_str());

            } else if attr_name == "err" {
                err = Some(value.as_str());
            }
        }
    }

    (ok, err, some)

}

macro_rules! field_block {
    ($name:expr, $($tt:tt)*) => (
        {
            if let Some(name) = $name {
                quote! {
                    f.field(#name, $($tt)*)
                }

            } else {
                quote! {
                    f.field($($tt)*)
                }
            }
        }
    );
}

fn implement_replace_attr(name: Option<&str>, value: &str) -> quote::Tokens {
    field_block!(name, &format_args!("{}", #value))
}

fn implement_some_attr(some: &str, name: Option<&str>, ident: &syn::Ident) -> quote::Tokens {
    field_block!(name, &format_args!("{}", if #ident.is_some() {
        #some

    } else {
        "None"
    }))
}

fn implement_result_attr(ok: &str, err: &str, name: Option<&str>, ident: &syn::Ident) -> quote::Tokens {
    field_block!(name, &format_args!("{}", if #ident.is_ok() {
        #ok

    } else {
        #err
    }))
}

fn implement_ok_attr(ok: &str, name: Option<&str>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_err() {
                f.field(#name, &format_args!("Err({:?})", #ident.as_ref().err().unwrap()))

            } else {
                f.field(#name, &format_args!("{}", #ok))
            })
        }

    } else {
        quote! {
            (if #ident.is_err() {
                f.field(&format_args!("Err({:?})", #ident.as_ref().err().unwrap()))

            } else {
                f.field(&format_args!("{}", #ok))
            })
        }
    }
}

fn implement_err_attr(err: &str, name: Option<&str>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_ok() {
                f.field(#name, &format_args!("Ok({:?})", #ident.as_ref().ok().unwrap()))

            } else {
                f.field(#name, &format_args!("{}", #err))
            })
        }

    } else {
        quote! {
            (if #ident.is_ok() {
                f.field(&format_args!("Ok({:?})", #ident.as_ref().ok().unwrap()))

            } else {
                f.field(&format_args!("{}", #err))
            })
        }
    }
}

