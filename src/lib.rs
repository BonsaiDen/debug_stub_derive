// Copyright (c) 2017 Ivo Wetzel

// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

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
//! use debug_stub_derive::DebugStub;
//!
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
//! ```
//!
//! Using `DebugStub` with enums:
//!
//! ```
//! # use debug_stub_derive::DebugStub;
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
//! ```
//!
//! Using `DebugStub` with `Option` and `Result` types:
//!
//! ```
//! # use debug_stub_derive::DebugStub;
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
//! }), "PubStruct { a: Some(ReplacementSomeValue), b: Ok(ReplacementOkValue) }");
//! ```
#![deny(
    trivial_casts, trivial_numeric_casts,
    unsafe_code,
    unused_import_braces, unused_qualifications
)]


// Crate Dependencies ---------------------------------------------------------
extern crate proc_macro;
#[macro_use] extern crate quote;


// External Dependencies ------------------------------------------------------
use proc_macro::TokenStream;


/// Implementation of the `#[derive(DebugStub)]` derive macro.
#[proc_macro_derive(DebugStub, attributes(debug_stub))]
pub fn derive_debug_stub(input: TokenStream) -> TokenStream {
    let input = syn::parse_derive_input(&input.to_string()).expect("Parse derive input failed.");
    match expand_derive_serialize(&input) {
        Ok(expanded) => expanded.parse().expect("Parse expansion failed."),
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
            let (_, tokens) = extract_value_attr(&ident, &field.attrs, Some(name));
            tokens

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

            let (idents, tokens) = generate_enum_variant_fields(fields.iter().map(|field| {
                let ident = field.ident.clone().expect("Tuple struct variant has unnamed fields");
                (ident.clone(), &field.attrs, Some(ident.to_string()))

            }).collect());

            quote! {
                #ident::#variant_ident { #(#idents),* } => {
                    let mut f = f.debug_struct(#variant_name);
                    #(#tokens);*;
                    f.finish()
                }
            }

        },
        syn::VariantData::Tuple(ref fields) => {

            let (idents, tokens) = generate_enum_variant_fields(fields.iter().enumerate().map(|(index, field)| {
                (syn::Ident::new(format!("tuple_{}", index)), &field.attrs, None)

            }).collect());

            quote! {
                #ident::#variant_ident( #(#idents),* ) => {
                    let mut f = f.debug_tuple(#variant_name);
                    #(#tokens);*;
                    f.finish()
                }
            }

        },
        syn::VariantData::Unit => {
            quote! {
                #ident::#variant_ident => {
                    f.debug_struct(#variant_name).finish()
                }
            }
        }
    }

}

fn generate_enum_variant_fields(
    fields: Vec<(syn::Ident, &Vec<syn::Attribute>, Option<String>)>

) -> (Vec<quote::Tokens>, Vec<quote::Tokens>) {

    let mut idents = Vec::new();
    let mut unused_fields = false;

    let tokens: Vec<quote::Tokens> = fields.into_iter().map(|(ident, attrs, name)| {

        let unnamed = name.is_none();
        let (ident_used, tokens) = extract_value_attr(&ident, attrs, name);

        if ident_used {
            idents.push(quote! { ref #ident });

        // Skip unused tuple fields to avoid "unused variable" warnings
        } else if unnamed {
            idents.push(quote! { _ });

        // Skip unused struct fields to avoid "unused variable" warnings
        } else {
            unused_fields = true;
        }

        tokens

    }).collect();

    if unused_fields {
        idents.push(quote! { .. });
    }

    (idents, tokens)

}

fn extract_value_attr(
    ident: &syn::Ident,
    attrs: &[syn::Attribute],
    name: Option<String>

) -> (bool, quote::Tokens) {

    for attr in attrs {
        if attr.value.name() != "debug_stub" {
            continue;

        } else if let syn::MetaItem::NameValue(_, syn::Lit::Str(ref value, _)) = attr.value {
            return (false, implement_replace_attr(name, value));

        } else if let syn::MetaItem::List(_, ref items) = attr.value {
            match extract_named_value_attrs(items) {
                (_, _, Some(some)) => return (true, implement_some_attr(some, name, ident)),
                (Some(ok), Some(err), None) => return (true, implement_result_attr(ok, err, name, ident)),
                (Some(ok), None, None) => return (true, implement_ok_attr(ok, name, ident)),
                (None, Some(err), None) => return (true, implement_err_attr(err, name, ident)),
                _ => {}
            }
        }
    }

    if let Some(name) = name {
        (true, quote! { f.field(#name, &#ident) })

    } else {
        (true, quote! { f.field(&#ident) })
    }

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

fn implement_replace_attr(name: Option<String>, value: &str) -> quote::Tokens {
    field_block!(name, &format_args!("{}", #value))
}

fn implement_some_attr(some: &str, name: Option<String>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_some() {
                f.field(#name, &Some::<_>(format_args!("{}", #some)))

            } else {
                f.field(#name, &format_args!("None"))
            })
        }

    } else {
        quote! {
            (if #ident.is_some() {
                f.field(&Some::<_>(format_args!("{}", #some)))

            } else {
                f.field(&format_args!("None"))
            })
        }
    }
}

fn implement_result_attr(ok: &str, err: &str, name: Option<String>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_ok() {
                f.field(#name, &Ok::<_, ()>(format_args!("{}", #ok)))

            } else {
                f.field(#name, &Err::<(), _>(format_args!("{}", #err)))
            })
        }

    } else {
        quote! {
            (if #ident.is_ok() {
                f.field(&Ok::<_, ()>(format_args!("{}", #ok)))

            } else {
                f.field(&Err::<(), _>(format_args!("{}", #err)))
            })
        }
    }
}

fn implement_ok_attr(ok: &str, name: Option<String>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_err() {
                f.field(#name, &Err::<(), _>(#ident.as_ref().err().unwrap()))

            } else {
                f.field(#name, &Ok::<_, ()>(format_args!("{}", #ok)))
            })
        }

    } else {
        quote! {
            (if #ident.is_err() {
                f.field(&Err::<(), _>(#ident.as_ref().err().unwrap()))

            } else {
                f.field(&Ok::<_, ()>(format_args!("{}", #ok)))
            })
        }
    }
}

fn implement_err_attr(err: &str, name: Option<String>, ident: &syn::Ident) -> quote::Tokens {
    if let Some(name) = name {
        quote! {
            (if #ident.is_ok() {
                f.field(#name, &Ok::<_, ()>(#ident.as_ref().ok().unwrap()))

            } else {
                f.field(#name, &Err::<(), _>(format_args!("{}", #err)))
            })
        }

    } else {
        quote! {
            (if #ident.is_ok() {
                f.field(&Ok::<_, ()>(#ident.as_ref().ok().unwrap()))

            } else {
                f.field(&Err::<(), _>(format_args!("{}", #err)))
            })
        }
    }
}

