//! This crate provides the `DebugStub` derive macro.
//!
//! The `DebugStub` derive macro can be used as a drop-in replacement for the
//! standard [`fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html)
//! when certain members of a struct or enum do not or cannot implement the
//! `fmt::Debug` trait themselves, but the containing struct or enum still wants
//! or needs to implement it.
//!
//! ```
//! #[macro_use]
//! extern crate debug_stub_derive;
//!
//! # fn main() {
//!
//! // A struct from an external crate which does not implement the `fmt::Debug`
//! // trait.
//! struct ExternalCrateStruct;
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
//! }), "PubStruct { a: true, b: \"ReplacementValue\" }");
//!
//! // A enum in the current crate which wants to cleanly expose
//! // itself to the outside world with an implementation of `fmt::Debug`.
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
//! )), "VariantA(42, \"ReplacementValue\")");
//!
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
                generate_field_tokens(data.fields())
            ))
        },
        syn::Body::Enum(ref variants) => {
            Ok(implement_enum_debug(
                &ast.ident,
                variants.iter().map(|variant| {
                    generate_enum_variant_tokens(&ast.ident, variant)

                }).collect()
            ))
        }
    }
}

fn generate_field_tokens(fields: &[syn::Field]) -> Vec<quote::Tokens> {
    fields.iter().map(|field| {
        if let Some(ref ident) = field.ident {
            let name = ident.to_string();
            if let Some(value) = extract_value_attr(&field.attrs) {
                quote! {
                    field(#name, &#value)
                }

            } else {
                quote! {
                    field(#name, &self.#ident)
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
        syn::VariantData::Struct(ref fields) => if fields.is_empty() {
            quote! {
                #ident::#variant_ident {} => {
                    f.debug_struct(#variant_name).finish()
                }
            }

        } else {

            let field_names: Vec<&syn::Ident> = fields.iter().map(|field| {
                field.ident.as_ref().unwrap()

            }).collect();

            let field_tokens: Vec<quote::Tokens> = fields.iter().zip(field_names.iter()).map(|(field, ident)| {
                let name = ident.to_string();
                if let Some(value) = extract_value_attr(&field.attrs) {
                    quote! {
                        field(#name, &#value)
                    }

                } else {
                    quote! {
                        field(#name, &#ident)
                    }
                }

            }).collect();

            quote! {
                #ident::#variant_ident{ #(ref #field_names),* } => {
                    f.debug_struct(#variant_name).#(#field_tokens).*.finish()
                }
            }

        },
        syn::VariantData::Tuple(ref fields) => if fields.is_empty() {
            quote! {
                #ident::#variant_ident {} => {
                    f.debug_tuple(#variant_name).finish()
                }
            }

        } else {

            let field_names: Vec<syn::Ident> = fields.iter().enumerate().map(|(index, _)| {
                syn::Ident::new(format!("tuple_{}", index))

            }).collect();

            let field_tokens: Vec<quote::Tokens> = fields.iter().zip(field_names.iter()).map(|(field, ident)| {
                if let Some(value) = extract_value_attr(&field.attrs) {
                    quote! {
                        field(&#value)
                    }

                } else {
                    quote! {
                        field(&#ident)
                    }
                }

            }).collect();

            quote! {
                #ident::#variant_ident( #(ref #field_names),* ) => {
                    f.debug_tuple(#variant_name).#(#field_tokens).*.finish()
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

fn extract_value_attr(attrs: &[syn::Attribute]) -> Option<&str> {
    for attr in attrs {
        if attr.value.name() == "debug_stub" {
            if let syn::MetaItem::NameValue(_, syn::Lit::Str(ref value, _)) = attr.value {
                return Some(value);
            }
        }
    }
    None
}

fn implement_struct_debug(ident: &syn::Ident, tokens: Vec<quote::Tokens>) -> quote::Tokens {

    let name = ident.to_string();
    if tokens.is_empty() {
        quote! {
            use std::fmt;
            impl fmt::Debug for #ident {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.debug_struct(#name).finish()
                }
            }
        }

    } else {
        quote! {
            impl ::std::fmt::Debug for #ident {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    f.debug_struct(#name).#(#tokens).*.finish()
                }
            }
        }
    }

}

fn implement_enum_debug(
    ident: &syn::Ident,
    cases: Vec<quote::Tokens>

) -> quote::Tokens {
    quote! {
        impl ::std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                match *self {
                    #(#cases),*
                }
            }
        }
    }
}

