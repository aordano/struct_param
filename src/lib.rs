use std::collections::HashSet;

use convert_case::{Case, Casing};
use field_types::FieldType;
use optional_struct::OptionalStruct;
use proc_macro::TokenStream;
use quote::quote;
use serde::{Deserialize, Serialize};
use syn::{
    parse::Parse, parse_macro_input, punctuated::Punctuated, DeriveInput, Ident, MetaList, Token,
};

struct Opts {
    options: HashSet<MetaList>,
}

impl Parse for Opts {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let options = Punctuated::<MetaList, Token![,]>::parse_terminated(input)?;
        Ok(Opts {
            options: options.into_iter().collect(),
        })
    }
}

#[proc_macro_attribute]
pub fn struct_params(metadata: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let opts = parse_macro_input!(metadata as Opts);

    // TODO Parse input

    // TODO Parse Options

    let struct_name = ""; // TODO Get struct name from input
    let optional_struct_name = format!("Optional{}", struct_name);
    let field_type_struct_name = format!("{}FieldType", struct_name);
    let trait_struct_name = format!("ToStruct{}", struct_name);
    let param_struct_name = format!("Param{}", struct_name);
    let visibility = ""; // TODO Get visibility from input
    let field_name = vec![""]; // TODO Get field names from input
    let field_visibility = vec![""]; // TODO Get field visibilities from input
    let field_type = vec!["strip the Option<> if it's an option"]; // TODO get field types from input
    let field_derives = vec![""]; // TODO Get field derives from options
    let converted_field_name: Vec<String> = field_name
        .clone()
        .into_iter()
        .map(|field| field.to_case(Case::Pascal))
        .collect();

    let with_serde_field_ser = vec!["#[serde(skip_serializing_if = \"Option::is_none\")]"];
    let with_serde_field_des = vec!["#[serde(default=None)]"];
    let with_serde_field_serdes =
        vec!["#[serde(skip_serializing_if = \"Option::is_none\"), default=None)]"];
    let with_serde_field = vec!["one of the others"]; // TODO Get correct one from options

    let with_serde_ser = "struct_params::serde::Serialize";
    let with_serde_des = "struct_params::serde::Deserialize";
    let with_serde_serdes = "struct_params::serde::Deserialize, struct_params::serde::Serialize";
    let with_serde = "one of the others"; // TODO Get correct one from options

    let with_clone = "Clone";
    let with_debug = "Debug";
    let with_clone_debug = "Clone, Debug";
    let with_extra = "one of the others"; // TODO Get correct one from options

    let expanded = quote! {
        #[derive(#with_extra, struct_params::optional_struct::OptionalStruct, struct_params::field_types::FieldType, #with_serde)]
        #[optional_derive(#(#field_derives, #with_serde)*)]
        #visibility struct #struct_name {
            #(
                #with_serde_field
                #field_visibility #field_name: Option<#field_type>,
            )*
        }

        impl #struct_name {
            fn default() -> Self {
                #struct_name {
                    #(
                        #field_visibility #field_name: None,
                    )*
                }
            }
        }

        impl #field_type_struct_name {
            fn to_option_struct(self: Self) -> #optional_struct_name {
                let mut params_struct = #optional_struct_name::default();
                match self {
                    #(
                        #field_type_struct_name::#converted_field_name(#field_name) => {
                            params_struct.#field_name = #field_name;
                            params_struct
                        }
                    )*
                }
            }
        }

        #visibility trait #trait_struct_name {
            fn to_struct(self) -> #struct_name;
        }

        #visibility type #param_struct_name = #field_type_struct_name;

        impl #trait_struct_name for Vec<#param_struct_name> {
            fn to_struct(self) -> #struct_name {
                let mut params_struct = #struct_name::default();
                for param in self {
                    params_struct.apply_options(param.to_option_struct());
                }
                params_struct
            }
        }
    };

    TokenStream::from(expanded)
}
