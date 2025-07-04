extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod gcbor;

#[proc_macro_derive(ToGCbor, attributes(gcbor))]
pub fn derive_to_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_to_gcbor(&quote!(webar_core), parse_macro_input!(input)).into()
}

#[proc_macro_derive(ToGCborSelf, attributes(gcbor))]
pub fn derive_to_gcbor_self(input: TokenStream) -> TokenStream {
    gcbor::derive_to_gcbor(&quote!(crate), parse_macro_input!(input)).into()
}

#[proc_macro_derive(FromGCbor, attributes(gcbor))]
pub fn derive_from_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_from_gcbor(&quote!(webar_core), parse_macro_input!(input)).into()
}

#[proc_macro_derive(GCborCodec, attributes(gcbor))]
pub fn derive_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_gcbor(&quote!(webar_core), parse_macro_input!(input)).into()
}

#[proc_macro_derive(GCborOrd, attributes(gcbor))]
pub fn derive_gcbor_ord(input: TokenStream) -> TokenStream {
    gcbor::derive_gcbor_ord(&quote!(webar_core), parse_macro_input!(input)).into()
}
