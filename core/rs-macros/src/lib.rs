extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod gcbor;

mod text {
    pub mod raw_utf8;
}

#[proc_macro_derive(ToGCbor, attributes(gcbor))]
pub fn derive_to_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_to_gcbor(
        &quote!(webar_core),
        &quote!(webar_core::codec::gcbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro_derive(FromGCbor, attributes(gcbor))]
pub fn derive_from_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_from_gcbor(
        &quote!(webar_core),
        &quote!(webar_core::codec::gcbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro_derive(GCborCodec, attributes(gcbor))]
pub fn derive_gcbor(input: TokenStream) -> TokenStream {
    gcbor::derive_gcbor(
        &quote!(webar_core),
        &quote!(webar_core::codec::gcbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro_derive(GCborOrd, attributes(gcbor))]
pub fn derive_gcbor_ord(input: TokenStream) -> TokenStream {
    gcbor::derive_gcbor_ord(
        &quote!(webar_core),
        &quote!(webar_core::codec::gcbor),
        parse_macro_input!(input),
    )
    .into()
}

#[proc_macro]
pub fn raw_utf8_str_has_unassigned(input: TokenStream) -> TokenStream {
    text::raw_utf8::raw_utf8_has_unassigned(parse_macro_input!(input)).into()
}
#[proc_macro]
pub fn raw_utf8_str_is_normalized(input: TokenStream) -> TokenStream {
    text::raw_utf8::raw_utf8_is_normalized(parse_macro_input!(input)).into()
}
