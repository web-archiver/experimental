use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::LitStr;
use webar_core_internal::text::raw_utf8::{has_unassigned, is_normalized};

pub fn raw_utf8_has_unassigned(input: LitStr) -> TokenStream {
    has_unassigned(&input.value()).to_token_stream()
}
pub fn raw_utf8_is_normalized(input: LitStr) -> TokenStream {
    is_normalized(&input.value()).to_token_stream()
}
