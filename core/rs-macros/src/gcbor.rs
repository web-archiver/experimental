use darling::{FromAttributes, FromMeta};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_quote, punctuated::Punctuated, Attribute, DeriveInput, Expr, Ident, Path, PathArguments,
    PathSegment,
};

#[derive(Clone, Copy, FromMeta)]
enum Rename {
    SnakeCase,
}

#[derive(FromAttributes)]
#[darling(attributes(gcbor))]
struct FieldOptions {
    #[darling(default)]
    rename: Option<String>,
    #[darling(default)]
    omissible: bool,
}

#[derive(FromAttributes)]
#[darling(attributes(gcbor))]
struct StructOptions {
    #[darling(default)]
    transparent: bool,
}

#[derive(Default, FromAttributes)]
#[darling(attributes(gcbor))]
struct VariantOptions {
    #[darling(default)]
    rename: Option<String>,
}

#[derive(FromAttributes)]
#[darling(attributes(gcbor))]
struct EnumOptions {
    #[darling(default)]
    rename_variants: Option<Rename>,
}

mod cmp;
mod decode;
mod encode;
mod generics;
mod rename;

struct FieldInfo<'a, T> {
    ident: &'a Ident,
    name: String,
    omissible: bool,
    info: T,
}
fn sort_fields<'a, T>(
    fields: &'a syn::FieldsNamed,
    info_fn: impl Fn(FieldInfo<'a, ()>) -> FieldInfo<'a, T>,
) -> Vec<FieldInfo<'a, T>> {
    let mut ret = fields
        .named
        .iter()
        .map(|f| {
            let opt = FieldOptions::from_attributes(&f.attrs).unwrap();
            let ident = f.ident.as_ref().unwrap();
            let name = opt.rename.unwrap_or_else(|| ident.to_string());
            if !name.is_ascii() {
                panic!("Only ascii character is allowed in field name: {name:?}");
            }
            info_fn(FieldInfo {
                ident,
                name,
                omissible: opt.omissible,
                info: (),
            })
        })
        .collect::<Vec<_>>();
    ret.sort_by(|l, r| webar_core_internal::cmp_cbor_str(&l.name, &r.name));
    ret
}
fn self_type_name() -> Expr {
    parse_quote! {
        internal::TypeInfo::new::<Self>()
    }
}
fn auto_derive_attr() -> Attribute {
    Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        meta: syn::Meta::Path(Path {
            leading_colon: None,
            segments: {
                let mut ret = Punctuated::new();
                ret.push(PathSegment {
                    ident: Ident::new("automatically_derived", proc_macro2::Span::call_site()),
                    arguments: PathArguments::None,
                });
                ret
            },
        }),
    }
}
fn wrap<T: quote::ToTokens>(base_mod: &TokenStream, inputs: &[T]) -> TokenStream {
    struct Many<'a, T>(&'a [T]);
    impl<'a, T: quote::ToTokens> quote::ToTokens for Many<'a, T> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            for i in self.0 {
                quote::ToTokens::to_tokens(i, tokens)
            }
        }
    }
    let t = Many(inputs);

    quote! {
        const _ : () =  {
            use #base_mod::codec::gcbor::internal;

            #t
        };
    }
}

pub fn derive_to_gcbor(base_mod: &TokenStream, input: DeriveInput) -> TokenStream {
    wrap(base_mod, &[encode::to_gcbor_impl(input)])
}

pub fn derive_from_gcbor(base_mod: &TokenStream, input: DeriveInput) -> TokenStream {
    wrap(base_mod, &[decode::from_gcbor_impl(input)])
}
pub fn derive_gcbor(base_mod: &TokenStream, input: DeriveInput) -> TokenStream {
    wrap(
        base_mod,
        &[
            encode::to_gcbor_impl(input.clone()),
            decode::from_gcbor_impl(input),
        ],
    )
}

pub fn derive_gcbor_ord(base_mod: &TokenStream, input: DeriveInput) -> TokenStream {
    wrap(base_mod, &[cmp::gcbor_ord_impl(input)])
}
