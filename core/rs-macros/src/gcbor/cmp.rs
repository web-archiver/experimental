use darling::FromAttributes;
use proc_macro2::Span;
use syn::{parse_quote, Block, Data, DeriveInput, Fields, Ident, ItemImpl, Stmt};

use super::{generics, StructOptions};

pub fn gcbor_ord_impl(input: DeriveInput) -> ItemImpl {
    let other = Ident::new("__other", Span::call_site());
    let body = match input.data {
        Data::Struct(s) => {
            let opt = StructOptions::from_attributes(&input.attrs).unwrap();
            if !opt.transparent {
                panic!("Non transparent deriving is not supported");
            }
            match s.fields {
                Fields::Unit => panic!("transparent deriving unit struct is not supported"),
                Fields::Unnamed(u) if u.unnamed.len() == 1 => Block {
                    brace_token: Default::default(),
                    stmts: Vec::from([Stmt::Expr(
                        parse_quote! {
                            internal::cmp::GCborOrd::cmp_gcbor(&self.0, &#other.0)
                        },
                        None,
                    )]),
                },
                Fields::Named(n) if n.named.len() == 1 => {
                    let field = n.named[0].ident.as_ref().unwrap();
                    Block {
                        brace_token: Default::default(),
                        stmts: Vec::from([Stmt::Expr(
                            parse_quote! {
                                internal::cmp::GCborOrd::cmp_gcbor(&self.#field, &#other.#field)
                            },
                            None,
                        )]),
                    }
                }
                _ => {
                    panic!("transparent deriving struct with more than one field is not supported")
                }
            }
        }
        Data::Enum(_) => panic!("deriving enum is not supported"),
        Data::Union(_) => panic!("deriving union is not supported"),
    };
    ItemImpl {
        attrs: Vec::from([super::auto_derive_attr()]),
        defaultness: None,
        unsafety: None,
        impl_token: Default::default(),
        self_ty: Box::new(generics::instantiate_type(
            input.ident,
            input.generics.params.clone(),
        )),
        generics: {
            let (params, clause) = generics::generics_bounds(
                &syn::TypeParamBound::Trait(syn::TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: None,
                    path: parse_quote!(internal::cmp::GCborOrd),
                }),
                input.generics,
            );
            syn::Generics {
                lt_token: None,
                params,
                gt_token: None,
                where_clause: clause,
            }
        },
        trait_: Some((
            None,
            parse_quote!(internal::cmp::GCborOrd),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([syn::ImplItem::Fn(syn::ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn cmp_gcbor(&self, #other: &Self) -> internal::core::cmp::Ordering
            },
            block: body,
        })]),
    }
}
