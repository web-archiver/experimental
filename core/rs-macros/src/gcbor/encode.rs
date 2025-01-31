use darling::FromAttributes;
use proc_macro2::{Literal, Span};
use quote::format_ident;
use syn::{
    parse_quote, punctuated::Punctuated, Arm, Block, Data, DataEnum, DataStruct, DeriveInput, Expr,
    ExprBinary, ExprBlock, ExprLit, FieldPat, Fields, FieldsNamed, FieldsUnnamed, Ident, ItemImpl,
    Pat, PatIdent, Path, Stmt,
};

use super::{
    generics, rename::camel_to_snake, self_type_name, EnumOptions, FieldInfo, Rename,
    StructOptions, VariantOptions,
};

fn derive_tuple(
    con: Path,
    s: FieldsUnnamed,
    encoder_expr: impl FnOnce(usize) -> Expr,
) -> (Pat, Block) {
    let ser = Ident::new("__enc", Span::call_site());
    let mut stmts = Vec::new();
    let mut pat = Punctuated::new();
    stmts.push({
        let expr = encoder_expr(s.unnamed.len());
        parse_quote! {
            let mut #ser = #expr?;
        }
    });
    for (idx, _) in s.unnamed.iter().enumerate() {
        let var = format_ident!("__f_{idx}");
        stmts.push(parse_quote! {
            internal::encoding::TupleStructEncoder::encode_field(&mut #ser, #var)?;
        });
        pat.push(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: var,
            subpat: None,
        }));
    }
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::TupleStructEncoder::end(#ser)
        },
        None,
    ));
    (
        Pat::TupleStruct(syn::PatTupleStruct {
            attrs: Vec::new(),
            qself: None,
            path: con,
            paren_token: Default::default(),
            elems: pat,
        }),
        Block {
            brace_token: Default::default(),
            stmts,
        },
    )
}
fn record_size(fields: &[FieldInfo<Ident>]) -> Expr {
    let mut required_size = 0;
    let mut omissible_expr = None;

    fn add(l: Expr, r: Expr) -> Expr {
        Expr::Binary(ExprBinary {
            attrs: Vec::new(),
            left: Box::new(l),
            op: syn::BinOp::Add(Default::default()),
            right: Box::new(r),
        })
    }
    for f in fields.iter() {
        if f.omissible {
            let ident = &f.info;
            let e = parse_quote! {
                (internal::core::option::Option::is_some(#ident) as usize)
            };
            match omissible_expr {
                Some(oe) => omissible_expr = Some(add(oe, e)),
                None => omissible_expr = Some(e),
            }
        } else {
            required_size += 1;
        }
    }
    let required_expr = Expr::Lit(ExprLit {
        attrs: Vec::new(),
        lit: syn::Lit::Int(Literal::usize_suffixed(required_size).into()),
    });
    match omissible_expr {
        Some(oe) => add(required_expr, oe),
        None => required_expr,
    }
}
fn derive_record(
    con: Path,
    s: FieldsNamed,
    encoder_expr: impl FnOnce(Expr) -> Expr,
) -> (Pat, Block) {
    let ser = Ident::new("__enc", Span::call_site());
    let mut pat = Punctuated::new();
    let mut stmts = Vec::new();
    let fields = super::sort_fields(&s, |fi| crate::gcbor::FieldInfo {
        ident: fi.ident,
        name: fi.name,
        omissible: fi.omissible,
        info: format_ident!("__f_{}", fi.ident, span = Span::call_site()),
    });
    stmts.push({
        let expr = encoder_expr(record_size(&fields));
        parse_quote! {
            let mut #ser = #expr?;
        }
    });
    for FieldInfo {
        ident,
        name,
        omissible,
        info: var,
    } in fields
    {
        stmts.push(if omissible {
            parse_quote! {
                internal::encoding::StructEncoder::encode_omissible_field(
                    &mut #ser,
                    internal::nf_str!(#name),
                    #var
                )?;
            }
        } else {
            parse_quote! {
                internal::encoding::StructEncoder::encode_field(
                    &mut #ser,
                    internal::nf_str!(#name),
                    #var
                )?;
            }
        });
        pat.push(FieldPat {
            attrs: Vec::new(),
            member: syn::Member::Named(ident.clone()),
            colon_token: Some(Default::default()),
            pat: Box::new(Pat::Ident(PatIdent {
                attrs: Vec::new(),
                by_ref: None,
                mutability: None,
                ident: var,
                subpat: None,
            })),
        });
    }
    stmts.push(Stmt::Expr(
        parse_quote! {
            internal::encoding::StructEncoder::end(#ser)
        },
        None,
    ));
    (
        Pat::Struct(syn::PatStruct {
            attrs: Vec::new(),
            qself: None,
            path: con,
            brace_token: Default::default(),
            fields: pat,
            rest: None,
        }),
        Block {
            brace_token: Default::default(),
            stmts,
        },
    )
}
fn derive_struct(s: DataStruct, encoder: &Ident) -> Block {
    let (pat, block) = match s.fields {
        Fields::Unit => panic!("Deriving unit struct is not supported"),
        Fields::Unnamed(u) => {
            let type_name = self_type_name();
            derive_tuple(parse_quote!(Self), u, |s| {
                parse_quote! {
                    internal::encoding::Encoder::encode_tuple_struct(
                        #encoder,
                        #type_name,
                        #s
                    )
                }
            })
        }
        Fields::Named(n) => {
            let type_name = self_type_name();
            derive_record(parse_quote!(Self), n, |s| {
                parse_quote! {
                    internal::encoding::Encoder::encode_struct(
                        #encoder,
                        #type_name,
                        #s
                    )
                }
            })
        }
    };
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([Stmt::Expr(
            Expr::Match(syn::ExprMatch {
                attrs: Vec::new(),
                match_token: Default::default(),
                expr: parse_quote!(&self),
                brace_token: Default::default(),
                arms: Vec::from([Arm {
                    attrs: Vec::new(),
                    pat,
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(Expr::Block(ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block,
                    })),
                    comma: None,
                }]),
            }),
            None,
        )]),
    }
}
fn derive_enum(e: DataEnum, enum_opt: EnumOptions, encoder: &Ident) -> Block {
    let type_var = Ident::new("__type", Span::call_site());
    let mut arms = Vec::with_capacity(e.variants.len());
    for v in e.variants {
        let opt = VariantOptions::from_attributes(&v.attrs).unwrap();
        let variant_name = opt.rename.unwrap_or_else(|| {
            match enum_opt
                .rename_variants
                .expect("When rename_variants is missing, all variants must have rename")
            {
                Rename::SnakeCase => camel_to_snake(&v.ident.to_string()),
            }
        });
        let ident = v.ident;
        arms.push(match v.fields {
            Fields::Unit => parse_quote! {
                Self::#ident => internal::encoding::Encoder::encode_unit_variant(
                    #encoder,
                    #type_var,
                    internal::nf_str!(#variant_name)
                ),
            },
            Fields::Unnamed(u) if u.unnamed.len() == 1 => parse_quote! {
                Self::#ident(__field) => internal::encoding::Encoder::encode_newtype_variant(
                    #encoder,
                    #type_var,
                    internal::nf_str!(#variant_name),
                    __field
                ),
            },
            Fields::Unnamed(u) => {
                let (pat, b) = derive_tuple(parse_quote!(Self::#ident), u, |s| {
                    parse_quote! {
                        internal::encoding::Encoder::encode_tuple_variant(
                            #encoder,
                            #type_var,
                            internal::nf_str!(#variant_name),
                            #s
                        )
                    }
                });
                Arm {
                    attrs: Vec::new(),
                    pat,
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(Expr::Block(syn::ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block: b,
                    })),
                    comma: None,
                }
            }
            Fields::Named(n) => {
                let (pat, b) = derive_record(parse_quote!(Self::#ident), n, |s| {
                    parse_quote! {
                        internal::encoding::Encoder::encode_struct_variant(
                            #encoder,
                            #type_var,
                            internal::nf_str!(#variant_name),
                            #s
                        )
                    }
                });
                Arm {
                    attrs: Vec::new(),
                    pat,
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(Expr::Block(ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block: b,
                    })),
                    comma: None,
                }
            }
        })
    }
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            {
                let expr = self_type_name();
                parse_quote! {
                    let #type_var = #expr;
                }
            },
            Stmt::Expr(
                Expr::Match(syn::ExprMatch {
                    attrs: Vec::new(),
                    match_token: Default::default(),
                    expr: parse_quote!(&self),
                    brace_token: Default::default(),
                    arms,
                }),
                None,
            ),
        ]),
    }
}
fn derive_struct_transparent(s: DataStruct, encoder: &Ident) -> Block {
    match s.fields {
        Fields::Named(n) if n.named.len() == 1 => {
            let name = n.named[0].ident.as_ref().unwrap();
            Block {
                brace_token: Default::default(),
                stmts: Vec::from([Stmt::Expr(
                    parse_quote! {
                        internal::encoding::ToGCbor::encode(&self.#name, #encoder)
                    },
                    None,
                )]),
            }
        }
        Fields::Unnamed(u) if u.unnamed.len() == 1 => Block {
            brace_token: Default::default(),
            stmts: Vec::from([Stmt::Expr(
                parse_quote! {
                    internal::encoding::ToGCbor::encode(&self.0, #encoder)
                },
                None,
            )]),
        },
        Fields::Unit => panic!("Transparent derive on unit struct is not allowed"),
        _ => panic!("Transparent derive on struct with more than one field is not supported"),
    }
}

pub fn to_gcbor_impl(input: DeriveInput) -> ItemImpl {
    let encoder = Ident::new("__encoder", Span::call_site());
    let body = match input.data {
        Data::Struct(s) => {
            let opt = StructOptions::from_attributes(&input.attrs).unwrap();
            if opt.transparent {
                derive_struct_transparent(s, &encoder)
            } else {
                derive_struct(s, &encoder)
            }
        }
        Data::Enum(e) => derive_enum(
            e,
            EnumOptions::from_attributes(&input.attrs).unwrap(),
            &encoder,
        ),
        Data::Union(_) => panic!("Deriving union is not supported"),
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
                    path: parse_quote!(internal::encoding::ToGCbor),
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
            parse_quote!(internal::encoding::ToGCbor),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([syn::ImplItem::Fn(syn::ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn encode<'__enc, __W: internal::encoding::Write>(
                    &self,
                    #encoder: internal::encoding::Encoder<'__enc, __W>
                ) -> internal::core::result::Result<
                    (),
                    internal::encoding::Error<__W::Error>
                >
            },
            block: body,
        })]),
    }
}
