use darling::FromAttributes;
use proc_macro2::Span;
use syn::{
    parse_quote, punctuated::Punctuated, Arm, Block, DataEnum, DataStruct, DeriveInput, Expr,
    ExprCall, FieldValue, Fields, FieldsNamed, FieldsUnnamed, Ident, ItemFn, ItemImpl, Pat,
    PatIdent, Path, Stmt,
};

use crate::gcbor::{
    generics, rename::camel_to_snake, self_type_name, EnumOptions, FieldInfo, Rename,
    StructOptions, VariantOptions,
};

fn local_def(ident: Ident, expr: Expr) -> Stmt {
    Stmt::Local(syn::Local {
        attrs: Vec::new(),
        let_token: Default::default(),
        pat: Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident,
            subpat: None,
        }),
        init: Some(syn::LocalInit {
            eq_token: Default::default(),
            expr: Box::new(expr),
            diverge: None,
        }),
        semi_token: Default::default(),
    })
}

fn derive_tuple(ty: &Expr, con: Path, s: FieldsUnnamed, decoder: &Ident) -> Block {
    let dec = Ident::new("__dec", Span::call_site());
    let next_field_expr: Expr = parse_quote! {
        internal::decoding::TupleStructDecoder::next_field(&mut #dec)?
    };
    let mut fields_expr = Punctuated::new();
    for _ in s.unnamed.iter() {
        fields_expr.push(next_field_expr.clone());
    }
    let ret = Ident::new("__ret", Span::call_site());
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            {
                let size = s.unnamed.len();
                parse_quote! {
                    let mut #dec = internal::decoding::Decoder::decode_tuple_struct_len(
                        #decoder,
                        #ty,
                        #size
                    )?;
                }
            },
            local_def(
                ret.clone(),
                Expr::Call(ExprCall {
                    attrs: Vec::new(),
                    func: Box::new(parse_quote!(#con)),
                    paren_token: Default::default(),
                    args: fields_expr,
                }),
            ),
            parse_quote! {
                internal::decoding::TupleStructDecoder::end(#dec)?;
            },
            Stmt::Expr(
                parse_quote! {
                    internal::core::result::Result::Ok(#ret)
                },
                None,
            ),
        ]),
    }
}

fn match_fields(name: &Ident, fields: &[FieldInfo<()>]) -> ItemFn {
    let s = Ident::new("__str", Span::call_site());
    let mut arms = fields
        .iter()
        .enumerate()
        .map(|(idx, fi)| {
            let name = &fi.name;
            parse_quote! {
                #name => internal::core::option::Option::Some(#idx),
            }
        })
        .collect::<Vec<_>>();
    arms.push(parse_quote! {
        _ => internal::core::option::Option::None,
    });
    ItemFn {
        attrs: Vec::new(),
        vis: syn::Visibility::Inherited,
        sig: parse_quote! {
            fn #name(#s: &str) -> internal::core::option::Option<usize>
        },
        block: Box::new(Block {
            brace_token: Default::default(),
            stmts: Vec::from([Stmt::Expr(
                Expr::Match(syn::ExprMatch {
                    attrs: Vec::new(),
                    match_token: Default::default(),
                    expr: parse_quote!(#s),
                    brace_token: Default::default(),
                    arms,
                }),
                None,
            )]),
        }),
    }
}
fn derive_record(ty: &Expr, con: Path, fields: FieldsNamed, decoder: &Ident) -> Block {
    let dec = Ident::new("__dec", Span::call_site());
    let field_to_idx = Ident::new("__field_index", Span::call_site());
    let fields = super::sort_fields(&fields, |v| v);
    let mut field_exprs = Punctuated::new();

    for (idx, fi) in fields.iter().enumerate() {
        let name = &fi.name;
        field_exprs.push(FieldValue {
            attrs: Vec::new(),
            member: syn::Member::Named(fi.ident.clone()),
            colon_token: Some(Default::default()),
            expr: if fi.omissible {
                parse_quote! {
                    internal::decoding::StructDecoder::next_omissible_field(
                        &mut #dec,
                        #field_to_idx,
                        #idx,
                        #name
                    )?
                }
            } else {
                parse_quote! {
                    internal::decoding::StructDecoder::next_required_field(
                        &mut #dec,
                        #field_to_idx,
                        #idx,
                        #name
                    )?
                }
            },
        });
    }

    let field_name_buf = Ident::new("__field_name_buf", Span::call_site());
    let ret = Ident::new("__ret", Span::call_site());
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            Stmt::Item(syn::Item::Fn(match_fields(&field_to_idx, &fields))),
            {
                let field_mx = fields.iter().map(|v| v.name.len()).max().unwrap_or(0);
                parse_quote! {
                    let mut #field_name_buf = [0u8; #field_mx];
                }
            },
            parse_quote! {
                let mut #dec = internal::decoding::Decoder::decode_struct(
                    #decoder,
                    #ty,
                    &mut #field_name_buf,
                    #field_to_idx
                )?;
            },
            local_def(
                ret.clone(),
                Expr::Struct(syn::ExprStruct {
                    attrs: Vec::new(),
                    qself: None,
                    path: con,
                    brace_token: Default::default(),
                    fields: field_exprs,
                    dot2_token: None,
                    rest: None,
                }),
            ),
            parse_quote! {
                internal::decoding::StructDecoder::end(#dec)?;
            },
            Stmt::Expr(
                parse_quote! {
                    internal::core::result::Result::Ok(#ret)
                },
                None,
            ),
        ]),
    }
}

fn derive_enum(enum_opt: EnumOptions, e: DataEnum, decoder: &Ident) -> Block {
    let variants =
        e.variants
            .into_iter()
            .map(|v| {
                let opt = VariantOptions::from_attributes(&v.attrs).unwrap();
                let name = opt.rename.unwrap_or_else(|| {
                    match enum_opt.rename_variants.expect(
                        "When rename_variants is missing, all variants must have rename attr",
                    ) {
                        Rename::SnakeCase => camel_to_snake(&v.ident.to_string()),
                    }
                });
                if !name.is_ascii() {
                    panic!("Only ascii character is allowed in variant name: {name:?}");
                }
                (v, name)
            })
            .collect::<Vec<_>>();
    let name_len = variants.iter().map(|v| v.1.len()).max().unwrap_or(0);
    let mut arms = Vec::new();
    let type_name = Ident::new("__type", Span::call_site());
    let type_name_expr = Expr::Path(syn::ExprPath {
        attrs: Vec::new(),
        qself: None,
        path: Path::from(type_name.clone()),
    });

    for (v, name) in variants {
        let con = v.ident;
        arms.push(match v.fields {
            Fields::Unit => Arm {
                attrs: Vec::new(),
                pat: parse_quote! {
                    internal::decoding::Enum::Unit(#name)
                },
                guard: None,
                fat_arrow_token: Default::default(),
                body: Box::new(parse_quote! {
                    internal::core::result::Result::Ok(Self::#con)
                }),
                comma: Some(Default::default()),
            },
            Fields::Unnamed(u) if u.unnamed.len() == 1 => {
                let dec = Ident::new("__var_decoder", Span::call_site());
                Arm {
                    attrs: Vec::new(),
                    pat: parse_quote! {
                        internal::decoding::Enum::Compound(#name, #dec)
                    },
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(parse_quote! {
                        internal::decoding::FromGCbor::decode(#dec).map(Self::#con)
                    }),
                    comma: Some(Default::default()),
                }
            }
            Fields::Unnamed(u) => {
                let dec = Ident::new("__var_decoder", Span::call_site());
                Arm {
                    attrs: Vec::new(),
                    pat: parse_quote! {
                        internal::decoding::Enum::Compound(#name, #dec)
                    },
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(Expr::Block(syn::ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block: derive_tuple(&type_name_expr, parse_quote!(Self::#con), u, &dec),
                    })),
                    comma: None,
                }
            }
            Fields::Named(n) => {
                let dec = Ident::new("__var_decoder", Span::call_site());
                Arm {
                    attrs: Vec::new(),
                    pat: parse_quote! {
                        internal::decoding::Enum::Compound(#name, #dec)
                    },
                    guard: None,
                    fat_arrow_token: Default::default(),
                    body: Box::new(Expr::Block(syn::ExprBlock {
                        attrs: Vec::new(),
                        label: None,
                        block: derive_record(&type_name_expr, parse_quote!(Self::#con), n, &dec),
                    })),
                    comma: None,
                }
            }
        });
    }
    arms.push(parse_quote! {
        __var => internal::core::result::Result::Err(
            internal::decoding::Error::unknown_variant(#type_name_expr, __var)
        ),
    });

    let variant_name_buf = Ident::new("__name_buf", Span::call_site());
    Block {
        brace_token: Default::default(),
        stmts: Vec::from([
            local_def(type_name, self_type_name()),
            parse_quote! {
                let mut #variant_name_buf = [0u8; #name_len];
            },
            Stmt::Expr(
                Expr::Match(syn::ExprMatch {
                    attrs: Vec::new(),
                    match_token: Default::default(),
                    expr: parse_quote! {
                        internal::decoding::Decoder::decode_enum(
                            #decoder,
                            #type_name_expr,
                            &mut #variant_name_buf
                        )?
                    },
                    brace_token: Default::default(),
                    arms,
                }),
                None,
            ),
        ]),
    }
}

fn derive_struct_transparent(s: DataStruct, decoder: &Ident) -> Block {
    match s.fields {
        Fields::Unit => panic!("Transparent derive for unit struct is not allowed"),
        Fields::Unnamed(u) if u.unnamed.len() == 1 => Block {
            brace_token: Default::default(),
            stmts: Vec::from([Stmt::Expr(
                parse_quote! {
                    internal::decoding::FromGCbor::decode(#decoder).map(Self)
                },
                None,
            )]),
        },
        Fields::Named(n) if n.named.len() == 1 => {
            let name = n.named[0].ident.as_ref().unwrap();
            Block {
                brace_token: Default::default(),
                stmts: Vec::from([Stmt::Expr(
                    parse_quote! {
                        internal::decoding::FromGCbor::decode(#decoder).map(|v| Self {
                            #name: v
                        })
                    },
                    None,
                )]),
            }
        }
        _ => panic!("Transparent deriving on struct with more than one field is not supported"),
    }
}

pub fn from_gcbor_impl(input: DeriveInput) -> ItemImpl {
    let decoder = Ident::new("__decoder", Span::call_site());
    let body = match input.data {
        syn::Data::Struct(s) => {
            let opt = StructOptions::from_attributes(&input.attrs).unwrap();
            if opt.transparent {
                derive_struct_transparent(s, &decoder)
            } else {
                match s.fields {
                    Fields::Unit => panic!("Deriving unit struct is not supported"),
                    Fields::Unnamed(u) => {
                        derive_tuple(&self_type_name(), parse_quote!(Self), u, &decoder)
                    }
                    Fields::Named(n) => {
                        derive_record(&self_type_name(), parse_quote!(Self), n, &decoder)
                    }
                }
            }
        }
        syn::Data::Enum(e) => derive_enum(
            EnumOptions::from_attributes(&input.attrs).unwrap(),
            e,
            &decoder,
        ),
        syn::Data::Union(_) => panic!("Deriving union is not supported"),
    };

    let reader = Ident::new("__Reader", Span::call_site());

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
            let (mut params, where_clause) = generics::generics_bounds(
                &syn::TypeParamBound::Trait(syn::TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: None,
                    path: parse_quote! {
                        internal::decoding::FromGCbor<#reader>
                    },
                }),
                input.generics,
            );
            params.push(syn::GenericParam::Type(syn::TypeParam {
                attrs: Vec::new(),
                ident: reader.clone(),
                colon_token: Some(Default::default()),
                bounds: {
                    let mut ret = Punctuated::new();
                    ret.push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: parse_quote! {
                            internal::decoding::Read
                        },
                    }));
                    ret
                },
                eq_token: None,
                default: None,
            }));
            syn::Generics {
                lt_token: Some(Default::default()),
                params,
                gt_token: Some(Default::default()),
                where_clause,
            }
        },
        trait_: Some((
            None,
            parse_quote!(internal::decoding::FromGCbor<#reader>),
            Default::default(),
        )),
        brace_token: Default::default(),
        items: Vec::from([syn::ImplItem::Fn(syn::ImplItemFn {
            attrs: Vec::new(),
            vis: syn::Visibility::Inherited,
            defaultness: None,
            sig: parse_quote! {
                fn decode<'__dec>(
                    #decoder : internal::decoding::Decoder<#reader>
                ) -> internal::core::result::Result<
                    Self,
                    internal::decoding::Error<#reader::Error>
                >
            },
            block: body,
        })]),
    }
}
