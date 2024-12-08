use syn::{
    punctuated::Punctuated, token::Comma, AngleBracketedGenericArguments, ConstParam, Expr,
    ExprPath, GenericArgument, GenericParam, Generics, Ident, LifetimeParam, Path, PathArguments,
    PathSegment, PredicateLifetime, PredicateType, Type, TypeParam, TypeParamBound, TypePath,
    WhereClause, WherePredicate,
};

fn ident_path(ident: Ident) -> Path {
    Path {
        leading_colon: None,
        segments: {
            let mut ret = Punctuated::new();
            ret.push(PathSegment {
                ident,
                arguments: PathArguments::None,
            });
            ret
        },
    }
}
fn infer_impl_generics(
    bound: &TypeParamBound,
    orig_param: Punctuated<GenericParam, Comma>,
    orig_clause: Option<WhereClause>,
) -> (
    Punctuated<GenericParam, Comma>,
    Punctuated<WherePredicate, Comma>,
) {
    let mut params = Punctuated::new();
    let mut predicates = Punctuated::new();
    for t in orig_param {
        match t {
            GenericParam::Const(c) => params.push(GenericParam::Const(ConstParam {
                attrs: Vec::new(),
                const_token: Default::default(),
                ident: c.ident,
                colon_token: Default::default(),
                ty: c.ty,
                eq_token: None,
                default: None,
            })),
            GenericParam::Lifetime(l) => {
                if !l.bounds.is_empty() {
                    predicates.push(WherePredicate::Lifetime(PredicateLifetime {
                        lifetime: l.lifetime.clone(),
                        colon_token: Default::default(),
                        bounds: l.bounds,
                    }));
                }
                params.push(GenericParam::Lifetime(LifetimeParam {
                    attrs: Vec::new(),
                    lifetime: l.lifetime,
                    colon_token: None,
                    bounds: Punctuated::new(),
                }));
            }
            GenericParam::Type(t) => {
                predicates.push(WherePredicate::Type(PredicateType {
                    lifetimes: None,
                    bounded_ty: Type::Path(TypePath {
                        qself: None,
                        path: ident_path(t.ident.clone()),
                    }),
                    colon_token: Default::default(),
                    bounds: {
                        let mut ret = t.bounds.clone();
                        ret.push(bound.clone());
                        ret
                    },
                }));
                params.push(GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: t.ident,
                    colon_token: None,
                    bounds: Punctuated::new(),
                    eq_token: None,
                    default: None,
                }));
            }
        }
    }
    if let Some(w) = orig_clause {
        predicates.extend(w.predicates);
    }
    (params, predicates)
}

pub fn generics_bounds(
    bound: &TypeParamBound,
    input: Generics,
) -> (Punctuated<GenericParam, Comma>, Option<WhereClause>) {
    let (param, predicates) = infer_impl_generics(bound, input.params, input.where_clause);
    (
        param,
        if predicates.is_empty() {
            None
        } else {
            Some(WhereClause {
                where_token: Default::default(),
                predicates,
            })
        },
    )
}

pub fn instantiate_type(ident: Ident, params: Punctuated<GenericParam, Comma>) -> Type {
    let mut segments = Punctuated::new();
    segments.push(PathSegment {
        ident,
        arguments: if params.is_empty() {
            PathArguments::None
        } else {
            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Default::default(),
                args: params
                    .into_iter()
                    .map(|p| match p {
                        GenericParam::Const(c) => GenericArgument::Const(Expr::Path(ExprPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: ident_path(c.ident),
                        })),
                        GenericParam::Lifetime(l) => GenericArgument::Lifetime(l.lifetime),
                        GenericParam::Type(t) => GenericArgument::Type(Type::Path(TypePath {
                            qself: None,
                            path: ident_path(t.ident),
                        })),
                    })
                    .collect(),
                gt_token: Default::default(),
            })
        },
    });
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments,
        },
    })
}
