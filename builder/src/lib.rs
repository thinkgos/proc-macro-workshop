use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{punctuated::Punctuated, spanned::Spanned, Expr, ExprLit, Lit, MetaNameValue, Token};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let di = syn::parse_macro_input!(input as syn::DeriveInput);
    expand(&di).unwrap_or_else(|e| e.to_compile_error()).into()
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

fn expand(di: &syn::DeriveInput) -> syn::Result<TokenStream2> {
    let st_ident = &di.ident;
    let st_builder_ident = format_ident!("{}Builder", st_ident);
    let st_fields = struct_fields(di)?;
    let st_builder_field_define = gen_st_builder_field_define(st_fields)?;
    let st_builder_field_init = gen_st_builder_field_init(st_fields)?;
    let st_builder_fn_setter = gen_st_builder_fn_setter(st_fields)?;
    let st_builder_fn = gen_st_builder_fn(st_fields, st_ident)?;

    Ok(quote! {
        pub struct #st_builder_ident {
            #(#st_builder_field_define),*
        }
        impl #st_ident {
            pub fn builder() -> #st_builder_ident {
                #st_builder_ident{
                    #(#st_builder_field_init),*
                }
            }
        }
        impl #st_builder_ident {
            #(#st_builder_fn_setter)*
            #st_builder_fn
        }
    })
}

fn struct_fields(di: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = di.data
    {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(
        di,
        "Must define on a Struct, not Enum",
    ))
}

fn gen_st_builder_field_define(fields: &StructFields) -> syn::Result<Vec<TokenStream2>> {
    fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = get_inner_type(&f.ty).unwrap_or(InnerType::Opt(&f.ty));
            match ty {
                InnerType::Vec(t) => Ok(quote! {
                    #ident: std::vec::Vec<#t>
                }),
                InnerType::Opt(t) => Ok(quote! {
                    #ident: std::option::Option<#t>
                }),
            }
        })
        .collect()
}

fn gen_st_builder_field_init(fields: &StructFields) -> syn::Result<Vec<TokenStream2>> {
    fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = get_inner_type(&f.ty);
            match ty {
                Some(InnerType::Vec(_)) => Ok(quote! {
                    #ident: std::vec::Vec::new()
                }),
                _ => Ok(quote! {
                    #ident: std::option::Option::None
                }),
            }
        })
        .collect()
}

fn gen_st_builder_fn_setter(fields: &StructFields) -> syn::Result<Vec<TokenStream2>> {
    fields
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let ty = get_inner_type(&f.ty).unwrap_or(InnerType::Opt(&f.ty));
            match ty {
                InnerType::Vec(t) => {
                    let tty = &f.ty;
                    // 原始
                    let mut ts = quote! {
                        fn #ident(&mut self, #ident: #tty) -> &mut Self {
                            self.#ident = #ident.clone();
                            self
                        }
                    };

                    let attr_ident = get_attribute_ident_for_vec(f)?;
                    // 是否同名
                    if let Some(ref special_ident) = attr_ident {
                        if special_ident != ident.as_ref().unwrap() {
                            ts.extend(quote! {
                                fn #special_ident(&mut self, #ident: #t) -> &mut Self {
                                    self.#ident.push(#ident);
                                    self
                                }
                            });
                        }
                    }
                    Ok(ts)
                }
                InnerType::Opt(ty) => Ok(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = std::option::Option::Some(#ident);
                        self
                    }
                }),
            }
        })
        .collect()
}

fn gen_st_builder_fn(
    fields: &StructFields,
    struct_ident: &syn::Ident,
) -> syn::Result<TokenStream2> {
    let (st_field_checker, st_field_result) = fields.iter().fold(
        (Vec::new(), Vec::new()),
        |(mut field_checker, mut field_result), f| {
            let ident = &f.ident;
            let ty = get_inner_type(&f.ty);
            if ty.is_some() {
                field_result.push(quote! {
                    #ident: self.#ident.clone()
                });
            } else {
                field_checker.push(quote! {
                    if self.#ident.is_none() {
                        let err = format!("{} field missing", stringify!(#ident));
                        return std::result::Result::Err(err.into())
                    }
                });
                field_result.push(quote! {
                    #ident: self.#ident.clone().unwrap()
                });
            }
            (field_checker, field_result)
        },
    );

    let ts = quote! {
        pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
            #(#st_field_checker)*

            std::result::Result::Ok(#struct_ident{
                #(#st_field_result),*
            })
        }
    };
    Ok(ts)
}

enum InnerType<'a> {
    /// std::vec::Vec<T>
    Vec(&'a syn::Type),
    // std::option::Option<T>
    Opt(&'a syn::Type),
}

// 这里我们取segments的最后一节来判断是不是`Option<T>`, `Vec<T>`
// 这样如果用户写的是`std::option::Option<T>`或 `std::vec::Vec<T>`我们也能识别, 否则直接用本身
fn get_inner_type(ty: &syn::Type) -> Option<InnerType> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" || seg.ident == "Vec" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    ref args,
                    ..
                }) = seg.arguments
                {
                    if let Some(syn::GenericArgument::Type(t)) = args.first() {
                        return match seg.ident.to_string().as_str() {
                            "Vec" => Some(InnerType::Vec(t)),
                            "Option" => Some(InnerType::Opt(t)),
                            _ => None,
                        };
                    }
                }
            }
        }
    }
    None
}
/// 格式为 #[builder(each = "arg")]
fn get_attribute_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if attr.path().is_ident("builder") {
            let meta =
                attr.parse_args_with(Punctuated::<MetaNameValue, Token![,]>::parse_terminated)?;
            if let Some(mm) = meta.first() {
                if mm.path.is_ident("each") {
                    if let Expr::Lit(ExprLit {
                        lit: Lit::Str(ref lit),
                        ..
                    }) = mm.value
                    {
                        return Ok(Some(syn::Ident::new(lit.value().as_str(), attr.span())));
                    };
                } else {
                    return Err(syn::Error::new_spanned(
                        &attr.meta,
                        r#"expected `builder(each = "...")`"#,
                    ));
                }
            }
        }
    }
    Ok(None)
}
