use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_quote, Expr, ExprLit, Lit, Meta, MetaNameValue};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let di = syn::parse_macro_input!(input as syn::DeriveInput);
    expand(&di).unwrap_or_else(|e| e.to_compile_error()).into()
}

type StructFields = syn::punctuated::Punctuated<syn::Field, syn::Token!(,)>;

fn expand(di: &syn::DeriveInput) -> syn::Result<TokenStream2> {
    let st_ident = &di.ident;
    let st_ident_literal = di.ident.to_string();
    let st_fields = struct_fields(di)?;
    let st_debug_field = gen_st_debug_field(st_fields)?;
    let st_generics = gen_generics(&di.generics, st_fields)?;

    let (impl_generics, type_generics, where_clause) = st_generics.split_for_impl();
    Ok(quote! {
        impl #impl_generics std::fmt::Debug for #st_ident #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#st_ident_literal)
                #(#st_debug_field)*
                 .finish()
            }
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

fn gen_generics(
    st_generics: &syn::Generics,
    st_fields: &StructFields,
) -> syn::Result<syn::Generics> {
    let mut st_generics = st_generics.clone();
    let mut field_type_ident = HashSet::new();
    let mut phantomdata_ident = HashSet::new();
    for field in st_fields {
        if let Some(ft) = get_field_type(&field.ty)? {
            field_type_ident.insert(ft.ident);
            if ft.ident == "PhantomData" {
                if let Some(gi) = ft.generic_ident {
                    phantomdata_ident.insert(gi);
                }
            }
        }
        // if let Some(tt) = get_type_ident(&field.ty)? {
        //     field_type_ident.insert(tt);
        // }
        // if let Some(tt) = get_phantom_data_type_generic_ident(&field.ty)? {
        //     phantomdata_ident.insert(tt);
        // }
    }

    // 我们需要对泛型参数添加一个`Debug` Trait限定(如果需要)
    let mut params = st_generics.params.iter_mut();
    while let Some(syn::GenericParam::Type(t)) = params.next() {
        let type_param_name = &t.ident;
        // 如果是PhantomData，就不要对泛型参数`T`本身再添加约束了,除非`T`本身也被直接使用了
        if !phantomdata_ident.contains(type_param_name)
            || field_type_ident.contains(type_param_name)
        {
            t.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    Ok(st_generics)
}

fn gen_st_debug_field(fields: &StructFields) -> syn::Result<Vec<TokenStream2>> {
    fields
        .iter()
        .map(|field| {
            let ident = field.ident.as_ref();
            let ident_literal = ident.unwrap().to_string();
            let lit = get_attribute_lit(field)?;
            let format_str = if let Some(s) = lit {
                s.value()
            } else {
                "{:?}".to_owned()
            };
            Ok(quote!(
                .field(#ident_literal, &std::format_args!(#format_str, &self.#ident) )
            ))
        })
        .collect()
}

fn get_attribute_lit(field: &syn::Field) -> syn::Result<Option<&syn::LitStr>> {
    for attr in &field.attrs {
        if attr.path().is_ident("debug") {
            if let Meta::NameValue(MetaNameValue {
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(ref lit),
                        ..
                    }),
                ..
            }) = attr.meta
            {
                return Ok(Some(lit));
            }
        }
    }
    Ok(None)
}

/// FieldType 字段类型
/// a: XXX 或 a: XXX<T> 中的XXX和T
struct FieldType<'a> {
    ident: &'a syn::Ident,
    generic_ident: Option<&'a syn::Ident>,
}

/// 获取FieldType
/// a: XXX 或 a: XXX<T> 中的XXX和T
fn get_field_type(ty: &syn::Type) -> syn::Result<Option<FieldType>> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        if let Some(syn::PathSegment {
            ref ident,
            ref arguments,
        }) = segments.last()
        {
            let mut generic_ident = None;
            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = arguments
            {
                if let Some(syn::GenericArgument::Type(syn::Type::Path(ref gp))) = args.first() {
                    if let Some(gi) = gp.path.segments.first() {
                        generic_ident = Some(&gi.ident);
                    }
                }
            }
            return Ok(Some(FieldType {
                ident,
                generic_ident,
            }));
        }
    }
    Ok(None)
}
