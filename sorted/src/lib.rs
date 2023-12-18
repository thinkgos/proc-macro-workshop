use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as syn::Item);
    expand(args.into(), &item)
        .unwrap_or_else(|e| {
            let mut ts = e.to_compile_error();
            ts.extend(item.to_token_stream());
            ts
        })
        .into()
}

fn expand(_args: TokenStream2, item: &syn::Item) -> syn::Result<TokenStream2> {
    match item {
        syn::Item::Enum(ref em) => {
            let unsorted_variant: Vec<_> = em.variants.iter().collect();
            let mut sorted_variant = unsorted_variant.clone();
            sorted_variant.sort_by(|a, b| a.ident.cmp(&b.ident));

            for (ua, sa) in unsorted_variant.iter().zip(sorted_variant.iter()) {
                if ua.ident != sa.ident {
                    return syn::Result::Err(syn::Error::new(
                        sa.ident.span(),
                        format!("{} should sort before {}", sa.ident, ua.ident),
                    ));
                }
            }

            Ok(item.to_token_stream())
        }
        _ => Err(syn::Error::new(
            Span::call_site(),
            r#"expected enum or match expression"#,
        )),
    }
}

#[proc_macro_attribute]
pub fn check(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_fn = syn::parse_macro_input!(input as syn::ItemFn);
    match do_match_expand(&mut item_fn) {
        Ok(ts) => ts.into(),
        Err(e) => {
            let mut ts = e.to_compile_error();
            ts.extend(item_fn.to_token_stream());
            ts.into()
        }
    }
}

fn do_match_expand(st: &mut syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let mut visitor = MatchVisitor { err: Ok(()) };
    visitor.visit_item_fn_mut(st);
    visitor.err.map(|_| st.to_token_stream())
}

// Path -> AA::BB::CC
fn to_path_string(p: &syn::Path) -> String {
    quote!(#p).to_string().replace(' ', "")
}

// MatchVisitor
struct MatchVisitor {
    err: syn::Result<()>,
}
impl syn::visit_mut::VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, expr: &mut syn::ExprMatch) {
        let mut found = false;
        expr.attrs.retain(|attr| {
            // 删除掉编译器不支持的写在match语句块上面的属性标签
            if to_path_string(attr.path()) == "sorted" {
                found = true;
                false
            } else {
                true
            }
        });

        if found {
            let unsorted_match_arm: syn::Result<Vec<_>> =
                expr.arms.iter().try_fold(Vec::new(), |mut acc, arm| {
                    let (path_str, path): (String, &dyn ToTokens) = match &arm.pat {
                        syn::Pat::Path(p) => (to_path_string(&p.path), &p.path),
                        syn::Pat::TupleStruct(p) => (to_path_string(&p.path), &p.path),
                        syn::Pat::Struct(p) => (to_path_string(&p.path), &p.path),
                        syn::Pat::Ident(p) => (p.ident.to_string(), &p.ident),
                        syn::Pat::Wild(p) => ("_".to_string(), &p.underscore_token),
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &arm.pat,
                                "unsupported by #[sorted]",
                            ));
                        }
                    };

                    acc.push((path_str, path));
                    Ok(acc)
                });
            match unsorted_match_arm {
                Ok(unsorted_match_arm) => {
                    let mut sorted_match_arm = unsorted_match_arm.clone();
                    sorted_match_arm.sort_by(|a, b| a.0.cmp(&b.0));
                    for (a, b) in unsorted_match_arm.iter().zip(sorted_match_arm.iter()) {
                        if a.0 != b.0 {
                            self.err = Err(syn::Error::new_spanned(
                                b.1,
                                format!("{} should sort before {}", b.0, a.0),
                            ));
                            return;
                        }
                    }
                }
                Err(e) => {
                    self.err = Err(e);
                    return;
                }
            }
        }
        // 继续迭代
        visit_mut::visit_expr_match_mut(self, expr)
    }
}
