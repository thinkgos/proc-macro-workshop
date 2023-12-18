use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::buffer::TokenBuffer;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    syn::parse_macro_input!(input as SeqParser)
        .expand()
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}

#[allow(unused)]
#[derive(Debug)]
struct SeqParser {
    variable_ident: syn::Ident,
    start: isize,
    end: isize,
    body: TokenStream2,
}

impl syn::parse::Parse for SeqParser {
    /// parser syntax like:
    /// `N in 0..512 {.......}`
    /// or
    /// `N in 0..=512 {.......}`
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        // 标识符 `N`
        let variable_ident: syn::Ident = input.parse()?;
        // 自定义 `in`
        input.parse::<syn::Token!(in)>()?;
        // 自定义 `0..512` 或 `0..=512`
        let (start, end) = {
            // `0`
            let start: isize = input.parse::<syn::LitInt>()?.base10_parse()?;
            // 自定义`..`
            input.parse::<syn::Token!(..)>()?;
            // `512` 或 `=512`
            let has_equal = input.peek(syn::Token!(=));
            if has_equal {
                input.parse::<syn::Token!(=)>()?;
            }
            let mut end: isize = input.parse::<syn::LitInt>()?.base10_parse()?;
            if has_equal {
                end += 1;
            }
            (start, end)
        };

        // body
        let body: proc_macro2::TokenStream = {
            let body_buf;
            syn::braced!(body_buf in input);
            body_buf.parse()?
        };
        Ok(SeqParser {
            variable_ident,
            start,
            end,
            body,
        })
    }
}

impl SeqParser {
    fn expand(&self) -> syn::Result<TokenStream2> {
        // TokenStream --> TokenBuffer
        let buffer = TokenBuffer::new2(self.body.clone());
        // 区配 `#(xxxxxxxxx)*`
        let (ts, found) = self.find_repeated_expand(buffer.begin());
        if found {
            return Ok(ts);
        }
        let ts = self.normal_expand();
        Ok(ts)
    }
    fn normal_expand(&self) -> TokenStream2 {
        let mut ts = TokenStream2::new();
        self.expand_body(&mut ts, &self.body);
        ts
    }

    fn find_repeated_expand(&self, c: syn::buffer::Cursor) -> (TokenStream2, bool) {
        let mut found = false;
        let mut ts = proc_macro2::TokenStream::new();
        let mut current_cursor = c;
        // syn包提供的Cursor机制，每次都会返回给你一个全新的Cursor，新的Cursor指向新的位置，旧的Cursor指向的位置保持不变
        while !current_cursor.eof() {
            //* 区配前缀 `#(xxxxx)*`` 的 #
            if let Some((repeated_prefix_punct, next_cursor1)) = current_cursor.punct() {
                if repeated_prefix_punct.as_char() == '#' {
                    //* 区配 `#(xxxxx)*`` 的 (xxxx)
                    if let Some((group_cursor, _, next_cursor2)) =
                        next_cursor1.group(proc_macro2::Delimiter::Parenthesis)
                    {
                        //* 区配后缀 `#(xxxxx)*`` 的 *
                        if let Some((repeated_suffix_punct, next_cursor3)) = next_cursor2.punct() {
                            if repeated_suffix_punct.as_char() == '*' {
                                // 匹配成功
                                self.expand_body(&mut ts, &group_cursor.token_stream());
                                // cursor游标向前移动
                                current_cursor = next_cursor3;
                                found = true;
                                continue;
                            }
                        }
                    }
                }
            }

            // `#(xxxxxxxxx)*`没有匹配到，那么就要按照普通代码的各个元素来处理了
            if let Some((group_cursor, _, next_cursor)) =
                current_cursor.group(proc_macro2::Delimiter::Brace)
            {
                let (t, f) = self.find_repeated_expand(group_cursor);
                found = f;
                ts.extend(quote::quote!({#t}));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((group_cursor, _, next_cursor)) =
                current_cursor.group(proc_macro2::Delimiter::Bracket)
            {
                let (t, f) = self.find_repeated_expand(group_cursor);
                found = f;
                ts.extend(quote::quote!([#t]));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((group_cursor, _, next_cursor)) =
                current_cursor.group(proc_macro2::Delimiter::Parenthesis)
            {
                let (t, f) = self.find_repeated_expand(group_cursor);
                found = f;
                ts.extend(quote::quote!((#t)));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((punct, next_cursor)) = current_cursor.punct() {
                ts.extend(quote::quote!(#punct));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((ident, next_cursor)) = current_cursor.ident() {
                ts.extend(quote::quote!(#ident));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((literal, next_cursor)) = current_cursor.literal() {
                ts.extend(quote::quote!(#literal));
                current_cursor = next_cursor;
                continue;
            }
            if let Some((lifetime, next_cur)) = current_cursor.lifetime() {
                // lifetime这种特殊的分类也是用cursor模式来处理的时候特有的，之前`proc_macro2::TokenTree`里面没有定义这个分类
                ts.extend(quote::quote!(#lifetime));
                current_cursor = next_cur;
                continue;
            }
        }
        (ts, found)
    }

    fn expand_body(&self, ts: &mut TokenStream2, body: &TokenStream2) {
        for i in self.start..self.end {
            ts.extend(self.expand_body_item(body, i))
        }
    }
    fn expand_body_item(&self, ts: &proc_macro2::TokenStream, n: isize) -> TokenStream2 {
        let buf = ts.clone().into_iter().collect::<Vec<_>>();
        let mut ts = proc_macro2::TokenStream::new();

        let mut idx = 0;
        while idx < buf.len() {
            let tree_node = &buf[idx];
            // 下面的match中只有Ident对应的分支需要调整
            match tree_node {
                proc_macro2::TokenTree::Group(g) => {
                    let new_stream = self.expand_body_item(&g.stream(), n);
                    let wrap_in_group = proc_macro2::Group::new(g.delimiter(), new_stream);
                    ts.extend(quote::quote!(#wrap_in_group));
                }
                proc_macro2::TokenTree::Ident(ident) => {
                    // 匹配 `~N`
                    if idx + 2 < buf.len() {
                        if let (
                            proc_macro2::TokenTree::Punct(prefix_punct),
                            proc_macro2::TokenTree::Ident(suffix_ident),
                        ) = (&buf[idx + 1], &buf[idx + 2])
                        {
                            // 匹配~N号并且确保是连续的, 无空格
                            if prefix_punct.as_char() == '~'
                                && suffix_ident == &self.variable_ident
                                && ident.span().end() == prefix_punct.span().start()
                                && prefix_punct.span().end() == suffix_ident.span().start()
                            {
                                let new_ident = proc_macro2::Ident::new(
                                    &format!("{}{}", ident, n),
                                    ident.span(),
                                );
                                ts.extend(quote::quote!(#new_ident));
                                idx += 3; // 消耗了3个Token
                                continue;
                            }
                        }
                    }
                    // 当有多个可能冲突的规则时，优先尝试最长的规则
                    if ident == &self.variable_ident {
                        let new_ident = proc_macro2::Literal::i64_unsuffixed(n as i64);
                        ts.extend(quote::quote!(#new_ident));
                    } else {
                        ts.extend(quote::quote!(#tree_node));
                    }
                }
                _ => {
                    ts.extend(quote::quote!(#tree_node));
                }
            }
            idx += 1;
        }
        ts
    }
}
