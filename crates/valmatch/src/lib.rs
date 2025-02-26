use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token, Block, Ident, Pat, Result, Token,
};

enum BindingKind {
    Node,
    Value,
}

struct Binding {
    kind: BindingKind,
    name: Ident,
}

struct NodeMatch {
    kind_pat: Pat,
    inputs: Option<Punctuated<GraphPat, Token![,]>>,
}

struct GraphPat {
    binding: Option<Binding>,
    node_match: Option<NodeMatch>,
}

struct ValueMatch {
    pat: GraphPat,
    ctx: Ident,
    val: Ident,
    body: Block,
}

fn parse_binding(input: ParseStream) -> Result<Option<Binding>> {
    let Ok(ident) = input.fork().parse::<Ident>() else {
        return Ok(None);
    };

    let kind = if ident == "node" {
        BindingKind::Node
    } else if ident == "val" {
        BindingKind::Value
    } else {
        return Ok(None);
    };

    input.parse::<Ident>()?;
    let name = input.parse()?;

    Ok(Some(Binding { kind, name }))
}

impl Parse for NodeMatch {
    fn parse(input: ParseStream) -> Result<Self> {
        let kind_pat = Pat::parse_single(input)?;

        let inputs = if input.peek(token::Bracket) {
            let content;
            bracketed!(content in input);
            let inputs = content.parse_terminated(GraphPat::parse, Token![,])?;
            Some(inputs)
        } else {
            None
        };

        Ok(Self { kind_pat, inputs })
    }
}

impl Parse for GraphPat {
    fn parse(input: ParseStream) -> Result<Self> {
        let binding = parse_binding(input)?;
        let node_match = match binding {
            Some(_) => {
                if input.peek(Token![@]) {
                    input.parse::<Token![@]>()?;
                    Some(input.parse()?)
                } else {
                    None
                }
            }
            None => Some(input.parse()?),
        };

        Ok(Self {
            binding,
            node_match,
        })
    }
}

impl Parse for ValueMatch {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token![if]>()?;
        input.parse::<Token![let]>()?;

        let pat = input.parse()?;

        input.parse::<Token![=]>()?;

        let ctx = input.parse()?;

        input.parse::<Token![,]>()?;

        let val = input.parse()?;
        let body = input.parse()?;

        Ok(ValueMatch {
            pat,
            ctx,
            val,
            body,
        })
    }
}

fn fresh_tmp_var(var_idx: &mut usize) -> Ident {
    let var_name = Ident::new(&format!("__{}", *var_idx), Span::mixed_site());
    *var_idx += 1;
    var_name
}

fn build_match_code(
    var_idx: &mut usize,
    pat: GraphPat,
    ctx: &Ident,
    val: &Ident,
    mut body: TokenStream2,
) -> TokenStream2 {
    if let Some(Binding {
        kind: BindingKind::Value,
        name,
    }) = &pat.binding
    {
        body = quote! {
            {
                let #name = #val;
                #body
            }
        };
    };

    let need_node = matches!(
        pat.binding,
        Some(Binding {
            kind: BindingKind::Node,
            ..
        })
    ) || pat.node_match.is_some();

    let node_tmp = if need_node {
        Some(fresh_tmp_var(var_idx))
    } else {
        None
    };

    if let Some(Binding {
        kind: BindingKind::Node,
        name,
    }) = pat.binding
    {
        body = quote! {
            {
                let #name = #node_tmp;
                #body
            }
        };
    }

    if let Some(NodeMatch { kind_pat, inputs }) = pat.node_match {
        let node_tmp = node_tmp.as_ref().unwrap();

        let input_match = match inputs {
            Some(inputs) => {
                let inner_varnames: Vec<_> =
                    inputs.iter().map(|_| fresh_tmp_var(var_idx)).collect();

                let mut inner_match = body;

                for (inner_pat, inner_var) in inputs.into_iter().zip(&inner_varnames) {
                    inner_match = build_match_code(var_idx, inner_pat, ctx, inner_var, inner_match)
                }

                quote! {
                    {
                        let [#(#inner_varnames),*] = #ctx.node_inputs_exact(#node_tmp);
                        #inner_match
                    }
                }
            }
            None => body,
        };

        body = quote! {
            if let #kind_pat = #ctx.node_kind(#node_tmp) #input_match
        };
    }

    if let Some(node_tmp) = node_tmp {
        body = quote! {
            if let (#node_tmp, 0) = #ctx.value_def(#val) {
                #body
            }
        };
    }

    body
}

#[proc_macro]
pub fn match_value(input: TokenStream) -> TokenStream {
    let val_match = parse_macro_input!(input as ValueMatch);

    let mut var_idx = 0;
    build_match_code(
        &mut var_idx,
        val_match.pat,
        &val_match.ctx,
        &val_match.val,
        val_match.body.to_token_stream(),
    )
    .into()
}
