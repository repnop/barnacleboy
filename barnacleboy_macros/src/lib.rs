#![cfg_attr(test, allow(warnings))]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use self::proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::parse::{Parse, ParseStream, Result};
use syn::{parse_macro_input, Expr, ExprClosure, Ident, ItemFn, LitStr, Token};

enum Either<T, U> {
    Left(T),
    Right(U),
}

struct TestArgs {
    name: LitStr,
    setup_fn: ExprClosure,
    test_fn: Either<LitStr, ExprClosure>,
}

impl Parse for TestArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<LitStr>()?;
        input.parse::<Token![,]>()?;
        let mut setup_fn = match input.parse::<Expr>()? {
            Expr::Closure(ec) => ec,
            _ => {
                return Err(syn::parse::Error::new(
                    Span::call_site(),
                    "Not a valid closure",
                ))
            }
        };

        input.parse::<Token![,]>()?;

        let mut test_fn = if input.peek(LitStr) {
            Either::Left(input.parse()?)
        } else {
            Either::Right(match input.parse::<Expr>()? {
                Expr::Closure(ec) => ec,
                _ => {
                    return Err(syn::parse::Error::new(
                        Span::call_site(),
                        "Not a valid closure",
                    ))
                }
            })
        };

        {
            let arg = setup_fn.inputs.iter_mut().next().unwrap();
            let pat = match arg {
                syn::FnArg::Inferred(pat) => pat.clone(),
                _ => unreachable!(),
            };

            let captured = |pat| {
                syn::FnArg::Captured(syn::ArgCaptured {
                    pat,
                    colon_token: syn::token::Colon {
                        spans: [Span::call_site()],
                    },
                    ty: syn::Type::Reference(syn::TypeReference {
                        and_token: syn::token::And {
                            spans: [Span::call_site()],
                        },
                        lifetime: None,
                        mutability: Some(syn::token::Mut {
                            span: Span::call_site(),
                        }),
                        elem: Box::new(syn::Type::Verbatim(syn::TypeVerbatim {
                            tts: TokenStream::from(quote! { SharpLR35902 }).into(),
                        })),
                    }),
                })
            };

            if let Either::Right(test_fn) = &mut test_fn {
                let arg = test_fn.inputs.iter_mut().next().unwrap();
                let pat = match arg {
                    syn::FnArg::Inferred(pat) => pat.clone(),
                    _ => unreachable!(),
                };

                *arg = captured(pat);
            }

            *arg = captured(pat);
        }

        Ok(TestArgs {
            name,
            setup_fn,
            test_fn,
        })
    }
}

#[proc_macro_attribute]
pub fn ins_test(args: TokenStream, input: TokenStream) -> TokenStream {
    let TestArgs {
        name,
        setup_fn,
        test_fn,
    } = parse_macro_input!(args as TestArgs);
    let input = parse_macro_input!(input as ItemFn);
    let fn_ident = input.ident.clone();
    let name = Ident::new(&name.value(), Span::call_site());

    let test_fn = match test_fn {
        Either::Left(id) => quote! {
            assert!(#id(&mut cpu));
        },
        Either::Right(closure) => quote! {
            assert!((#closure)(&mut cpu));
        },
    };

    TokenStream::from(quote! {
        #input

        // AAAAAAAA

        #[test]
        fn #name() {
            let mut dummy = super::cpu_tests::DummyMemoryInterface::default();
            let mut cpu = SharpLR35902::new(&mut dummy);

            (#setup_fn)(&mut cpu);

            #fn_ident(&mut cpu).unwrap();

            #test_fn
        }
    })
}
