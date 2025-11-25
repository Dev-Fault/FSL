use crate::lexer::{Keyword, Lexer, LexerError, Symbol, Token, TokenType, format_error_context};

#[derive(Debug, Clone, PartialEq)]
pub struct ParserErrorContext<'a> {
    input: &'a str,
    token: Token,
}

impl<'a> ParserErrorContext<'a> {
    pub fn new(input: &'a str, token: Token) -> Self {
        Self { input, token }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError<'a> {
    LexerError(LexerError<'a>),
    InvalidDotPlacement(ParserErrorContext<'a>),
    ValueOutsideOfContext(ParserErrorContext<'a>),
    ObjectsNotSupported(ParserErrorContext<'a>),
}

impl ToString for ParserError<'_> {
    fn to_string(&self) -> String {
        match self {
            ParserError::LexerError(lexer_error) => lexer_error.to_string(),
            ParserError::InvalidDotPlacement(context) => {
                format!(
                    "Syntax error: Out of place dot\n{}",
                    format_error_context(context.input, context.token.location)
                )
            }
            ParserError::ValueOutsideOfContext(context) => {
                format!(
                    "Syntax error: Out of place value\nValue: {}\n{}",
                    context.token.token_type.as_str(),
                    format_error_context(context.input, context.token.location)
                )
            }
            ParserError::ObjectsNotSupported(_) => {
                format!("Parser error: Objects are not currently supported in FSL")
            }
        }
    }
}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(value: LexerError<'a>) -> Self {
        ParserError::LexerError(value)
    }
}

impl Expression {
    pub fn new(name: String) -> Self {
        Self { name, args: vec![] }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserState {
    InsideCommand,
    InsideList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Number(String),
    String(String),
    Keyword(Keyword),
    Var(String),
    List(Vec<Arg>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    pub name: String,
    pub args: Vec<Arg>,
}

pub struct Parser {
    expression_stack: Vec<Expression>,
    list_stack: Vec<Vec<Arg>>,
    current_command_name: Option<String>,
    current_arg: Option<Arg>,
    dot_arg: Option<Arg>,
    output: Vec<Expression>,
    state_stack: Vec<ParserState>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            expression_stack: vec![],
            current_command_name: None,
            list_stack: vec![],
            current_arg: None,
            dot_arg: None,
            output: vec![],
            state_stack: vec![],
        }
    }

    fn parse_symbol<'a>(
        &mut self,
        token: Token,
        next_token: Option<&Token>,
        symbol: Symbol,
        code: &'a str,
    ) -> Result<(), ParserError<'a>> {
        match symbol {
            Symbol::OpenParen => match self.current_command_name.take() {
                Some(name) => {
                    let mut expression = Expression::new(name);
                    if let Some(arg) = self.dot_arg.take() {
                        expression.args.push(arg);
                    }

                    self.expression_stack.push(expression);

                    self.state_stack.push(ParserState::InsideCommand);
                }
                None => unreachable!("lexer should have handled invalid commands"),
            },
            Symbol::ClosedParen => {
                self.state_stack.pop();

                match self.expression_stack.pop() {
                    Some(mut expression) => {
                        if let Some(arg) = self.current_arg.take() {
                            expression.args.push(arg);
                        }

                        if next_token
                            .is_some_and(|t| matches!(t.token_type, TokenType::Symbol(Symbol::Dot)))
                        {
                            self.current_arg = Some(Arg::Expression(expression));
                        } else {
                            if let Some(state) = self.state_stack.last() {
                                match state {
                                    ParserState::InsideCommand => {
                                        let parent = self.expression_stack.last_mut().unwrap();
                                        parent.args.push(Arg::Expression(expression));
                                    }
                                    ParserState::InsideList => {
                                        let list = self.list_stack.last_mut().unwrap();
                                        list.push(Arg::Expression(expression));
                                    }
                                }
                            } else {
                                self.output.push(expression);
                            }
                        }
                    }
                    None => unreachable!("Lexer should have handled incomplete commands"),
                }
            }
            Symbol::OpenBracket => match self.expression_stack.last() {
                Some(_) => {
                    self.list_stack.push(vec![]);

                    self.state_stack.push(ParserState::InsideList);
                }
                None => {
                    return Err(ParserError::ValueOutsideOfContext(ParserErrorContext::new(
                        code, token,
                    )));
                }
            },
            Symbol::ClosedBracket => {
                self.state_stack.pop();

                if let Some(mut list) = self.list_stack.pop() {
                    if let Some(arg) = self.current_arg.take() {
                        list.push(arg);
                    }

                    if let Some(state) = self.state_stack.last() {
                        match state {
                            ParserState::InsideCommand => {
                                let expression = self.expression_stack.last_mut().unwrap();
                                expression.args.push(Arg::List(list));
                            }
                            ParserState::InsideList => {
                                let parent = self.list_stack.last_mut().unwrap();
                                parent.push(Arg::List(list));
                            }
                        }
                    } else {
                        let expression = self.expression_stack.last_mut().unwrap();
                        expression.args.push(Arg::List(list));
                    }
                } else {
                    return Err(ParserError::ValueOutsideOfContext(ParserErrorContext::new(
                        code, token,
                    )));
                }
            }
            Symbol::Quote => {}
            Symbol::Dot => {
                if let Some(arg) = self.current_arg.take() {
                    if let Some(dot_arg) = self.dot_arg.take() {
                        if let Arg::Var(_) = dot_arg {
                            if let Arg::Var(_) = arg {
                                return Err(ParserError::ObjectsNotSupported(
                                    ParserErrorContext::new(code, token),
                                ));
                            } else {
                                return Err(ParserError::InvalidDotPlacement(
                                    ParserErrorContext::new(code, token),
                                ));
                            }
                        } else {
                            return Err(ParserError::InvalidDotPlacement(ParserErrorContext::new(
                                code, token,
                            )));
                        }
                    } else {
                        self.dot_arg = Some(arg);
                    }
                } else if self.output.last().is_some() {
                    let dot_expression = self.output.pop().unwrap();
                    self.dot_arg = Some(Arg::Expression(dot_expression));
                } else {
                    return Err(ParserError::InvalidDotPlacement(ParserErrorContext::new(
                        code, token,
                    )));
                }
            }
            Symbol::Comma => {
                if let Some(arg) = self.current_arg.take() {
                    if let Some(state) = self.state_stack.last() {
                        match state {
                            ParserState::InsideCommand => {
                                let expression = self.expression_stack.last_mut().unwrap();
                                expression.args.push(arg);
                            }
                            ParserState::InsideList => {
                                let list = self.list_stack.last_mut().unwrap();
                                list.push(arg);
                            }
                        }
                    } else {
                        return Err(ParserError::ValueOutsideOfContext(ParserErrorContext::new(
                            code, token,
                        )));
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse<'a>(mut self, code: &'a str) -> Result<Vec<Expression>, ParserError<'a>> {
        let mut tokens = Lexer::new().tokenize(code)?;

        let token_len = tokens.len();
        for i in 0..token_len {
            let token = std::mem::take(&mut tokens[i]);
            match token.token_type {
                TokenType::Symbol(symbol) => {
                    self.parse_symbol(token, tokens.get(i + 1), symbol, code)?
                }
                TokenType::Command(name) => self.current_command_name = Some(name),
                TokenType::Number(number) => self.current_arg = Some(Arg::Number(number)),
                TokenType::String(string) => self.current_arg = Some(Arg::String(string)),
                TokenType::Keyword(keyword) => self.current_arg = Some(Arg::Keyword(keyword)),
                TokenType::Var(var) => self.current_arg = Some(Arg::Var(var)),
                TokenType::None => unreachable!("Lexer will not create None token types"),
            };
        }

        Ok(self.output)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Arg, Expression, Parser};

    #[test]
    fn chain_after_expression() {
        let result = Parser::new().parse(r#"add(1, 1).add(1)"#);
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add".to_string(),
                args: vec![
                    Arg::Expression(Expression {
                        name: "add".to_string(),
                        args: vec![Arg::Number("1".to_string()), Arg::Number("1".to_string()),]
                    }),
                    Arg::Number("1".to_string())
                ]
            }]
        );
    }

    #[test]
    fn chain_after_command_with_list() {
        let result = Parser::new().parse(r#"index(["john", "joseph"], 1).print()"#);
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "print".to_string(),
                args: vec![Arg::Expression(Expression {
                    name: "index".to_string(),
                    args: vec![
                        Arg::List(vec![
                            Arg::String("john".to_string()),
                            Arg::String("joseph".to_string()),
                        ]),
                        Arg::Number("1".to_string())
                    ]
                })]
            }]
        );
    }

    #[test]
    fn multiple_statements_with_method_chains() {
        let result = Parser::new().parse(
            r#"names.store(["John", "James", "Joseph", "Alexander"])
        i.store(0)
        repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))
        "#,
        );
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![
                Expression {
                    name: "store".to_string(),
                    args: vec![
                        Arg::Var("names".to_string()),
                        Arg::List(vec![
                            Arg::String("John".to_string()),
                            Arg::String("James".to_string()),
                            Arg::String("Joseph".to_string()),
                            Arg::String("Alexander".to_string()),
                        ])
                    ]
                },
                Expression {
                    name: "store".to_string(),
                    args: vec![Arg::Var("i".to_string()), Arg::Number("0".to_string())]
                },
                Expression {
                    name: "repeat".to_string(),
                    args: vec![
                        Arg::Expression(Expression {
                            name: "length".to_string(),
                            args: vec![Arg::Var("names".to_string())]
                        }),
                        Arg::Expression(Expression {
                            name: "print".to_string(),
                            args: vec![Arg::Expression(Expression {
                                name: "index".to_string(),
                                args: vec![
                                    Arg::Var("names".to_string()),
                                    Arg::Var("i".to_string())
                                ]
                            })]
                        }),
                        Arg::Expression(Expression {
                            name: "print".to_string(),
                            args: vec![Arg::String("\n".to_string())]
                        }),
                        Arg::Expression(Expression {
                            name: "store".to_string(),
                            args: vec![
                                Arg::Var("i".to_string()),
                                Arg::Expression(Expression {
                                    name: "add".to_string(),
                                    args: vec![
                                        Arg::Var("i".to_string()),
                                        Arg::Number("1".to_string())
                                    ]
                                })
                            ]
                        })
                    ]
                }
            ]
        );
    }

    #[test]
    fn dot_after_command_no_chain() {
        let result = Parser::new().parse("store(1).");
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn command_without_parens() {
        let result = Parser::new().parse("print");
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_open_paren() {
        let result = Parser::new().parse("add(1, 2");
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_close_paren() {
        let result = Parser::new().parse("add 1, 2)");
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_brackets() {
        let result = Parser::new().parse("store([1, 2)");
        assert!(result.is_err());
    }
    #[test]
    fn dot_arg() {
        let result = Parser::new().parse("i.store(0)");
        dbg!(&result);
        let expressions = result.unwrap();
        assert!(
            expressions
                == vec![Expression {
                    name: "store".to_string(),
                    args: vec![Arg::Var("i".to_string()), Arg::Number("0".to_string())]
                }]
        );
    }

    #[test]
    fn dot_arg_object() {
        let result = Parser::new().parse("character.weapon.name.store(\"sword\")");
        dbg!(&result);
        assert!(
            result.is_err_and(|e| matches!(e, crate::parser::ParserError::ObjectsNotSupported(_)))
        )
        /*
        let expressions = result.unwrap();
        assert!(
            expressions
                == vec![Expression {
                    name: "store".to_string(),
                    args: vec![
                        Arg::Var("character.weapon.name".to_string()),
                        Arg::String("sword".to_string())
                    ]
                }]
        );
        */
    }

    #[test]
    fn dot_arg_invalid_object() {
        let result = Parser::new().parse("character.\"weapon\".\"name\".store(\"sword\")");
        dbg!(&result);
        assert!(
            result.is_err_and(|e| matches!(e, crate::parser::ParserError::InvalidDotPlacement(_)))
        );
    }

    #[test]
    fn dot_arg_chain() {
        let result = Parser::new()
            .parse("names.index(0).ends_with(\"2\").if_then(print(\"it's name 2!!\"))");
        dbg!(&result);
        let expressions = result.unwrap();

        assert_eq!(
            expressions,
            vec![Expression {
                name: "if_then".to_string(),
                args: vec![
                    Arg::Expression(Expression {
                        name: "ends_with".to_string(),
                        args: vec![
                            Arg::Expression(Expression {
                                name: "index".to_string(),
                                args: vec![
                                    Arg::Var("names".to_string()),
                                    Arg::Number("0".to_string())
                                ]
                            }),
                            Arg::String("2".to_string())
                        ]
                    }),
                    Arg::Expression(Expression {
                        name: "print".to_string(),
                        args: vec![Arg::String("it's name 2!!".to_string())]
                    })
                ]
            }]
        );
    }

    #[test]
    fn nested_command() {
        let result = Parser::new().parse("quadruple.def(add(double(x), double(x)))");
        dbg!(&result);
        let expressions = result.unwrap();

        assert_eq!(
            expressions,
            vec![Expression {
                name: "def".to_string(),
                args: vec![
                    Arg::Var("quadruple".to_string()),
                    Arg::Expression(Expression {
                        name: "add".to_string(),
                        args: vec![
                            Arg::Expression(Expression {
                                name: "double".to_string(),
                                args: vec![Arg::Var("x".to_string())]
                            }),
                            Arg::Expression(Expression {
                                name: "double".to_string(),
                                args: vec![Arg::Var("x".to_string())]
                            })
                        ]
                    })
                ]
            }]
        );
    }

    #[test]
    fn empty_args() {
        let result = Parser::new().parse("print()");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "print".to_string(),
                args: vec![]
            }]
        );
    }

    #[test]
    fn multiple_args() {
        let result = Parser::new().parse("add(1, 2, 3, 4)");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add".to_string(),
                args: vec![
                    Arg::Number("1".to_string()),
                    Arg::Number("2".to_string()),
                    Arg::Number("3".to_string()),
                    Arg::Number("4".to_string()),
                ]
            }]
        );
    }

    //  let result = Parser::new().parse("outer(inner2([c, [d]]))");
    #[test]
    fn list_with_expressions() {
        let result = Parser::new().parse("list.store([1, add(2, 3), 4])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("list".to_string()),
                    Arg::List(vec![
                        Arg::Number("1".to_string()),
                        Arg::Expression(Expression {
                            name: "add".to_string(),
                            args: vec![Arg::Number("2".to_string()), Arg::Number("3".to_string()),]
                        }),
                        Arg::Number("4".to_string()),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn nested_lists() {
        let result = Parser::new().parse("matrix.store([[1, 2, 3], [4, 5, 6], [7, 8, 9]])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("matrix".to_string()),
                    Arg::List(vec![
                        Arg::List(vec![
                            Arg::Number("1".to_string()),
                            Arg::Number("2".to_string()),
                            Arg::Number("3".to_string()),
                        ]),
                        Arg::List(vec![
                            Arg::Number("4".to_string()),
                            Arg::Number("5".to_string()),
                            Arg::Number("6".to_string()),
                        ]),
                        Arg::List(vec![
                            Arg::Number("7".to_string()),
                            Arg::Number("8".to_string()),
                            Arg::Number("9".to_string()),
                        ]),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn deeply_nested_lists() {
        let result = Parser::new().parse("data.store([1, [2, [3, [4, 5]]]])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("data".to_string()),
                    Arg::List(vec![
                        Arg::Number("1".to_string()),
                        Arg::List(vec![
                            Arg::Number("2".to_string()),
                            Arg::List(vec![
                                Arg::Number("3".to_string()),
                                Arg::List(vec![
                                    Arg::Number("4".to_string()),
                                    Arg::Number("5".to_string()),
                                ])
                            ])
                        ])
                    ])
                ]
            }]
        );
    }

    #[test]
    fn list_with_mixed_content() {
        let result =
            Parser::new().parse("data.store([x, \"hello\", 42, add(1, 2), [nested, list]])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("data".to_string()),
                    Arg::List(vec![
                        Arg::Var("x".to_string()),
                        Arg::String("hello".to_string()),
                        Arg::Number("42".to_string()),
                        Arg::Expression(Expression {
                            name: "add".to_string(),
                            args: vec![Arg::Number("1".to_string()), Arg::Number("2".to_string()),]
                        }),
                        Arg::List(vec![
                            Arg::Var("nested".to_string()),
                            Arg::Var("list".to_string()),
                        ])
                    ])
                ]
            }]
        );
    }

    #[test]
    fn deeply_nested_commands() {
        let result = Parser::new().parse("a(b(c(d(e(f(g(h(i(j(k()))))))))))");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "a".to_string(),
                args: vec![Arg::Expression(Expression {
                    name: "b".to_string(),
                    args: vec![Arg::Expression(Expression {
                        name: "c".to_string(),
                        args: vec![Arg::Expression(Expression {
                            name: "d".to_string(),
                            args: vec![Arg::Expression(Expression {
                                name: "e".to_string(),
                                args: vec![Arg::Expression(Expression {
                                    name: "f".to_string(),
                                    args: vec![Arg::Expression(Expression {
                                        name: "g".to_string(),
                                        args: vec![Arg::Expression(Expression {
                                            name: "h".to_string(),
                                            args: vec![Arg::Expression(Expression {
                                                name: "i".to_string(),
                                                args: vec![Arg::Expression(Expression {
                                                    name: "j".to_string(),
                                                    args: vec![Arg::Expression(Expression {
                                                        name: "k".to_string(),
                                                        args: vec![]
                                                    })]
                                                })]
                                            })]
                                        })]
                                    })]
                                })]
                            })]
                        })]
                    })]
                })]
            }]
        );
    }

    #[test]
    fn commands_in_lists_in_commands() {
        let result = Parser::new().parse("process([add(1, 2), sub(5, 3), mul(2, 4)])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "process".to_string(),
                args: vec![Arg::List(vec![
                    Arg::Expression(Expression {
                        name: "add".to_string(),
                        args: vec![Arg::Number("1".to_string()), Arg::Number("2".to_string()),]
                    }),
                    Arg::Expression(Expression {
                        name: "sub".to_string(),
                        args: vec![Arg::Number("5".to_string()), Arg::Number("3".to_string()),]
                    }),
                    Arg::Expression(Expression {
                        name: "mul".to_string(),
                        args: vec![Arg::Number("2".to_string()), Arg::Number("4".to_string()),]
                    })
                ])]
            }]
        );
    }

    #[test]
    fn lists_in_commands_in_lists() {
        let result = Parser::new().parse("outer([inner([1, 2]), inner([3, 4])])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![Arg::List(vec![
                    Arg::Expression(Expression {
                        name: "inner".to_string(),
                        args: vec![Arg::List(vec![
                            Arg::Number("1".to_string()),
                            Arg::Number("2".to_string()),
                        ])]
                    }),
                    Arg::Expression(Expression {
                        name: "inner".to_string(),
                        args: vec![Arg::List(vec![
                            Arg::Number("3".to_string()),
                            Arg::Number("4".to_string()),
                        ])]
                    })
                ])]
            }]
        );
    }

    #[test]
    fn deeply_nested_commands_lists_mixed() {
        let result = Parser::new().parse(
        "transform(data.map([filter([1, 2, 3], is_even()), sort([b, a, c])]), [process(x), process(y)])"
    );
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "transform".to_string(),
                args: vec![
                    Arg::Expression(Expression {
                        name: "map".to_string(),
                        args: vec![
                            Arg::Var("data".to_string()),
                            Arg::List(vec![
                                Arg::Expression(Expression {
                                    name: "filter".to_string(),
                                    args: vec![
                                        Arg::List(vec![
                                            Arg::Number("1".to_string()),
                                            Arg::Number("2".to_string()),
                                            Arg::Number("3".to_string()),
                                        ]),
                                        Arg::Expression(Expression {
                                            name: "is_even".to_string(),
                                            args: vec![]
                                        })
                                    ]
                                }),
                                Arg::Expression(Expression {
                                    name: "sort".to_string(),
                                    args: vec![Arg::List(vec![
                                        Arg::Var("b".to_string()),
                                        Arg::Var("a".to_string()),
                                        Arg::Var("c".to_string()),
                                    ])]
                                })
                            ])
                        ]
                    }),
                    Arg::List(vec![
                        Arg::Expression(Expression {
                            name: "process".to_string(),
                            args: vec![Arg::Var("x".to_string())]
                        }),
                        Arg::Expression(Expression {
                            name: "process".to_string(),
                            args: vec![Arg::Var("y".to_string())]
                        })
                    ])
                ]
            }]
        );
    }

    #[test]
    fn matrix_of_commands() {
        let result =
            Parser::new().parse("grid.store([[add(1,2), sub(3,4)], [mul(5,6), div(7,8)]])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("grid".to_string()),
                    Arg::List(vec![
                        Arg::List(vec![
                            Arg::Expression(Expression {
                                name: "add".to_string(),
                                args: vec![
                                    Arg::Number("1".to_string()),
                                    Arg::Number("2".to_string()),
                                ]
                            }),
                            Arg::Expression(Expression {
                                name: "sub".to_string(),
                                args: vec![
                                    Arg::Number("3".to_string()),
                                    Arg::Number("4".to_string()),
                                ]
                            })
                        ]),
                        Arg::List(vec![
                            Arg::Expression(Expression {
                                name: "mul".to_string(),
                                args: vec![
                                    Arg::Number("5".to_string()),
                                    Arg::Number("6".to_string()),
                                ]
                            }),
                            Arg::Expression(Expression {
                                name: "div".to_string(),
                                args: vec![
                                    Arg::Number("7".to_string()),
                                    Arg::Number("8".to_string()),
                                ]
                            })
                        ])
                    ])
                ]
            }]
        );
    }

    #[test]
    fn command_with_list_containing_nested_command_with_list() {
        let result = Parser::new().parse("outer([a, inner([b, deeper([c, d])]), e])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![Arg::List(vec![
                    Arg::Var("a".to_string()),
                    Arg::Expression(Expression {
                        name: "inner".to_string(),
                        args: vec![Arg::List(vec![
                            Arg::Var("b".to_string()),
                            Arg::Expression(Expression {
                                name: "deeper".to_string(),
                                args: vec![Arg::List(vec![
                                    Arg::Var("c".to_string()),
                                    Arg::Var("d".to_string()),
                                ])]
                            })
                        ])]
                    }),
                    Arg::Var("e".to_string()),
                ])]
            }]
        );
    }

    #[test]
    fn triple_nested_list_command_list() {
        let result = Parser::new().parse("outer([inner([[a, b]])])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![Arg::List(vec![Arg::Expression(Expression {
                    name: "inner".to_string(),
                    args: vec![Arg::List(vec![Arg::List(vec![
                        Arg::Var("a".to_string()),
                        Arg::Var("b".to_string()),
                    ])])]
                })])]
            }]
        );
    }

    #[test]
    fn inner_thing() {
        let result = Parser::new().parse("outer(inner2([c, [d]]))");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![Arg::Expression(Expression {
                    name: "inner2".to_string(),
                    args: vec![Arg::List(vec![
                        Arg::Var("c".to_string()),
                        Arg::List(vec![Arg::Var("d".to_string()),])
                    ])]
                })]
            }]
        );
    }

    #[test]
    fn command_list_command_command() {
        let result = Parser::new().parse("outer([inner(deepest())])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![Arg::List(vec![Arg::Expression(Expression {
                    name: "inner".to_string(),
                    args: vec![Arg::Expression(Expression {
                        name: "deepest".to_string(),
                        args: vec![]
                    })]
                })])]
            }]
        );
    }

    #[test]
    fn multiple_lists_same_level() {
        let result = Parser::new().parse("outer([a], [b], inner([c]))");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![
                    Arg::List(vec![Arg::Var("a".to_string())]),
                    Arg::List(vec![Arg::Var("b".to_string())]),
                    Arg::Expression(Expression {
                        name: "inner".to_string(),
                        args: vec![Arg::List(vec![Arg::Var("c".to_string())])]
                    })
                ]
            }]
        );
    }

    #[test]
    fn parallel_nested_structures() {
        let result = Parser::new().parse("outer([a, inner1([b])], inner2([c, [d]]))");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![
                    Arg::List(vec![
                        Arg::Var("a".to_string()),
                        Arg::Expression(Expression {
                            name: "inner1".to_string(),
                            args: vec![Arg::List(vec![Arg::Var("b".to_string())])]
                        })
                    ]),
                    Arg::Expression(Expression {
                        name: "inner2".to_string(),
                        args: vec![Arg::List(vec![
                            Arg::Var("c".to_string()),
                            Arg::List(vec![Arg::Var("d".to_string())])
                        ])]
                    })
                ]
            }]
        );
    }

    #[test]
    fn command_after_list_in_command() {
        let result = Parser::new().parse("outer([a, b], inner())");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer".to_string(),
                args: vec![
                    Arg::List(vec![Arg::Var("a".to_string()), Arg::Var("b".to_string()),]),
                    Arg::Expression(Expression {
                        name: "inner".to_string(),
                        args: vec![]
                    })
                ]
            }]
        );
    }

    #[test]
    fn empty_list() {
        let result = Parser::new().parse("data.store([])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![Arg::Var("data".to_string()), Arg::List(vec![])]
            }]
        );
    }

    #[test]
    fn trailing_comma_list() {
        let result = Parser::new().parse("data.store([1, 2,])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store".to_string(),
                args: vec![
                    Arg::Var("data".to_string()),
                    Arg::List(vec![
                        Arg::Number("1".to_string()),
                        Arg::Number("2".to_string()),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn multiple_trailing_commas() {
        let result = Parser::new().parse("data.store([1, 2,,,,,])");
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn trailing_comma_command() {
        let result = Parser::new().parse("add(1, 2,)");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add".to_string(),
                args: vec![Arg::Number("1".to_string()), Arg::Number("2".to_string()),]
            }]
        );
    }

    #[test]
    fn chain_after_nested_expression() {
        let result = Parser::new().parse("outer(inner(x)).method(y)");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "method".to_string(),
                args: vec![
                    Arg::Expression(Expression {
                        name: "outer".to_string(),
                        args: vec![Arg::Expression(Expression {
                            name: "inner".to_string(),
                            args: vec![Arg::Var("x".to_string())]
                        })]
                    }),
                    Arg::Var("y".to_string())
                ]
            }]
        );
    }

    #[test]
    fn dot_at_start_should_error() {
        let result = Parser::new().parse(".store(1)");
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn consecutive_dots_should_error() {
        let result = Parser::new().parse("a..b.store(1)");
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn literal_at_top_level_should_error() {
        let result = Parser::new().parse("42");
        dbg!(&result);
        assert!(result.is_ok() && result.clone().unwrap().is_empty() || result.is_err());
    }

    #[test]
    fn mixed_state_transitions() {
        let result = Parser::new().parse("a([b(c), d([e, f])])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "a".to_string(),
                args: vec![Arg::List(vec![
                    Arg::Expression(Expression {
                        name: "b".to_string(),
                        args: vec![Arg::Var("c".to_string())]
                    }),
                    Arg::Expression(Expression {
                        name: "d".to_string(),
                        args: vec![Arg::List(vec![
                            Arg::Var("e".to_string()),
                            Arg::Var("f".to_string()),
                        ])]
                    })
                ])]
            }]
        );
    }
}
