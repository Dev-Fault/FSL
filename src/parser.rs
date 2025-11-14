use std::ops::Deref;

use crate::lexer::{Lexer, LexerError, Symbol, Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError<'a> {
    LexerError(LexerError<'a>),
    OutOfPlaceVar(Token),
    LiteralOutsideOfCommand(Token),
    InvalidDotPlacement(Token),
    IncompleteString(Token),
}

impl<'a> From<LexerError<'a>> for ParserError<'a> {
    fn from(value: LexerError<'a>) -> Self {
        ParserError::LexerError(value)
    }
}

// TODO handle nested lists
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct List {
    parent: Option<Box<List>>,
    data: Vec<Arg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Number(String),
    String(String),
    Keyword(String),
    Var(String),
    List(Vec<Arg>),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
    parent: Option<Box<Expression>>,
    name: String,
    args: Vec<Arg>,
}

impl Expression {
    pub fn new(name: String) -> Self {
        Self {
            parent: None,
            name,
            args: vec![],
        }
    }
}

pub struct Parser {
    current_expression: Option<Box<Expression>>,
    current_list: Option<Vec<Arg>>,
    current_command_name: Option<String>,
    current_arg: Option<Arg>,
    dot_arg: Option<Arg>,
    output: Vec<Expression>,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            current_expression: None,
            current_command_name: None,
            current_list: None,
            current_arg: None,
            dot_arg: None,
            output: vec![],
        }
    }

    fn parse_symbol<'a>(&mut self, token: &Token, symbol: Symbol) -> Result<(), ParserError<'a>> {
        match symbol {
            Symbol::OpenParen => match self.current_command_name.take() {
                Some(name) => {
                    let mut expression = Expression::new(name);
                    if let Some(arg) = self.dot_arg.take() {
                        expression.args.push(arg);
                    }
                    match self.current_expression.take() {
                        Some(current_expression) => {
                            expression.parent = Some(current_expression);
                            self.current_expression = Some(Box::new(expression));
                        }
                        None => {
                            self.current_expression = Some(Box::new(expression));
                        }
                    }
                }
                None => unreachable!("lexer should have handled invalid commands"),
            },
            Symbol::ClosedParen => match self.current_expression.take() {
                Some(mut expression) => {
                    if let Some(arg) = self.current_arg.take() {
                        expression.args.push(arg);
                    }

                    match expression.parent.take() {
                        Some(mut parent) => {
                            parent.args.push(Arg::Expression(*expression));
                            self.current_expression = Some(parent);
                        }
                        None => {
                            self.output.push(*expression);
                        }
                    }
                }

                None => unreachable!("Lexer should have handled incomplete commands"),
            },
            Symbol::OpenBracket => match self.current_expression {
                Some(_) => {
                    self.current_list = Some(vec![]);
                }
                None => return Err(ParserError::LiteralOutsideOfCommand(token.clone())),
            },
            Symbol::ClosedBracket => match &mut self.current_expression {
                Some(expression) => match self.current_list.take() {
                    Some(mut list) => {
                        if let Some(arg) = self.current_arg.take() {
                            list.push(arg);
                        }
                        expression.args.push(Arg::List(list));
                    }
                    None => unreachable!("lexer should have handled incomplete lists"),
                },
                None => {
                    return Err(ParserError::LiteralOutsideOfCommand(token.clone()));
                }
            },
            Symbol::Quote => {}
            Symbol::Dot => {
                if let Some(arg) = self.current_arg.take() {
                    if let Some(dot_arg) = self.dot_arg.take() {
                        if let Arg::Var(object) = dot_arg {
                            if let Arg::Var(property) = arg {
                                self.dot_arg = Some(Arg::Var(format!("{}.{}", object, property)))
                            } else {
                                return Err(ParserError::InvalidDotPlacement(token.clone()));
                            }
                        } else {
                            return Err(ParserError::InvalidDotPlacement(token.clone()));
                        }
                    } else {
                        self.dot_arg = Some(arg);
                    }
                } else if self.output.last().is_some() {
                    let dot_expression = self.output.pop().unwrap();
                    self.dot_arg = Some(Arg::Expression(dot_expression));
                }
            }
            Symbol::Comma => {
                if let Some(arg) = self.current_arg.take() {
                    if let Some(list) = &mut self.current_list {
                        list.push(arg);
                    } else if let Some(expression) = &mut self.current_expression {
                        expression.args.push(arg);
                    }
                }
            }
        }
        Ok(())
    }

    pub fn parse<'a>(mut self, code: &'a str) -> Result<Vec<Expression>, ParserError<'a>> {
        let tokens = Lexer::new().tokenize(code)?;

        for (i, token) in tokens.iter().enumerate() {
            match token.token_type.clone() {
                TokenType::Symbol(symbol) => self.parse_symbol(token, symbol)?,
                TokenType::Command(name) => self.current_command_name = Some(name),
                TokenType::Number(number) => self.current_arg = Some(Arg::Number(number)),
                TokenType::String(string) => self.current_arg = Some(Arg::String(string)),
                TokenType::Keyword(keyword) => self.current_arg = Some(Arg::Keyword(keyword)),
                TokenType::Var(var) => self.current_arg = Some(Arg::Var(var)),
            };
        }

        Ok(self.output)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Arg, Expression, Parser};

    #[test]
    fn dot_arg() {
        let result = Parser::new().parse("i.store(0)");
        dbg!(&result);
        let expressions = result.unwrap();
        assert!(
            expressions
                == vec![Expression {
                    parent: None,
                    name: "store".to_string(),
                    args: vec![Arg::Var("i".to_string()), Arg::Number("0".to_string())]
                }]
        );
    }

    #[test]
    fn dot_arg_object() {
        let result = Parser::new().parse("character.weapon.name.store(\"sword\")");
        dbg!(&result);
        let expressions = result.unwrap();
        assert!(
            expressions
                == vec![Expression {
                    parent: None,
                    name: "store".to_string(),
                    args: vec![
                        Arg::Var("character.weapon.name".to_string()),
                        Arg::String("sword".to_string())
                    ]
                }]
        );
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
                parent: None,
                name: "if_then".to_string(),
                args: vec![
                    Arg::Expression(Expression {
                        parent: None,
                        name: "ends_with".to_string(),
                        args: vec![
                            Arg::Expression(Expression {
                                parent: None,
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
                        parent: None,
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
                parent: None,
                name: "def".to_string(),
                args: vec![
                    Arg::Var("quadruple".to_string()),
                    Arg::Expression(Expression {
                        parent: None,
                        name: "add".to_string(),
                        args: vec![
                            Arg::Expression(Expression {
                                parent: None,
                                name: "double".to_string(),
                                args: vec![Arg::Var("x".to_string())]
                            }),
                            Arg::Expression(Expression {
                                parent: None,
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
                parent: None,
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
                parent: None,
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

    #[test]
    fn list_with_expressions() {
        let result = Parser::new().parse("list.store([1, add(2, 3), 4])");
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                parent: None,
                name: "store".to_string(),
                args: vec![
                    Arg::Var("list".to_string()),
                    Arg::List(vec![
                        Arg::Number("1".to_string()),
                        Arg::Expression(Expression {
                            parent: None,
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
                parent: None,
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
                parent: None,
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
                parent: None,
                name: "store".to_string(),
                args: vec![
                    Arg::Var("data".to_string()),
                    Arg::List(vec![
                        Arg::Var("x".to_string()),
                        Arg::String("hello".to_string()),
                        Arg::Number("42".to_string()),
                        Arg::Expression(Expression {
                            parent: None,
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
}
