use std::{borrow::Cow, collections::VecDeque, fmt::Display};

use crate::lexer::{LexError, Lexer, Symbol, Token, TokenType};

type Map<'code> = Vec<(&'code str, ArgType<'code>)>;
type List<'code> = Vec<ArgType<'code>>;
type Path<'code> = VecDeque<ArgType<'code>>;

#[derive(Debug, Clone, PartialEq)]
pub enum ArgType<'code> {
    Number(&'code str),
    String(Cow<'code, str>),
    Keyword(&'code str),
    Identifier(&'code str),
    List(List<'code>),
    Map(Map<'code>),
    Expression(Expression<'code>),
}

impl<'code> From<List<'code>> for ArgType<'code> {
    fn from(value: List<'code>) -> Self {
        Self::List(value)
    }
}

impl<'code> From<Map<'code>> for ArgType<'code> {
    fn from(value: Map<'code>) -> Self {
        Self::Map(value)
    }
}

impl<'code> From<Expression<'code>> for ArgType<'code> {
    fn from(value: Expression<'code>) -> Self {
        Self::Expression(value)
    }
}

impl<'code> From<PendingArg<'code>> for ArgType<'code> {
    fn from(value: PendingArg<'code>) -> Self {
        match value {
            PendingArg::DotArg(arg) => arg,
            PendingArg::PathArg(path) => ArgType::List(path.into()),
            PendingArg::Expression(expression) => ArgType::Expression(expression),
            PendingArg::List(list) => ArgType::List(list),
            PendingArg::Map(map) => ArgType::Map(map),
            PendingArg::UnitializedCollection => todo!(),
            PendingArg::Done(arg) => arg,
            PendingArg::Key(key) => ArgType::Identifier(key),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum PendingArg<'code> {
    DotArg(ArgType<'code>),
    PathArg(Path<'code>),
    Key(&'code str),
    Expression(Expression<'code>),
    List(List<'code>),
    Map(Map<'code>),
    UnitializedCollection,
    Done(ArgType<'code>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'code> {
    LexError(LexError<'code>),
    OutOfPlaceSymbol(Token<'code>),
    OutOfPlaceValue(Token<'code>),
}

impl<'code> Display for ParseError<'code> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::LexError(lex_error) => write!(f, "{}", lex_error),
            ParseError::OutOfPlaceSymbol(token) => {
                write!(
                    f,
                    "Unexpected symbol \"{}\" on line {}\n{}: {}",
                    token.token_type,
                    token.line_number(),
                    token.line_number(),
                    token.line(),
                )
            }
            ParseError::OutOfPlaceValue(token) => {
                write!(
                    f,
                    "Unexpected value \"{}\" on line {}\n{}: {}",
                    token.token_type,
                    token.line_number(),
                    token.line_number(),
                    token.line(),
                )
            }
        }
    }
}

impl<'code> From<LexError<'code>> for ParseError<'code> {
    fn from(value: LexError<'code>) -> Self {
        Self::LexError(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'code> {
    pub name: &'code str,
    pub args: Vec<ArgType<'code>>,
}

impl<'code> Expression<'code> {
    pub fn new(name: &'code str) -> Expression<'code> {
        Self {
            name,
            args: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct Pending<'code> {
    arg: PendingArg<'code>,
    token: Token<'code>,
}

impl<'code> Pending<'code> {
    pub fn new(arg: PendingArg<'code>, token: Token<'code>) -> Self {
        Self { arg, token }
    }
}

pub struct Parser<'code> {
    lexer: Option<Lexer<'code>>,
    pending: Vec<Pending<'code>>,
    parsed: Vec<Expression<'code>>,
}

impl<'code> Parser<'code> {
    pub fn new(input: &'code str) -> Parser<'code> {
        Self {
            lexer: Some(Lexer::new(input)),
            pending: Vec::new(),
            parsed: Vec::new(),
        }
    }

    fn pend_map(&mut self, map: Map<'code>, token: Token<'code>) {
        self.pending.push(Pending::new(PendingArg::Map(map), token));
    }

    fn pend_map_with_value(
        &mut self,
        map: Map<'code>,
        token: Token<'code>,
        key: &'code str,
        value: Option<ArgType<'code>>,
    ) {
        let mut map = map;
        if let Some(value) = value {
            map.push((key, value));
        }
        self.pending.push(Pending::new(PendingArg::Map(map), token));
    }

    fn try_pend_map_with_value(
        &mut self,
        token: Token<'code>,
        key: &'code str,
        value: Option<ArgType<'code>>,
    ) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(maybe_map) => match maybe_map.arg {
                PendingArg::Map(map) => {
                    Ok(self.pend_map_with_value(map, maybe_map.token, key, value))
                }
                PendingArg::UnitializedCollection => {
                    Ok(self.pend_map_with_value(Map::new(), maybe_map.token, key, value))
                }
                _ => Err(ParseError::OutOfPlaceValue(maybe_map.token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn pend_list_with_value(
        &mut self,
        list: List<'code>,
        token: Token<'code>,
        value: Option<ArgType<'code>>,
    ) {
        let mut list = list;
        if let Some(value) = value {
            list.push(value);
        }
        self.pending
            .push(Pending::new(PendingArg::List(list), token));
    }

    fn pend_uninit_collection(&mut self, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::UnitializedCollection, token));
    }

    fn pend_expr_with_value(
        &mut self,
        expr: Expression<'code>,
        token: Token<'code>,
        value: ArgType<'code>,
    ) {
        let mut expr = expr;
        expr.args.push(value);
        self.pending
            .push(Pending::new(PendingArg::Expression(expr), token));
    }

    fn pend_expr(&mut self, expr: Expression<'code>, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::Expression(expr), token));
    }

    fn pend_list(&mut self, list: Vec<ArgType<'code>>, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::List(list), token));
    }

    fn pend_done(&mut self, done: ArgType<'code>, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::Done(done), token));
    }

    fn pend_dot_arg(&mut self, arg: ArgType<'code>, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::DotArg(arg), token));
    }

    fn pend_path(&mut self, path: Path<'code>, token: Token<'code>) {
        self.pending
            .push(Pending::new(PendingArg::PathArg(path), token));
    }

    fn pend_key(&mut self, key: &'code str, token: Token<'code>) {
        self.pending.push(Pending::new(PendingArg::Key(key), token));
    }

    fn pend_last_as_done(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(last) => match last.arg {
                PendingArg::Expression(expr) => Ok(self.pend_done(expr.into(), token)),
                PendingArg::List(list) => Ok(self.pend_done(list.into(), token)),
                PendingArg::Map(map) => Ok(self.pend_done(map.into(), token)),
                _ => Err(ParseError::OutOfPlaceValue(last.token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn get_remaining_arg(&mut self) -> Option<ArgType<'code>> {
        match self.pending.pop() {
            Some(pending) => {
                if let PendingArg::Done(arg) = pending.arg {
                    Some(arg)
                } else {
                    self.pending.push(pending);
                    None
                }
            }
            None => None,
        }
    }

    fn parse_open_paren(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(pending) => match pending.arg {
                PendingArg::Expression(mut expr) => {
                    let pending_dot = match self.pending.pop() {
                        Some(pending) => match pending.arg {
                            PendingArg::DotArg(dot_arg) => {
                                Some((PendingArg::DotArg(dot_arg), pending.token))
                            }
                            PendingArg::PathArg(path) => {
                                Some((PendingArg::PathArg(path), pending.token))
                            }
                            _ => {
                                self.pending.push(pending);
                                None
                            }
                        },
                        None => None,
                    };
                    if let Some((pending_arg, pending_token)) = pending_dot {
                        match pending_arg {
                            PendingArg::DotArg(dot_arg) => {
                                if expr.args.len() > 0 {
                                    unreachable!(
                                        "should not have put args in expression before dot arg"
                                    );
                                } else {
                                    expr.args.push(dot_arg);
                                }
                            }
                            PendingArg::PathArg(mut path) => {
                                if expr.args.len() > 0 {
                                    unreachable!(
                                        "should not have put args in expression before path"
                                    );
                                } else {
                                    expr.args.push(path.pop_front().unwrap());
                                    let path: Result<VecDeque<ArgType<'code>>, ParseError<'code>> =
                                        path.iter()
                                            .map(|item| match item {
                                                ArgType::Identifier(ident) => {
                                                    Ok(ArgType::String(Cow::Borrowed(ident)))
                                                }
                                                _ => {
                                                    Err(ParseError::OutOfPlaceValue(pending_token))
                                                }
                                            })
                                            .collect();
                                    let path = path?;
                                    expr.args.push(ArgType::List(path.into()));
                                }
                            }
                            _ => {}
                        }
                    }
                    self.pending
                        .push(Pending::new(PendingArg::Expression(expr), pending.token));
                    Ok(())
                }
                _ => Err(ParseError::OutOfPlaceValue(pending.token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_closed_paren(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        let last_arg = self.get_remaining_arg();
        match self.pending.pop() {
            Some(maybe_expr) => match maybe_expr.arg {
                PendingArg::Expression(mut expr) => {
                    if let Some(arg) = last_arg {
                        expr.args.push(arg);
                    }
                    match self.pending.pop() {
                        Some(parent) => match parent.arg {
                            PendingArg::Expression(parent_expr) => {
                                Ok(self.pend_expr_with_value(parent_expr, token, expr.into()))
                            }
                            PendingArg::List(list) => {
                                Ok(self.pend_list_with_value(list, token, Some(expr.into())))
                            }
                            PendingArg::Key(key) => {
                                Ok(self.try_pend_map_with_value(token, key, Some(expr.into()))?)
                            }
                            PendingArg::UnitializedCollection => Ok(self.pend_list_with_value(
                                List::new(),
                                token,
                                Some(expr.into()),
                            )),
                            PendingArg::Done(done) => {
                                self.pend_done(done.into(), token);
                                self.pend_done(expr.into(), token);
                                Ok(())
                            }
                            _ => Err(ParseError::OutOfPlaceSymbol(token)),
                        },
                        None => {
                            self.pend_done(expr.into(), token);
                            Ok(())
                        }
                    }
                }
                _ => Err(ParseError::OutOfPlaceValue(maybe_expr.token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_open_bracket(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.last() {
            Some(pending) => match &pending.arg {
                PendingArg::Key(_) => Ok(()),
                PendingArg::Expression(_) => Ok(()),
                PendingArg::List(_) => Ok(()),
                PendingArg::UnitializedCollection => Ok(()),
                _ => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            None => Ok(()),
        }?;

        Ok(self.pend_uninit_collection(token))
    }

    fn parse_closed_bracket(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        let last_arg = self.get_remaining_arg();
        match self.pending.pop() {
            Some(collection) => match collection.arg {
                PendingArg::Key(key) => {
                    if last_arg.is_none() {
                        return Err(ParseError::OutOfPlaceValue(collection.token));
                    }
                    self.try_pend_map_with_value(token, key, last_arg)?
                }
                PendingArg::List(list) => self.pend_list_with_value(list, token, last_arg),
                PendingArg::Map(_) => {
                    /*
                    This means user left trailing comma which handled map
                    Can safely be marked as done
                    */
                    self.pending.push(collection);
                }
                PendingArg::UnitializedCollection => {
                    self.pend_list_with_value(List::new(), token, last_arg)
                }
                _ => {
                    return Err(ParseError::OutOfPlaceSymbol(token));
                }
            },
            None => {
                return Err(ParseError::OutOfPlaceSymbol(token));
            }
        };

        self.pend_last_as_done(token)?;
        Ok(())
    }

    fn parse_colon(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(pending) => match pending.arg {
                PendingArg::Done(ArgType::Identifier(key)) => Ok(self.pend_key(key, token)),
                _ => Err(ParseError::OutOfPlaceValue(pending.token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn try_find_last_expression(
        &mut self,
        token: Token<'code>,
        popped: Pending<'code>,
    ) -> Result<(), ParseError<'code>> {
        match popped.arg {
            PendingArg::Expression(mut expr) => match expr.args.pop() {
                Some(last_arg) => {
                    self.pend_expr(expr, token);
                    Ok(self.pend_dot_arg(last_arg, token))
                }
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            PendingArg::List(mut list) => match list.pop() {
                Some(last_arg) => match last_arg {
                    ArgType::Expression(_) => {
                        self.pend_list(list, token);
                        Ok(self.pend_dot_arg(last_arg, token))
                    }
                    _ => Err(ParseError::OutOfPlaceSymbol(token)),
                },
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            PendingArg::Map(mut map) => match map.pop() {
                Some((key, value)) => match value {
                    ArgType::Expression(_) => {
                        self.pend_map(map, token);
                        self.pend_key(key, token);
                        self.pend_dot_arg(value, token);
                        Ok(())
                    }
                    _ => Err(ParseError::OutOfPlaceSymbol(token)),
                },
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            _ => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_dot(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(pending) => match pending.arg {
                PendingArg::Done(arg) => match self.pending.pop() {
                    Some(pending) => match pending.arg {
                        PendingArg::DotArg(parent_dot_arg) => {
                            let mut path = Path::new();
                            path.push_back(parent_dot_arg);
                            path.push_back(arg);
                            self.pend_path(path, token);
                            Ok(())
                        }
                        PendingArg::PathArg(mut path) => {
                            path.push_back(arg);
                            self.pend_path(path, token);
                            Ok(())
                        }
                        _ => {
                            self.pending.push(pending);
                            Ok(self.pend_dot_arg(arg, token))
                        }
                    },
                    None => Ok(self.pend_dot_arg(arg, token)),
                },
                _ => self.try_find_last_expression(token, pending),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_comma(&mut self, token: Token<'code>) -> Result<(), ParseError<'code>> {
        match self.pending.pop() {
            Some(pending) => match pending.arg {
                PendingArg::Done(value) => match self.pending.pop() {
                    Some(parent) => match parent.arg {
                        PendingArg::Expression(expression) => {
                            Ok(self.pend_expr_with_value(expression, parent.token, value.into()))
                        }
                        PendingArg::UnitializedCollection => {
                            Ok(self.pend_list_with_value(List::new(), parent.token, value.into()))
                        }
                        PendingArg::List(list) => {
                            Ok(self.pend_list_with_value(list, parent.token, value.into()))
                        }
                        PendingArg::Key(key) => {
                            Ok(self.try_pend_map_with_value(token, key, value.into())?)
                        }
                        PendingArg::Done(done) => {
                            self.pend_done(done, token);
                            self.pend_done(value, pending.token);
                            Ok(())
                        }
                        _ => Err(ParseError::OutOfPlaceValue(parent.token)),
                    },
                    None => {
                        // Top level commas not allowed
                        Err(ParseError::OutOfPlaceSymbol(token))
                    }
                },
                _ => {
                    // ) or ] may have already handled pending arg
                    self.pending.push(pending);
                    Ok(())
                }
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_symbol(
        &mut self,
        token: Token<'code>,
        symbol: Symbol,
    ) -> Result<(), ParseError<'code>> {
        match symbol {
            Symbol::OpenParen => self.parse_open_paren(token),
            Symbol::ClosedParen => self.parse_closed_paren(token),
            Symbol::OpenBracket => self.parse_open_bracket(token),
            Symbol::ClosedBracket => self.parse_closed_bracket(token),
            Symbol::Colon => self.parse_colon(token),
            Symbol::Dot => self.parse_dot(token),
            Symbol::Comma => self.parse_comma(token),
            Symbol::Quote => Ok(()),
            Symbol::Hashtag => Ok(()),
            Symbol::Star => Ok(()),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Expression<'code>>, ParseError<'code>> {
        let lexer = self
            .lexer
            .take()
            .expect("parser should be called once after new");
        let tokens = lexer.collect::<Result<Vec<Token<'code>>, LexError<'code>>>()?;
        let mut tokens = tokens.into_iter().peekable();

        while let Some(token) = tokens.next() {
            // dbg!(token.token_type);
            match token.token_type {
                TokenType::Symbol(symbol) => {
                    self.parse_symbol(token, Symbol::from(symbol))?;
                }
                TokenType::Command(name) => {
                    let pending =
                        Pending::new(PendingArg::Expression(Expression::new(name)), token);
                    self.pending.push(pending);
                }
                TokenType::Number(number) => {
                    let pending = Pending::new(PendingArg::Done(ArgType::Number(number)), token);
                    self.pending.push(pending);
                }
                TokenType::String(string) => {
                    // TODO parse escaped strings
                    let pending = Pending::new(
                        PendingArg::Done(ArgType::String(parse_string(string))),
                        token,
                    );
                    self.pending.push(pending);
                }
                TokenType::Keyword(keyword) => {
                    let pending = Pending::new(PendingArg::Done(ArgType::Keyword(keyword)), token);
                    self.pending.push(pending);
                }
                TokenType::Identifier(identifier) => {
                    let pending =
                        Pending::new(PendingArg::Done(ArgType::Identifier(identifier)), token);
                    self.pending.push(pending);
                }
                TokenType::Comment(_) => { /* Skip Comments */ }
                TokenType::None(_) => {
                    unreachable!("lexer should have handled invalid token errors")
                }
            }
        }
        for pending in self.pending.drain(..) {
            match pending.arg {
                PendingArg::Done(ArgType::Expression(expr)) => {
                    self.parsed.push(expr);
                }
                _ => {
                    return Err(ParseError::OutOfPlaceValue(pending.token));
                }
            }
        }
        Ok(self.parsed)
    }
}

fn parse_string(s: &'_ str) -> Cow<'_, str> {
    if !s.contains('\\') {
        Cow::Borrowed(s)
    } else {
        let mut result = String::with_capacity(s.len());
        let mut chars = s.chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.next() {
                    Some('n') => {
                        result.push('\n');
                    }
                    Some('t') => result.push('\t'),
                    Some('r') => result.push('\r'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some(ch) => {
                        result.push('\\');
                        result.push(ch);
                    }
                    None => result.push('\\'),
                }
            } else {
                result.push(ch);
            }
        }
        Cow::Owned(result)
    }
}

#[cfg(test)]
mod tests {
    use std::{borrow::Cow, collections::HashMap};

    use crate::parser::{ArgType, Expression, Map, Parser};

    #[test]
    fn parse_simple() {
        let parser = Parser::new("print(\"hello world\")");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "print",
            args: vec![ArgType::String(Cow::Borrowed("hello world"))],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed);
    }

    #[test]
    fn parse_simple_dot() {
        let parser = Parser::new("i.store(0)");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![ArgType::Identifier("i"), ArgType::Number("0")],
        }];
        dbg!(&parsed);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_simple_path() {
        let parser = Parser::new("player.weapon.name.damage.get()");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "get",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::List(vec![
                    ArgType::String(Cow::Borrowed("weapon")),
                    ArgType::String(Cow::Borrowed("name")),
                    ArgType::String(Cow::Borrowed("damage")),
                ]),
            ],
        }];
        dbg!(&parsed);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_simple_map() {
        let parser = Parser::new("map.store([value_one: 1, value_two: 2, value_three: [1, 2, 3]])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("map"),
                ArgType::Map(Map::from([
                    ("value_one", ArgType::Number("1")),
                    ("value_two", ArgType::Number("2")),
                    (
                        "value_three",
                        ArgType::List(vec![
                            ArgType::Number("1"),
                            ArgType::Number("2"),
                            ArgType::Number("3"),
                        ]),
                    ),
                ])),
            ],
        }];
        dbg!(&parsed);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_trailing_comma() {
        let parser = Parser::new("list.store([1,2,3,])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("list"),
                ArgType::List(vec![
                    ArgType::Number("1"),
                    ArgType::Number("2"),
                    ArgType::Number("3"),
                ]),
            ],
        }];
        dbg!(&parsed);
        assert!(expected == parsed)
    }

    #[tokio::test]
    async fn parse_empty_list_in_command() {
        let parser = Parser::new("print([])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "print",
            args: vec![ArgType::List(vec![])],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_nested_list() {
        let parser = Parser::new("matrix.store([[1, [2, 3], 4], [5, 6], 7])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("matrix"),
                ArgType::List(vec![
                    ArgType::List(vec![
                        ArgType::Number("1"),
                        ArgType::List(vec![ArgType::Number("2"), ArgType::Number("3")]),
                        ArgType::Number("4"),
                    ]),
                    ArgType::List(vec![ArgType::Number("5"), ArgType::Number("6")]),
                    ArgType::Number("7"),
                ]),
            ],
        }];
        dbg!(&parsed);
        assert!(expected == parsed)
    }
    #[test]
    fn parse_nested_expression() {
        let parser = Parser::new("print(add(1, 2))");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "print",
            args: vec![ArgType::Expression(Expression {
                name: "add",
                args: vec![ArgType::Number("1"), ArgType::Number("2")],
            })],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_multiple_expressions() {
        let parser = Parser::new("i.store(0) i.inc()");
        let parsed = parser.parse().unwrap();
        let expected = vec![
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("i"), ArgType::Number("0")],
            },
            Expression {
                name: "inc",
                args: vec![ArgType::Identifier("i")],
            },
        ];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_keyword_arg() {
        let parser = Parser::new("loop.store(true)");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![ArgType::Identifier("loop"), ArgType::Keyword("true")],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_empty_expression() {
        let parser = Parser::new("i.inc()");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "inc",
            args: vec![ArgType::Identifier("i")],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_expression_in_list() {
        let parser = Parser::new("result.store([add(1, 2), add(3, 4)])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("result"),
                ArgType::List(vec![
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("1"), ArgType::Number("2")],
                    }),
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("3"), ArgType::Number("4")],
                    }),
                ]),
            ],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_map_in_expression_arg() {
        let parser = Parser::new("create(player, [name: \"hero\", health: 100])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "create",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::Map(Map::from([
                    ("name", ArgType::String(Cow::Borrowed("hero"))),
                    ("health", ArgType::Number("100")),
                ])),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_chained_dot_expressions() {
        let parser = Parser::new("player.health.get()");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "get",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::List(vec![ArgType::String(Cow::Borrowed("health"))]),
            ],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_empty_list() {
        let parser = Parser::new("items.store([])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![ArgType::Identifier("items"), ArgType::List(vec![])],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_string_arg() {
        let parser = Parser::new("say(\"hello world\")");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "say",
            args: vec![ArgType::String(Cow::Borrowed("hello world"))],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_deeply_nested_expression() {
        let parser = Parser::new("print(concat(add(1, 2), add(3, 4)))");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "print",
            args: vec![ArgType::Expression(Expression {
                name: "concat",
                args: vec![
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("1"), ArgType::Number("2")],
                    }),
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("3"), ArgType::Number("4")],
                    }),
                ],
            })],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn command_as_structure() {
        let parser = Parser::new(
            r#"
            counter.store(0)
            repeat(0,
                counter.store(add(counter, 1))
            )
            print(counter)
        "#
            .trim_matches(|c: char| c.is_whitespace()),
        );
        let parsed = parser.parse().unwrap();
        let expected = vec![
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("counter"), ArgType::Number("0")],
            },
            Expression {
                name: "repeat",
                args: vec![
                    ArgType::Number("0"),
                    ArgType::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgType::Identifier("counter"),
                            ArgType::Expression(Expression {
                                name: "add",
                                args: vec![ArgType::Identifier("counter"), ArgType::Number("1")],
                            }),
                        ],
                    }),
                ],
            },
            Expression {
                name: "print",
                args: vec![ArgType::Identifier("counter")],
            },
        ];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn large_chained_command() {
        let parser = Parser::new(
            r#"
                text.remove_whitespace().uppercase().concat("!!!")
            "#
            .trim_matches(|c: char| c.is_whitespace()),
        );
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "concat",
            args: vec![
                ArgType::Expression(Expression {
                    name: "uppercase",
                    args: vec![ArgType::Expression(Expression {
                        name: "remove_whitespace",
                        args: vec![ArgType::Identifier("text")],
                    })],
                }),
                ArgType::String(Cow::Borrowed("!!!")),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn command_with_large_chained_arg() {
        let parser = Parser::new(
            r#"
            text.store("  hello world  ")
            result.store(
                text
                    .remove_whitespace()
                    .uppercase()
                    .concat("!!!")
            )
            print(result)
            "#
            .trim_matches(|c: char| c.is_whitespace()),
        );
        let parsed = parser.parse().unwrap();
        let expected = vec![
            Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("text"),
                    ArgType::String(Cow::Borrowed("  hello world  ")),
                ],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("result"),
                    ArgType::Expression(Expression {
                        name: "concat",
                        args: vec![
                            ArgType::Expression(Expression {
                                name: "uppercase",
                                args: vec![ArgType::Expression(Expression {
                                    name: "remove_whitespace",
                                    args: vec![ArgType::Identifier("text")],
                                })],
                            }),
                            ArgType::String(Cow::Borrowed("!!!")),
                        ],
                    }),
                ],
            },
            Expression {
                name: "print",
                args: vec![ArgType::Identifier("result")],
            },
        ];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }
    #[test]
    fn multiple_commands_mixed_values() {
        let parser = Parser::new(
            r#"
                empty.store([])
                print("Length: ", empty.length())
                empty.insert(0, 42)
                print(" After insert: ", empty.index(0))
            "#,
        );
        let parsed = parser.parse().unwrap();
        let expected = vec![
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("empty"), ArgType::List(vec![])],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgType::String(Cow::Borrowed("Length: ")),
                    ArgType::Expression(Expression {
                        name: "length",
                        args: vec![ArgType::Identifier("empty")],
                    }),
                ],
            },
            Expression {
                name: "insert",
                args: vec![
                    ArgType::Identifier("empty"),
                    ArgType::Number("0"),
                    ArgType::Number("42"),
                ],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgType::String(Cow::Borrowed(" After insert: ")),
                    ArgType::Expression(Expression {
                        name: "index",
                        args: vec![ArgType::Identifier("empty"), ArgType::Number("0")],
                    }),
                ],
            },
        ];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }
    #[test]
    fn chained_command_with_list_with_command() {
        let parser = Parser::new(
            r#"
                index([1, add(1, 1), 3], ["one"]).print()
            "#,
        );
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "print",
            args: vec![ArgType::Expression(Expression {
                name: "index",
                args: vec![
                    ArgType::List(vec![
                        ArgType::Number("1"),
                        ArgType::Expression(Expression {
                            name: "add",
                            args: vec![ArgType::Number("1"), ArgType::Number("1")],
                        }),
                        ArgType::Number("3"),
                    ]),
                    ArgType::List(vec![ArgType::String(Cow::Borrowed("one"))]),
                ],
            })],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn deeply_nested_commands() {
        let result = Parser::new("a(b(c(d(e(f(g(h(i(j(k()))))))))))").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "a",
                args: vec![ArgType::Expression(Expression {
                    name: "b",
                    args: vec![ArgType::Expression(Expression {
                        name: "c",
                        args: vec![ArgType::Expression(Expression {
                            name: "d",
                            args: vec![ArgType::Expression(Expression {
                                name: "e",
                                args: vec![ArgType::Expression(Expression {
                                    name: "f",
                                    args: vec![ArgType::Expression(Expression {
                                        name: "g",
                                        args: vec![ArgType::Expression(Expression {
                                            name: "h",
                                            args: vec![ArgType::Expression(Expression {
                                                name: "i",
                                                args: vec![ArgType::Expression(Expression {
                                                    name: "j",
                                                    args: vec![ArgType::Expression(Expression {
                                                        name: "k",
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
        let result = Parser::new("process([add(1, 2), sub(5, 3), mul(2, 4)])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "process",
                args: vec![ArgType::List(vec![
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("1"), ArgType::Number("2"),]
                    }),
                    ArgType::Expression(Expression {
                        name: "sub",
                        args: vec![ArgType::Number("5"), ArgType::Number("3"),]
                    }),
                    ArgType::Expression(Expression {
                        name: "mul",
                        args: vec![ArgType::Number("2"), ArgType::Number("4"),]
                    })
                ])]
            }]
        );
    }

    #[test]
    fn lists_in_commands_in_lists() {
        let result = Parser::new("outer([inner([1, 2]), inner([3, 4])])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![ArgType::List(vec![
                    ArgType::Expression(Expression {
                        name: "inner",
                        args: vec![ArgType::List(vec![
                            ArgType::Number("1"),
                            ArgType::Number("2"),
                        ])]
                    }),
                    ArgType::Expression(Expression {
                        name: "inner",
                        args: vec![ArgType::List(vec![
                            ArgType::Number("3"),
                            ArgType::Number("4"),
                        ])]
                    })
                ])]
            }]
        );
    }

    #[test]
    fn chain_after_expression() {
        let result = Parser::new(r#"add(1, 1).add(1)"#).parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add",
                args: vec![
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![ArgType::Number("1"), ArgType::Number("1"),]
                    }),
                    ArgType::Number("1")
                ]
            }]
        );
    }

    #[test]
    fn chain_after_command_with_list() {
        let result = Parser::new(r#"index(["john", "joseph"], 1).print()"#).parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "print",
                args: vec![ArgType::Expression(Expression {
                    name: "index",
                    args: vec![
                        ArgType::List(vec![
                            ArgType::String(Cow::Borrowed("john")),
                            ArgType::String(Cow::Borrowed("joseph")),
                        ]),
                        ArgType::Number("1")
                    ]
                })]
            }]
        );
    }

    #[test]
    fn multiple_statements_with_method_chains() {
        let result = Parser::new(
            r#"names.store(["John", "James", "Joseph", "Alexander"])
        i.store(0)
        repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))
        "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![
                Expression {
                    name: "store",
                    args: vec![
                        ArgType::Identifier("names"),
                        ArgType::List(vec![
                            ArgType::String(Cow::Borrowed("John")),
                            ArgType::String(Cow::Borrowed("James")),
                            ArgType::String(Cow::Borrowed("Joseph")),
                            ArgType::String(Cow::Borrowed("Alexander")),
                        ])
                    ]
                },
                Expression {
                    name: "store",
                    args: vec![ArgType::Identifier("i"), ArgType::Number("0")]
                },
                Expression {
                    name: "repeat",
                    args: vec![
                        ArgType::Expression(Expression {
                            name: "length",
                            args: vec![ArgType::Identifier("names")]
                        }),
                        ArgType::Expression(Expression {
                            name: "print",
                            args: vec![ArgType::Expression(Expression {
                                name: "index",
                                args: vec![ArgType::Identifier("names"), ArgType::Identifier("i")]
                            })]
                        }),
                        ArgType::Expression(Expression {
                            name: "print",
                            args: vec![ArgType::String(Cow::Borrowed("\n"))]
                        }),
                        ArgType::Expression(Expression {
                            name: "store",
                            args: vec![
                                ArgType::Identifier("i"),
                                ArgType::Expression(Expression {
                                    name: "add",
                                    args: vec![ArgType::Identifier("i"), ArgType::Number("1")]
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
        let result = Parser::new("store(1).").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn command_without_parens() {
        let result = Parser::new("print").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_open_paren() {
        let result = Parser::new("add(1, 2").parse();
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_close_paren() {
        let result = Parser::new("add 1, 2)").parse();
        assert!(result.is_err());
    }

    #[test]
    fn unbalanced_brackets() {
        let result = Parser::new("store([1, 2)").parse();
        assert!(result.is_err());
    }
    #[test]
    fn dot_arg() {
        let result = Parser::new("i.store(0)").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert!(
            expressions
                == vec![Expression {
                    name: "store",
                    args: vec![ArgType::Identifier("i"), ArgType::Number("0")]
                }]
        );
    }

    #[test]
    fn dot_arg_object() {
        let result = Parser::new("character.weapon.name.set(\"sword\")").parse();
        let parsed = result.unwrap();
        let expected = vec![Expression {
            name: "set",
            args: vec![
                ArgType::Identifier("character"),
                ArgType::List(vec![
                    ArgType::String(Cow::Borrowed("weapon")),
                    ArgType::String(Cow::Borrowed("name")),
                ]),
                ArgType::String(Cow::Borrowed("sword")),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(parsed == expected);
    }

    #[test]
    fn dot_arg_invalid_object() {
        let result = Parser::new("character.\"weapon\".\"name\".store(\"sword\")").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn dot_arg_chain() {
        let result =
            Parser::new("names.index(0).ends_with(\"2\").if_then(print(\"it's name 2!!\"))")
                .parse();
        dbg!(&result);
        let expressions = result.unwrap();

        assert_eq!(
            expressions,
            vec![Expression {
                name: "if_then",
                args: vec![
                    ArgType::Expression(Expression {
                        name: "ends_with",
                        args: vec![
                            ArgType::Expression(Expression {
                                name: "index",
                                args: vec![ArgType::Identifier("names"), ArgType::Number("0")]
                            }),
                            ArgType::String(Cow::Borrowed("2"))
                        ]
                    }),
                    ArgType::Expression(Expression {
                        name: "print",
                        args: vec![ArgType::String(Cow::Borrowed("it's name 2!!"))]
                    })
                ]
            }]
        );
    }

    #[test]
    fn nested_command() {
        let result = Parser::new("quadruple.def(add(double(x), double(x)))").parse();
        dbg!(&result);
        let expressions = result.unwrap();

        assert_eq!(
            expressions,
            vec![Expression {
                name: "def",
                args: vec![
                    ArgType::Identifier("quadruple"),
                    ArgType::Expression(Expression {
                        name: "add",
                        args: vec![
                            ArgType::Expression(Expression {
                                name: "double",
                                args: vec![ArgType::Identifier("x")]
                            }),
                            ArgType::Expression(Expression {
                                name: "double",
                                args: vec![ArgType::Identifier("x")]
                            })
                        ]
                    })
                ]
            }]
        );
    }

    #[test]
    fn empty_args() {
        let result = Parser::new("print()").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "print",
                args: vec![]
            }]
        );
    }

    #[test]
    fn multiple_args() {
        let result = Parser::new("add(1, 2, 3, 4)").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add",
                args: vec![
                    ArgType::Number("1"),
                    ArgType::Number("2"),
                    ArgType::Number("3"),
                    ArgType::Number("4"),
                ]
            }]
        );
    }

    #[test]
    fn list_with_expressions() {
        let result = Parser::new("list.store([1, add(2, 3), 4])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("list"),
                    ArgType::List(vec![
                        ArgType::Number("1"),
                        ArgType::Expression(Expression {
                            name: "add",
                            args: vec![ArgType::Number("2"), ArgType::Number("3"),]
                        }),
                        ArgType::Number("4"),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn nested_lists() {
        let result = Parser::new("matrix.store([[1, 2, 3], [4, 5, 6], [7, 8, 9]])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("matrix"),
                    ArgType::List(vec![
                        ArgType::List(vec![
                            ArgType::Number("1"),
                            ArgType::Number("2"),
                            ArgType::Number("3"),
                        ]),
                        ArgType::List(vec![
                            ArgType::Number("4"),
                            ArgType::Number("5"),
                            ArgType::Number("6"),
                        ]),
                        ArgType::List(vec![
                            ArgType::Number("7"),
                            ArgType::Number("8"),
                            ArgType::Number("9"),
                        ]),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn deeply_nested_lists() {
        let result = Parser::new("data.store([1, [2, [3, [4, 5]]]])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("data"),
                    ArgType::List(vec![
                        ArgType::Number("1"),
                        ArgType::List(vec![
                            ArgType::Number("2"),
                            ArgType::List(vec![
                                ArgType::Number("3"),
                                ArgType::List(vec![ArgType::Number("4"), ArgType::Number("5"),])
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
            Parser::new("data.store([x, \"hello\", 42, add(1, 2), [nested, list]])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("data"),
                    ArgType::List(vec![
                        ArgType::Identifier("x"),
                        ArgType::String(Cow::Borrowed("hello")),
                        ArgType::Number("42"),
                        ArgType::Expression(Expression {
                            name: "add",
                            args: vec![ArgType::Number("1"), ArgType::Number("2"),]
                        }),
                        ArgType::List(vec![
                            ArgType::Identifier("nested"),
                            ArgType::Identifier("list"),
                        ])
                    ])
                ]
            }]
        );
    }

    #[test]
    fn deeply_nested_commands_lists_mixed() {
        let result = Parser::new(
        "transform(data.map([filter([1, 2, 3], is_even()), sort([b, a, c])]), [process(x), process(y)])"
    ).parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "transform",
                args: vec![
                    ArgType::Expression(Expression {
                        name: "map",
                        args: vec![
                            ArgType::Identifier("data"),
                            ArgType::List(vec![
                                ArgType::Expression(Expression {
                                    name: "filter",
                                    args: vec![
                                        ArgType::List(vec![
                                            ArgType::Number("1"),
                                            ArgType::Number("2"),
                                            ArgType::Number("3"),
                                        ]),
                                        ArgType::Expression(Expression {
                                            name: "is_even",
                                            args: vec![]
                                        })
                                    ]
                                }),
                                ArgType::Expression(Expression {
                                    name: "sort",
                                    args: vec![ArgType::List(vec![
                                        ArgType::Identifier("b"),
                                        ArgType::Identifier("a"),
                                        ArgType::Identifier("c"),
                                    ])]
                                })
                            ])
                        ]
                    }),
                    ArgType::List(vec![
                        ArgType::Expression(Expression {
                            name: "process",
                            args: vec![ArgType::Identifier("x")]
                        }),
                        ArgType::Expression(Expression {
                            name: "process",
                            args: vec![ArgType::Identifier("y")]
                        })
                    ])
                ]
            }]
        );
    }

    #[test]
    fn matrix_of_commands() {
        let result =
            Parser::new("grid.store([[add(1,2), sub(3,4)], [mul(5,6), div(7,8)]])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("grid"),
                    ArgType::List(vec![
                        ArgType::List(vec![
                            ArgType::Expression(Expression {
                                name: "add",
                                args: vec![ArgType::Number("1"), ArgType::Number("2"),]
                            }),
                            ArgType::Expression(Expression {
                                name: "sub",
                                args: vec![ArgType::Number("3"), ArgType::Number("4"),]
                            })
                        ]),
                        ArgType::List(vec![
                            ArgType::Expression(Expression {
                                name: "mul",
                                args: vec![ArgType::Number("5"), ArgType::Number("6"),]
                            }),
                            ArgType::Expression(Expression {
                                name: "div",
                                args: vec![ArgType::Number("7"), ArgType::Number("8"),]
                            })
                        ])
                    ])
                ]
            }]
        );
    }

    #[test]
    fn command_with_list_containing_nested_command_with_list() {
        let result = Parser::new("outer([a, inner([b, deeper([c, d])]), e])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![ArgType::List(vec![
                    ArgType::Identifier("a"),
                    ArgType::Expression(Expression {
                        name: "inner",
                        args: vec![ArgType::List(vec![
                            ArgType::Identifier("b"),
                            ArgType::Expression(Expression {
                                name: "deeper",
                                args: vec![ArgType::List(vec![
                                    ArgType::Identifier("c"),
                                    ArgType::Identifier("d"),
                                ])]
                            })
                        ])]
                    }),
                    ArgType::Identifier("e"),
                ])]
            }]
        );
    }

    #[test]
    fn triple_nested_list_command_list() {
        let result = Parser::new("outer([inner([[a, b]])])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![ArgType::List(vec![ArgType::Expression(Expression {
                    name: "inner",
                    args: vec![ArgType::List(vec![ArgType::List(vec![
                        ArgType::Identifier("a"),
                        ArgType::Identifier("b"),
                    ])])]
                })])]
            }]
        );
    }

    #[test]
    fn inner_thing() {
        let result = Parser::new("outer(inner2([c, [d]]))").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![ArgType::Expression(Expression {
                    name: "inner2",
                    args: vec![ArgType::List(vec![
                        ArgType::Identifier("c"),
                        ArgType::List(vec![ArgType::Identifier("d"),])
                    ])]
                })]
            }]
        );
    }

    #[test]
    fn command_list_command_command() {
        let result = Parser::new("outer([inner(deepest())])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![ArgType::List(vec![ArgType::Expression(Expression {
                    name: "inner",
                    args: vec![ArgType::Expression(Expression {
                        name: "deepest",
                        args: vec![]
                    })]
                })])]
            }]
        );
    }

    #[test]
    fn multiple_lists_same_level() {
        let result = Parser::new("outer([a], [b], inner([c]))").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![
                    ArgType::List(vec![ArgType::Identifier("a")]),
                    ArgType::List(vec![ArgType::Identifier("b")]),
                    ArgType::Expression(Expression {
                        name: "inner",
                        args: vec![ArgType::List(vec![ArgType::Identifier("c")])]
                    })
                ]
            }]
        );
    }

    #[test]
    fn parallel_nested_structures() {
        let result = Parser::new("outer([a, inner1([b])], inner2([c, [d]]))").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![
                    ArgType::List(vec![
                        ArgType::Identifier("a"),
                        ArgType::Expression(Expression {
                            name: "inner1",
                            args: vec![ArgType::List(vec![ArgType::Identifier("b")])]
                        })
                    ]),
                    ArgType::Expression(Expression {
                        name: "inner2",
                        args: vec![ArgType::List(vec![
                            ArgType::Identifier("c"),
                            ArgType::List(vec![ArgType::Identifier("d")])
                        ])]
                    })
                ]
            }]
        );
    }

    #[test]
    fn command_after_list_in_command() {
        let result = Parser::new("outer([a, b], inner())").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "outer",
                args: vec![
                    ArgType::List(vec![ArgType::Identifier("a"), ArgType::Identifier("b"),]),
                    ArgType::Expression(Expression {
                        name: "inner",
                        args: vec![]
                    })
                ]
            }]
        );
    }

    #[test]
    fn empty_list() {
        let result = Parser::new("data.store([])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![ArgType::Identifier("data"), ArgType::List(vec![])]
            }]
        );
    }

    #[test]
    fn trailing_comma_list() {
        let result = Parser::new("data.store([1, 2,])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("data"),
                    ArgType::List(vec![ArgType::Number("1"), ArgType::Number("2"),])
                ]
            }]
        );
    }

    #[test]
    fn multiple_trailing_commas_should_error() {
        let result = Parser::new("data.store([1, 2,,,,,])").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn trailing_comma_command() {
        let result = Parser::new("add(1, 2,)").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "add",
                args: vec![ArgType::Number("1"), ArgType::Number("2"),]
            }]
        );
    }

    #[test]
    fn chain_after_nested_expression() {
        let result = Parser::new("outer(inner(x)).method(y)").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "method",
                args: vec![
                    ArgType::Expression(Expression {
                        name: "outer",
                        args: vec![ArgType::Expression(Expression {
                            name: "inner",
                            args: vec![ArgType::Identifier("x")]
                        })]
                    }),
                    ArgType::Identifier("y")
                ]
            }]
        );
    }

    #[test]
    fn dot_at_start_should_error() {
        let result = Parser::new(".store(1)").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn consecutive_dots_should_error() {
        let result = Parser::new("a..b.store(1)").parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn literal_at_top_level_should_error() {
        let result = Parser::new("42").parse();
        dbg!(&result);
        assert!(result.is_ok() && result.clone().unwrap().is_empty() || result.is_err());
    }

    #[test]
    fn mixed_state_transitions() {
        let result = Parser::new("a([b(c), d([e, f])])").parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "a",
                args: vec![ArgType::List(vec![
                    ArgType::Expression(Expression {
                        name: "b",
                        args: vec![ArgType::Identifier("c")]
                    }),
                    ArgType::Expression(Expression {
                        name: "d",
                        args: vec![ArgType::List(vec![
                            ArgType::Identifier("e"),
                            ArgType::Identifier("f"),
                        ])]
                    })
                ])]
            }]
        );
    }

    #[test]
    fn map_with_mixed_content() {
        let result = Parser::new(
            r#"
                player.store([
                name: "blah",
                health: 100,
                dodge: 0,
                strength: 0
                ])
            "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("player"),
                    ArgType::Map(vec![
                        ("name", ArgType::String(Cow::Borrowed("blah"))),
                        ("health", ArgType::Number("100")),
                        ("dodge", ArgType::Number("0")),
                        ("strength", ArgType::Number("0")),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn map_with_list() {
        let result = Parser::new(
            r#"
                player.store([
                name: "blah",
                items: [x, y, z]
                ])
            "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("player"),
                    ArgType::Map(vec![
                        ("name", ArgType::String(Cow::Borrowed("blah"))),
                        (
                            "items",
                            ArgType::List(vec![
                                ArgType::Identifier("x"),
                                ArgType::Identifier("y"),
                                ArgType::Identifier("z"),
                            ])
                        ),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn map_inside_list_inside_map() {
        let result = Parser::new(
            r#"
                player.store([
                name: "blah",
                inventory: [
                    [
                        name: "sword",
                        damage: 10
                    ],
                    [
                        name: "shield",
                        defense: 5
                    ]
                ]
                ])
            "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("player"),
                    ArgType::Map(vec![
                        ("name", ArgType::String(Cow::Borrowed("blah"))),
                        (
                            "inventory",
                            ArgType::List(vec![
                                ArgType::Map(vec![
                                    ("name", ArgType::String(Cow::Borrowed("sword"))),
                                    ("damage", ArgType::Number("10")),
                                ]),
                                ArgType::Map(vec![
                                    ("name", ArgType::String(Cow::Borrowed("shield"))),
                                    ("defense", ArgType::Number("5")),
                                ]),
                            ])
                        ),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn map_inside_map() {
        let result = Parser::new(
            r#"
                player.store([
                name: "blah",
                stats: [
                    health: 100,
                    strength: 10
                ]
                ])
            "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("player"),
                    ArgType::Map(vec![
                        ("name", ArgType::String(Cow::Borrowed("blah"))),
                        (
                            "stats",
                            ArgType::Map(vec![
                                ("health", ArgType::Number("100")),
                                ("strength", ArgType::Number("10")),
                            ])
                        ),
                    ])
                ]
            }]
        );
    }

    #[test]
    fn deeply_nested_maps() {
        let result = Parser::new(
            r#"
                world.store([
                player: [
                    name: "blah",
                    location: [
                        x: 0,
                        y: 0
                    ]
                ]
                ])
            "#,
        )
        .parse();
        dbg!(&result);
        let expressions = result.unwrap();
        assert_eq!(
            expressions,
            vec![Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("world"),
                    ArgType::Map(vec![(
                        "player",
                        ArgType::Map(vec![
                            ("name", ArgType::String(Cow::Borrowed("blah"))),
                            (
                                "location",
                                ArgType::Map(vec![
                                    ("x", ArgType::Number("0")),
                                    ("y", ArgType::Number("0")),
                                ])
                            ),
                        ])
                    )])
                ]
            }]
        );
    }

    #[test]
    fn map_value_is_expression() {
        let parsed = Parser::new(
            r#"
                player.store([
                name: get_name(),
                health: add(50, 50)
                ])
            "#,
        )
        .parse()
        .unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::Map(vec![
                    (
                        "name",
                        ArgType::Expression(Expression {
                            name: "get_name",
                            args: vec![],
                        }),
                    ),
                    (
                        "health",
                        ArgType::Expression(Expression {
                            name: "add",
                            args: vec![ArgType::Number("50"), ArgType::Number("50")],
                        }),
                    ),
                ]),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn parameters() {
        let parsed = Parser::new(
            r#"
            C_MODS.store(0)
            P_MOD.store(0)
            P_DUR.store(1)
            attacker_mods.store([])
            attacker.store([attacker_mods])
            potion.store([1, 1])
            mods.store([])
            add_potion_modifier.def(potion,
			    mods.store(attacker.index(C_MODS).clone())
			    mods.push([potion.index(P_MOD).clone(), potion.index(P_DUR).clone()])
			    attacker.index(C_MODS).store(mods.clone())
            )
            add_potion_modifier([50, 3])
			print("potion: [1, 1]\n", concat("mods: ", mods, "\nattacker_mods: ", attacker.index(C_MODS)))
            "#,
        )
        .parse()
        .unwrap();
        let expected = vec![
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("C_MODS"), ArgType::Number("0")],
            },
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("P_MOD"), ArgType::Number("0")],
            },
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("P_DUR"), ArgType::Number("1")],
            },
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("attacker_mods"), ArgType::List(vec![])],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("attacker"),
                    ArgType::List(vec![ArgType::Identifier("attacker_mods")]),
                ],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgType::Identifier("potion"),
                    ArgType::List(vec![ArgType::Number("1"), ArgType::Number("1")]),
                ],
            },
            Expression {
                name: "store",
                args: vec![ArgType::Identifier("mods"), ArgType::List(vec![])],
            },
            Expression {
                name: "def",
                args: vec![
                    ArgType::Identifier("add_potion_modifier"),
                    ArgType::Identifier("potion"),
                    ArgType::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgType::Identifier("mods"),
                            ArgType::Expression(Expression {
                                name: "clone",
                                args: vec![ArgType::Expression(Expression {
                                    name: "index",
                                    args: vec![
                                        ArgType::Identifier("attacker"),
                                        ArgType::Identifier("C_MODS"),
                                    ],
                                })],
                            }),
                        ],
                    }),
                    ArgType::Expression(Expression {
                        name: "push",
                        args: vec![
                            ArgType::Identifier("mods"),
                            ArgType::List(vec![
                                ArgType::Expression(Expression {
                                    name: "clone",
                                    args: vec![ArgType::Expression(Expression {
                                        name: "index",
                                        args: vec![
                                            ArgType::Identifier("potion"),
                                            ArgType::Identifier("P_MOD"),
                                        ],
                                    })],
                                }),
                                ArgType::Expression(Expression {
                                    name: "clone",
                                    args: vec![ArgType::Expression(Expression {
                                        name: "index",
                                        args: vec![
                                            ArgType::Identifier("potion"),
                                            ArgType::Identifier("P_DUR"),
                                        ],
                                    })],
                                }),
                            ]),
                        ],
                    }),
                    ArgType::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgType::Expression(Expression {
                                name: "index",
                                args: vec![
                                    ArgType::Identifier("attacker"),
                                    ArgType::Identifier("C_MODS"),
                                ],
                            }),
                            ArgType::Expression(Expression {
                                name: "clone",
                                args: vec![ArgType::Identifier("mods")],
                            }),
                        ],
                    }),
                ],
            },
            Expression {
                name: "add_potion_modifier",
                args: vec![ArgType::List(vec![
                    ArgType::Number("50"),
                    ArgType::Number("3"),
                ])],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgType::String(Cow::Borrowed("potion: [1, 1]\n")),
                    ArgType::Expression(Expression {
                        name: "concat",
                        args: vec![
                            ArgType::String(Cow::Borrowed("mods: ")),
                            ArgType::Identifier("mods"),
                            ArgType::String(Cow::Borrowed("\nattacker_mods: ")),
                            ArgType::Expression(Expression {
                                name: "index",
                                args: vec![
                                    ArgType::Identifier("attacker"),
                                    ArgType::Identifier("C_MODS"),
                                ],
                            }),
                        ],
                    }),
                ],
            },
        ];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert_eq!(parsed, expected);
    }

    #[test]
    fn mixed_list_and_map_is_error() {
        let result = Parser::new(
            r#"
            player.store([
                "blah",
                name: "blah"
            ])
        "#,
        )
        .parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn key_without_value_is_error() {
        let result = Parser::new(
            r#"
            player.store([
                name: "blah",
                weapon:
            ])
        "#,
        )
        .parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn comma_outside_of_container() {
        let result = Parser::new(
            r#"
            print("invalid"),
            print("comma")
            "#,
        )
        .parse();
        dbg!(&result);
        assert!(result.is_err());
    }

    #[test]
    fn map_dot_arg() {
        let parsed = Parser::new(
            r#"
            player.store([
                name: "blah",
                weapon: "big".uppercase().concat(" ", "sword")
            ])
        "#,
        )
        .parse()
        .unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::Map(vec![
                    ("name", ArgType::String("blah".into())),
                    (
                        "weapon",
                        ArgType::Expression(Expression {
                            name: "concat",
                            args: vec![
                                ArgType::Expression(Expression {
                                    name: "uppercase",
                                    args: vec![ArgType::String("big".into())],
                                }),
                                ArgType::String(" ".into()),
                                ArgType::String("sword".into()),
                            ],
                        }),
                    ),
                ]),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(parsed == expected);
    }

    #[test]
    fn map_dot_arg_with_trailing_commas() {
        let parsed = Parser::new(
            r#"
            player.store([
                name: "blah",
                weapon: "big".uppercase().concat(" ", "sword"),
            ],)
        "#,
        )
        .parse()
        .unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgType::Identifier("player"),
                ArgType::Map(vec![
                    ("name", ArgType::String("blah".into())),
                    (
                        "weapon",
                        ArgType::Expression(Expression {
                            name: "concat",
                            args: vec![
                                ArgType::Expression(Expression {
                                    name: "uppercase",
                                    args: vec![ArgType::String("big".into())],
                                }),
                                ArgType::String(" ".into()),
                                ArgType::String("sword".into()),
                            ],
                        }),
                    ),
                ]),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(parsed == expected);
    }
}
