use std::{borrow::Cow, collections::VecDeque, fmt::Display};

use crate::lexer::{LexError, Lexer, Symbol, Token, TokenType};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span<'c> {
    pub start: Token<'c>,
    pub end: Token<'c>,
}

impl<'c> Span<'c> {
    pub fn new(start: Token<'c>, end: Token<'c>) -> Self {
        Self { start, end }
    }

    pub fn as_str(&self) -> &'c str {
        Token::span(self.start, self.end)
    }
}

impl<'c> From<Token<'c>> for Span<'c> {
    fn from(value: Token<'c>) -> Self {
        Self {
            start: value,
            end: value,
        }
    }
}

impl<'c> From<&Expression<'c>> for Span<'c> {
    fn from(value: &Expression<'c>) -> Self {
        Self {
            start: value.name,
            end: value.end,
        }
    }
}

impl<'c> From<&List<'c>> for Span<'c> {
    fn from(value: &List<'c>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<'c> From<&Map<'c>> for Span<'c> {
    fn from(value: &Map<'c>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path<'c> {
    pub start: Token<'c>,
    pub data: VecDeque<Arg<'c>>,
    pub end: Token<'c>,
}

impl<'c> Path<'c> {
    pub fn new(start: Token<'c>, end: Token<'c>) -> Self {
        Self {
            start,
            end: end,
            data: VecDeque::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct List<'c> {
    pub start: Token<'c>,
    pub data: Vec<Arg<'c>>,
    pub end: Token<'c>,
}

impl<'c> List<'c> {
    pub fn new(start: Token<'c>, end: Token<'c>) -> Self {
        Self {
            start,
            data: Vec::new(),
            end: end,
        }
    }
}

impl<'c> From<Path<'c>> for List<'c> {
    fn from(value: Path<'c>) -> Self {
        Self {
            start: value.start,
            end: value.end,
            data: value.data.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map<'c> {
    pub start: Token<'c>,
    pub data: Vec<(Token<'c>, Arg<'c>)>,
    pub end: Token<'c>,
}

impl<'c> Map<'c> {
    pub fn new(start: Token<'c>, end: Token<'c>) -> Self {
        Self {
            start,
            data: Vec::new(),
            end: end,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'c> {
    pub name: Token<'c>,
    pub start: Token<'c>,
    pub args: Vec<Arg<'c>>,
    pub end: Token<'c>,
}

impl<'c> Expression<'c> {
    pub fn new(name: Token<'c>, end: Token<'c>) -> Expression<'c> {
        Self {
            name,
            start: name,
            args: Vec::new(),
            end: end,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgKind<'c> {
    Number(&'c str),
    String(Cow<'c, str>),
    Keyword(&'c str),
    Identifier(&'c str),
    List(List<'c>),
    Map(Map<'c>),
    Expression(Expression<'c>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg<'c> {
    pub kind: ArgKind<'c>,
    pub token: Token<'c>,
}

impl<'c> Arg<'c> {
    pub fn new(kind: ArgKind<'c>, token: Token<'c>) -> Self {
        Self { kind, token }
    }
}

impl<'c> From<List<'c>> for ArgKind<'c> {
    fn from(value: List<'c>) -> Self {
        Self::List(value)
    }
}

impl<'c> From<Map<'c>> for ArgKind<'c> {
    fn from(value: Map<'c>) -> Self {
        Self::Map(value)
    }
}

impl<'c> From<Expression<'c>> for ArgKind<'c> {
    fn from(value: Expression<'c>) -> Self {
        Self::Expression(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PendingArg<'c> {
    DotArg(Arg<'c>),
    PathArg(Path<'c>),
    Key(Token<'c>),
    Expression(Expression<'c>),
    List(List<'c>),
    Map(Map<'c>),
    UnitializedCollection(Token<'c>),
    Done(Arg<'c>),
}

impl<'c> PendingArg<'c> {
    pub fn start(&self) -> Token<'c> {
        match self {
            PendingArg::DotArg(arg) => arg.token,
            PendingArg::PathArg(path) => path.start,
            PendingArg::Key(token) => *token,
            PendingArg::Expression(expr) => expr.start,
            PendingArg::List(list) => list.start,
            PendingArg::Map(map) => map.start,
            PendingArg::UnitializedCollection(token) => *token,
            PendingArg::Done(arg) => arg.token,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError<'c> {
    LexError(LexError<'c>),
    OutOfPlaceSymbol(Token<'c>),
    OutOfPlaceValue(Token<'c>),
    UnfinishedExpression(Expression<'c>),
    UnfinishedMap(Map<'c>),
    UnfinishedList(List<'c>),
    ValueOutsideOfExpression(Span<'c>),
}

impl<'c> Display for ParseError<'c> {
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
            ParseError::ValueOutsideOfExpression(span) => {
                let start = span.start;
                let value = Token::span(start, span.end).to_string();
                write!(
                    f,
                    "Value outside of expression \"{}\" on line {}\n{}: {}",
                    value,
                    start.line_number(),
                    start.line_number(),
                    start.line(),
                )
            }
            ParseError::UnfinishedExpression(expr) => {
                let value = Token::span(expr.name, expr.end).to_string();
                write!(
                    f,
                    "Unfinished expression \"{}\" on line {}\n{}: {}\n",
                    value,
                    expr.start.line_number(),
                    expr.start.line_number(),
                    expr.start.line(),
                )
            }
            ParseError::UnfinishedList(span) => {
                let start = span.start;
                let value = Token::span(start, span.end).to_string();
                write!(
                    f,
                    "Unfinished list \"{}\" on line {}\n{}: {}",
                    value,
                    start.line_number(),
                    start.line_number(),
                    start.line(),
                )
            }
            ParseError::UnfinishedMap(map) => {
                write!(
                    f,
                    "Unfinished map \"{}\" on line {}\n{}: {}\n",
                    map.start.token_type,
                    map.start.line_number(),
                    map.start.line_number(),
                    map.start.line(),
                )
            }
        }
    }
}

impl<'c> From<LexError<'c>> for ParseError<'c> {
    fn from(value: LexError<'c>) -> Self {
        Self::LexError(value)
    }
}

pub struct Parser<'c> {
    lexer: Option<Lexer<'c>>,
    pending: Vec<PendingArg<'c>>,
}

impl<'c> Parser<'c> {
    pub fn new(input: &'c str) -> Parser<'c> {
        Self {
            lexer: Some(Lexer::new(input)),
            pending: Vec::new(),
        }
    }

    fn pend_map(&mut self, map: Map<'c>) {
        self.pending.push(PendingArg::Map(map));
    }

    fn pend_map_with_value(&mut self, map: Map<'c>, key: Token<'c>, value: Option<Arg<'c>>) {
        let mut map = map;
        if let Some(value) = value {
            map.end = value.token;
            map.data.push((key, value));
        }
        self.pending.push(PendingArg::Map(map));
    }

    fn try_pend_map_with_value(
        &mut self,
        token: Token<'c>,
        key: Token<'c>,
        value: Option<Arg<'c>>,
    ) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(maybe_map) => match maybe_map {
                PendingArg::Map(map) => Ok(self.pend_map_with_value(map, key, value)),
                PendingArg::UnitializedCollection(start) => {
                    Ok(self.pend_map_with_value(Map::new(start, start), key, value))
                }
                _ => Err(ParseError::OutOfPlaceValue(maybe_map.start())),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn pend_list_with_value(&mut self, list: List<'c>, value: Option<Arg<'c>>) {
        let mut list = list;
        if let Some(value) = value {
            list.end = value.token;
            list.data.push(value);
        }
        self.pending.push(PendingArg::List(list));
    }

    fn pend_uninit_collection(&mut self, token: Token<'c>) {
        self.pending.push(PendingArg::UnitializedCollection(token));
    }

    fn pend_expr_with_value(&mut self, expr: Expression<'c>, value: Arg<'c>) {
        let mut expr = expr;
        expr.end = value.token;
        expr.args.push(value);
        self.pending.push(PendingArg::Expression(expr));
    }

    fn pend_expr(&mut self, expr: Expression<'c>) {
        self.pending.push(PendingArg::Expression(expr));
    }

    fn pend_list(&mut self, list: List<'c>) {
        self.pending.push(PendingArg::List(list));
    }

    fn pend_done(&mut self, done: Arg<'c>) {
        self.pending.push(PendingArg::Done(done));
    }

    fn pend_dot_arg(&mut self, arg: Arg<'c>) {
        self.pending.push(PendingArg::DotArg(arg));
    }

    fn pend_path(&mut self, path: Path<'c>) {
        self.pending.push(PendingArg::PathArg(path));
    }

    fn pend_key(&mut self, key: Token<'c>) {
        self.pending.push(PendingArg::Key(key));
    }

    fn pend_last_as_done(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(last) => match last {
                PendingArg::Expression(expr) => Ok(self.pend_done(Arg::new(expr.into(), token))),
                PendingArg::List(mut list) => {
                    list.end = token;
                    Ok(self.pend_done(Arg::new(list.into(), token)))
                }
                PendingArg::Map(mut map) => {
                    map.end = token;
                    Ok(self.pend_done(Arg::new(map.into(), token)))
                }
                _ => Err(ParseError::OutOfPlaceValue(last.start())),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn get_remaining_arg(&mut self) -> Option<Arg<'c>> {
        match self.pending.pop() {
            Some(pending) => {
                if let PendingArg::Done(arg) = pending {
                    Some(arg)
                } else {
                    self.pending.push(pending);
                    None
                }
            }
            None => None,
        }
    }

    fn parse_open_paren(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(pending) => match pending {
                PendingArg::Expression(mut expr) => {
                    expr.start = token;

                    let pending_dot = match self.pending.pop() {
                        Some(pending) => match pending {
                            PendingArg::DotArg(dot_arg) => Some(PendingArg::DotArg(dot_arg)),
                            PendingArg::PathArg(path) => Some(PendingArg::PathArg(path)),
                            _ => {
                                self.pending.push(pending);
                                None
                            }
                        },
                        None => None,
                    };
                    if let Some(pending_arg) = pending_dot {
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
                                    expr.args.push(path.data.pop_front().unwrap());
                                    let data: Result<VecDeque<Arg<'c>>, ParseError<'c>> = path
                                        .data
                                        .iter()
                                        .map(|item| match item.kind {
                                            ArgKind::Identifier(ident) => Ok(Arg::new(
                                                ArgKind::String(Cow::Borrowed(ident)),
                                                item.token,
                                            )),
                                            _ => Err(ParseError::OutOfPlaceValue(item.token)),
                                        })
                                        .collect();

                                    path.data = data?;
                                    expr.args.push(Arg::new(ArgKind::List(path.into()), token));
                                }
                            }
                            _ => {}
                        }
                    }
                    self.pending.push(PendingArg::Expression(expr));
                    Ok(())
                }
                _ => {
                    unreachable!("Lexer should have inserted empty command token before (")
                }
            },
            None => {
                unreachable!("Lexer should have inserted empty command token before (")
            }
        }
    }

    fn parse_closed_paren(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        let last_arg = self.get_remaining_arg();
        match self.pending.pop() {
            Some(maybe_expr) => match maybe_expr {
                PendingArg::Expression(mut expr) => {
                    expr.end = token;
                    if let Some(arg) = last_arg {
                        expr.args.push(arg);
                    }
                    match self.pending.pop() {
                        Some(parent) => match parent {
                            PendingArg::Expression(parent_expr) => Ok(self
                                .pend_expr_with_value(parent_expr, Arg::new(expr.into(), token))),
                            PendingArg::List(list) => {
                                Ok(self
                                    .pend_list_with_value(list, Some(Arg::new(expr.into(), token))))
                            }
                            PendingArg::Key(key) => Ok(self.try_pend_map_with_value(
                                token,
                                key,
                                Some(Arg::new(expr.into(), token)),
                            )?),
                            PendingArg::UnitializedCollection(start) => Ok(self
                                .pend_list_with_value(
                                    List::new(start, start),
                                    Some(Arg::new(expr.into(), token)),
                                )),
                            PendingArg::Done(done) => {
                                self.pend_done(done.into());
                                self.pend_done(Arg::new(expr.into(), token));
                                Ok(())
                            }
                            _ => Err(ParseError::OutOfPlaceSymbol(token)),
                        },
                        None => {
                            self.pend_done(Arg::new(expr.into(), token));
                            Ok(())
                        }
                    }
                }
                _ => Err(ParseError::OutOfPlaceValue(maybe_expr.start())),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_open_bracket(&mut self, token: Token<'c>) {
        self.pend_uninit_collection(token);
    }

    fn parse_closed_bracket(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        let last_arg = self.get_remaining_arg();
        match self.pending.pop() {
            Some(collection) => match collection {
                PendingArg::Key(key) => {
                    if last_arg.is_none() {
                        return Err(ParseError::OutOfPlaceValue(collection.start()));
                    }
                    self.try_pend_map_with_value(token, key, last_arg)?
                }
                PendingArg::List(list) => self.pend_list_with_value(list, last_arg),
                PendingArg::Map(_) => {
                    /*
                    This means user left trailing comma which handled map
                    Can safely be marked as done
                    */
                    self.pending.push(collection);
                }
                PendingArg::UnitializedCollection(start) => {
                    self.pend_list_with_value(List::new(start, token), last_arg)
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

    fn parse_colon(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(pending) => match pending {
                PendingArg::Done(Arg {
                    kind: ArgKind::Identifier(_),
                    token,
                }) => Ok(self.pend_key(token)),
                _ => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn try_find_last_expression(
        &mut self,
        token: Token<'c>,
        popped: PendingArg<'c>,
    ) -> Result<(), ParseError<'c>> {
        match popped {
            PendingArg::Expression(mut expr) => match expr.args.pop() {
                Some(last_arg) => {
                    self.pend_expr(expr);
                    Ok(self.pend_dot_arg(last_arg))
                }
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            PendingArg::List(mut list) => match list.data.pop() {
                Some(last_arg) => match last_arg.kind {
                    ArgKind::Expression(_) => {
                        self.pend_list(list);
                        Ok(self.pend_dot_arg(last_arg))
                    }
                    _ => Err(ParseError::OutOfPlaceSymbol(token)),
                },
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            PendingArg::Map(mut map) => match map.data.pop() {
                Some((key, value)) => match value.kind {
                    ArgKind::Expression(_) => {
                        self.pend_map(map);
                        self.pend_key(key);
                        self.pend_dot_arg(value);
                        Ok(())
                    }
                    _ => Err(ParseError::OutOfPlaceSymbol(token)),
                },
                None => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            _ => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_dot(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(pending) => match pending {
                PendingArg::Done(arg) => match self.pending.pop() {
                    Some(pending) => match pending {
                        PendingArg::DotArg(parent_dot_arg) => {
                            let mut path = Path::new(parent_dot_arg.token, arg.token);
                            path.data.push_back(parent_dot_arg);
                            path.data.push_back(arg);
                            self.pend_path(path);
                            Ok(())
                        }
                        PendingArg::PathArg(mut path) => {
                            path.end = arg.token;
                            path.data.push_back(arg);
                            self.pend_path(path);
                            Ok(())
                        }
                        _ => {
                            self.pending.push(pending);
                            Ok(self.pend_dot_arg(arg))
                        }
                    },
                    None => Ok(self.pend_dot_arg(arg)),
                },
                _ => self.try_find_last_expression(token, pending),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn parse_comma(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.pop() {
            Some(pending) => match pending {
                PendingArg::Done(value) => match self.pending.pop() {
                    Some(parent) => match parent {
                        PendingArg::Expression(expression) => {
                            Ok(self.pend_expr_with_value(expression, value.into()))
                        }
                        PendingArg::UnitializedCollection(start) => {
                            Ok(self
                                .pend_list_with_value(List::new(start, value.token), value.into()))
                        }
                        PendingArg::List(list) => Ok(self.pend_list_with_value(list, value.into())),
                        PendingArg::Key(key) => {
                            Ok(self.try_pend_map_with_value(token, key, value.into())?)
                        }
                        PendingArg::Done(_) => {
                            // Top level commas not allowed
                            Err(ParseError::OutOfPlaceSymbol(token))
                        }
                        PendingArg::DotArg(arg) => Err(ParseError::OutOfPlaceValue(arg.token)),
                        PendingArg::PathArg(path) => Err(ParseError::OutOfPlaceValue(path.start)),
                        PendingArg::Map(_) => {
                            unreachable!("map should be listed as done if comma is encountered")
                        }
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

    fn parse_symbol(&mut self, token: Token<'c>, symbol: Symbol) -> Result<(), ParseError<'c>> {
        match symbol {
            Symbol::OpenParen => self.parse_open_paren(token),
            Symbol::ClosedParen => self.parse_closed_paren(token),
            Symbol::OpenBracket => Ok(self.parse_open_bracket(token)),
            Symbol::ClosedBracket => self.parse_closed_bracket(token),
            Symbol::Colon => self.parse_colon(token),
            Symbol::Dot => self.parse_dot(token),
            Symbol::Comma => self.parse_comma(token),
            Symbol::Quote => Ok(()),
            Symbol::Hashtag => Ok(()),
            Symbol::Star => Ok(()),
        }
    }

    pub fn parse_tokens(&mut self) -> Result<(), ParseError<'c>> {
        let lexer = self
            .lexer
            .take()
            .expect("parser should be called once after new");
        let tokens = lexer.collect::<Result<Vec<Token<'c>>, LexError<'c>>>()?;
        let mut tokens = tokens.into_iter().peekable();

        while let Some(token) = tokens.next() {
            // dbg!(token.token_type);
            match token.token_type {
                TokenType::Symbol(symbol) => {
                    self.parse_symbol(token, Symbol::from(symbol))?;
                }
                TokenType::Command(_) => {
                    let pending = PendingArg::Expression(Expression::new(token, token));
                    self.pending.push(pending);
                }
                TokenType::Number(number) => {
                    let pending = PendingArg::Done(Arg::new(ArgKind::Number(number), token));
                    self.pending.push(pending);
                }
                TokenType::String(string) => {
                    let pending =
                        PendingArg::Done(Arg::new(ArgKind::String(parse_string(string)), token));
                    self.pending.push(pending);
                }
                TokenType::Keyword(keyword) => {
                    let pending = PendingArg::Done(Arg::new(ArgKind::Keyword(keyword), token));
                    self.pending.push(pending);
                }
                TokenType::Identifier(identifier) => {
                    let pending =
                        PendingArg::Done(Arg::new(ArgKind::Identifier(identifier), token));
                    self.pending.push(pending);
                }
                TokenType::Comment(_) => { /* Skip Comments */ }
                TokenType::None(_) => {
                    unreachable!("lexer should have handled invalid token errors")
                }
            }
        }
        Ok(())
    }

    fn generate_pending_err(pending: PendingArg<'c>) -> ParseError<'c> {
        match pending {
            PendingArg::Done(arg) => {
                let mut start = arg.token;
                let end;
                match arg.kind {
                    ArgKind::List(list) => {
                        start = list.start;
                        end = list.end;
                    }
                    ArgKind::Map(map) => {
                        start = map.start;
                        end = map.end;
                    }
                    _ => end = arg.token,
                }
                ParseError::ValueOutsideOfExpression(Span::new(start, end))
            }
            PendingArg::DotArg(arg) => ParseError::ValueOutsideOfExpression(Span::from(arg.token)),
            PendingArg::PathArg(path) => {
                ParseError::ValueOutsideOfExpression(Span::new(path.start, path.end))
            }
            PendingArg::Key(token) => ParseError::ValueOutsideOfExpression(Span::from(token)),
            PendingArg::Expression(expression) => ParseError::UnfinishedExpression(expression),
            PendingArg::List(list) => ParseError::UnfinishedList(list),
            PendingArg::Map(map) => ParseError::UnfinishedMap(map),
            PendingArg::UnitializedCollection(token) => ParseError::OutOfPlaceSymbol(token),
        }
    }

    #[allow(dead_code)]
    pub fn parse(mut self) -> Result<Vec<Expression<'c>>, ParseError<'c>> {
        self.parse_tokens()?;
        let mut parsed = Vec::with_capacity(self.pending.len());
        for pending in self.pending.drain(..) {
            match pending {
                PendingArg::Done(Arg {
                    kind: ArgKind::Expression(expr),
                    ..
                }) => {
                    parsed.push(expr);
                }
                _ => {
                    return Err(Self::generate_pending_err(pending));
                }
            }
        }
        Ok(parsed)
    }

    pub fn filter_parse(mut self, filter: &[&str]) -> Result<ParseFilter<'c>, ParseError<'c>> {
        self.parse_tokens()?;
        let mut unfiltered = Vec::with_capacity(self.pending.len());
        let mut filtered = Vec::with_capacity(self.pending.len());
        for pending in self.pending.drain(..) {
            match pending {
                PendingArg::Done(Arg {
                    kind: ArgKind::Expression(expr),
                    ..
                }) => {
                    if filter.contains(&expr.name.as_str()) {
                        filtered.push(expr);
                    } else {
                        unfiltered.push(expr);
                    }
                }
                _ => {
                    return Err(Self::generate_pending_err(pending));
                }
            }
        }
        // dbg!(&unfiltered);
        Ok(ParseFilter {
            filtered,
            unfiltered,
        })
    }
}

pub struct ParseFilter<'c> {
    pub filtered: Vec<Expression<'c>>,
    pub unfiltered: Vec<Expression<'c>>,
}

impl<'c> ParseFilter<'c> {
    pub fn all(
        &self,
    ) -> std::iter::Chain<std::slice::Iter<'_, Expression<'c>>, std::slice::Iter<'_, Expression<'c>>>
    {
        self.unfiltered.iter().chain(self.filtered.iter())
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
    use std::borrow::Cow;

    use crate::parser::{ArgKind, Expression, Parser};

    #[derive(Debug, PartialEq)]
    enum Tree<'c> {
        Number(&'c str),
        String(Cow<'c, str>),
        Keyword(&'c str),
        Identifier(&'c str),
        List(Vec<Tree<'c>>),
        Map(Vec<(&'c str, Tree<'c>)>),
        Expression((&'c str, Vec<Tree<'c>>)),
    }

    trait ToTree<'c> {
        fn to_tree(&self) -> Tree<'c>;
    }

    impl<'c> ToTree<'c> for ArgKind<'c> {
        fn to_tree(&self) -> Tree<'c> {
            match self {
                ArgKind::Number(number) => Tree::Number(number),
                ArgKind::String(string) => Tree::String(string.clone()),
                ArgKind::Keyword(keyword) => Tree::Keyword(keyword),
                ArgKind::Identifier(ident) => Tree::Identifier(ident),
                ArgKind::List(list) => {
                    let mut tree_list: Vec<Tree<'c>> = Vec::new();
                    for item in &list.data {
                        tree_list.push(item.kind.to_tree());
                    }
                    Tree::List(tree_list)
                }
                ArgKind::Map(map) => {
                    let mut tree_map: Vec<(&'c str, Tree<'c>)> = Vec::new();
                    for (key, value) in &map.data {
                        tree_map.push((key.as_str(), value.kind.to_tree()));
                    }
                    Tree::Map(tree_map)
                }
                ArgKind::Expression(expr) => {
                    let mut tree_args: Vec<Tree<'c>> = Vec::new();
                    for arg in &expr.args {
                        tree_args.push(arg.kind.to_tree());
                    }
                    let name = expr.name.as_str();
                    Tree::Expression((name, tree_args))
                }
            }
        }
    }

    trait ToTreeVec<'c> {
        fn to_tree_vec(&self) -> Vec<Tree<'c>>;
    }

    impl<'c> ToTreeVec<'c> for Vec<Expression<'c>> {
        fn to_tree_vec(&self) -> Vec<Tree<'c>> {
            let mut tree = Vec::new();
            for expr in self {
                let mut tree_args: Vec<Tree<'c>> = Vec::new();
                for arg in &expr.args {
                    tree_args.push(arg.kind.to_tree());
                }
                let name = expr.name.as_str();
                tree.push(Tree::Expression((name, tree_args)))
            }
            tree
        }
    }

    macro_rules! leaf {
        // Expression (recursive tree)
        (Expression ( $name:ident { $($rest:tt)* } )) => {
            tree!($name { $($rest)* })
        };
        // Lists
        ($parent:ident [ $( $type:ident $value:tt ),* $(,)? ]) => {
            Tree::$parent(vec![ $(leaf!($type $value)),* ])
        };
        // Maps
        ($parent:ident [ $( ($key:tt, $type:ident $value:tt) ),* $(,)? ]) => {
            Tree::$parent(vec![ $( (stringify!($key), leaf!($type $value)) ),* ])
        };
        // String literal
        (String ($value:literal)) => {
            Tree::String(Cow::Borrowed($value))
        };

        // Other literals (ident, number, keyword - passed as string literals in macro)
        ($type:ident ($value:literal)) => {
            Tree::$type($value)
        };
    }

    macro_rules! tree {
        ($name:ident { $( $child:tt $child_rest:tt ),* $(,)? }) => {
            Tree::Expression((
                stringify!($name),
                vec![ $( leaf!($child $child_rest) ),* ]
            ))
        };
    }
    #[test]
    fn parse_simple() {
        let parser = Parser::new("print(\"hello world\")");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            print {
                String("hello world")
            }
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
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("i"), Number("0"),
            }
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_simple_path() {
        let parser = Parser::new("player.weapon.name.damage.get()");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            get {
                Identifier("player"),
                List[
                    String("weapon"),
                    String("name"),
                    String("damage"),
                ],
            }
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }

    #[test]
    fn parse_simple_map() {
        let parser = Parser::new("map.store([value_one: 1, value_two: 2, value_three: [1, 2, 3]])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("map"),
                Map[
                    (value_one, Number("1")),
                    (value_two, Number("2")),
                    (value_three, List[
                        Number("1"),
                        Number("2"),
                        Number("3"),
                    ]),
                ],
            }
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(expected == parsed)
    }
    #[test]
    fn parse_trailing_comma() {
        let parser = Parser::new("list.store([1,2,3,])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("list"),
                List[
                    Number("1"),
                    Number("2"),
                    Number("3"),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[tokio::test]
    async fn parse_empty_list_in_command() {
        let parser = Parser::new("print([])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            print {
                List[],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_nested_list() {
        let parser = Parser::new("matrix.store([[1, [2, 3], 4], [5, 6], 7])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("matrix"),
                List[
                    List[Number("1"), List[Number("2"), Number("3")], Number("4")],
                    List[Number("5"), Number("6")],
                    Number("7"),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_nested_expression() {
        let parser = Parser::new("print(add(1, 2))");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            print {
                Expression(add {
                    Number("1"),
                    Number("2"),
                }),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_multiple_expressions() {
        let parser = Parser::new("i.store(0) i.inc()");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![
            tree! { store { Identifier("i"), Number("0"), } },
            tree! { inc { Identifier("i"), } },
        ];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_keyword_arg() {
        let parser = Parser::new("loop.store(true)");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("loop"),
                Keyword("true"),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_empty_expression() {
        let parser = Parser::new("i.inc()");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            inc {
                Identifier("i"),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_expression_in_list() {
        let parser = Parser::new("result.store([add(1, 2), add(3, 4)])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("result"),
                List[
                    Expression(add { Number("1"), Number("2"), }),
                    Expression(add { Number("3"), Number("4"), }),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_map_in_expression_arg() {
        let parser = Parser::new("create(player, [name: \"hero\", health: 100])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            create {
                Identifier("player"),
                Map[
                    (name, String("hero")),
                    (health, Number("100")),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_chained_dot_expressions() {
        let parser = Parser::new("player.health.get()");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            get {
                Identifier("player"),
                List[String("health")],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_empty_list() {
        let parser = Parser::new("items.store([])");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("items"),
                List[],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_string_arg() {
        let parser = Parser::new("say(\"hello world\")");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            say {
                String("hello world"),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_deeply_nested_expression() {
        let parser = Parser::new("print(concat(add(1, 2), add(3, 4)))");
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            print {
                Expression(concat {
                    Expression(add { Number("1"), Number("2"), }),
                    Expression(add { Number("3"), Number("4"), }),
                }),
            }
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
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![
            tree! { store { Identifier("counter"), Number("0"), } },
            tree! {
                repeat {
                    Number("0"),
                    Expression(store {
                        Identifier("counter"),
                        Expression(add {
                            Identifier("counter"),
                            Number("1"),
                        }),
                    }),
                }
            },
            tree! { print { Identifier("counter"), } },
        ];
        assert!(expected == parsed)
    }

    #[test]
    fn large_chained_command() {
        let parser = Parser::new(
            r#"text.remove_whitespace().uppercase().concat("!!!")"#
                .trim_matches(|c: char| c.is_whitespace()),
        );
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            concat {
                Expression(uppercase {
                    Expression(remove_whitespace {
                        Identifier("text"),
                    }),
                }),
                String("!!!"),
            }
        }];
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
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![
            tree! { store { Identifier("text"), String("  hello world  "), } },
            tree! {
                store {
                    Identifier("result"),
                    Expression(concat {
                        Expression(uppercase {
                            Expression(remove_whitespace {
                                Identifier("text"),
                            }),
                        }),
                        String("!!!"),
                    }),
                }
            },
            tree! { print { Identifier("result"), } },
        ];
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
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![
            tree! { store { Identifier("empty"), List[], } },
            tree! {
                print {
                    String("Length: "),
                    Expression(length { Identifier("empty"), }),
                }
            },
            tree! { insert { Identifier("empty"), Number("0"), Number("42"), } },
            tree! {
                print {
                    String(" After insert: "),
                    Expression(index { Identifier("empty"), Number("0"), }),
                }
            },
        ];
        assert!(expected == parsed)
    }

    #[test]
    fn chained_command_with_list_with_command() {
        let parser = Parser::new(r#"index([1, add(1, 1), 3], ["one"]).print()"#);
        let parsed = parser.parse().unwrap().to_tree_vec();
        let expected = vec![tree! {
            print {
                Expression(index {
                    List[
                        Number("1"),
                        Expression(add { Number("1"), Number("1"), }),
                        Number("3"),
                    ],
                    List[String("one")],
                }),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn deeply_nested_commands() {
        let parsed = Parser::new("a(b(c(d(e(f(g(h(i(j(k()))))))))))")
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            a {
                Expression(b {
                    Expression(c {
                        Expression(d {
                            Expression(e {
                                Expression(f {
                                    Expression(g {
                                        Expression(h {
                                            Expression(i {
                                                Expression(j {
                                                    Expression(k {}),
                                                }),
                                            }),
                                        }),
                                    }),
                                }),
                            }),
                        }),
                    }),
                }),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn commands_in_lists_in_commands() {
        let parsed = Parser::new("process([add(1, 2), sub(5, 3), mul(2, 4)])")
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            process {
                List[
                    Expression(add { Number("1"), Number("2"), }),
                    Expression(sub { Number("5"), Number("3"), }),
                    Expression(mul { Number("2"), Number("4"), }),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn lists_in_commands_in_lists() {
        let parsed = Parser::new("outer([inner([1, 2]), inner([3, 4])])")
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            outer {
                List[
                    Expression(inner { List[Number("1"), Number("2")], }),
                    Expression(inner { List[Number("3"), Number("4")], }),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn chain_after_expression() {
        let parsed = Parser::new("add(1, 1).add(1)")
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            add {
                Expression(add { Number("1"), Number("1"), }),
                Number("1"),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn chain_after_command_with_list() {
        let parsed = Parser::new(r#"index(["john", "joseph"], 1).print()"#)
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            print {
                Expression(index {
                    List[String("john"), String("joseph")],
                    Number("1"),
                }),
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn multiple_statements_with_method_chains() {
        let parsed = Parser::new(
            r#"names.store(["John", "James", "Joseph", "Alexander"])
        i.store(0)
        repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![
            tree! {
                store {
                    Identifier("names"),
                    List[
                        String("John"),
                        String("James"),
                        String("Joseph"),
                        String("Alexander"),
                    ],
                }
            },
            tree! { store { Identifier("i"), Number("0"), } },
            tree! {
                repeat {
                    Expression(length { Identifier("names"), }),
                    Expression(print {
                        Expression(index { Identifier("names"), Identifier("i"), }),
                    }),
                    Expression(print { String("\n"), }),
                    Expression(store {
                        Identifier("i"),
                        Expression(add { Identifier("i"), Number("1"), }),
                    }),
                }
            },
        ];
        assert!(expected == parsed)
    }

    #[test]
    fn mixed_state_transitions() {
        let parsed = Parser::new("a([b(c), d([e, f])])")
            .parse()
            .unwrap()
            .to_tree_vec();
        let expected = vec![tree! {
            a {
                List[
                    Expression(b { Identifier("c"), }),
                    Expression(d { List[Identifier("e"), Identifier("f")], }),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn map_with_mixed_content() {
        let parsed = Parser::new(
            r#"
            player.store([
            name: "blah",
            health: 100,
            dodge: 0,
            strength: 0
            ])
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (health, Number("100")),
                    (dodge, Number("0")),
                    (strength, Number("0")),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn map_with_list() {
        let parsed = Parser::new(
            r#"
            player.store([
            name: "blah",
            items: [x, y, z]
            ])
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (items, List[Identifier("x"), Identifier("y"), Identifier("z")]),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn map_inside_list_inside_map() {
        let parsed = Parser::new(
            r#"
            player.store([
            name: "blah",
            inventory: [
                [name: "sword", damage: 10],
                [name: "shield", defense: 5]
            ]
            ])
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (inventory, List[
                        Map[(name, String("sword")), (damage, Number("10"))],
                        Map[(name, String("shield")), (defense, Number("5"))],
                    ]),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn map_inside_map() {
        let parsed = Parser::new(
            r#"
            player.store([
            name: "blah",
            stats: [health: 100, strength: 10]
            ])
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (stats, Map[(health, Number("100")), (strength, Number("10"))]),
                ],
            }
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn deeply_nested_maps() {
        let parsed = Parser::new(
            r#"
            world.store([
            player: [
                name: "blah",
                location: [x: 0, y: 0]
            ]
            ])
        "#,
        )
        .parse()
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("world"),
                Map[
                    (player, Map[
                        (name, String("blah")),
                        (location, Map[(x, Number("0")), (y, Number("0"))]),
                    ]),
                ],
            }
        }];
        assert!(expected == parsed)
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
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, Expression(get_name {})),
                    (health, Expression(add { Number("50"), Number("50"), })),
                ],
            }
        }];
        assert!(expected == parsed)
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
    ).parse().unwrap().to_tree_vec();
        let expected = vec![
            tree! { store { Identifier("C_MODS"), Number("0"), } },
            tree! { store { Identifier("P_MOD"), Number("0"), } },
            tree! { store { Identifier("P_DUR"), Number("1"), } },
            tree! { store { Identifier("attacker_mods"), List[], } },
            tree! { store { Identifier("attacker"), List[Identifier("attacker_mods")], } },
            tree! { store { Identifier("potion"), List[Number("1"), Number("1")], } },
            tree! { store { Identifier("mods"), List[], } },
            tree! {
                def {
                    Identifier("add_potion_modifier"),
                    Identifier("potion"),
                    Expression(store {
                        Identifier("mods"),
                        Expression(clone {
                            Expression(index { Identifier("attacker"), Identifier("C_MODS"), }),
                        }),
                    }),
                    Expression(push {
                        Identifier("mods"),
                        List[
                            Expression(clone {
                                Expression(index { Identifier("potion"), Identifier("P_MOD"), }),
                            }),
                            Expression(clone {
                                Expression(index { Identifier("potion"), Identifier("P_DUR"), }),
                            }),
                        ],
                    }),
                    Expression(store {
                        Expression(index { Identifier("attacker"), Identifier("C_MODS"), }),
                        Expression(clone { Identifier("mods"), }),
                    }),
                }
            },
            tree! { add_potion_modifier { List[Number("50"), Number("3")], } },
            tree! {
                print {
                    String("potion: [1, 1]\n"),
                    Expression(concat {
                        String("mods: "),
                        Identifier("mods"),
                        String("\nattacker_mods: "),
                        Expression(index { Identifier("attacker"), Identifier("C_MODS"), }),
                    }),
                }
            },
        ];
        assert!(expected == parsed)
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
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (weapon, Expression(concat {
                        Expression(uppercase { String("big"), }),
                        String(" "),
                        String("sword"),
                    })),
                ],
            }
        }];
        assert!(expected == parsed)
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
        .unwrap()
        .to_tree_vec();
        let expected = vec![tree! {
            store {
                Identifier("player"),
                Map[
                    (name, String("blah")),
                    (weapon, Expression(concat {
                        Expression(uppercase { String("big"), }),
                        String(" "),
                        String("sword"),
                    })),
                ],
            }
        }];
        assert!(expected == parsed)
    }
}
