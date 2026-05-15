use std::{borrow::Cow, collections::VecDeque, fmt::Display};

use crate::lexer::{LexError, Lexer, Symbol, Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Path<'c> {
    pub start: Token<'c>,
    pub data: VecDeque<Arg<'c>>,
}

impl<'c> Path<'c> {
    pub fn new(start: Token<'c>) -> Self {
        Self {
            start,
            data: VecDeque::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct List<'c> {
    pub start: Token<'c>,
    pub data: Vec<Arg<'c>>,
    pub end: Option<Token<'c>>,
}

impl<'c> List<'c> {
    pub fn new(start: Token<'c>) -> Self {
        Self {
            start,
            data: Vec::new(),
            end: None,
        }
    }
}

impl<'c> From<Path<'c>> for List<'c> {
    fn from(value: Path<'c>) -> Self {
        Self {
            start: value.start,
            data: value.data.into(),
            end: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map<'c> {
    pub start: Token<'c>,
    pub data: Vec<(Token<'c>, Arg<'c>)>,
    pub end: Option<Token<'c>>,
}

impl<'c> Map<'c> {
    pub fn new(start: Token<'c>) -> Self {
        Self {
            start,
            data: Vec::new(),
            end: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'c> {
    pub name: Token<'c>,
    pub start: Token<'c>,
    pub args: Vec<Arg<'c>>,
    pub end: Option<Token<'c>>,
}

impl<'c> Expression<'c> {
    pub fn new(name: Token<'c>) -> Expression<'c> {
        Self {
            name,
            start: name,
            args: Vec::new(),
            end: None,
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
    UnfinishedExpression((Token<'c>, Token<'c>)),
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
            ParseError::UnfinishedExpression((start, end)) => {
                write!(
                    f,
                    "
                    Unfinished expression started with \"{}\" on line {}\n{}: {}\n
                    Ending with with \"{}\" on line {}\n{}: {}
                    ",
                    start.token_type,
                    start.line_number(),
                    start.line_number(),
                    start.line(),
                    end.token_type,
                    end.line_number(),
                    end.line_number(),
                    end.line(),
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
                    Ok(self.pend_map_with_value(Map::new(start), key, value))
                }
                _ => Err(ParseError::OutOfPlaceValue(maybe_map.start())),
            },
            None => Err(ParseError::OutOfPlaceSymbol(token)),
        }
    }

    fn pend_list_with_value(&mut self, list: List<'c>, value: Option<Arg<'c>>) {
        let mut list = list;
        if let Some(value) = value {
            list.data.push(value);
        }
        self.pending.push(PendingArg::List(list));
    }

    fn pend_uninit_collection(&mut self, token: Token<'c>) {
        self.pending.push(PendingArg::UnitializedCollection(token));
    }

    fn pend_expr_with_value(&mut self, expr: Expression<'c>, value: Arg<'c>) {
        let mut expr = expr;
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
                    list.end = Some(token);
                    Ok(self.pend_done(Arg::new(list.into(), token)))
                }
                PendingArg::Map(mut map) => {
                    map.end = Some(token);
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
                                                token,
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
                    // Likely impossible since lexer always pushes a command token even if empty before (
                    Err(ParseError::OutOfPlaceSymbol(token))
                }
            },
            None => {
                // Likely impossible since lexer always pushes a command token even if empty before (
                Err(ParseError::OutOfPlaceSymbol(token))
            }
        }
    }

    fn parse_closed_paren(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        let last_arg = self.get_remaining_arg();
        match self.pending.pop() {
            Some(maybe_expr) => match maybe_expr {
                PendingArg::Expression(mut expr) => {
                    expr.end = Some(token);
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
                                    List::new(start),
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

    fn parse_open_bracket(&mut self, token: Token<'c>) -> Result<(), ParseError<'c>> {
        match self.pending.last() {
            Some(pending) => match &pending {
                PendingArg::Key(_) => Ok(()),
                PendingArg::Expression(_) => Ok(()),
                PendingArg::List(_) => Ok(()),
                PendingArg::UnitializedCollection(_) => Ok(()),
                _ => Err(ParseError::OutOfPlaceSymbol(token)),
            },
            None => Ok(()),
        }?;

        Ok(self.pend_uninit_collection(token))
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
                    self.pend_list_with_value(List::new(start), last_arg)
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
                _ => Err(ParseError::OutOfPlaceValue(pending.start())),
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
                            let mut path = Path::new(token);
                            path.data.push_back(parent_dot_arg);
                            path.data.push_back(arg);
                            self.pend_path(path);
                            Ok(())
                        }
                        PendingArg::PathArg(mut path) => {
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
                            Ok(self.pend_list_with_value(List::new(start), value.into()))
                        }
                        PendingArg::List(list) => Ok(self.pend_list_with_value(list, value.into())),
                        PendingArg::Key(key) => {
                            Ok(self.try_pend_map_with_value(token, key, value.into())?)
                        }
                        PendingArg::Done(done) => {
                            self.pend_done(done);
                            self.pend_done(value);
                            Ok(())
                        }
                        _ => Err(ParseError::OutOfPlaceValue(parent.start())),
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
                    let pending = PendingArg::Expression(Expression::new(token));
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
                    return Err(ParseError::OutOfPlaceValue(pending.start()));
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
            dbg!(&pending);
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
                    dbg!("found this error at the end");
                    return Err(ParseError::OutOfPlaceValue(pending.start()));
                }
            }
        }
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

    use crate::parser::{ArgKind, Expression, Map, Parser};

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
             store{
            Identifier("i"), Number("0"),}
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

    /*
    #[test]
    fn parse_trailing_comma() {
        let parser = Parser::new("list.store([1,2,3,])");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgKind::Identifier("list"),
                ArgKind::List(vec![
                    ArgKind::Number("1"),
                    ArgKind::Number("2"),
                    ArgKind::Number("3"),
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
            args: vec![ArgKind::List(vec![])],
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
                ArgKind::Identifier("matrix"),
                ArgKind::List(vec![
                    ArgKind::List(vec![
                        ArgKind::Number("1"),
                        ArgKind::List(vec![ArgKind::Number("2"), ArgKind::Number("3")]),
                        ArgKind::Number("4"),
                    ]),
                    ArgKind::List(vec![ArgKind::Number("5"), ArgKind::Number("6")]),
                    ArgKind::Number("7"),
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
            args: vec![ArgKind::Expression(Expression {
                name: "add",
                args: vec![ArgKind::Number("1"), ArgKind::Number("2")],
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
                args: vec![ArgKind::Identifier("i"), ArgKind::Number("0")],
            },
            Expression {
                name: "inc",
                args: vec![ArgKind::Identifier("i")],
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
            args: vec![ArgKind::Identifier("loop"), ArgKind::Keyword("true")],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_empty_expression() {
        let parser = Parser::new("i.inc()");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "inc",
            args: vec![ArgKind::Identifier("i")],
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
                ArgKind::Identifier("result"),
                ArgKind::List(vec![
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("1"), ArgKind::Number("2")],
                    }),
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("3"), ArgKind::Number("4")],
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
                ArgKind::Identifier("player"),
                ArgKind::Map(Map::from([
                    ("name", ArgKind::String(Cow::Borrowed("hero"))),
                    ("health", ArgKind::Number("100")),
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
                ArgKind::Identifier("player"),
                ArgKind::List(vec![ArgKind::String(Cow::Borrowed("health"))]),
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
            args: vec![ArgKind::Identifier("items"), ArgKind::List(vec![])],
        }];
        assert!(expected == parsed)
    }

    #[test]
    fn parse_string_arg() {
        let parser = Parser::new("say(\"hello world\")");
        let parsed = parser.parse().unwrap();
        let expected = vec![Expression {
            name: "say",
            args: vec![ArgKind::String(Cow::Borrowed("hello world"))],
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
            args: vec![ArgKind::Expression(Expression {
                name: "concat",
                args: vec![
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("1"), ArgKind::Number("2")],
                    }),
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("3"), ArgKind::Number("4")],
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
                args: vec![ArgKind::Identifier("counter"), ArgKind::Number("0")],
            },
            Expression {
                name: "repeat",
                args: vec![
                    ArgKind::Number("0"),
                    ArgKind::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgKind::Identifier("counter"),
                            ArgKind::Expression(Expression {
                                name: "add",
                                args: vec![ArgKind::Identifier("counter"), ArgKind::Number("1")],
                            }),
                        ],
                    }),
                ],
            },
            Expression {
                name: "print",
                args: vec![ArgKind::Identifier("counter")],
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
                ArgKind::Expression(Expression {
                    name: "uppercase",
                    args: vec![ArgKind::Expression(Expression {
                        name: "remove_whitespace",
                        args: vec![ArgKind::Identifier("text")],
                    })],
                }),
                ArgKind::String(Cow::Borrowed("!!!")),
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
                    ArgKind::Identifier("text"),
                    ArgKind::String(Cow::Borrowed("  hello world  ")),
                ],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgKind::Identifier("result"),
                    ArgKind::Expression(Expression {
                        name: "concat",
                        args: vec![
                            ArgKind::Expression(Expression {
                                name: "uppercase",
                                args: vec![ArgKind::Expression(Expression {
                                    name: "remove_whitespace",
                                    args: vec![ArgKind::Identifier("text")],
                                })],
                            }),
                            ArgKind::String(Cow::Borrowed("!!!")),
                        ],
                    }),
                ],
            },
            Expression {
                name: "print",
                args: vec![ArgKind::Identifier("result")],
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
                args: vec![ArgKind::Identifier("empty"), ArgKind::List(vec![])],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgKind::String(Cow::Borrowed("Length: ")),
                    ArgKind::Expression(Expression {
                        name: "length",
                        args: vec![ArgKind::Identifier("empty")],
                    }),
                ],
            },
            Expression {
                name: "insert",
                args: vec![
                    ArgKind::Identifier("empty"),
                    ArgKind::Number("0"),
                    ArgKind::Number("42"),
                ],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgKind::String(Cow::Borrowed(" After insert: ")),
                    ArgKind::Expression(Expression {
                        name: "index",
                        args: vec![ArgKind::Identifier("empty"), ArgKind::Number("0")],
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
            args: vec![ArgKind::Expression(Expression {
                name: "index",
                args: vec![
                    ArgKind::List(vec![
                        ArgKind::Number("1"),
                        ArgKind::Expression(Expression {
                            name: "add",
                            args: vec![ArgKind::Number("1"), ArgKind::Number("1")],
                        }),
                        ArgKind::Number("3"),
                    ]),
                    ArgKind::List(vec![ArgKind::String(Cow::Borrowed("one"))]),
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
                args: vec![ArgKind::Expression(Expression {
                    name: "b",
                    args: vec![ArgKind::Expression(Expression {
                        name: "c",
                        args: vec![ArgKind::Expression(Expression {
                            name: "d",
                            args: vec![ArgKind::Expression(Expression {
                                name: "e",
                                args: vec![ArgKind::Expression(Expression {
                                    name: "f",
                                    args: vec![ArgKind::Expression(Expression {
                                        name: "g",
                                        args: vec![ArgKind::Expression(Expression {
                                            name: "h",
                                            args: vec![ArgKind::Expression(Expression {
                                                name: "i",
                                                args: vec![ArgKind::Expression(Expression {
                                                    name: "j",
                                                    args: vec![ArgKind::Expression(Expression {
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
                args: vec![ArgKind::List(vec![
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("1"), ArgKind::Number("2"),]
                    }),
                    ArgKind::Expression(Expression {
                        name: "sub",
                        args: vec![ArgKind::Number("5"), ArgKind::Number("3"),]
                    }),
                    ArgKind::Expression(Expression {
                        name: "mul",
                        args: vec![ArgKind::Number("2"), ArgKind::Number("4"),]
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
                args: vec![ArgKind::List(vec![
                    ArgKind::Expression(Expression {
                        name: "inner",
                        args: vec![ArgKind::List(vec![
                            ArgKind::Number("1"),
                            ArgKind::Number("2"),
                        ])]
                    }),
                    ArgKind::Expression(Expression {
                        name: "inner",
                        args: vec![ArgKind::List(vec![
                            ArgKind::Number("3"),
                            ArgKind::Number("4"),
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
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![ArgKind::Number("1"), ArgKind::Number("1"),]
                    }),
                    ArgKind::Number("1")
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
                args: vec![ArgKind::Expression(Expression {
                    name: "index",
                    args: vec![
                        ArgKind::List(vec![
                            ArgKind::String(Cow::Borrowed("john")),
                            ArgKind::String(Cow::Borrowed("joseph")),
                        ]),
                        ArgKind::Number("1")
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
                        ArgKind::Identifier("names"),
                        ArgKind::List(vec![
                            ArgKind::String(Cow::Borrowed("John")),
                            ArgKind::String(Cow::Borrowed("James")),
                            ArgKind::String(Cow::Borrowed("Joseph")),
                            ArgKind::String(Cow::Borrowed("Alexander")),
                        ])
                    ]
                },
                Expression {
                    name: "store",
                    args: vec![ArgKind::Identifier("i"), ArgKind::Number("0")]
                },
                Expression {
                    name: "repeat",
                    args: vec![
                        ArgKind::Expression(Expression {
                            name: "length",
                            args: vec![ArgKind::Identifier("names")]
                        }),
                        ArgKind::Expression(Expression {
                            name: "print",
                            args: vec![ArgKind::Expression(Expression {
                                name: "index",
                                args: vec![ArgKind::Identifier("names"), ArgKind::Identifier("i")]
                            })]
                        }),
                        ArgKind::Expression(Expression {
                            name: "print",
                            args: vec![ArgKind::String(Cow::Borrowed("\n"))]
                        }),
                        ArgKind::Expression(Expression {
                            name: "store",
                            args: vec![
                                ArgKind::Identifier("i"),
                                ArgKind::Expression(Expression {
                                    name: "add",
                                    args: vec![ArgKind::Identifier("i"), ArgKind::Number("1")]
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
                    args: vec![ArgKind::Identifier("i"), ArgKind::Number("0")]
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
                ArgKind::Identifier("character"),
                ArgKind::List(vec![
                    ArgKind::String(Cow::Borrowed("weapon")),
                    ArgKind::String(Cow::Borrowed("name")),
                ]),
                ArgKind::String(Cow::Borrowed("sword")),
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
                    ArgKind::Expression(Expression {
                        name: "ends_with",
                        args: vec![
                            ArgKind::Expression(Expression {
                                name: "index",
                                args: vec![ArgKind::Identifier("names"), ArgKind::Number("0")]
                            }),
                            ArgKind::String(Cow::Borrowed("2"))
                        ]
                    }),
                    ArgKind::Expression(Expression {
                        name: "print",
                        args: vec![ArgKind::String(Cow::Borrowed("it's name 2!!"))]
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
                    ArgKind::Identifier("quadruple"),
                    ArgKind::Expression(Expression {
                        name: "add",
                        args: vec![
                            ArgKind::Expression(Expression {
                                name: "double",
                                args: vec![ArgKind::Identifier("x")]
                            }),
                            ArgKind::Expression(Expression {
                                name: "double",
                                args: vec![ArgKind::Identifier("x")]
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
                    ArgKind::Number("1"),
                    ArgKind::Number("2"),
                    ArgKind::Number("3"),
                    ArgKind::Number("4"),
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
                    ArgKind::Identifier("list"),
                    ArgKind::List(vec![
                        ArgKind::Number("1"),
                        ArgKind::Expression(Expression {
                            name: "add",
                            args: vec![ArgKind::Number("2"), ArgKind::Number("3"),]
                        }),
                        ArgKind::Number("4"),
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
                    ArgKind::Identifier("matrix"),
                    ArgKind::List(vec![
                        ArgKind::List(vec![
                            ArgKind::Number("1"),
                            ArgKind::Number("2"),
                            ArgKind::Number("3"),
                        ]),
                        ArgKind::List(vec![
                            ArgKind::Number("4"),
                            ArgKind::Number("5"),
                            ArgKind::Number("6"),
                        ]),
                        ArgKind::List(vec![
                            ArgKind::Number("7"),
                            ArgKind::Number("8"),
                            ArgKind::Number("9"),
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
                    ArgKind::Identifier("data"),
                    ArgKind::List(vec![
                        ArgKind::Number("1"),
                        ArgKind::List(vec![
                            ArgKind::Number("2"),
                            ArgKind::List(vec![
                                ArgKind::Number("3"),
                                ArgKind::List(vec![ArgKind::Number("4"), ArgKind::Number("5"),])
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
                    ArgKind::Identifier("data"),
                    ArgKind::List(vec![
                        ArgKind::Identifier("x"),
                        ArgKind::String(Cow::Borrowed("hello")),
                        ArgKind::Number("42"),
                        ArgKind::Expression(Expression {
                            name: "add",
                            args: vec![ArgKind::Number("1"), ArgKind::Number("2"),]
                        }),
                        ArgKind::List(vec![
                            ArgKind::Identifier("nested"),
                            ArgKind::Identifier("list"),
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
                    ArgKind::Expression(Expression {
                        name: "map",
                        args: vec![
                            ArgKind::Identifier("data"),
                            ArgKind::List(vec![
                                ArgKind::Expression(Expression {
                                    name: "filter",
                                    args: vec![
                                        ArgKind::List(vec![
                                            ArgKind::Number("1"),
                                            ArgKind::Number("2"),
                                            ArgKind::Number("3"),
                                        ]),
                                        ArgKind::Expression(Expression {
                                            name: "is_even",
                                            args: vec![]
                                        })
                                    ]
                                }),
                                ArgKind::Expression(Expression {
                                    name: "sort",
                                    args: vec![ArgKind::List(vec![
                                        ArgKind::Identifier("b"),
                                        ArgKind::Identifier("a"),
                                        ArgKind::Identifier("c"),
                                    ])]
                                })
                            ])
                        ]
                    }),
                    ArgKind::List(vec![
                        ArgKind::Expression(Expression {
                            name: "process",
                            args: vec![ArgKind::Identifier("x")]
                        }),
                        ArgKind::Expression(Expression {
                            name: "process",
                            args: vec![ArgKind::Identifier("y")]
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
                    ArgKind::Identifier("grid"),
                    ArgKind::List(vec![
                        ArgKind::List(vec![
                            ArgKind::Expression(Expression {
                                name: "add",
                                args: vec![ArgKind::Number("1"), ArgKind::Number("2"),]
                            }),
                            ArgKind::Expression(Expression {
                                name: "sub",
                                args: vec![ArgKind::Number("3"), ArgKind::Number("4"),]
                            })
                        ]),
                        ArgKind::List(vec![
                            ArgKind::Expression(Expression {
                                name: "mul",
                                args: vec![ArgKind::Number("5"), ArgKind::Number("6"),]
                            }),
                            ArgKind::Expression(Expression {
                                name: "div",
                                args: vec![ArgKind::Number("7"), ArgKind::Number("8"),]
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
                args: vec![ArgKind::List(vec![
                    ArgKind::Identifier("a"),
                    ArgKind::Expression(Expression {
                        name: "inner",
                        args: vec![ArgKind::List(vec![
                            ArgKind::Identifier("b"),
                            ArgKind::Expression(Expression {
                                name: "deeper",
                                args: vec![ArgKind::List(vec![
                                    ArgKind::Identifier("c"),
                                    ArgKind::Identifier("d"),
                                ])]
                            })
                        ])]
                    }),
                    ArgKind::Identifier("e"),
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
                args: vec![ArgKind::List(vec![ArgKind::Expression(Expression {
                    name: "inner",
                    args: vec![ArgKind::List(vec![ArgKind::List(vec![
                        ArgKind::Identifier("a"),
                        ArgKind::Identifier("b"),
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
                args: vec![ArgKind::Expression(Expression {
                    name: "inner2",
                    args: vec![ArgKind::List(vec![
                        ArgKind::Identifier("c"),
                        ArgKind::List(vec![ArgKind::Identifier("d"),])
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
                args: vec![ArgKind::List(vec![ArgKind::Expression(Expression {
                    name: "inner",
                    args: vec![ArgKind::Expression(Expression {
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
                    ArgKind::List(vec![ArgKind::Identifier("a")]),
                    ArgKind::List(vec![ArgKind::Identifier("b")]),
                    ArgKind::Expression(Expression {
                        name: "inner",
                        args: vec![ArgKind::List(vec![ArgKind::Identifier("c")])]
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
                    ArgKind::List(vec![
                        ArgKind::Identifier("a"),
                        ArgKind::Expression(Expression {
                            name: "inner1",
                            args: vec![ArgKind::List(vec![ArgKind::Identifier("b")])]
                        })
                    ]),
                    ArgKind::Expression(Expression {
                        name: "inner2",
                        args: vec![ArgKind::List(vec![
                            ArgKind::Identifier("c"),
                            ArgKind::List(vec![ArgKind::Identifier("d")])
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
                    ArgKind::List(vec![ArgKind::Identifier("a"), ArgKind::Identifier("b"),]),
                    ArgKind::Expression(Expression {
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
                args: vec![ArgKind::Identifier("data"), ArgKind::List(vec![])]
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
                    ArgKind::Identifier("data"),
                    ArgKind::List(vec![ArgKind::Number("1"), ArgKind::Number("2"),])
                ]
            }]
        );
    }

    #[test]
    fn multiple_trailing_commas() {
        // For now allow multiple trailing commas since it's a non issue to the parser and would add complexity for little gain to handle
        let parsed = Parser::new("data.store([1, 2,,,,,])").parse().unwrap();
        let expected = vec![Expression {
            name: "store",
            args: vec![
                ArgKind::Identifier("data"),
                ArgKind::List(vec![ArgKind::Number("1"), ArgKind::Number("2")]),
            ],
        }];
        println!("==GOT==\n");
        dbg!(&parsed);
        println!("==EXPECTED==\n");
        dbg!(&expected);
        assert!(parsed == expected);
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
                args: vec![ArgKind::Number("1"), ArgKind::Number("2"),]
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
                    ArgKind::Expression(Expression {
                        name: "outer",
                        args: vec![ArgKind::Expression(Expression {
                            name: "inner",
                            args: vec![ArgKind::Identifier("x")]
                        })]
                    }),
                    ArgKind::Identifier("y")
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
                args: vec![ArgKind::List(vec![
                    ArgKind::Expression(Expression {
                        name: "b",
                        args: vec![ArgKind::Identifier("c")]
                    }),
                    ArgKind::Expression(Expression {
                        name: "d",
                        args: vec![ArgKind::List(vec![
                            ArgKind::Identifier("e"),
                            ArgKind::Identifier("f"),
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
                    ArgKind::Identifier("player"),
                    ArgKind::Map(vec![
                        ("name", ArgKind::String(Cow::Borrowed("blah"))),
                        ("health", ArgKind::Number("100")),
                        ("dodge", ArgKind::Number("0")),
                        ("strength", ArgKind::Number("0")),
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
                    ArgKind::Identifier("player"),
                    ArgKind::Map(vec![
                        ("name", ArgKind::String(Cow::Borrowed("blah"))),
                        (
                            "items",
                            ArgKind::List(vec![
                                ArgKind::Identifier("x"),
                                ArgKind::Identifier("y"),
                                ArgKind::Identifier("z"),
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
                    ArgKind::Identifier("player"),
                    ArgKind::Map(vec![
                        ("name", ArgKind::String(Cow::Borrowed("blah"))),
                        (
                            "inventory",
                            ArgKind::List(vec![
                                ArgKind::Map(vec![
                                    ("name", ArgKind::String(Cow::Borrowed("sword"))),
                                    ("damage", ArgKind::Number("10")),
                                ]),
                                ArgKind::Map(vec![
                                    ("name", ArgKind::String(Cow::Borrowed("shield"))),
                                    ("defense", ArgKind::Number("5")),
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
                    ArgKind::Identifier("player"),
                    ArgKind::Map(vec![
                        ("name", ArgKind::String(Cow::Borrowed("blah"))),
                        (
                            "stats",
                            ArgKind::Map(vec![
                                ("health", ArgKind::Number("100")),
                                ("strength", ArgKind::Number("10")),
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
                    ArgKind::Identifier("world"),
                    ArgKind::Map(vec![(
                        "player",
                        ArgKind::Map(vec![
                            ("name", ArgKind::String(Cow::Borrowed("blah"))),
                            (
                                "location",
                                ArgKind::Map(vec![
                                    ("x", ArgKind::Number("0")),
                                    ("y", ArgKind::Number("0")),
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
                ArgKind::Identifier("player"),
                ArgKind::Map(vec![
                    (
                        "name",
                        ArgKind::Expression(Expression {
                            name: "get_name",
                            args: vec![],
                        }),
                    ),
                    (
                        "health",
                        ArgKind::Expression(Expression {
                            name: "add",
                            args: vec![ArgKind::Number("50"), ArgKind::Number("50")],
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
                args: vec![ArgKind::Identifier("C_MODS"), ArgKind::Number("0")],
            },
            Expression {
                name: "store",
                args: vec![ArgKind::Identifier("P_MOD"), ArgKind::Number("0")],
            },
            Expression {
                name: "store",
                args: vec![ArgKind::Identifier("P_DUR"), ArgKind::Number("1")],
            },
            Expression {
                name: "store",
                args: vec![ArgKind::Identifier("attacker_mods"), ArgKind::List(vec![])],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgKind::Identifier("attacker"),
                    ArgKind::List(vec![ArgKind::Identifier("attacker_mods")]),
                ],
            },
            Expression {
                name: "store",
                args: vec![
                    ArgKind::Identifier("potion"),
                    ArgKind::List(vec![ArgKind::Number("1"), ArgKind::Number("1")]),
                ],
            },
            Expression {
                name: "store",
                args: vec![ArgKind::Identifier("mods"), ArgKind::List(vec![])],
            },
            Expression {
                name: "def",
                args: vec![
                    ArgKind::Identifier("add_potion_modifier"),
                    ArgKind::Identifier("potion"),
                    ArgKind::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgKind::Identifier("mods"),
                            ArgKind::Expression(Expression {
                                name: "clone",
                                args: vec![ArgKind::Expression(Expression {
                                    name: "index",
                                    args: vec![
                                        ArgKind::Identifier("attacker"),
                                        ArgKind::Identifier("C_MODS"),
                                    ],
                                })],
                            }),
                        ],
                    }),
                    ArgKind::Expression(Expression {
                        name: "push",
                        args: vec![
                            ArgKind::Identifier("mods"),
                            ArgKind::List(vec![
                                ArgKind::Expression(Expression {
                                    name: "clone",
                                    args: vec![ArgKind::Expression(Expression {
                                        name: "index",
                                        args: vec![
                                            ArgKind::Identifier("potion"),
                                            ArgKind::Identifier("P_MOD"),
                                        ],
                                    })],
                                }),
                                ArgKind::Expression(Expression {
                                    name: "clone",
                                    args: vec![ArgKind::Expression(Expression {
                                        name: "index",
                                        args: vec![
                                            ArgKind::Identifier("potion"),
                                            ArgKind::Identifier("P_DUR"),
                                        ],
                                    })],
                                }),
                            ]),
                        ],
                    }),
                    ArgKind::Expression(Expression {
                        name: "store",
                        args: vec![
                            ArgKind::Expression(Expression {
                                name: "index",
                                args: vec![
                                    ArgKind::Identifier("attacker"),
                                    ArgKind::Identifier("C_MODS"),
                                ],
                            }),
                            ArgKind::Expression(Expression {
                                name: "clone",
                                args: vec![ArgKind::Identifier("mods")],
                            }),
                        ],
                    }),
                ],
            },
            Expression {
                name: "add_potion_modifier",
                args: vec![ArgKind::List(vec![
                    ArgKind::Number("50"),
                    ArgKind::Number("3"),
                ])],
            },
            Expression {
                name: "print",
                args: vec![
                    ArgKind::String(Cow::Borrowed("potion: [1, 1]\n")),
                    ArgKind::Expression(Expression {
                        name: "concat",
                        args: vec![
                            ArgKind::String(Cow::Borrowed("mods: ")),
                            ArgKind::Identifier("mods"),
                            ArgKind::String(Cow::Borrowed("\nattacker_mods: ")),
                            ArgKind::Expression(Expression {
                                name: "index",
                                args: vec![
                                    ArgKind::Identifier("attacker"),
                                    ArgKind::Identifier("C_MODS"),
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
                ArgKind::Identifier("player"),
                ArgKind::Map(vec![
                    ("name", ArgKind::String("blah".into())),
                    (
                        "weapon",
                        ArgKind::Expression(Expression {
                            name: "concat",
                            args: vec![
                                ArgKind::Expression(Expression {
                                    name: "uppercase",
                                    args: vec![ArgKind::String("big".into())],
                                }),
                                ArgKind::String(" ".into()),
                                ArgKind::String("sword".into()),
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
                ArgKind::Identifier("player"),
                ArgKind::Map(vec![
                    ("name", ArgKind::String("blah".into())),
                    (
                        "weapon",
                        ArgKind::Expression(Expression {
                            name: "concat",
                            args: vec![
                                ArgKind::Expression(Expression {
                                    name: "uppercase",
                                    args: vec![ArgKind::String("big".into())],
                                }),
                                ArgKind::String(" ".into()),
                                ArgKind::String("sword".into()),
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
    */
}
