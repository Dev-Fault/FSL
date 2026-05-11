use std::{fmt::Display, iter::Peekable, str::CharIndices};

const QUOTE: char = '\"';
const OPEN_PAREN: char = '(';
const CLOSED_PAREN: char = ')';
const OPEN_BRACKET: char = '[';
const CLOSED_BRACKET: char = ']';
const COLON: char = ':';
const DOT: char = '.';
const COMMA: char = ',';
const HASHTAG: char = '#';
const STAR: char = '*';
const ESCAPE: char = '\\';

pub const TRUE: &str = "true";
pub const FALSE: &str = "false";

const KEYWORDS: &[&str] = &[TRUE, FALSE];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Quote,
    OpenParen,
    ClosedParen,
    OpenBracket,
    ClosedBracket,
    Colon,
    Dot,
    Comma,
    Hashtag,
    Star,
}

impl From<&str> for Symbol {
    fn from(value: &str) -> Self {
        match value {
            "\"" => Symbol::Quote,
            "(" => Symbol::OpenParen,
            ")" => Symbol::ClosedParen,
            "[" => Symbol::OpenBracket,
            "]" => Symbol::ClosedBracket,
            ":" => Symbol::Colon,
            "." => Symbol::Dot,
            "," => Symbol::Comma,
            "#" => Symbol::Hashtag,
            "*" => Symbol::Star,
            _ => unreachable!("match should handle all symbols"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType<'c> {
    Symbol(&'c str),
    Command(&'c str),
    Number(&'c str),
    String(&'c str),
    Keyword(&'c str),
    Identifier(&'c str),
    Comment(&'c str),
    None(&'c str),
}

impl<'c> Display for TokenType<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Symbol(out) => write!(f, "{}", out),
            TokenType::Command(out) => write!(f, "{}", out),
            TokenType::Number(out) => write!(f, "{}", out),
            TokenType::String(out) => write!(f, "{}", out),
            TokenType::Keyword(out) => write!(f, "{}", out),
            TokenType::Identifier(out) => write!(f, "{}", out),
            TokenType::Comment(out) => write!(f, "{}", out),
            TokenType::None(out) => write!(f, "{}", out),
        }
    }
}

impl<'c> Default for TokenType<'c> {
    fn default() -> Self {
        Self::None("")
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Token<'c> {
    pub token_type: TokenType<'c>,
    pub source: &'c str,
    pub location: usize,
}

impl<'c> Token<'c> {
    pub fn symbol(source: &'c str, symbol: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::Symbol(symbol),
            source,
            location,
        }
    }

    pub fn string(source: &'c str, string: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::String(string),
            source,
            location,
        }
    }

    pub fn command(source: &'c str, command: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::Command(command),
            source,
            location,
        }
    }

    pub fn identifier(source: &'c str, identifier: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::Identifier(identifier),
            source,
            location,
        }
    }

    pub fn comment(source: &'c str, comment: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::Comment(comment),
            source,
            location,
        }
    }

    pub fn number(
        source: &'c str,
        number: &'c str,
        location: usize,
    ) -> Result<Token<'c>, LexError<'c>> {
        let token = Token {
            token_type: TokenType::Number(number),
            source,
            location,
        };

        if number.parse::<f64>().is_err() {
            Err(LexError::InvalidNumber(token))
        } else {
            Ok(token)
        }
    }

    pub fn parse(source: &'c str, value: &'c str, location: usize) -> Token<'c> {
        let token_type = if value.parse::<f64>().is_ok() {
            TokenType::Number(value)
        } else if KEYWORDS.contains(&value) {
            TokenType::Keyword(value)
        } else {
            TokenType::Identifier(value)
        };

        Token {
            token_type,
            source,
            location,
        }
    }

    pub fn invalid(source: &'c str, value: &'c str, location: usize) -> Token<'c> {
        Token {
            token_type: TokenType::None(value),
            source,
            location,
        }
    }

    pub fn line(&self) -> &str {
        let slice = if let Some(start_of_line) = &self.source[..self.location].rfind('\n') {
            if let Some(end_of_line) = &self.source[self.location..].find('\n') {
                &self.source
                    [self.location - start_of_line - self.len()..self.location + end_of_line]
            } else {
                &self.source[self.location - start_of_line - self.len()..self.location + self.len()]
            }
        } else {
            if let Some(end_of_line) = &self.source[self.location..].find('\n') {
                &self.source[..self.location + end_of_line]
            } else {
                &self.source[..self.location + self.len()]
            }
        };
        let line = slice.lines().last();
        line.unwrap_or(self.source)
    }

    pub fn line_number(&self) -> usize {
        let slice = &self.source[..self.location];
        let line = slice.lines().count();
        line
    }

    pub fn len(&self) -> usize {
        match self.token_type {
            TokenType::Symbol(s) => s,
            TokenType::Command(s) => s,
            TokenType::Number(s) => s,
            TokenType::String(s) => s,
            TokenType::Keyword(s) => s,
            TokenType::Identifier(s) => s,
            TokenType::Comment(s) => s,
            TokenType::None(s) => s,
        }
        .len()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Context<'c> {
    String(Token<'c>),
    Float(Token<'c>),
    SingleLineComment,
    MultiLineComment(Token<'c>),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LexError<'c> {
    UnexpectedToken(Token<'c>),
    InvalidNumber(Token<'c>),
    UnclosedString(Token<'c>),
    UnclosedComment(Token<'c>),
}

impl<'c> Display for LexError<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedToken(token) => {
                write!(
                    f,
                    "Unexpected token \"{}\" on line {}\n{}: {}",
                    token.token_type,
                    token.line_number(),
                    token.line_number(),
                    token.line()
                )
            }
            LexError::InvalidNumber(token) => write!(f, "Invalid Number: {}", token.token_type),
            LexError::UnclosedString(token) => {
                write!(
                    f,
                    "Unclosed string detected on line {}\n{}: {}",
                    token.line_number(),
                    token.line_number(),
                    token.line()
                )
            }
            LexError::UnclosedComment(token) => {
                write!(
                    f,
                    "Unclosed comment detected on line {}\n{}: {}",
                    token.line_number(),
                    token.line_number(),
                    token.line()
                )
            }
        }
    }
}

impl<'c> std::error::Error for LexError<'c> {}

#[derive(Debug)]
pub struct Lexer<'c> {
    input: &'c str,
    rest: Peekable<CharIndices<'c>>,
    location: usize,
    context: Context<'c>,
    pending: Option<Token<'c>>,
    partial: &'c str,
}

impl<'c> Lexer<'c> {
    pub fn new(input: &'c str) -> Self {
        Self {
            input,
            rest: input.char_indices().peekable(),
            location: 0,
            context: Context::None,
            pending: None,
            partial: "",
        }
    }

    pub fn ignore_quote(&self) -> bool {
        match self.context {
            Context::String(_) => false,
            Context::Float(_) => false,
            Context::SingleLineComment => true,
            Context::MultiLineComment(_) => true,
            Context::None => false,
        }
    }

    pub fn ignore_hashtag(&self) -> bool {
        match self.context {
            Context::String(_) => true,
            Context::Float(_) => false,
            Context::SingleLineComment => true,
            Context::MultiLineComment(_) => true,
            Context::None => false,
        }
    }

    pub fn ignore_new_line(&self) -> bool {
        match self.context {
            Context::String(_) => true,
            Context::Float(_) => true,
            Context::SingleLineComment => false,
            Context::MultiLineComment(_) => true,
            Context::None => true,
        }
    }

    pub fn ignore_star(&self) -> bool {
        match self.context {
            Context::String(_) => true,
            Context::Float(_) => false,
            Context::SingleLineComment => true,
            Context::MultiLineComment(_) => false,
            Context::None => false,
        }
    }

    pub fn no_context(&self) -> bool {
        matches!(self.context, Context::None)
    }

    pub fn reading_float(&self) -> bool {
        matches!(self.context, Context::Float(_))
    }
}

impl<'c> Iterator for Lexer<'c> {
    type Item = Result<Token<'c>, LexError<'c>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(pending) = self.pending.take() {
            return Some(Ok(pending));
        }

        while let Some((i, ch)) = self.rest.next() {
            let token = match ch {
                ch if ch.is_whitespace() && self.no_context() => {
                    self.location += 1;
                    continue;
                }
                QUOTE if !self.ignore_quote() => {
                    let mut token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    if let Context::String(_) = self.context {
                        self.pending = Some(token);
                        token = Token::string(self.input, self.partial, self.location);
                        self.location += self.partial.len();
                        self.partial = "";
                        self.context = Context::None;
                    } else {
                        self.context = Context::String(token);
                    }
                    token
                }
                HASHTAG if !self.ignore_hashtag() => {
                    self.context = Context::SingleLineComment;
                    Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i)
                }
                STAR if !self.ignore_star() => {
                    let mut token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    if let Context::MultiLineComment(_) = self.context {
                        self.pending = Some(token);
                        token = Token::comment(self.input, self.partial, self.location);
                        self.location += self.partial.len();
                        self.partial = "";
                        self.context = Context::None;
                    } else {
                        self.context = Context::MultiLineComment(token);
                    }
                    token
                }
                OPEN_PAREN if self.no_context() => {
                    let token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    self.pending = Some(token);
                    let token = Token::command(self.input, self.partial, self.location);
                    self.location += self.partial.len();
                    self.partial = "";
                    token
                }
                OPEN_BRACKET if self.no_context() => {
                    Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i)
                }
                COLON if self.no_context() => {
                    let token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    self.pending = Some(token);
                    let token = Token::identifier(self.input, self.partial, self.location);
                    self.location += self.partial.len();
                    self.partial = "";
                    token
                }
                DOT if self.no_context() => {
                    let token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    self.pending = Some(token);
                    if self.partial.len() > 0 {
                        let token = Token::parse(self.input, self.partial, self.location);
                        if matches!(token.token_type, TokenType::Number(_))
                            && self.rest.peek().is_some_and(|(_, c)| c.is_ascii_digit())
                        {
                            // Don't update self.location so that _ => branch includes dot in partial skipping need to recombine float later
                            self.context = Context::Float(token);
                            self.pending.take();
                            continue;
                        }
                        self.location += self.partial.len();
                        self.partial = "";
                        token
                    } else {
                        self.pending.take().unwrap()
                    }
                }
                CLOSED_PAREN | CLOSED_BRACKET | COMMA if self.no_context() => {
                    if self.partial.len() > 0 {
                        let token = Token::parse(self.input, self.partial, self.location);
                        self.location += self.partial.len();
                        self.partial = "";
                        let symbol =
                            Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                        self.pending = Some(symbol);
                        token
                    } else {
                        if let Some(token) = self.pending {
                            return Some(Err(LexError::UnexpectedToken(token)));
                        }
                        Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i)
                    }
                }
                '\n' if !self.ignore_new_line() => {
                    self.context = Context::None;
                    let token = Token::comment(self.input, self.partial, self.location);
                    self.location += self.partial.len();
                    self.partial = "";
                    token
                }
                ch if self.reading_float() && !ch.is_ascii_digit() => {
                    let token = Token::symbol(self.input, &self.input[i..i + ch.len_utf8()], i);
                    self.pending = Some(token);
                    self.context = Context::None;
                    let token = match Token::number(self.input, self.partial, self.location) {
                        Ok(token) => token,
                        Err(e) => {
                            return Some(Err(e));
                        }
                    };
                    self.location += self.partial.len();
                    self.partial = "";
                    token
                }
                _ => {
                    if matches!(self.context, Context::String(_)) {
                        if ch == ESCAPE && self.rest.peek().is_some_and(|(_, ch)| *ch == QUOTE) {
                            self.rest.next();
                            // Include escape and quote in partial string
                            self.partial =
                                &self.input[self.location..(i + ch.len_utf8()) + QUOTE.len_utf8()];
                            continue;
                        }
                    }
                    self.partial = &self.input[self.location..i + ch.len_utf8()];
                    continue;
                }
            };
            self.location += 1;
            return Some(Ok(token));
        }

        match self.context {
            Context::String(string) => {
                return Some(Err(LexError::UnclosedString(string)));
            }
            Context::Float(number) => {
                return Some(Err(LexError::InvalidNumber(number)));
            }
            Context::MultiLineComment(comment) => {
                return Some(Err(LexError::UnclosedComment(comment)));
            }
            Context::SingleLineComment => {}
            Context::None => {}
        }

        if !self.partial.is_empty() {
            return Some(Err(LexError::UnexpectedToken(Token::invalid(
                self.input,
                self.partial,
                self.location,
            ))));
        }

        None
    }
}

#[macro_export]
macro_rules! token {
    (($type:ident, $var:expr)) => {
        TokenType::$type($var)
    };
}

#[macro_export]
macro_rules! tokens {
        ( $( $token:tt ),* $(,)? ) => {
            &[
                $( token!($token)),*
            ]
        }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{LexError, Lexer, TokenType};

    #[test]
    fn tokenize_symbols() {
        let lexer = Lexer::new("\"\"()[]:.,#\n**");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Symbol, "\""),
            (String, ""),
            (Symbol, "\""),
            (Command, ""),
            (Symbol, "("),
            (Symbol, ")"),
            (Symbol, "["),
            (Symbol, "]"),
            (Identifier, ""),
            (Symbol, ":"),
            (Symbol, "."),
            (Symbol, ","),
            (Symbol, "#"),
            (Comment, ""),
            (Symbol, "*"),
            (Comment, ""),
            (Symbol, "*"),
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_string() {
        let lexer = Lexer::new(r#""hello world""#);
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![(Symbol, "\""), (String, r#"hello world"#), (Symbol, "\"")];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_escaped_string() {
        let lexer = Lexer::new(r#""hello \n \"quoted\" world""#);
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Symbol, "\""),
            (String, r#"hello \n \"quoted\" world"#),
            (Symbol, "\"")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_complex() {
        let input = "
        i.store(0) # comment
        names.store([\"name 1\", \"name 2\"])
        * multi line
        comment
        here
        *
        names.index(i).print() 
        i.add(1)
        print(nl())
        names.index(i).print()
        nl().print()
    ";

        let lexer = Lexer::new(&input);
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Number, "0"),
            (Symbol, ")"),
            (Symbol, "#"),
            (Comment, " comment"),
            (Identifier, "names"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Symbol, "["),
            (Symbol, "\""),
            (String, "name 1"),
            (Symbol, "\""),
            (Symbol, ","),
            (Symbol, "\""),
            (String, "name 2"),
            (Symbol, "\""),
            (Symbol, "]"),
            (Symbol, ")"),
            (Symbol, "*"),
            (
                Comment,
                " multi line\n        comment\n        here\n        "
            ),
            (Symbol, "*"),
            (Identifier, "names"),
            (Symbol, "."),
            (Command, "index"),
            (Symbol, "("),
            (Identifier, "i"),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "print"),
            (Symbol, "("),
            (Symbol, ")"),
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "add"),
            (Symbol, "("),
            (Number, "1"),
            (Symbol, ")"),
            (Command, "print"),
            (Symbol, "("),
            (Command, "nl"),
            (Symbol, "("),
            (Symbol, ")"),
            (Symbol, ")"),
            (Identifier, "names"),
            (Symbol, "."),
            (Command, "index"),
            (Symbol, "("),
            (Identifier, "i"),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "print"),
            (Symbol, "("),
            (Symbol, ")"),
            (Command, "nl"),
            (Symbol, "("),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "print"),
            (Symbol, "("),
            (Symbol, ")"),
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_negative_number() {
        let lexer = Lexer::new("i.store(-1)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Number, "-1"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_float() {
        let lexer = Lexer::new("i.store(3.14)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Number, "3.14"),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    #[should_panic]
    fn tokenize_float_missing_suffix() {
        let lexer = Lexer::new("i.store(3.)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        // Leaving this here incase it is desirable to support missing float suffixes in the future
        let expected = tokens![
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Number, "3"),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
        dbg!(&tokens);
    }

    #[test]
    fn tokenize_boolean_keywords() {
        let lexer = Lexer::new("if(true, false)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "if"),
            (Symbol, "("),
            (Keyword, "true"),
            (Symbol, ","),
            (Keyword, "false"),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_map() {
        let lexer = Lexer::new("[key: value]");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Symbol, "["),
            (Identifier, "key"),
            (Symbol, ":"),
            (Identifier, "value"),
            (Symbol, "]")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_chained_calls() {
        let lexer = Lexer::new("player.health.get()");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "player"),
            (Symbol, "."),
            (Identifier, "health"),
            (Symbol, "."),
            (Command, "get"),
            (Symbol, "("),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_empty_string() {
        let lexer = Lexer::new(r#""""#);
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![(Symbol, "\""), (String, ""), (Symbol, "\"")];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn unclosed_string_error() {
        let lexer = Lexer::new(r#""unclosed"#);
        let result = lexer.collect::<Result<Vec<_>, _>>();
        dbg!(&result);
        assert!(matches!(result, Err(LexError::UnclosedString(_))));
    }

    #[test]
    fn unclosed_comment_error() {
        let lexer = Lexer::new("* unclosed comment");
        let result = lexer.collect::<Result<Vec<_>, _>>();
        dbg!(&result);
        assert!(matches!(result, Err(LexError::UnclosedComment(_))));
    }

    #[test]
    fn tokenize_inline_comment() {
        let lexer = Lexer::new("i.store(0) # this is a comment\ni.inc()");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Number, "0"),
            (Symbol, ")"),
            (Symbol, "#"),
            (Comment, " this is a comment"),
            (Identifier, "i"),
            (Symbol, "."),
            (Command, "inc"),
            (Symbol, "("),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_nested_calls() {
        let lexer = Lexer::new("print(add(1, 2))");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "print"),
            (Symbol, "("),
            (Command, "add"),
            (Symbol, "("),
            (Number, "1"),
            (Symbol, ","),
            (Number, "2"),
            (Symbol, ")"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_many_escapes() {
        let lexer = Lexer::new(r#"trim("\"hey\".", ".\"").print()"#);
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "trim"),
            (Symbol, "("),
            (Symbol, "\""),
            (String, "\\\"hey\\\"."),
            (Symbol, "\""),
            (Symbol, ","),
            (Symbol, "\""),
            (String, ".\\\""),
            (Symbol, "\""),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "print"),
            (Symbol, "("),
            (Symbol, ")"),
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_advanced_1() {
        let lexer = Lexer::new(
            r#"
        text.store("  hello world  ")
        result.store(
            text
                .remove_whitespace()
                .uppercase()
                .concat("!!!")
        )
        print(result)
        "#,
        );
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Identifier, "text"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Symbol, "\""),
            (String, "  hello world  "),
            (Symbol, "\""),
            (Symbol, ")"),
            (Identifier, "result"),
            (Symbol, "."),
            (Command, "store"),
            (Symbol, "("),
            (Identifier, "text"),
            (Symbol, "."),
            (Command, "remove_whitespace"),
            (Symbol, "("),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "uppercase"),
            (Symbol, "("),
            (Symbol, ")"),
            (Symbol, "."),
            (Command, "concat"),
            (Symbol, "("),
            (Symbol, "\""),
            (String, "!!!"),
            (Symbol, "\""),
            (Symbol, ")"),
            (Symbol, ")"),
            (Command, "print"),
            (Symbol, "("),
            (Identifier, "result"),
            (Symbol, ")"),
        ];
        assert!(tokens == expected);
    }
    #[test]
    fn tokenize_integer() {
        let lexer = Lexer::new("store(42)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "store"),
            (Symbol, "("),
            (Number, "42"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_float_in_command() {
        let lexer = Lexer::new("store(3.14)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "store"),
            (Symbol, "("),
            (Number, "3.14"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_negative_integer() {
        let lexer = Lexer::new("store(-42)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "store"),
            (Symbol, "("),
            (Number, "-42"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_negative_float() {
        let lexer = Lexer::new("store(-3.14)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "store"),
            (Symbol, "("),
            (Number, "-3.14"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_float_method_chain() {
        let lexer = Lexer::new("3.14.floor()");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Number, "3.14"),
            (Symbol, "."),
            (Command, "floor"),
            (Symbol, "("),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_int_method_chain() {
        let lexer = Lexer::new("42.add(1)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Number, "42"),
            (Symbol, "."),
            (Command, "add"),
            (Symbol, "("),
            (Number, "1"),
            (Symbol, ")")
        ];
        println!("\n===GOT===\n");
        dbg!(&tokens);
        println!("\n===EXPECTED===\n");
        dbg!(&expected);
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_float_in_list() {
        let lexer = Lexer::new("[1.5, 2.5, 3.5]");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Symbol, "["),
            (Number, "1.5"),
            (Symbol, ","),
            (Number, "2.5"),
            (Symbol, ","),
            (Number, "3.5"),
            (Symbol, "]")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_zero_float() {
        let lexer = Lexer::new("store(0.0)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "store"),
            (Symbol, "("),
            (Number, "0.0"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }

    #[test]
    fn tokenize_multiple_numbers() {
        let lexer = Lexer::new("add(1, 2.5, -3, -4.5)");
        let tokens = lexer.map(|t| t.unwrap().token_type).collect::<Vec<_>>();
        let expected = tokens![
            (Command, "add"),
            (Symbol, "("),
            (Number, "1"),
            (Symbol, ","),
            (Number, "2.5"),
            (Symbol, ","),
            (Number, "-3"),
            (Symbol, ","),
            (Number, "-4.5"),
            (Symbol, ")")
        ];
        assert!(tokens == expected);
    }
}
