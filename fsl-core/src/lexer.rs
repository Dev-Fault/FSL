use std::{collections::VecDeque, sync::OnceLock};

const QUOTE: &str = "\"";
const OPEN_PAREN: &str = "(";
const CLOSED_PAREN: &str = ")";
const OPEN_BRACKET: &str = "[";
const CLOSED_BRACKET: &str = "]";
const COLON: &str = ":";
const DOT: &str = ".";
const COMMA: &str = ",";
const HASHTAG: &str = "#";
const STAR: &str = r#"*"#;

const SYMBOLS: &[&str] = &[
    QUOTE,
    OPEN_PAREN,
    CLOSED_PAREN,
    OPEN_BRACKET,
    CLOSED_BRACKET,
    COLON,
    DOT,
    COMMA,
    HASHTAG,
    STAR,
];

static SORTED_SYMBOLS: OnceLock<Vec<&str>> = OnceLock::new();

const KEYWORD_TRUE: &str = "true";
const KEYWORD_FALSE: &str = "false";

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LexerErrorContext<'a> {
    input: &'a str,
    location: usize,
}

pub fn format_error_context(input: &str, location: usize) -> String {
    let left: usize = input[..location]
        .char_indices()
        .rev()
        .take(50)
        .last()
        .map(|(i, _)| i)
        .unwrap_or(location);
    let right = input[location..]
        .char_indices()
        .take(50)
        .last()
        .map(|(i, _)| i + location)
        .unwrap_or(location);
    let context = &input[left..right];
    format!("Area near where error was detected:\n{}", context,)
}

impl<'a> LexerErrorContext<'a> {
    pub fn new(input: &'a str, location: usize) -> Self {
        Self { input, location }
    }
}

impl ToString for LexerErrorContext<'_> {
    fn to_string(&self) -> String {
        format_error_context(self.input, self.location)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexerError<'a> {
    DotPreceededByInvalidToken(LexerErrorContext<'a>),
    CommaPrecededByInvalidToken(LexerErrorContext<'a>),
    ColonPrecededByInvalidToken(LexerErrorContext<'a>),
    ClosedParenPrecededByInvalidToken(LexerErrorContext<'a>),
    ClosedBracketPrecededByInvalidToken(LexerErrorContext<'a>),
    UnclosedOpenParenthesis(LexerErrorContext<'a>),
    UnclosedOpenBracket(LexerErrorContext<'a>),
    UnclosedString(LexerErrorContext<'a>),
    UnmatchedClosingParen(LexerErrorContext<'a>),
    UnmatchedClosingBracket(LexerErrorContext<'a>),
    SymbolUsedOutsideOfContext(LexerErrorContext<'a>),
    TrailingToken(LexerErrorContext<'a>),
    InvalidEscapeSequence(LexerErrorContext<'a>),
    InvalidNumber(LexerErrorContext<'a>),
}

impl ToString for LexerError<'_> {
    fn to_string(&self) -> String {
        match self {
            LexerError::DotPreceededByInvalidToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place dot operator",
                    error_context.to_string()
                )
            }
            LexerError::CommaPrecededByInvalidToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place comma",
                    error_context.to_string()
                )
            }
            LexerError::ColonPrecededByInvalidToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place colon",
                    error_context.to_string()
                )
            }
            LexerError::ClosedParenPrecededByInvalidToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place closing parenthesis",
                    error_context.to_string()
                )
            }
            LexerError::ClosedBracketPrecededByInvalidToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place closed bracket",
                    error_context.to_string()
                )
            }
            LexerError::UnclosedOpenParenthesis(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Unclosed open parenthesis",
                    error_context.to_string()
                )
            }
            LexerError::UnclosedOpenBracket(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Unclosed open bracket",
                    error_context.to_string()
                )
            }
            LexerError::UnclosedString(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Unclosed quotes",
                    error_context.to_string()
                )
            }
            LexerError::UnmatchedClosingParen(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Unmatched closing parenthesis",
                    error_context.to_string()
                )
            }
            LexerError::UnmatchedClosingBracket(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Unmatched closing bracket",
                    error_context.to_string()
                )
            }
            LexerError::SymbolUsedOutsideOfContext(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Out of place symbol",
                    error_context.to_string()
                )
            }
            LexerError::TrailingToken(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Symbols outside of command",
                    error_context.to_string()
                )
            }
            LexerError::InvalidEscapeSequence(error_context) => {
                format!(
                    "{}\n{}",
                    r#"Syntax error: Unknown escape sequence (Did you mean to use a back slash? try \\)"#,
                    error_context.to_string()
                )
            }
            LexerError::InvalidNumber(error_context) => {
                format!(
                    "{}\n{}",
                    "Syntax error: Invalid number",
                    error_context.to_string()
                )
            }
        }
    }
}

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

impl Symbol {
    pub fn as_str(&self) -> &str {
        match self {
            Symbol::Quote => QUOTE,
            Symbol::OpenParen => OPEN_PAREN,
            Symbol::ClosedParen => CLOSED_PAREN,
            Symbol::OpenBracket => OPEN_BRACKET,
            Symbol::ClosedBracket => CLOSED_BRACKET,
            Symbol::Colon => COLON,
            Symbol::Dot => DOT,
            Symbol::Comma => COMMA,
            Symbol::Hashtag => HASHTAG,
            Symbol::Star => STAR,
        }
    }
}

fn symbol_from_str(value: &str) -> Option<Symbol> {
    Some(match value {
        QUOTE => Symbol::Quote,
        OPEN_PAREN => Symbol::OpenParen,
        CLOSED_PAREN => Symbol::ClosedParen,
        OPEN_BRACKET => Symbol::OpenBracket,
        CLOSED_BRACKET => Symbol::ClosedBracket,
        COLON => Symbol::Colon,
        DOT => Symbol::Dot,
        COMMA => Symbol::Comma,
        HASHTAG => Symbol::Hashtag,
        STAR => Symbol::Star,
        _ => {
            return None;
        }
    })
}

fn get_symbol(value: &str) -> Option<Symbol> {
    let longest_first = SORTED_SYMBOLS.get_or_init(|| {
        let mut symbols = SYMBOLS.to_vec();
        symbols.sort_by(|a, b| b.len().cmp(&a.len()));
        symbols
    });

    for symbol in longest_first {
        if value.ends_with(symbol) {
            return symbol_from_str(symbol);
        }
    }
    None
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    True,
    False,
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::True => KEYWORD_TRUE,
            Keyword::False => KEYWORD_FALSE,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Symbol(Symbol),
    Command(String),
    Number(String),
    String(String),
    Keyword(Keyword),
    Var(String),
    None,
}

impl Default for TokenType {
    fn default() -> Self {
        Self::None
    }
}

impl TokenType {
    pub fn as_str(&self) -> &str {
        match self {
            TokenType::Symbol(symbol) => symbol.as_str(),
            TokenType::Command(command) => command,
            TokenType::Number(number) => number,
            TokenType::String(string) => string,
            TokenType::Keyword(keyword) => keyword.as_str(),
            TokenType::Var(var) => var,
            TokenType::None => "",
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub location: usize,
}

impl Token {
    pub fn new(token_type: TokenType, location: usize) -> Self {
        Self {
            token_type,
            location,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Lexer {
    command_depth: usize,
    list_depth: usize,
    inside_string: bool,
    inside_single_line_comment: bool,
    inside_multi_line_comment: bool,
}

fn get_keyword(input: &str) -> Option<Keyword> {
    match input {
        KEYWORD_TRUE => Some(Keyword::True),
        KEYWORD_FALSE => Some(Keyword::False),
        _ => None,
    }
}

fn parse_token(input: String) -> TokenType {
    if input.parse::<f64>().is_ok() {
        TokenType::Number(input)
    } else if let Some(keyword) = get_keyword(&input) {
        TokenType::Keyword(keyword)
    } else {
        TokenType::Var(input)
    }
}

pub trait LastType {
    fn last_type(&self) -> Option<&TokenType>;
}

impl LastType for VecDeque<Token> {
    fn last_type(&self) -> Option<&TokenType> {
        Some(&self.into_iter().last()?.token_type)
    }
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            command_depth: 0,
            list_depth: 0,
            inside_string: false,
            inside_single_line_comment: false,
            inside_multi_line_comment: false,
        }
    }

    pub fn inside_code(&self) -> bool {
        !self.inside_string && !self.inside_comment()
    }

    pub fn inside_comment(&self) -> bool {
        self.inside_single_line_comment || self.inside_multi_line_comment
    }

    /// Turns input into a vec of Tokens
    pub fn tokenize<'a>(mut self, code: &'a str) -> Result<VecDeque<Token>, LexerError<'a>> {
        let mut tokens = VecDeque::new();
        let mut buf = String::with_capacity(u8::MAX.into());
        let mut prev_ch = '\0';
        let mut escaped = false;

        for (i, ch) in code.char_indices() {
            let err_context = LexerErrorContext::new(code, i);

            if ch == '\n' && self.inside_single_line_comment {
                buf.clear();
                self.inside_single_line_comment = false;
            }
            if self.inside_single_line_comment {
                continue;
            }

            if !self.inside_string && ch.is_whitespace() {
                continue;
            } else if self.inside_string && escaped {
                escaped = false;
                continue;
            } else if self.inside_string
                && ch == '\\'
                && let Some(next_ch) = code[i + ch.len_utf8()..].chars().next()
            {
                escaped = true;
                match next_ch {
                    '\\' => {
                        buf.push_str("\\");
                        continue;
                    }
                    'n' => {
                        buf.push_str("\n");
                        continue;
                    }
                    't' => {
                        buf.push_str("\t");
                        continue;
                    }
                    '"' => {
                        buf.push_str("\"");
                        continue;
                    }
                    _ => {
                        return Err(LexerError::InvalidEscapeSequence(err_context));
                    }
                }
            }

            buf.push(ch);

            if let Some(symbol) = get_symbol(&buf) {
                for _ in 0..symbol.as_str().len() {
                    buf.pop();
                }

                match symbol {
                    Symbol::Quote if !self.inside_comment() => {
                        if self.inside_string {
                            tokens.push_back(Token::new(
                                TokenType::String(std::mem::take(&mut buf)),
                                i,
                            ));
                            tokens.push_back(Token::new(TokenType::Symbol(Symbol::Quote), i));
                        } else {
                            tokens.push_back(Token::new(TokenType::Symbol(Symbol::Quote), i));
                        }

                        self.inside_string = !self.inside_string;
                        buf.clear();
                    }
                    Symbol::OpenParen if self.inside_code() => {
                        tokens
                            .push_back(Token::new(TokenType::Command(std::mem::take(&mut buf)), i));
                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::OpenParen), i));
                        self.command_depth += 1;
                    }
                    Symbol::ClosedParen if self.inside_code() => {
                        const INVALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Dot, Symbol::OpenBracket];

                        if !buf.is_empty() {
                            tokens.push_back(Token::new(parse_token(std::mem::take(&mut buf)), i));
                        }

                        if tokens.is_empty() || self.command_depth == 0 {
                            return Err(LexerError::UnmatchedClosingParen(err_context));
                        } else if let Some(TokenType::Symbol(symbol)) = tokens.last_type()
                            && INVALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::ClosedParenPrecededByInvalidToken(err_context));
                        } else {
                            tokens.push_back(Token::new(TokenType::Symbol(Symbol::ClosedParen), i));
                            self.command_depth -= 1;
                        }
                    }
                    Symbol::OpenBracket if self.inside_code() => {
                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::OpenBracket), i));
                        self.list_depth += 1;
                        buf.clear();
                    }
                    Symbol::ClosedBracket if self.inside_code() => {
                        const INVALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Dot, Symbol::OpenParen];

                        if tokens.is_empty() || self.list_depth == 0 {
                            return Err(LexerError::UnmatchedClosingBracket(err_context));
                        } else if let Some(TokenType::Symbol(symbol)) = tokens.last_type()
                            && INVALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::ClosedBracketPrecededByInvalidToken(
                                err_context,
                            ));
                        } else {
                            if !buf.is_empty() {
                                tokens.push_back(Token::new(
                                    parse_token(std::mem::take(&mut buf)),
                                    i,
                                ));
                            }
                            tokens
                                .push_back(Token::new(TokenType::Symbol(Symbol::ClosedBracket), i));
                            self.list_depth -= 1;
                        }
                    }
                    Symbol::Dot if self.inside_code() => {
                        const VALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Quote, Symbol::ClosedParen, Symbol::ClosedBracket];

                        let buf_might_be_number =
                            buf.chars().all(|ch| ch.is_numeric() || ch == '.');
                        let number_already_has_decimal =
                            buf_might_be_number && buf.contains(symbol.as_str());
                        let prev_symbol =
                            if let Some(TokenType::Symbol(symbol)) = tokens.last_type() {
                                Some(symbol)
                            } else {
                                None
                            };

                        if number_already_has_decimal {
                            return Err(LexerError::InvalidNumber(err_context));
                        } else if buf_might_be_number && prev_ch.is_numeric() {
                            buf.push_str(Symbol::Dot.as_str());
                        } else if buf.is_empty()
                            && prev_symbol
                                .is_some_and(|symbol| !VALID_PRECEDING_SYMBOLS.contains(symbol))
                        {
                            return Err(LexerError::DotPreceededByInvalidToken(err_context));
                        } else {
                            if !buf.is_empty() {
                                tokens.push_back(Token::new(
                                    TokenType::Var(std::mem::take(&mut buf)),
                                    i,
                                ));
                            } else if tokens.is_empty() {
                                return Err(LexerError::DotPreceededByInvalidToken(err_context));
                            }
                            tokens.push_back(Token::new(TokenType::Symbol(Symbol::Dot), i));
                        }
                    }
                    Symbol::Comma if self.inside_code() => {
                        const VALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Quote, Symbol::ClosedParen, Symbol::ClosedBracket];

                        if !buf.is_empty() {
                            tokens.push_back(Token::new(parse_token(std::mem::take(&mut buf)), i));
                        } else if let Some(TokenType::Symbol(symbol)) = tokens.last_type()
                            && !VALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::CommaPrecededByInvalidToken(err_context));
                        }

                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::Comma), i));
                    }
                    Symbol::Colon if self.inside_code() => {
                        if self.list_depth == 0 {
                            return Err(LexerError::SymbolUsedOutsideOfContext(err_context));
                        } else if !buf.is_empty() {
                            tokens.push_back(Token::new(parse_token(std::mem::take(&mut buf)), i));
                        } else if !tokens
                            .last_type()
                            .is_some_and(|token| matches!(token, TokenType::Var(_)))
                        {
                            return Err(LexerError::ColonPrecededByInvalidToken(err_context));
                        }

                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::Colon), i));
                    }
                    Symbol::Hashtag if !self.inside_string && !self.inside_multi_line_comment => {
                        self.inside_single_line_comment = true;
                    }
                    Symbol::Star if !self.inside_string && !self.inside_single_line_comment => {
                        buf.clear();
                        self.inside_multi_line_comment = !self.inside_multi_line_comment;
                    }
                    _ => {
                        if self.inside_string {
                            buf.push_str(symbol.as_str());
                        } else if !self.inside_comment() {
                            return Err(LexerError::SymbolUsedOutsideOfContext(err_context));
                        }
                    }
                };
            }

            prev_ch = ch;
        }

        if self.inside_string {
            return Err(LexerError::UnclosedString(LexerErrorContext::new(
                code,
                code.len(),
            )));
        } else if self.command_depth > 0 {
            return Err(LexerError::UnclosedOpenParenthesis(LexerErrorContext::new(
                code,
                code.len(),
            )));
        } else if self.list_depth > 0 {
            return Err(LexerError::UnclosedOpenBracket(LexerErrorContext::new(
                code,
                code.len(),
            )));
        } else if !buf.is_empty() && !self.inside_comment() {
            return Err(LexerError::TrailingToken(LexerErrorContext::new(
                code,
                code.len(),
            )));
        } else if tokens
            .last_type()
            .is_some_and(|t| *t != TokenType::Symbol(Symbol::ClosedParen))
        {
            return Err(LexerError::TrailingToken(LexerErrorContext::new(
                code,
                code.len(),
            )));
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, LexerError, Symbol, TokenType};

    macro_rules! token {
        (($sym:path)) => {
            TokenType::Symbol($sym)
        };
        (($type:ident, $var:expr)) => {
            TokenType::$type($var.to_string())
        };
    }

    macro_rules! tokens {
        ( $( $token:tt ),* $(,)? ) => {
            &[
                $( token!($token)),*
            ]
        }
    }

    #[test]
    fn print_error_context() {
        let lexer = Lexer::new();

        let input = r#"
            selections.store(["rock", "paper", "scissors"])
            selection.store(selections.index(random_range(0, 2)))
            response.store(lowercase(ask("Rock paper or scissors?")))
            if_then_else(
            	not(
            		or(
            			response.eq("rock"),
            			response.eq("paper"),
            			response.eq("scissors")
            		)
            	),
            	say("Only rock, paper, or scissors are acceptable values."),
            	(
            		sayselection),
            		if_then(selection.eq(response), print("Tie!"))
            		if_then(and(selection.eq("rock"), response.eq("scissors")), print("I win!"))
            		if_then(and(selection.eq("rock"), response.eq("paper")), print("Dang..."))
            		if_then(and(selection.eq("paper"), response.eq("scissors")), print("Dang..."))
            		if_then(and(selection.eq("paper"), response.eq("rock")), print("I win!"))
            		if_then(and(selection.eq("scissors"), response.eq("rock")), print("Dang..."))
            		if_then(and(selection.eq("scissors"), response.eq("paper")), print("I win!"))
            	)
            )
        "#;

        let tokens = lexer.tokenize(input);

        assert!(tokens.is_err());
        println!("\n{}\n", tokens.err().unwrap().to_string());
    }

    #[test]
    fn tokenize_identifer_and_string() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("a.store(\"hello\")")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Var, "a"),
            (Symbol::Dot),
            (Command, "store"),
            (Symbol::OpenParen),
            (Symbol::Quote),
            (String, "hello"),
            (Symbol::Quote),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_args() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("add(12,34)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "add"),
            (Symbol::OpenParen),
            (Number, 12),
            (Symbol::Comma),
            (Number, 34),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_list() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("list.store([1, 2, 3, 4])")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens: &[TokenType] = &[
            TokenType::Var("list".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("store".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::OpenBracket),
            TokenType::Number("1".to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number("2".to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number("3".to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number("4".to_string()),
            TokenType::Symbol(Symbol::ClosedBracket),
            TokenType::Symbol(Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }
    #[test]
    fn tokenize_map() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("map.store([name: \"blah\", address: \"blah\"])")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Var, "map"),
            (Symbol::Dot),
            (Command, "store"),
            (Symbol::OpenParen),
            (Symbol::OpenBracket),
            (Var, "name"),
            (Symbol::Colon),
            (Symbol::Quote),
            (String, "blah"),
            (Symbol::Quote),
            (Symbol::Comma),
            (Var, "address"),
            (Symbol::Colon),
            (Symbol::Quote),
            (String, "blah"),
            (Symbol::Quote),
            (Symbol::ClosedBracket),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn colon_without_key() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize("map.store([: \"blah\"])");
        assert!(tokens.is_err());
    }

    #[test]
    fn colon_outside_of_map() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize("name: \"blah\"");
        assert!(tokens.is_err());
    }

    #[test]
    fn double_colon() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize("map.store([name:: \"blah\"])");
        assert!(tokens.is_err());
    }

    #[test]
    fn tokenize_complex() {
        let input = "
            i.store(0)
            names.store([\"name 1\", \"name 2\"])
            names.index(i).print() 
            i.add(1)
            print(nl())
            names.index(i).print()
            nl().print()
        ";

        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(input)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Var, "i"),
            (Symbol::Dot),
            (Command, "store"),
            (Symbol::OpenParen),
            (Number, 0),
            (Symbol::ClosedParen),
            (Var, "names"),
            (Symbol::Dot),
            (Command, "store"),
            (Symbol::OpenParen),
            (Symbol::OpenBracket),
            (Symbol::Quote),
            (String, "name 1"),
            (Symbol::Quote),
            (Symbol::Comma),
            (Symbol::Quote),
            (String, "name 2"),
            (Symbol::Quote),
            (Symbol::ClosedBracket),
            (Symbol::ClosedParen),
            (Var, "names"),
            (Symbol::Dot),
            (Command, "index"),
            (Symbol::OpenParen),
            (Var, "i"),
            (Symbol::ClosedParen),
            (Symbol::Dot),
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
            (Var, "i"),
            (Symbol::Dot),
            (Command, "add"),
            (Symbol::OpenParen),
            (Number, 1),
            (Symbol::ClosedParen),
            (Command, "print"),
            (Symbol::OpenParen),
            (Command, "nl"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
            (Symbol::ClosedParen),
            (Var, "names"),
            (Symbol::Dot),
            (Command, "index"),
            (Symbol::OpenParen),
            (Var, "i"),
            (Symbol::ClosedParen),
            (Symbol::Dot),
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
            (Command, "nl"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
            (Symbol::Dot),
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_new_line_symbol() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("new\nline")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }

        println!("{}", text_output);
        assert!(text_output == "new\nline");
    }

    #[test]
    fn tokenize_multiple_new_line_symbols() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("hello\nmy\nwonderful\nfriend")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }

        println!("{}", text_output);
        assert!(text_output == "hello\nmy\nwonderful\nfriend");
    }

    #[test]
    fn tokenize_tab_symbol() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("tabbed\ttext")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == "tabbed\ttext");
    }

    #[test]
    fn tokenize_escaped_quote() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("escaped\"quote")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == "escaped\"quote");
    }

    #[test]
    fn tokenize_rolitary_slash() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""\""#);

        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))))
    }

    #[test]
    fn escape_at_end() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""\""#);
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))));
    }

    #[test]
    fn escape_single_slash() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#"print("\\")"#);
        dbg!(&tokens);
        let tokens = tokens
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }

        println!("OUTPUT: {}", text_output);
        assert!(text_output == r#"\"#);
    }

    #[test]
    fn escaped_unclosed_string() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""\\\""#);
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))));
    }

    #[test]
    fn invalid_escape_sequence() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#" "back\slash" "#);
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::InvalidEscapeSequence(_))));
    }

    #[test]
    fn tokenize_empty_string() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in &tokens {
            if let TokenType::String(text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("start{}end", text_output);

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::Quote),
            (String, ""),
            (Symbol::Quote),
            (Symbol::ClosedParen),
        ];

        assert!(tokens == valid_tokens);

        assert!(text_output == r"");
    }

    #[test]
    fn tokenize_empty_string_with_spaces() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("  ")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::Quote),
            (String, "  "),
            (Symbol::Quote),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_empty_string_with_spaces_in_command() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("  ")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("start{}end", text_output);

        assert!(text_output == r#"  "#);
    }

    #[test]
    fn tokenize_string_spaces() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print(" h    e ")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == r#" h    e "#);
    }

    #[test]
    fn tokenize_double_back_slash() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(r#"print("back\\slash")"#)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let TokenType::String(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == r"back\slash");
    }

    #[test]
    fn tokenize_unescaped_quote() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#"print("unescaped\\"quote")"#);
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))));
    }

    #[test]
    fn tokenize_symbols_in_strings() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("print(\"h(el.lo), Wor[ld]\")")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Symbol::Quote),
            (String, "h(el.lo), Wor[ld]"),
            (Symbol::Quote),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_list_dot() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("[1, 2, 3].length()")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Symbol::OpenBracket),
            (Number, 1),
            (Symbol::Comma),
            (Number, 2),
            (Symbol::Comma),
            (Number, 3),
            (Symbol::ClosedBracket),
            (Symbol::Dot),
            (Command, "length"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_string_dot() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("\"123\".length()")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Symbol::Quote),
            (String, "123"),
            (Symbol::Quote),
            (Symbol::Dot),
            (Command, "length"),
            (Symbol::OpenParen),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_var_dot() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("character.weapon.name.store(\"sword\")")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Var, "character"),
            (Symbol::Dot),
            (Var, "weapon"),
            (Symbol::Dot),
            (Var, "name"),
            (Symbol::Dot),
            (Command, "store"),
            (Symbol::OpenParen),
            (Symbol::Quote),
            (String, "sword"),
            (Symbol::Quote),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn tokenize_var_dot_in_command() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("print(character.weapon.name)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Var, "character"),
            (Symbol::Dot),
            (Var, "weapon"),
            (Symbol::Dot),
            (Var, "name"),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn decimal_number() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize("print(1.1)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Number, 1.1),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn big_decimal_number() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize("print(11.11)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Number, 11.11),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn number_with_multiple_dots() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize("print(1.11.1.1)");
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::InvalidNumber(_))));
    }

    #[test]
    fn number_ending_with_dot() {
        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize("print(1.,)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Number, "1."),
            (Symbol::Comma),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn number_starting_with_dot() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize("print(.1)");

        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::DotPreceededByInvalidToken(_))));
    }

    #[test]
    fn ignore_escapes_outside_string() {
        let lexer = Lexer::new();

        let tokens = lexer
            .tokenize("print(\\n)")
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens = tokens![
            (Command, "print"),
            (Symbol::OpenParen),
            (Var, r#"\n"#),
            (Symbol::ClosedParen),
        ];

        println!("");
        for token in &tokens {
            println!("{:?}", token);
        }

        assert!(tokens == valid_tokens);
    }

    #[test]
    fn err_on_trailing_tokens() {
        let lexer = Lexer::new();

        let result = lexer.tokenize("print(\"hello world\")blah");

        assert!(result.is_err());
    }

    #[test]
    fn err_on_unclosed_string() {
        let lexer = Lexer::new();

        let result = lexer.tokenize("print(\"hello world)");

        assert!(result.is_err());
    }

    #[test]
    fn err_on_out_of_place_comma() {
        let result = Lexer::new().tokenize("print(,)");

        assert!(result.is_err());

        let result = Lexer::new().tokenize("print(1,,1)");

        assert!(result.is_err());

        let result = Lexer::new().tokenize("print([,1,2])");

        assert!(result.is_err());
    }

    #[test]
    fn err_on_out_of_place_dot() {
        let _ = Lexer::new();

        let result = Lexer::new().tokenize("print(.)");

        dbg!(&result);
        assert!(result.is_err_and(|e| matches!(e, LexerError::DotPreceededByInvalidToken(_))));

        let result = Lexer::new().tokenize(".print()");

        dbg!(&result);
        assert!(result.is_err_and(|e| matches!(e, LexerError::DotPreceededByInvalidToken(_))));

        let result = Lexer::new().tokenize("print([1,2].)");
        dbg!(&result);

        assert!(
            result.is_err_and(|e| matches!(e, LexerError::ClosedParenPrecededByInvalidToken(_)))
        );
    }

    #[test]
    fn emoji_lexing() {
        let result = Lexer::new().tokenize("print(\"🧪\")");

        dbg!(&result);

        assert!(result.is_ok());
    }

    #[test]
    fn emoji_lexing_with_escape_sequences() {
        let result = Lexer::new().tokenize(r#"emoji.store("🧪") print("\n", emoji)"#);

        dbg!(&result);

        assert!(result.is_ok());
    }

    #[test]
    fn grapheme_handling() {
        let result = Lexer::new().tokenize(r#"print("é ñ ü")"#);

        dbg!(&result);

        assert!(result.is_ok());
    }

    #[test]
    fn error_context_with_multibyte_chars() {
        let result = Lexer::new().tokenize(r#"print("é ñ ü") trailing"#);
        dbg!(&result);
        assert!(result.is_err());
    }
}
