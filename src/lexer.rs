use std::{collections::VecDeque, sync::OnceLock};

const QUOTE: &str = "\"";
const OPEN_PAREN: &str = "(";
const CLOSED_PAREN: &str = ")";
const OPEN_BRACKET: &str = "[";
const CLOSED_BRACKET: &str = "]";
const DOT: &str = ".";
const COMMA: &str = ",";

const SYMBOLS: &[&str] = &[
    QUOTE,
    OPEN_PAREN,
    CLOSED_PAREN,
    OPEN_BRACKET,
    CLOSED_BRACKET,
    DOT,
    COMMA,
];

static SORTED_SYMBOLS: OnceLock<Vec<&str>> = OnceLock::new();

const KEYWORD_TRUE: &str = "true";
const KEYWORD_FALSE: &str = "false";

const KEYWORDS: &[&str] = &[KEYWORD_TRUE, KEYWORD_FALSE];

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ErrorContext<'a> {
    input: &'a str,
    location: usize,
}

pub fn format_error_context(input: &str, location: usize) -> String {
    let left = location.saturating_sub(50);
    let right = (location.saturating_add(50)).clamp(0, input.len());
    let context = &input[left..right];
    format!("Area near where error was detected:\n{}", context,)
}

impl<'a> ErrorContext<'a> {
    pub fn new(input: &'a str, location: usize) -> Self {
        Self { input, location }
    }
}

impl ToString for ErrorContext<'_> {
    fn to_string(&self) -> String {
        format_error_context(self.input, self.location)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexerError<'a> {
    DotPreceededByInvalidToken(ErrorContext<'a>),
    CommaPrecededByInvalidToken(ErrorContext<'a>),
    ClosedParenPrecededByInvalidToken(ErrorContext<'a>),
    ClosedBracketPrecededByInvalidToken(ErrorContext<'a>),
    UnclosedOpenParenthesis(ErrorContext<'a>),
    UnclosedOpenBracket(ErrorContext<'a>),
    UnclosedString(ErrorContext<'a>),
    UnmatchedClosingParen(ErrorContext<'a>),
    UnmatchedClosingBracket(ErrorContext<'a>),
    SymbolUsedOutsideOfContext(ErrorContext<'a>),
    TrailingToken(ErrorContext<'a>),
    InvalidEscapeSequence(ErrorContext<'a>),
    InvalidNumber(ErrorContext<'a>),
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
    Dot,
    Comma,
}

impl Symbol {
    pub fn as_str(&self) -> &str {
        match self {
            Symbol::Quote => QUOTE,
            Symbol::OpenParen => OPEN_PAREN,
            Symbol::ClosedParen => CLOSED_PAREN,
            Symbol::OpenBracket => OPEN_BRACKET,
            Symbol::ClosedBracket => CLOSED_BRACKET,
            Symbol::Dot => DOT,
            Symbol::Comma => COMMA,
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
        DOT => Symbol::Dot,
        COMMA => Symbol::Comma,
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
        }
    }

    /// Turns input into a vec of Tokens
    pub fn tokenize<'a>(mut self, code: &'a str) -> Result<VecDeque<Token>, LexerError<'a>> {
        let mut tokens = VecDeque::new();
        let mut buf = String::with_capacity(u8::MAX.into());
        let mut prev_ch = '\0';
        let mut escaped = false;

        for (i, ch) in code.chars().enumerate() {
            let err_context = ErrorContext::new(code, i);

            if !self.inside_string && ch.is_whitespace() {
                continue;
            } else if self.inside_string && escaped {
                escaped = false;
                continue;
            } else if self.inside_string
                && ch == '\\'
                && let Some(next_ch) = code.get(i + 1..i + 2)
            {
                escaped = true;
                match next_ch {
                    "\\" => {
                        buf.push_str("\\");
                        continue;
                    }
                    "n" => {
                        buf.push_str("\n");
                        continue;
                    }
                    "t" => {
                        buf.push_str("\t");
                        continue;
                    }
                    "\"" => {
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
                    Symbol::Quote => {
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
                    Symbol::OpenParen if !self.inside_string => {
                        if buf.is_empty() {
                            //return Err(LexerError::OpenParenNotPrecededByCommand(err_context));
                        }

                        tokens
                            .push_back(Token::new(TokenType::Command(std::mem::take(&mut buf)), i));
                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::OpenParen), i));
                        self.command_depth += 1;
                    }
                    Symbol::ClosedParen if !self.inside_string => {
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
                    Symbol::OpenBracket if !self.inside_string => {
                        tokens.push_back(Token::new(TokenType::Symbol(Symbol::OpenBracket), i));
                        self.list_depth += 1;
                        buf.clear();
                    }
                    Symbol::ClosedBracket if !self.inside_string => {
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
                    Symbol::Dot if !self.inside_string => {
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
                    Symbol::Comma if !self.inside_string => {
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
                    _ => {
                        if self.inside_string {
                            buf.push_str(symbol.as_str());
                        } else {
                            return Err(LexerError::SymbolUsedOutsideOfContext(err_context));
                        }
                    }
                };
            }

            prev_ch = ch;
        }

        if self.inside_string {
            return Err(LexerError::UnclosedString(ErrorContext::new(
                code,
                code.len(),
            )));
        } else if self.command_depth > 0 {
            return Err(LexerError::UnclosedOpenParenthesis(ErrorContext::new(
                code,
                code.len(),
            )));
        } else if self.list_depth > 0 {
            return Err(LexerError::UnclosedOpenBracket(ErrorContext::new(
                code,
                code.len(),
            )));
        } else if !buf.is_empty() {
            return Err(LexerError::TrailingToken(ErrorContext::new(
                code,
                code.len(),
            )));
        } else if tokens
            .last_type()
            .is_some_and(|t| *t != TokenType::Symbol(Symbol::ClosedParen))
        {
            return Err(LexerError::TrailingToken(ErrorContext::new(
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
            	say("Fuck off."),
            	(
            		sayselection),
            		if_then(selection.eq(response), print("Tie!"))
            		if_then(and(selection.eq("rock"), response.eq("scissors")), print("I win!"))
            		if_then(and(selection.eq("rock"), response.eq("paper")), print("Goddamnit..."))
            		if_then(and(selection.eq("paper"), response.eq("scissors")), print("Goddamnit..."))
            		if_then(and(selection.eq("paper"), response.eq("rock")), print("I win!"))
            		if_then(and(selection.eq("scissors"), response.eq("rock")), print("Goddamnit..."))
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Var("a".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("store".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("hello".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("add".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("12".to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number("34".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
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
    fn tokenize_complex() {
        let input = "i.store(0)
        names.store([\"name 1\", \"name 2\"])
        names.index(i).print() 
        i.add(1)
        print(nl())
        names.index(i).print()
        nl().print()";

        let lexer = Lexer::new();
        let tokens = lexer
            .tokenize(input)
            .unwrap()
            .iter()
            .map(|t| t.token_type.clone())
            .collect::<Vec<TokenType>>();

        let valid_tokens: &[TokenType] = &[
            TokenType::Var("i".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("store".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("0".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Var("names".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("store".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::OpenBracket),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("name 1".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("name 2".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedBracket),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Var("names".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("index".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Var("i".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Var("i".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("add".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("1".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Command("nl".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Var("names".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("index".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Var("i".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Command("nl".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("  ".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("h(el.lo), Wor[ld]".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Symbol(Symbol::OpenBracket),
            TokenType::Number(1.to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number(2.to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Number(3.to_string()),
            TokenType::Symbol(Symbol::ClosedBracket),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command(format!("length")),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("123".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command(format!("length")),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Var("character".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Var("weapon".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Var("name".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Command("store".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Symbol(Symbol::Quote),
            TokenType::String("sword".to_string()),
            TokenType::Symbol(Symbol::Quote),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Var("character".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Var("weapon".to_string()),
            TokenType::Symbol(Symbol::Dot),
            TokenType::Var("name".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command(format!("print")),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("1.1".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command(format!("print")),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("11.11".to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command(format!("print")),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Number("1.".to_string()),
            TokenType::Symbol(Symbol::Comma),
            TokenType::Symbol(Symbol::ClosedParen),
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

        let valid_tokens: &[TokenType] = &[
            TokenType::Command("print".to_string()),
            TokenType::Symbol(Symbol::OpenParen),
            TokenType::Var(r#"\n"#.to_string()),
            TokenType::Symbol(Symbol::ClosedParen),
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
        let lexer = Lexer::new();

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
}
