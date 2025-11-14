use std::sync::OnceLock;

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

const KEYWORDS: &[&str] = &["true", "false"];

#[derive(Debug, PartialEq, Eq)]
pub struct ErrorContext<'a> {
    pub input: &'a str,
    pub location: usize,
}

impl<'a> ErrorContext<'a> {
    pub fn new(input: &'a str, location: usize) -> Self {
        Self { input, location }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum LexerError<'a> {
    DotPreceededByInvalidToken(ErrorContext<'a>),
    CommaPrecededByInvalidToken(ErrorContext<'a>),
    ClosedParenPrecededByInvalidToken(ErrorContext<'a>),
    ClosedBracketPrecededByInvalidToken(ErrorContext<'a>),
    OpenParenNotPrecededByCommand(ErrorContext<'a>),
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq)]
pub enum Token {
    Symbol(Symbol),
    Command(String),
    Number(String),
    Text(String),
    Keyword(String),
    Var(String),
}

#[derive(Debug, Copy, Clone)]
pub struct Lexer {
    command_depth: usize,
    list_depth: usize,
    inside_string: bool,
}

fn parse_token(input: String) -> Token {
    if input.parse::<f64>().is_ok() {
        Token::Number(input)
    } else if KEYWORDS.contains(&input.as_str()) {
        Token::Keyword(input)
    } else {
        Token::Var(input)
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
    pub fn tokenize<'a>(mut self, code: &'a str) -> Result<Vec<Token>, LexerError<'a>> {
        let mut tokens = Vec::new();
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
                            tokens.push(Token::Text(buf.clone()));
                            tokens.push(Token::Symbol(Symbol::Quote));
                        } else {
                            tokens.push(Token::Symbol(Symbol::Quote));
                        }

                        self.inside_string = !self.inside_string;
                        buf.clear();
                    }
                    Symbol::OpenParen if !self.inside_string => {
                        if buf.is_empty() {
                            return Err(LexerError::OpenParenNotPrecededByCommand(err_context));
                        }

                        tokens.push(Token::Command(buf.clone()));
                        tokens.push(Token::Symbol(Symbol::OpenParen));
                        self.command_depth += 1;
                        buf.clear();
                    }
                    Symbol::ClosedParen if !self.inside_string => {
                        const INVALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Dot, Symbol::OpenBracket];

                        if !buf.is_empty() {
                            tokens.push(parse_token(buf.clone()));
                        }

                        if tokens.is_empty() || self.command_depth == 0 {
                            return Err(LexerError::UnmatchedClosingParen(err_context));
                        } else if let Some(Token::Symbol(symbol)) = tokens.last()
                            && INVALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::ClosedParenPrecededByInvalidToken(err_context));
                        } else {
                            tokens.push(Token::Symbol(Symbol::ClosedParen));
                            self.command_depth -= 1;
                            buf.clear();
                        }
                    }
                    Symbol::OpenBracket if !self.inside_string => {
                        tokens.push(Token::Symbol(Symbol::OpenBracket));
                        self.list_depth += 1;
                        buf.clear();
                    }
                    Symbol::ClosedBracket if !self.inside_string => {
                        const INVALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Dot, Symbol::OpenParen];

                        if tokens.is_empty() || self.list_depth == 0 {
                            return Err(LexerError::UnmatchedClosingBracket(err_context));
                        } else if let Some(Token::Symbol(symbol)) = tokens.last()
                            && INVALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::ClosedBracketPrecededByInvalidToken(
                                err_context,
                            ));
                        } else {
                            if !buf.is_empty() {
                                tokens.push(parse_token(buf.clone()));
                            }
                            tokens.push(Token::Symbol(Symbol::ClosedBracket));
                            self.list_depth -= 1;
                            buf.clear();
                        }
                    }
                    Symbol::Dot if !self.inside_string => {
                        const VALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Quote, Symbol::ClosedParen, Symbol::ClosedBracket];

                        let buf_might_be_number =
                            buf.chars().all(|ch| ch.is_numeric() || ch == '.');
                        let number_already_has_decimal =
                            buf_might_be_number && buf.contains(symbol.as_str());
                        let prev_symbol = if let Some(Token::Symbol(symbol)) = tokens.last() {
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
                                tokens.push(Token::Var(buf.clone()));
                            } else if tokens.is_empty() {
                                return Err(LexerError::DotPreceededByInvalidToken(err_context));
                            }
                            tokens.push(Token::Symbol(Symbol::Dot));
                            buf.clear();
                        }
                    }
                    Symbol::Comma if !self.inside_string => {
                        const VALID_PRECEDING_SYMBOLS: &[Symbol] = &[Symbol::Quote];

                        if !buf.is_empty() {
                            tokens.push(parse_token(buf.clone()));
                        } else if let Some(Token::Symbol(symbol)) = tokens.last()
                            && !VALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::CommaPrecededByInvalidToken(err_context));
                        }

                        tokens.push(Token::Symbol(Symbol::Comma));
                        buf.clear();
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
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, LexerError, Symbol, Token};

    #[test]
    fn tokenize_identifer_and_string() {
        let lexer = Lexer::new();

        let tokens = lexer.tokenize("a.store(\"hello\")").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Var("a".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("store".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::Quote),
            Token::Text("hello".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("add(12,34)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command("add".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("12".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number("34".to_string()),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("list.store([1, 2, 3, 4])").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Var("list".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("store".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::OpenBracket),
            Token::Number("1".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number("2".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number("3".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number("4".to_string()),
            Token::Symbol(Symbol::ClosedBracket),
            Token::Symbol(Symbol::ClosedParen),
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
        let tokens = lexer.tokenize(input).unwrap();

        let valid_tokens: &[Token] = &[
            Token::Var("i".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("store".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("0".to_string()),
            Token::Symbol(Symbol::ClosedParen),
            Token::Var("names".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("store".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::OpenBracket),
            Token::Symbol(Symbol::Quote),
            Token::Text("name 1".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::Comma),
            Token::Symbol(Symbol::Quote),
            Token::Text("name 2".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::ClosedBracket),
            Token::Symbol(Symbol::ClosedParen),
            Token::Var("names".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("index".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Var("i".to_string()),
            Token::Symbol(Symbol::ClosedParen),
            Token::Symbol(Symbol::Dot),
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
            Token::Var("i".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("add".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("1".to_string()),
            Token::Symbol(Symbol::ClosedParen),
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Command("nl".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
            Token::Symbol(Symbol::ClosedParen),
            Token::Var("names".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("index".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Var("i".to_string()),
            Token::Symbol(Symbol::ClosedParen),
            Token::Symbol(Symbol::Dot),
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
            Token::Command("nl".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
            Token::Symbol(Symbol::Dot),
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
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
        let tokens = lexer.tokenize(r#" "new\nline" "#).unwrap();
        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#" "tabbed\ttext" "#).unwrap();
        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#" "escaped\"quote" "#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#""\\""#);
        dbg!(&tokens);
        let tokens = tokens.unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#""""#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in &tokens {
            if let Token::Text(text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("start{}end", text_output);

        let valid_tokens: &[Token] = &[
            Token::Symbol(Symbol::Quote),
            Token::Text("".to_string()),
            Token::Symbol(Symbol::Quote),
        ];

        assert!(tokens == valid_tokens);

        assert!(text_output == r"");
    }

    #[test]
    fn tokenize_empty_string_with_spaces() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""  ""#).unwrap();

        let valid_tokens: &[Token] = &[
            Token::Symbol(Symbol::Quote),
            Token::Text("  ".to_string()),
            Token::Symbol(Symbol::Quote),
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
        let tokens = lexer.tokenize(r#"print("  ")"#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#"print(" h    e ")"#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#" "back\\slash" "#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
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
        let tokens = lexer.tokenize(r#" "unescaped\\"quote" "#);
        dbg!(&tokens);
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))));
    }

    #[test]
    fn tokenize_symbols_in_strings() {
        let lexer = Lexer::new();

        let tokens = lexer.tokenize("print(\"h(el.lo), Wor[ld]\")").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::Quote),
            Token::Text("h(el.lo), Wor[ld]".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("[1, 2, 3].length()").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Symbol(Symbol::OpenBracket),
            Token::Number(1.to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number(2.to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Number(3.to_string()),
            Token::Symbol(Symbol::ClosedBracket),
            Token::Symbol(Symbol::Dot),
            Token::Command(format!("length")),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("\"123\".length()").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Symbol(Symbol::Quote),
            Token::Text("123".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::Dot),
            Token::Command(format!("length")),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::ClosedParen),
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
            .unwrap();

        let valid_tokens: &[Token] = &[
            Token::Var("character".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Var("weapon".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Var("name".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Command("store".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::Quote),
            Token::Text("sword".to_string()),
            Token::Symbol(Symbol::Quote),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("print(character.weapon.name)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Var("character".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Var("weapon".to_string()),
            Token::Symbol(Symbol::Dot),
            Token::Var("name".to_string()),
            Token::Symbol(Symbol::ClosedParen),
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
        let tokens = lexer.tokenize("print(1.1)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command(format!("print")),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("1.1".to_string()),
            Token::Symbol(Symbol::ClosedParen),
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
        let tokens = lexer.tokenize("print(11.11)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command(format!("print")),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("11.11".to_string()),
            Token::Symbol(Symbol::ClosedParen),
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
        let tokens = lexer.tokenize("print(1.,)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command(format!("print")),
            Token::Symbol(Symbol::OpenParen),
            Token::Number("1.".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Symbol(Symbol::ClosedParen),
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

        let tokens = lexer.tokenize("print(\\n)").unwrap();

        let valid_tokens: &[Token] = &[
            Token::Command("print".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Var(r#"\n"#.to_string()),
            Token::Symbol(Symbol::ClosedParen),
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
