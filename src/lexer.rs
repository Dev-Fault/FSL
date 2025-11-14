use std::sync::OnceLock;

const QUOTE: &str = "\"";
const OPEN_PAREN: &str = "(";
const CLOSED_PAREN: &str = ")";
const OPEN_BRACKET: &str = "[";
const CLOSED_BRACKET: &str = "]";
const DOT: &str = ".";
const COMMA: &str = ",";
const NEW_LINE: &str = r"\n";
const TAB: &str = r"\t";

const SYMBOLS: &[&str] = &[
    QUOTE,
    OPEN_PAREN,
    CLOSED_PAREN,
    OPEN_BRACKET,
    CLOSED_BRACKET,
    DOT,
    COMMA,
    NEW_LINE,
    TAB,
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
    StringSymbolOutsideOfString(ErrorContext<'a>),
    OpenParenNotPrecededByCommand(ErrorContext<'a>),
    DotNotPrecededByValue(ErrorContext<'a>),
    CommaNotPrecededByValue(ErrorContext<'a>),
    UnclosedOpenParenthesis(ErrorContext<'a>),
    UnclosedOpenBracket(ErrorContext<'a>),
    UnmatchedClosingParen(ErrorContext<'a>),
    UnmatchedClosingBracket(ErrorContext<'a>),
    UnclosedString(ErrorContext<'a>),
    TrailingTokenError(ErrorContext<'a>),
    ClosedParenPrecededByInvalidSymbol(ErrorContext<'a>),
    ClosedBracketPrecededByInvalidSymbol(ErrorContext<'a>),
    InvalidNumber(ErrorContext<'a>),
}

#[derive(Debug, PartialEq, Eq)]
enum Symbol {
    Quote,
    OpenParen,
    ClosedParen,
    OpenBracket,
    ClosedBracket,
    Dot,
    Comma,
    NewLine,
    Tab,
}

impl Symbol {
    fn as_str(&self) -> &str {
        match self {
            Symbol::Quote => QUOTE,
            Symbol::OpenParen => OPEN_PAREN,
            Symbol::ClosedParen => CLOSED_PAREN,
            Symbol::OpenBracket => OPEN_BRACKET,
            Symbol::ClosedBracket => CLOSED_BRACKET,
            Symbol::Dot => DOT,
            Symbol::Comma => COMMA,
            Symbol::NewLine => NEW_LINE,
            Symbol::Tab => TAB,
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
        NEW_LINE => Symbol::NewLine,
        TAB => Symbol::Tab,
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

#[derive(Debug)]
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

        for (i, ch) in code.chars().enumerate() {
            if !self.inside_string && ch.is_whitespace() {
                continue;
            }

            buf.push(ch);

            if let Some(symbol) = get_symbol(&buf) {
                let err_context = ErrorContext::new(code, i);
                let escaping_quote = self.inside_string && prev_ch == '\\' && ch == '\"';
                if !(escaping_quote) {
                    for _ in 0..symbol.as_str().len() {
                        buf.pop();
                    }
                }

                match symbol {
                    Symbol::Quote => {
                        if prev_ch == '\\' && self.inside_string {
                            buf.remove(buf.len() - 2);
                        } else {
                            if self.inside_string {
                                tokens.push(Token::Text(buf.clone()));
                                tokens.push(Token::Symbol(Symbol::Quote));
                            } else {
                                tokens.push(Token::Symbol(Symbol::Quote));
                            }

                            self.inside_string = !self.inside_string;
                            buf.clear();
                        }
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
                        const VALID_PRECEDING_SYMBOLS: &[Symbol] =
                            &[Symbol::Dot, Symbol::OpenBracket];

                        if !buf.is_empty() {
                            tokens.push(parse_token(buf.clone()));
                        }

                        if tokens.is_empty() || self.command_depth == 0 {
                            return Err(LexerError::UnmatchedClosingParen(err_context));
                        } else if let Some(Token::Symbol(symbol)) = tokens.last()
                            && VALID_PRECEDING_SYMBOLS.contains(symbol)
                        {
                            return Err(LexerError::ClosedParenPrecededByInvalidSymbol(
                                err_context,
                            ));
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
                            return Err(LexerError::ClosedBracketPrecededByInvalidSymbol(
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

                        let buf_is_numeric = buf.chars().all(|ch| ch.is_numeric() || ch == '.');
                        let buf_is_invalid_number = buf_is_numeric && buf.contains(symbol.as_str());
                        let prev_symbol = if let Some(Token::Symbol(symbol)) = tokens.last() {
                            Some(symbol)
                        } else {
                            None
                        };

                        if buf_is_invalid_number {
                            return Err(LexerError::InvalidNumber(err_context));
                        } else if buf_is_numeric && prev_ch.is_numeric() {
                            prev_ch = ch;
                            buf.push_str(Symbol::Dot.as_str());
                            continue;
                        } else if buf.is_empty()
                            && prev_symbol
                                .is_some_and(|symbol| !VALID_PRECEDING_SYMBOLS.contains(symbol))
                        {
                            return Err(LexerError::DotNotPrecededByValue(err_context));
                        } else {
                            if !buf.is_empty() {
                                tokens.push(Token::Var(buf.clone()));
                            } else if tokens.is_empty() {
                                return Err(LexerError::DotNotPrecededByValue(err_context));
                            }
                            tokens.push(Token::Symbol(Symbol::Dot));
                            buf.clear();
                        }
                    }
                    Symbol::Comma if !self.inside_string => {
                        if !buf.is_empty() {
                            tokens.push(parse_token(buf.clone()));
                        } else if !tokens
                            .last()
                            .is_some_and(|token| *token == Token::Symbol(Symbol::Quote))
                        {
                            return Err(LexerError::CommaNotPrecededByValue(err_context));
                        }

                        tokens.push(Token::Symbol(Symbol::Comma));
                        buf.clear();
                    }
                    Symbol::NewLine if self.inside_string => {
                        buf.push_str("\n");
                    }
                    Symbol::Tab if self.inside_string => {
                        buf.push_str("\t");
                    }
                    _ => {
                        if self.inside_string {
                            buf.push_str(symbol.as_str());
                        } else {
                            return Err(LexerError::StringSymbolOutsideOfString(err_context));
                        }
                    }
                };
            }

            prev_ch = ch;
        }

        println!("{:?}", tokens);
        println!("{}", buf);

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
            return Err(LexerError::TrailingTokenError(ErrorContext::new(
                code,
                code.len(),
            )));
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{KEYWORDS, Lexer, LexerError, Symbol, Token};

    // TODO HANDLE SOLITARY BACKSLASHES

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
    fn tokenize_solitary_slash() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""\""#);

        assert!(tokens.is_err_and(|e| matches!(e, LexerError::UnclosedString(_))))
    }

    #[test]
    fn tokenize_solitary_double_slash() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#""\\""#).unwrap();

        println!("");
        let mut text_output = String::new();
        for token in tokens {
            if let Token::Text(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == "\\");
    }

    #[test]
    fn tokenize_back_slash() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#" "back\slash" "#).unwrap();

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

        assert!(text_output == r"back\\slash");
    }

    #[test]
    fn tokenize_unescaped_quote() {
        let lexer = Lexer::new();
        let tokens = lexer.tokenize(r#" "unesca\ped\\"quote" "#).unwrap();
        let mut text_output = String::new();

        println!("");
        for token in tokens {
            if let Token::Text(ref text) = token {
                text_output = format!("{}", text);
            }
            println!("{:?}", token);
        }
        println!("{}", text_output);

        assert!(text_output == r#"unesca\ped\"quote"#);
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
    fn number_allows_dot() {
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
        assert!(tokens.is_err_and(|e| matches!(e, LexerError::DotNotPrecededByValue(_))));
    }

    #[test]
    fn err_on_string_symbol_outside_string() {
        let lexer = Lexer::new();

        let result = lexer.tokenize("print(\\n)");

        assert!(result.is_err());
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
        assert!(result.is_err_and(|e| matches!(e, LexerError::DotNotPrecededByValue(_))));

        let result = Lexer::new().tokenize(".print()");

        dbg!(&result);
        assert!(result.is_err_and(|e| matches!(e, LexerError::DotNotPrecededByValue(_))));

        let result = Lexer::new().tokenize("print([1,2].)");
        dbg!(&result);

        assert!(
            result.is_err_and(|e| matches!(e, LexerError::ClosedParenPrecededByInvalidSymbol(_)))
        );
    }
}
