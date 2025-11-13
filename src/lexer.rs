use crate::types::Error;

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

const KEYWORDS: &[&str] = &["true", "false"];

#[derive(Debug)]
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
    for symbol in SYMBOLS {
        if value.ends_with(symbol) {
            return symbol_from_str(symbol);
        }
    }
    None
}

#[derive(Debug)]
enum Token {
    Symbol(Symbol),
    Command(String),
    Number(String),
    Text(String),
    Keyword(String),
    Var(String),
}

#[derive(Debug)]
struct Lexer {
    command_depth: usize,
    list_depth: usize,
    inside_string: bool,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            command_depth: 0,
            list_depth: 0,
            inside_string: false,
        }
    }

    pub fn tokenize(&mut self, code: &str) -> Result<Vec<Token>, Error> {
        let mut tokens = Vec::new();
        let mut buf = String::with_capacity(code.len());
        let mut prev_ch = '\0';

        for ch in code.chars() {
            if !self.inside_string && ch.is_whitespace() {
                continue;
            }
            buf.push(ch);

            if let Some(symbol) = get_symbol(&buf) {
                let buf_left = buf[0..buf.len() - 1].to_string();
                match symbol {
                    Symbol::Quote => {
                        if self.inside_string {
                            if prev_ch == '\\' {
                                buf.remove(buf.len() - 2);
                                continue;
                            }
                            tokens.push(Token::Text(buf_left));
                            tokens.push(Token::Symbol(Symbol::Quote));
                            buf.clear();
                        } else {
                            tokens.push(Token::Symbol(Symbol::Quote));
                            buf.clear();
                        }
                        self.inside_string = !self.inside_string;
                    }
                    Symbol::OpenParen if !self.inside_string => {
                        if buf_left.len() == 0 {
                            return Err("opening parenthesis not preceded by command".to_string());
                        }
                        tokens.push(Token::Command(buf_left));
                        buf.clear();
                        tokens.push(Token::Symbol(Symbol::OpenParen));
                        self.command_depth += 1;
                    }
                    Symbol::ClosedParen if !self.inside_string => {
                        if self.command_depth == 0 {
                            return Err("closing parenthesis outside of command".to_string());
                        }
                        if buf_left.len() > 0 {
                            if buf_left.parse::<f64>().is_ok() {
                                tokens.push(Token::Number(buf_left));
                            } else if KEYWORDS.contains(&buf_left.as_str()) {
                                tokens.push(Token::Keyword(buf_left));
                            } else {
                                tokens.push(Token::Var(buf_left));
                            }
                        }
                        buf.clear();
                        tokens.push(Token::Symbol(Symbol::ClosedParen));
                        self.command_depth -= 1;
                    }
                    Symbol::OpenBracket if !self.inside_string => {
                        tokens.push(Token::Symbol(Symbol::OpenBracket));
                        buf.clear();
                        self.list_depth += 1;
                    }
                    Symbol::ClosedBracket if !self.inside_string => {
                        if buf_left.len() > 0 {
                            if buf_left.parse::<f64>().is_ok() {
                                tokens.push(Token::Number(buf_left));
                            } else if KEYWORDS.contains(&buf_left.as_str()) {
                                tokens.push(Token::Keyword(buf_left));
                            } else {
                                tokens.push(Token::Var(buf_left));
                            }
                        }
                        buf.clear();
                        tokens.push(Token::Symbol(Symbol::ClosedBracket));
                        self.list_depth -= 1;
                    }
                    Symbol::Dot if !self.inside_string => {
                        if buf_left.len() != 0 {
                            tokens.push(Token::Var(buf_left));
                        }
                        buf.clear();
                        tokens.push(Token::Symbol(Symbol::Dot));
                    }
                    Symbol::Comma if !self.inside_string => {
                        if buf_left.len() > 0 {
                            if buf_left.parse::<f64>().is_ok() {
                                tokens.push(Token::Number(buf_left));
                            } else if KEYWORDS.contains(&buf_left.as_str()) {
                                tokens.push(Token::Keyword(buf_left));
                            } else {
                                tokens.push(Token::Var(buf_left));
                            }
                        }
                        buf.clear();
                        tokens.push(Token::Symbol(Symbol::Comma));
                    }
                    Symbol::NewLine if self.inside_string => {
                        for _ in 0..NEW_LINE.len() {
                            buf.pop();
                        }
                        buf.push_str("\n");
                    }
                    Symbol::Tab if self.inside_string => {
                        for _ in 0..TAB.len() {
                            buf.pop();
                        }
                        buf.push_str("\t");
                    }
                    _ => {
                        if self.inside_string {
                            break;
                        } else {
                            panic!("symbol matched outside of context")
                        }
                    }
                };
            }

            prev_ch = ch;
        }

        if buf.is_empty() {
            Ok(tokens)
        } else {
            Err(format!("unkown token {}", buf))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{KEYWORDS, Lexer, Token};

    #[test]
    fn key_word_contains() {
        let test = "true".to_string();
        assert!(KEYWORDS.contains(&test.as_str()));
    }

    #[test]
    fn tokenize_identifer_and_string() {
        let mut lexer = Lexer::new();

        let tokens = lexer.tokenize("a.store(\"hello\")").unwrap();

        let valid_tokens: &[Token] = &[Token::Var("a".to_string())];
        println!("");
        for token in tokens {
            println!("{:?}", token);
        }
    }

    #[test]
    fn tokenize_args() {
        let mut lexer = Lexer::new();

        let tokens = lexer.tokenize("add(12,34)").unwrap();

        let valid_tokens: &[Token] = &[Token::Var("a".to_string())];
        println!("");
        for token in tokens {
            println!("{:?}", token);
        }
    }

    #[test]
    fn tokenize_list() {
        let mut lexer = Lexer::new();

        let tokens = lexer.tokenize("list.store([1, 2, 3, 4])").unwrap();

        let valid_tokens: &[Token] = &[Token::Var("a".to_string())];
        println!("");
        for token in tokens {
            println!("{:?}", token);
        }
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

        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(input).unwrap();

        let valid_tokens: &[Token] = &[Token::Var("a".to_string())];
        println!("");
        for token in tokens {
            println!("{:?}", token);
        }
    }

    #[test]
    fn tokenize_new_line_symbol() {
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(r#" "new\nline" "#).unwrap();

        println!("");
        let mut text_output = String::new();
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
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(r#" "tabbed\ttext" "#).unwrap();

        println!("");
        let mut text_output = String::new();
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
        let mut lexer = Lexer::new();
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
}
