use crate::types::Error;

const QUOTE: &str = "\"";
const OPEN_PAREN: &str = "(";
const CLOSED_PAREN: &str = ")";
const OPEN_BRACKET: &str = "[";
const CLOSED_BRACKET: &str = "]";
const DOT: &str = ".";
const COMMA: &str = ",";
const ESCAPED_QUOTE: &str = r#"\""#;
const ESCAPED_LINE: &str = r"\n";
const ESCAPED_TAB: &str = r"\t";

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
    EscapedQuote,
    EscapedLine,
    EscapedTab,
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
        ESCAPED_QUOTE => Symbol::EscapedQuote,
        ESCAPED_LINE => Symbol::EscapedLine,
        ESCAPED_TAB => Symbol::EscapedTab,
        _ => {
            return None;
        }
    })
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

        for ch in code.chars() {
            if !self.inside_string && ch.is_whitespace() {
                continue;
            }
            buf.push(ch);

            if let Some(symbol) = symbol_from_str(&format!("{}", ch)) {
                let buf_left = buf[0..buf.len() - 1].to_string();
                match symbol {
                    Symbol::Quote => {
                        if self.inside_string {
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
                    Symbol::EscapedQuote if self.inside_string => todo!(),
                    Symbol::EscapedLine if self.inside_string => todo!(),
                    Symbol::EscapedTab if self.inside_string => todo!(),
                    _ => {
                        if self.inside_string {
                            break;
                        } else {
                            panic!("symbol matched outside of context")
                        }
                    }
                };
            }
        }

        Ok(tokens)
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
}
