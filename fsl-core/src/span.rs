use crate::{
    lexer::Token,
    parser::{Arg, Expression, List, Map},
};

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

impl<'c> From<Arg<'c>> for Span<'c> {
    fn from(value: Arg<'c>) -> Self {
        Self {
            start: value.token,
            end: value.token,
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
