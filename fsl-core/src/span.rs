use std::ops::{Range, RangeBounds};

use crate::{
    lexer::Token,
    parser::{Arg, Expression, ParsedList, ParsedMap},
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl RangeBounds<usize> for Span {
    fn start_bound(&self) -> std::ops::Bound<&usize> {
        std::ops::Bound::Included(&self.start)
    }

    fn end_bound(&self) -> std::ops::Bound<&usize> {
        std::ops::Bound::Excluded(&self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(val: Span) -> Self {
        Range {
            start: val.start,
            end: val.end,
        }
    }
}

impl Span {
    pub fn new<'c>(start: Token<'c>, end: Token<'c>) -> Self {
        Self {
            start: start.location,
            end: end.location + end.len(),
        }
    }

    fn line_start(&self, source: &str) -> usize {
        source[..self.start].rfind('\n').map(|i| i + 1).unwrap_or(0)
    }

    fn line_end(&self, source: &str) -> usize {
        source[self.start..]
            .find('\n')
            .map(|i| self.start + i)
            .unwrap_or(source.len())
    }

    pub fn line<'a>(&self, source: &'a str) -> &'a str {
        let start = self.line_start(source);
        let end = self.line_end(source);
        &source[start..end]
    }

    pub fn line_location(&self, source: &str) -> usize {
        let start = self.line_start(source);
        self.start - start
    }

    pub fn line_number(&self, source: &str) -> usize {
        let slice = &source[..self.start];
        
        slice.lines().count().max(1)
    }
}

impl<'c> From<&Expression<'c>> for Span {
    fn from(expression: &Expression<'c>) -> Self {
        Self {
            start: expression.name.location,
            end: expression.end.location + expression.end.len(),
        }
    }
}

impl<'c> From<&Arg<'c>> for Span {
    fn from(arg: &Arg<'c>) -> Self {
        Self {
            start: arg.token.location,
            end: arg.token.location + arg.token.len(),
        }
    }
}

impl<'c> From<&Token<'c>> for Span {
    fn from(token: &Token<'c>) -> Self {
        Self {
            start: token.location,
            end: token.location + token.len(),
        }
    }
}

impl<'c> From<&ParsedList<'c>> for Span {
    fn from(list: &ParsedList<'c>) -> Self {
        Self {
            start: list.start.location,
            end: list.end.location + list.end.len(),
        }
    }
}

impl<'c> From<&ParsedMap<'c>> for Span {
    fn from(map: &ParsedMap<'c>) -> Self {
        Self {
            start: map.start.location,
            end: map.end.location + map.end.len(),
        }
    }
}
