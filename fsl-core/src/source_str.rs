use std::{fmt::Display, ops::Deref};

use bytes::Bytes;

use crate::{data::InterpreterData, lexer::Token, span::Span};

#[derive(Debug, Clone)]
pub enum SourceStr {
    Static(&'static str),
    Borrowed(Bytes),
    Owned(String),
}

impl SourceStr {
    pub fn from_token<'c>(token: Token<'c>, source: Bytes) -> Self {
        Self::Borrowed(source.slice(Span::from(&token)))
    }

    pub fn from_span<'c>(span: Span, data: &InterpreterData) -> Self {
        Self::Borrowed(data.source.slice(span))
    }
}

impl Default for SourceStr {
    fn default() -> Self {
        Self::Static("")
    }
}

impl Deref for SourceStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            SourceStr::Borrowed(bytes) => {
                // SAFETY: source is always valid UTF-8 because it is created from a String
                unsafe { std::str::from_utf8_unchecked(bytes) }
            }
            SourceStr::Owned(owned) => owned,
            SourceStr::Static(str) => str,
        }
    }
}

impl PartialEq for SourceStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for SourceStr {}

impl std::hash::Hash for SourceStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state);
    }
}

impl Display for SourceStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl AsRef<str> for SourceStr {
    fn as_ref(&self) -> &str {
        self
    }
}

impl From<String> for SourceStr {
    fn from(value: String) -> Self {
        Self::Owned(value)
    }
}

impl From<&'static str> for SourceStr {
    fn from(value: &'static str) -> Self {
        Self::Static(value)
    }
}
