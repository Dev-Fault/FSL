use crate::{
    error::RuntimeError,
    types::command::{ArgPos, ArgRule, CommandSignature, ExpectedArgs},
};

pub mod argument;
pub mod command;
pub mod list;
pub mod map;
pub mod value;

pub const ANY: &[ValueType] = &[
    ValueType::Int,
    ValueType::Float,
    ValueType::Bool,
    ValueType::Text,
    ValueType::List,
    ValueType::Map,
    ValueType::Var,
    ValueType::Command,
    ValueType::None,
];

pub const NOT_NONE: &[ValueType] = &[
    ValueType::Int,
    ValueType::Float,
    ValueType::Bool,
    ValueType::Text,
    ValueType::List,
    ValueType::Map,
    ValueType::Var,
    ValueType::Command,
];

pub const STORABLE: &[ValueType] = &[
    ValueType::Int,
    ValueType::Float,
    ValueType::Bool,
    ValueType::Text,
    ValueType::List,
    ValueType::Map,
    ValueType::Var,
    ValueType::Command,
];

pub const LITERAL: &[ValueType] = &[
    ValueType::Int,
    ValueType::Float,
    ValueType::Bool,
    ValueType::Text,
    ValueType::List,
    ValueType::Map,
];

pub const MAYBE_NUMBER: &[ValueType] = &[
    ValueType::Int,
    ValueType::Float,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const NUMBER: &[ValueType] = &[ValueType::Int, ValueType::Float, ValueType::Text];

pub const MAYBE_INT: &[ValueType] = &[
    ValueType::Int,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const MAYBE_KEY: &[ValueType] = &[
    ValueType::List,
    ValueType::Int,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const KEY: &[ValueType] = &[ValueType::List, ValueType::Int, ValueType::Text];

pub const MAYBE_LIST_KEY: &[ValueType] = &[
    ValueType::List,
    ValueType::Int,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const LIST_KEY: &[ValueType] = &[ValueType::List, ValueType::Int, ValueType::Text];

pub const MAYBE_MAP_KEY: &[ValueType] = &[
    ValueType::List,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const MAP_KEY: &[ValueType] = &[ValueType::List, ValueType::Text];

pub const MAYBE_INDEXABLE: &[ValueType] = &[
    ValueType::List,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const INDEXABLE: &[ValueType] = &[ValueType::List, ValueType::Text];

pub const MAYBE_LIST: &[ValueType] = &[ValueType::List, ValueType::Command, ValueType::Var];

pub const MAYBE_MAP: &[ValueType] = &[ValueType::Map, ValueType::Command, ValueType::Var];

pub const MAYBE_COLLECTION: &[ValueType] = &[
    ValueType::Map,
    ValueType::List,
    ValueType::Text,
    ValueType::Command,
    ValueType::Var,
];

pub const COLLECTION: &[ValueType] = &[ValueType::Map, ValueType::List, ValueType::Text];

pub const MAYBE_TEXT: &[ValueType] = &[ValueType::Command, ValueType::Var, ValueType::Text];

pub const MAYBE_BOOL: &[ValueType] = &[
    ValueType::Bool,
    ValueType::Command,
    ValueType::Var,
    ValueType::Text,
];

pub const NO_ARGS: &CommandSignature = &CommandSignature::Count(ExpectedArgs::None);

pub const MATH_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Literal(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::AnyFrom(1)),
]);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
    Int,
    Float,
    Bool,
    Text,
    List,
    Map,
    Var,
    Command,
    None,
}

impl std::fmt::Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Text => "text",
            ValueType::Bool => "bool",
            ValueType::List => "list",
            ValueType::Map => "map",
            ValueType::Var => "var",
            ValueType::Command => "command",
            ValueType::None => "none",
        }
        .to_string();
        write!(f, "{}", text)
    }
}

impl ValueType {
    pub fn as_str(&self) -> &str {
        match self {
            ValueType::Int => "int",
            ValueType::Float => "float",
            ValueType::Text => "text",
            ValueType::Bool => "bool",
            ValueType::List => "list",
            ValueType::Map => "map",
            ValueType::Var => "var",
            ValueType::Command => "command",
            ValueType::None => "none",
        }
    }

    pub fn gen_conversion_err_to_type(&self, to: ValueType) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: vec![to],
        }
    }

    pub fn gen_conversion_err_to_types(&self, to: &[ValueType]) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: to.to_vec(),
        }
    }
}
