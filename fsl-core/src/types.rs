use crate::{
    error::RuntimeError,
    types::command::{ArgPos, ArgRule},
};

pub mod command;
pub mod list;
pub mod map;
pub mod value;

pub const ANY: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
    FslType::None,
];

pub const NOT_NONE: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const STORABLE: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const LITERAL: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
];

pub const MAYBE_NUMBER: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const NUMBER: &[FslType] = &[FslType::Int, FslType::Float, FslType::Text];

pub const MAYBE_INT: &[FslType] = &[FslType::Int, FslType::Command, FslType::Var, FslType::Text];

pub const MAYBE_KEY: &[FslType] = &[
    FslType::List,
    FslType::Int,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const KEY: &[FslType] = &[FslType::List, FslType::Int, FslType::Text];

pub const MAYBE_LIST_KEY: &[FslType] = &[
    FslType::List,
    FslType::Int,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const LIST_KEY: &[FslType] = &[FslType::List, FslType::Int, FslType::Text];

pub const MAYBE_MAP_KEY: &[FslType] =
    &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const MAP_KEY: &[FslType] = &[FslType::List, FslType::Text];

pub const MAYBE_INDEXABLE: &[FslType] =
    &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const INDEXABLE: &[FslType] = &[FslType::List, FslType::Text];

pub const MAYBE_LIST: &[FslType] = &[FslType::List, FslType::Command, FslType::Var];

pub const MAYBE_MAP: &[FslType] = &[FslType::Map, FslType::Command, FslType::Var];

pub const MAYBE_COLLECTION: &[FslType] = &[
    FslType::Map,
    FslType::List,
    FslType::Text,
    FslType::Command,
    FslType::Var,
];

pub const COLLECTION: &[FslType] = &[FslType::Map, FslType::List, FslType::Text];

pub const MAYBE_TEXT: &[FslType] = &[FslType::Command, FslType::Var, FslType::Text];

pub const MAYBE_BOOL: &[FslType] = &[FslType::Bool, FslType::Command, FslType::Var, FslType::Text];

pub const NO_ARGS: &[ArgRule] = &[ArgRule::none()];

pub const MATH_RULES: &[ArgRule] = &[
    ArgRule {
        position: ArgPos::AnyFrom(0),
        valid_types: MAYBE_NUMBER,
    },
    ArgRule {
        position: ArgPos::Index(0),
        valid_types: MAYBE_NUMBER,
    },
    ArgRule {
        position: ArgPos::Index(1),
        valid_types: MAYBE_NUMBER,
    },
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FslType {
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

impl std::fmt::Display for FslType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let text = match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Map => "map",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
        .to_string();
        write!(f, "{}", text)
    }
}

impl FslType {
    pub fn as_str(&self) -> &str {
        match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Map => "map",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
    }

    pub fn gen_conversion_err_to_type(&self, to: FslType) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: vec![to],
        }
    }

    pub fn gen_conversion_err_to_types(&self, to: &[FslType]) -> RuntimeError {
        RuntimeError::InvalidConversion {
            from: self.to_string(),
            to: to.to_vec(),
        }
    }
}
