use crate::error::RuntimeError;

pub mod command;
pub mod value;

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
