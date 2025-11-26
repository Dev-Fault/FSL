use crate::types::value::ValueError;

pub mod command;
pub mod value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FslType {
    Int,
    Float,
    Bool,
    Text,
    List,
    Var,
    Command,
    None,
}

impl FslType {
    pub fn as_str(&self) -> &str {
        match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
    }

    pub fn gen_conversion_err_to_type(&self, to: FslType) -> ValueError {
        ValueError::InvalidConversion(format!(
            "cannot convert from type {} to type {}",
            self.as_str(),
            to.as_str(),
        ))
    }

    pub fn gen_conversion_err_to_types(&self, to: &[FslType]) -> ValueError {
        ValueError::InvalidConversion(format!(
            "cannot convert type {} to type {:?}",
            self.as_str(),
            to,
        ))
    }

    pub fn gen_parse_err(&self, to: FslType) -> ValueError {
        ValueError::FailedParse(format!(
            "failed to parse type {} to type {}",
            self.as_str(),
            to.as_str()
        ))
    }
}
