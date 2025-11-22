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
}
