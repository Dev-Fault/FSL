use std::fmt::Display;

use crate::error::{InterpreterError, InterpreterErrorType};

pub mod exec;
pub mod io;
pub mod standard;

pub enum Library {
    Exec,
    Io,
    Std,
}

impl Library {
    pub fn from_str(str: &str) -> Result<Library, InterpreterError> {
        match str {
            "exec" => Ok(Library::Exec),
            "io" => Ok(Library::Io),
            "std" => Ok(Library::Std),
            _ => Err(InterpreterErrorType::Import(format!("cannot unknown library {str}")).into()),
        }
    }
}

impl Display for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Library::Exec => "exec",
            Library::Io => "io",
            Library::Std => "std",
        };

        write!(f, "{}", output)
    }
}
