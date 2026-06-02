use std::fmt::Display;

use crate::error::InterpreterError;

pub mod r#async;
pub mod exec;
pub mod io;
pub mod standard;

pub enum Library {
    Exec,
    Io,
    Std,
    Async,
}

impl Library {
    pub fn from_str(str: &str) -> Result<Library, InterpreterError> {
        match str {
            "exec" => Ok(Library::Exec),
            "io" => Ok(Library::Io),
            "std" => Ok(Library::Std),
            "async" => Ok(Library::Async),
            _ => Err(InterpreterError::Import(format!("cannot unknown library {str}"))),
        }
    }
}

impl Display for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            Library::Exec => "exec",
            Library::Io => "io",
            Library::Std => "std",
            Library::Async => "async",
        };

        write!(f, "{}", output)
    }
}
