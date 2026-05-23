use std::fmt::Debug;

use unicode_width::UnicodeWidthStr;

use crate::{parser::ParseError, types::FslType};

pub use crate::parser::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorContext<T> {
    pub kind: T,
    pub context: String,
}

impl<T> ErrorContext<T> {
    pub fn new(kind: T, context: String) -> Self {
        Self { kind, context }
    }
}

impl<'c> From<ExecutionError<'c>> for ErrorContext<RuntimeError> {
    fn from(value: ExecutionError<'c>) -> Self {
        let context = value.to_string();
        let kind = value.command_error;
        Self { kind, context }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    Lex(String),
    Parse(String),
    Execution(ErrorContext<RuntimeError>),
    Runtime(RuntimeError),
    Import(String),
    UnmatchedCurlyBraces,
    Exit,
}

impl From<RuntimeError> for InterpreterError {
    fn from(value: RuntimeError) -> Self {
        InterpreterError::Runtime(value)
    }
}

impl<'c> From<ParseError<'c>> for InterpreterError {
    fn from(value: ParseError<'c>) -> Self {
        InterpreterError::Parse(value.to_string())
    }
}

impl<'c> From<ExecutionError<'c>> for InterpreterError {
    fn from(value: ExecutionError<'c>) -> Self {
        InterpreterError::Execution(ErrorContext::from(value))
    }
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output = match self {
            InterpreterError::Lex(output) => output,
            InterpreterError::Parse(output) => output,
            InterpreterError::Execution(exeuction_error) => &exeuction_error.context,
            InterpreterError::Runtime(command_error) => &command_error.to_string(),
            InterpreterError::UnmatchedCurlyBraces => "unmatched curly braces",
            InterpreterError::Import(output) => output,
            InterpreterError::Exit => "",
        };
        write!(f, "Error: {}", output)
    }
}
impl std::error::Error for InterpreterError {}

#[derive(Debug, Clone, PartialEq)]
pub struct ExecutionError<'c> {
    pub command_error: RuntimeError,
    pub span: Span<'c>,
}

impl<'c> ExecutionError<'c> {
    pub fn new(command_error: RuntimeError, span: Span<'c>) -> Self {
        Self {
            command_error,
            span,
        }
    }
    pub fn exited_program(&self) -> bool {
        self.command_error.exited_program()
    }
}

fn normalize_tab(c: char) -> String {
    const TAB_WIDTH: usize = 4;
    if c == '\t' {
        " ".repeat(TAB_WIDTH)
    } else {
        c.to_string()
    }
}

impl<'c> std::fmt::Display for ExecutionError<'c> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_header = format!("{}: ", self.span.start.line_number());

        let upto_line_position = &self.span.start.line()[..self.span.start.line_location()];

        let upto_line_position: String = upto_line_position.chars().map(normalize_tab).collect();

        let line: String = self.span.start.line().chars().map(normalize_tab).collect();

        let padding = upto_line_position.width() + line_header.width();
        write!(
            f,
            "{}\n{}{}\n{}^",
            self.command_error.to_string(),
            line_header,
            line,
            " ".repeat(padding),
        )
    }
}

impl<'c> std::error::Error for ExecutionError<'c> {}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpectedArgs {
    None,
    Exactly(usize),
    AtLeast(usize),
    AtMost(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    // Command
    WrongArgType {
        command_label: String,
        arg_number: usize,
        fsl_type: FslType,
        expected: &'static [FslType],
    },
    MissingArg {
        command_label: String,
        arg_number: usize,
    },
    WrongArgCount {
        command_label: String,
        expected: ExpectedArgs,
        got: usize,
    },
    InvalidArgument(String),
    ParametersOutOfOrder,
    NonExistantCommand {
        label: String,
    },
    // Control Flow
    BreakOutsideLoop,
    ContinueOutsideLoop,
    SwitchMustHaveSingleFallbackCommand,
    MultipleThenCommandsInIf,
    MultipleElseCommandsInIf,
    InvalidCommandInIf,
    IfMustContainThen,
    ElseIfMustBePairedWithElse,
    // Math
    DivisionByZero,
    NonFiniteValue,
    Overflow,
    // Var
    NonExistantVar {
        label: String,
    },
    AttemptToOverwriteConst {
        label: String,
    },
    AttemptToTakeConst {
        label: String,
    },
    InvalidVarValue {
        invalid_value: FslType,
    },
    // Type
    NotAVar {
        value: String,
    },
    NotAMap {
        key: String,
    },
    FailedParse {
        value: String,
        valid_types: Vec<FslType>,
    },
    InvalidComparison {
        a: String,
        b: String,
    },
    InvalidConversion {
        from: String,
        to: Vec<FslType>,
    },
    // Index
    IndexOutOfBounds,
    NegativeIndex,
    InvalidRange,
    NotIndexable,
    MissingIndex,
    MissingKey,
    NonExistantKey {
        key: String,
    },
    // Limits
    LoopLimitReached,
    OutputLimitExceeded,
    VarMemoryLimitReached,
    // Exec
    FailedToRun {
        process: String,
    },
    OutputFailure(String),
    // Custom
    Custom(String),
    // Not real errors, need to remove later
    ProgramExited,
    ConditionFalse,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output: &str = match self {
            RuntimeError::WrongArgType {
                command_label,
                arg_number,
                fsl_type,
                expected,
            } => {
                let expected: Vec<_> = expected.iter().map(|t| t.as_str()).collect();
                let expected = expected.join(", ");
                &format!(
                    "argument {} of command `{}` has invalid type `{}`\nExpected: {}",
                    arg_number + 1,
                    command_label.to_string(),
                    fsl_type.as_str(),
                    expected
                )
            }
            RuntimeError::MissingArg {
                command_label,
                arg_number,
            } => &format!(
                "missing argument {} in command `{}`",
                arg_number + 1,
                command_label.to_string(),
            ),
            RuntimeError::WrongArgCount {
                command_label,
                expected,
                got,
            } => match expected {
                ExpectedArgs::None => &format!(
                    "command `{}` expected no arguments and got {}",
                    command_label.to_string(),
                    got,
                ),
                ExpectedArgs::Exactly(n) => &format!(
                    "command `{}` expected exactly {} arguments and got {}",
                    command_label.to_string(),
                    n,
                    got,
                ),
                ExpectedArgs::AtLeast(n) => &format!(
                    "command `{}` expected at least {} arguments and got {}",
                    command_label.to_string(),
                    n,
                    got,
                ),
                ExpectedArgs::AtMost(n) => &format!(
                    "command `{}` expected at most {} arguments and got {}",
                    command_label.to_string(),
                    n,
                    got,
                ),
            },
            RuntimeError::ParametersOutOfOrder => {
                "parameters must come first in command definition"
            }
            RuntimeError::DivisionByZero => "cannot divide by zero",
            RuntimeError::LoopLimitReached => "maximum loop limit reached",
            RuntimeError::IndexOutOfBounds => "index out of bounds",
            RuntimeError::InvalidRange => "invalid range (min should be less than max)",
            RuntimeError::NonFiniteValue => "cannot use non finite value",
            RuntimeError::BreakOutsideLoop => "break can only be called inside loop",
            RuntimeError::ContinueOutsideLoop => "continue can only be called inside loop",
            RuntimeError::NonExistantCommand { label } => {
                &format!("command `{}` does not exist", label)
            }
            RuntimeError::ProgramExited => "",
            RuntimeError::Custom(error_text) => error_text,
            RuntimeError::SwitchMustHaveSingleFallbackCommand => {
                "switch statement must have single fallback"
            }
            RuntimeError::ConditionFalse => "",
            RuntimeError::MultipleThenCommandsInIf => "if can only contain one then",
            RuntimeError::MultipleElseCommandsInIf => "if can only contain one else",
            RuntimeError::InvalidCommandInIf => {
                "if must only contain then, else_if, and else commands"
            }
            RuntimeError::IfMustContainThen => "if must contain a then command",
            RuntimeError::ElseIfMustBePairedWithElse => "else_if must be paired with else",
            RuntimeError::InvalidArgument(error_text) => error_text,
            RuntimeError::OutputLimitExceeded => "memory limit for print exceeded",
            RuntimeError::Overflow => "calculation resulted in overflow",
            RuntimeError::InvalidComparison { a, b } => {
                &format!("cannot compare `{}` with `{}`", a, b)
            }
            RuntimeError::InvalidConversion { from, to } => {
                let to: Vec<_> = to.iter().map(|t| t.as_str()).collect();
                let to = to.join(", ");
                &format!("cannot convert value `{}` to type `{}`", from, to)
            }
            RuntimeError::FailedParse { value, valid_types } => {
                let valid_types: Vec<_> = valid_types.iter().map(|t| t.as_str()).collect();
                let valid_types = valid_types.join(", ");
                &format!("failed to parse `{}` as type `{}`", value, valid_types)
            }
            RuntimeError::NonExistantVar { label } => {
                &format!("cannot get value of non existant var `{}`", label)
            }
            RuntimeError::NotAVar { value } => &format!("`{}` is not a var", value),
            RuntimeError::NegativeIndex => "index cannot be a negative value",
            RuntimeError::InvalidVarValue { invalid_value } => {
                &format!("cannot store `{}` in var", invalid_value)
            }
            RuntimeError::VarMemoryLimitReached => "interpreter var memory limit reached",
            RuntimeError::AttemptToOverwriteConst { label } => {
                &format!("cannot overwrite const var `{}`", label)
            }
            RuntimeError::AttemptToTakeConst { label } => {
                &format!("cannot take const var `{}`", label)
            }
            RuntimeError::NotAMap { key } => {
                &format!("cannot use key `{}` on value that is not a map", key)
            }
            RuntimeError::NotIndexable => "used index on value that cannot be indexed",
            RuntimeError::NonExistantKey { key } => &format!("key `{}` not present in map", key),
            RuntimeError::MissingIndex => "indexing requires at least one value",
            RuntimeError::MissingKey => "key is required to access map value",
            RuntimeError::FailedToRun { process } => {
                &format!("process `{}` failed to run", process)
            }
            RuntimeError::OutputFailure(failure) => failure,
        };

        write!(f, "{}", output)
    }
}

pub trait ToExecutionError<'c> {
    fn to_exec(self, span: Span<'c>) -> ExecutionError<'c>;
}

impl<'c> ToExecutionError<'c> for RuntimeError {
    fn to_exec(self, span: Span<'c>) -> ExecutionError<'c> {
        ExecutionError {
            command_error: self,
            span,
        }
    }
}

impl RuntimeError {
    pub fn exited_program(&self) -> bool {
        match self {
            RuntimeError::ProgramExited => true,
            _ => false,
        }
    }
}

impl From<tokio::task::JoinError> for RuntimeError {
    fn from(e: tokio::task::JoinError) -> Self {
        RuntimeError::Custom(format!("join error: {}", e))
    }
}

impl std::error::Error for RuntimeError {}
