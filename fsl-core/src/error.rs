use std::{fmt::Debug, sync::Arc};

use unicode_width::UnicodeWidthStr;

use crate::{
    data::InterpreterData,
    parser::ParseError,
    span::Span,
    types::{ValueType, command::ExpectedArgs},
};

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorContext<T> {
    pub kind: T,
    pub context: String,
}

impl ErrorContext<RuntimeError> {
    fn from_span(value: SpannedError, data: Arc<InterpreterData>) -> Self {
        let context = value.to_string(data);
        let kind = value.command_error;
        Self { kind, context }
    }
}

impl<T> ErrorContext<T> {
    pub fn new(kind: T, context: String) -> Self {
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

#[derive(Debug, Clone)]
pub struct SpannedError {
    pub command_error: RuntimeError,
    pub span: Span,
}

pub trait SpanToInterpreterError<T> {
    fn into_interpreter_error(self, data: Arc<InterpreterData>) -> Result<T, InterpreterError>;
}

impl<T> SpanToInterpreterError<T> for Result<T, SpannedError> {
    fn into_interpreter_error(self, data: Arc<InterpreterData>) -> Result<T, InterpreterError> {
        self.map_err(|e| e.into_interpreter_error(data))
    }
}

impl SpannedError {
    pub fn into_interpreter_error(self, data: Arc<InterpreterData>) -> InterpreterError {
        InterpreterError::Execution(ErrorContext::from_span(self, data))
    }
}

impl PartialEq for SpannedError {
    fn eq(&self, other: &Self) -> bool {
        self.command_error == other.command_error && self.span == other.span
    }
}

impl SpannedError {
    pub fn new(command_error: RuntimeError, span: Span) -> Self {
        Self {
            command_error,
            span,
        }
    }

    pub fn exited_program(&self) -> bool {
        self.command_error.exited_program()
    }

    pub fn to_string(&self, data: Arc<InterpreterData>) -> String {
        let source = &data.source_str();
        let line_header = format!("{}: ", self.span.line_number(source));

        let upto_line_position = &self.span.line(source)[..self.span.line_location(source)];

        let upto_line_position: String = upto_line_position.chars().map(normalize_tab).collect();

        let line: String = self.span.line(source).chars().map(normalize_tab).collect();

        let padding = upto_line_position.width() + line_header.width();
        format!(
            "{}\n{}{}\n{}^",
            self.command_error,
            line_header,
            line,
            " ".repeat(padding),
        )
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

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    // Command
    WrongArgType {
        command_label: String,
        arg_number: usize,
        fsl_type: ValueType,
        expected: &'static [ValueType],
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
    ValueDef,
    // Control Flow
    BreakOutsideLoop,
    ContinueOutsideLoop,
    SwitchMustHaveSingleFallbackCommand,
    InvalidCommandInSwitch,
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
    InvalidVarValue {
        invalid_value: ValueType,
    },
    // Type
    NotAVar {
        value: String,
    },
    NotAMap {
        key: String,
    },
    NotAccessible {
        value: String,
        key: String,
    },
    FailedParse {
        value: String,
        valid_types: Vec<ValueType>,
    },
    InvalidComparison {
        a: String,
        b: String,
    },
    InvalidConversion {
        from: String,
        to: Vec<ValueType>,
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
    InvalidIndex {
        being_indexed: ValueType,
        should_be: ValueType,
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
                    command_label,
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
                command_label,
            ),
            RuntimeError::WrongArgCount {
                command_label,
                expected,
                got,
            } => match expected {
                ExpectedArgs::None => &format!(
                    "command `{}` expected no arguments and got {}",
                    command_label, got,
                ),
                ExpectedArgs::Exactly(n) => &format!(
                    "command `{}` expected exactly {} arguments and got {}",
                    command_label, n, got,
                ),
                ExpectedArgs::AtLeast(n) => &format!(
                    "command `{}` expected at least {} arguments and got {}",
                    command_label, n, got,
                ),
                ExpectedArgs::AtMost(n) => &format!(
                    "command `{}` expected at most {} arguments and got {}",
                    command_label, n, got,
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
            RuntimeError::ValueDef => "defs need to be at top level or inside another def.",
            RuntimeError::InvalidCommandInSwitch => "switch can only contain case() and fallback()",
            RuntimeError::InvalidIndex {
                being_indexed,
                should_be,
            } => &format!(
                "invalid index for `{}` should be `{}`",
                being_indexed, should_be
            ),
            RuntimeError::NotAccessible { value, key } => {
                &format!("value `{value}` is not accessible with key `{key}`")
            }
        };

        write!(f, "{}", output)
    }
}

pub trait ToSpannedError {
    fn span(self, span: Span) -> SpannedError;
}

impl ToSpannedError for RuntimeError {
    fn span(self, span: Span) -> SpannedError {
        SpannedError {
            command_error: self,
            span,
        }
    }
}

pub trait SpanError<T> {
    fn span(self, span: Span) -> Result<T, SpannedError>;
}

impl<T, E: ToSpannedError> SpanError<T> for Result<T, E> {
    fn span(self, span: Span) -> Result<T, SpannedError> {
        self.map_err(|e| e.span(span))
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
