use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
    pin::Pin,
    sync::Arc,
};

use async_recursion::async_recursion;
use futures::FutureExt;
use tokio::sync::RwLock;

use crate::{
    data::{InterpreterData, UserDefinitions},
    error::{InterpreterError, RuntimeError, ToExecutionError},
    libraries::{
        Library,
        r#async::register_async,
        exec::register_exec,
        io::register_io,
        standard::{self, *},
    },
    parser::{Arg, ArgKind, Expression, Parser},
    source_str::SourceStr,
    span::Span,
    types::{
        FslType,
        command::{ArgRule, Argument, Command, CommandDef, Handler, UserDef},
        value::{FslValue, Value},
    },
};

pub mod data;
pub mod error;
mod lexer;
pub mod libraries;
mod parser;
pub mod source_str;
pub mod span;
pub mod types;
mod vars;
pub type CommandDefinitions = HashMap<&'static str, CommandDef>;

#[derive(Debug, Clone)]
pub struct FslInterpreter {
    pub command_definitions: Arc<RwLock<CommandDefinitions>>,
}

type ProcessResult<'c, T, E> = Pin<Box<dyn Future<Output = Result<T, E>> + Send + 'c>>;

#[macro_export]
macro_rules! register_commands {
    ($self:expr, [
        $( ($label:expr, $rules:expr, $executor:path) ),* $(,)?
    ]) => {
        $(
        $self
            .register(
                $label,
                $rules,
                Handler::new(|command, data| futures::FutureExt::boxed($executor(command, data))),
            )
            .await;
        )*
    };
}

/// # Examples
/// Registers command to the interpreter
/// Command can either be a path to fn with the correct signature
/// or an async closure
///
/// Greet command:
/// ```
/// # use fsl_core::*;
/// # use fsl_core::data::*;
/// # use fsl_core::types::*;
/// # use fsl_core::types::value::*;
/// # use fsl_core::types::command::*;
/// # use std::sync::Arc;
/// let rt = tokio::runtime::Runtime::new().unwrap();
/// rt.block_on(async {
///     let mut interpreter = FslInterpreter::new().await;
///     register_command!(interpreter, "greet", NO_ARGS, |cmd, data| {
///         async move {
///             data.output.lock().await.push_str("Hello!");
///             Ok(Value::None)
///         }
///     });
///     let result = interpreter.interpret(
///         "greet()".to_string(),
///         InterpreterData::default()
///     ).await.unwrap();
///     assert_eq!(result, "Hello!");
/// });
/// ```
#[macro_export]
macro_rules! register_command {
    ($self:expr, $label:expr, $rules:expr, $executor:path) => {
        $self
            .register(
                $label,
                $rules,
                Handler::new(|command, data| futures::FutureExt::boxed($executor(command, data))),
            )
            .await;
    };
    ($self:expr, $label:expr, $rules:expr, |$cmd:ident, $dat:ident| $body:block) => {
        $self
            .register(
                $label,
                $rules,
                Handler::new(move |$cmd, $dat| futures::FutureExt::boxed($body)),
            )
            .await;
    };
}

#[macro_export]
macro_rules! fsl {
    ($code:literal) => {{
        // 1. Stringify the Rust tokens into a raw string slice at compile time
        let code = $code;

        let mut interpreter = FslInterpreter::new().await;
        interpreter.register_all_libraries().await;
        interpreter
            .interpret(code.to_string(), InterpreterData::default())
            .await
    }};
}

impl FslInterpreter {
    pub async fn new() -> Self {
        let mut interpreter = Self {
            command_definitions: Arc::new(RwLock::new(CommandDefinitions::new())),
        };
        interpreter.register_library(Library::Std).await;
        interpreter
    }

    /// Register command to interpreter allowing it to execute it, overwrites commands with same name if they exist
    pub async fn register(
        &mut self,
        label: &'static str,
        rules: &'static [ArgRule],
        executor: Handler,
    ) {
        self.command_definitions
            .write()
            .await
            .insert(label, CommandDef::new(label, rules, executor));
    }

    pub async fn register_library(&mut self, library: Library) {
        match library {
            Library::Exec => register_exec(self).await,
            Library::Io => register_io(self).await,
            Library::Std => register_std(self).await,
            Library::Async => register_async(self).await,
        }
    }

    pub async fn register_all_libraries(&mut self) {
        register_std(self).await;
        register_exec(self).await;
        register_io(self).await;
        register_async(self).await;
    }

    /// Interprets plain fsl code
    pub async fn interpret(
        &self,
        input: String,
        data: InterpreterData,
    ) -> Result<String, InterpreterError> {
        let mut data = data;
        data.set_source(input);
        let data = Arc::new(data);
        Self::execute_expressions(
            data.clone(),
            self.command_definitions.clone(),
            data.source_str(),
        )
        .await
    }

    /// Recursively interprets code inside {}'s
    pub async fn interpret_embedded_code(
        &self,
        input: String,
        data: InterpreterData,
    ) -> Result<String, InterpreterError> {
        let mut data = data;
        data.set_source(input);

        let mut output = String::with_capacity(data.source_str().len());
        let mut code_stack: Vec<String> = Vec::new();

        let mut code_depth: i16 = 0;

        for c in data.source_str().chars() {
            if c == '{' {
                code_stack.push(String::new());
                code_depth += 1;
            } else if c == '}' {
                code_depth -= 1;
                if code_depth < 0 {
                    return Err(InterpreterError::UnmatchedCurlyBraces);
                } else {
                    match code_stack.pop() {
                        Some(code) => {
                            let mut inner_data = InterpreterData::from(&data);
                            inner_data.set_source(code);
                            let inner_data = Arc::new(inner_data);
                            let result = Self::execute_expressions(
                                inner_data.clone(),
                                self.command_definitions.clone(),
                                inner_data.source_str(),
                            )
                            .await;
                            data.inc_loops_by(&inner_data.total_loops)?;
                            match result {
                                Ok(result) => match code_stack.last_mut() {
                                    Some(code) => code.push_str(&result),
                                    None => {
                                        if let Some(max_output_len) = data.limits.max_output_len
                                            && output.len() + result.len() > max_output_len
                                        {
                                            return Err(RuntimeError::OutputLimitExceeded.into());
                                        }
                                        output.push_str(&result)
                                    }
                                },
                                Err(e) => return Err(e),
                            };
                        }
                        None => {}
                    }
                }
            } else if code_depth == 0 {
                if let Some(max_output_len) = data.limits.max_output_len
                    && output.len() + c.len_utf8() > max_output_len
                {
                    return Err(InterpreterError::Runtime(RuntimeError::OutputLimitExceeded));
                }
                output.push(c);
            } else {
                match code_stack.last_mut() {
                    Some(s) => s.push(c),
                    None => {}
                }
            }
        }

        if code_depth != 0 {
            return Err(InterpreterError::UnmatchedCurlyBraces);
        }

        Ok(output)
    }

    #[async_recursion]
    async fn forward_declare_defs<'c>(
        expression: &Expression<'c>,
        user_defs: &mut UserDefinitions,
        data: Arc<InterpreterData>,
    ) {
        if expression.name.as_str() == DEF {
            if let Some(label) = expression.args.get(0) {
                let label = SourceStr::Borrowed(data.source.slice(Span::from(&label.token)));
                let def = Arc::new(UserDef::declaration(label.clone()));
                for arg in &expression.args {
                    if let ArgKind::Expression(inner) = &arg.kind {
                        let mut user_defs = def.local_defs.lock().await;
                        Self::forward_declare_defs(inner, &mut user_defs, data.clone()).await;
                    }
                }
                user_defs.insert(label, def);
            }
        } else {
            for arg in &expression.args {
                if let ArgKind::Expression(inner) = &arg.kind {
                    Self::forward_declare_defs(inner, user_defs, data.clone()).await;
                }
            }
        }
    }

    #[async_recursion]
    async fn execute_def<'c>(
        data: Arc<InterpreterData>,
        defs: Arc<RwLock<CommandDefinitions>>,
        expression: &Expression<'c>,
    ) -> Result<(), InterpreterError> {
        if expression.name.as_str() == DEF {
            {
                let def_label = match expression.args.get(0) {
                    Some(label) => SourceStr::Borrowed(data.source.slice(Span::from(&label.token))),
                    None => {
                        return Err((RuntimeError::MissingArg {
                            command_label: expression.name.to_string(),
                            arg_number: 0,
                        })
                        .to_exec(Span::from(expression), data.clone())
                        .into());
                    }
                };
                let mut ctx = data.ctx.write().await;
                ctx.def_stack.push(def_label);
            }
        }
        for arg in &expression.args {
            if let ArgKind::Expression(inner) = &arg.kind {
                Self::execute_def(data.clone(), defs.clone(), &inner).await?;
            }
        }
        if expression.name.as_str() == DEF {
            // TODO attempt to optimize expression.clone()
            Self::interpret_command(data.clone(), defs, expression.clone()).await?;
        }
        Ok(())
    }

    async fn execute_expressions<'c>(
        data: Arc<InterpreterData>,
        defs: Arc<RwLock<CommandDefinitions>>,
        source: SourceStr,
    ) -> Result<String, InterpreterError> {
        let expressions = Parser::new(&source).filter_parse(&[DEF])?;

        for expression in expressions.all() {
            let mut user_commands = data.user_defs.lock().await;
            Self::forward_declare_defs(&expression, &mut user_commands, data.clone()).await;
        }

        for expression in expressions.all() {
            Self::execute_def(data.clone(), defs.clone(), &expression).await?;
            let mut ctx = data.ctx.write().await;
            ctx.def_stack.clear();
        }

        for expression in expressions.unfiltered {
            let result = Self::interpret_command(data.clone(), defs.clone(), expression).await;
            if let Err(e) = result {
                match e == InterpreterError::Exit {
                    true => break,
                    false => return Err(e),
                }
            }
        }
        let mut output = data.output.lock().await;
        let output = std::mem::take(&mut *output);
        Ok(output)
    }

    async fn interpret_command<'c>(
        data: Arc<InterpreterData>,
        defs: Arc<RwLock<CommandDefinitions>>,
        expression: Expression<'c>,
    ) -> Result<(), InterpreterError> {
        let result = Self::process_expression(data.clone(), defs, expression).await;
        match result {
            Ok(value) => match value {
                Value::Command(command) => match command.execute(data.clone()).await {
                    Ok(_) => Ok(()),
                    Err(e) if e.exited_program() => Err(InterpreterError::Exit),
                    Err(e) => Err(InterpreterError::Execution(e.into())),
                },
                _ => {
                    unreachable!("parse expression should always return a command")
                }
            },
            Err(e) => Err(e),
        }
    }

    async fn process_expression<'c>(
        data: Arc<InterpreterData>,
        defs: Arc<RwLock<CommandDefinitions>>,
        expression: Expression<'c>,
    ) -> Result<Value, InterpreterError> {
        let defs_read = defs.read().await;
        let def = defs_read.get(expression.name.as_str());

        if let Some(def) = def {
            let mut command = Command::from_def(def, Span::from(&expression));

            let mut args: VecDeque<_> = VecDeque::with_capacity(expression.args.len());

            for arg in expression.args {
                args.push_back(Self::process_arg(data.clone(), defs.clone(), arg).await?);
            }

            command.set_args(args);

            Ok(Value::from_command(command))
        } else {
            let user_def = data
                .find_user_def(&SourceStr::from_token(expression.name, data.source.clone()))
                .await;

            if let Some(def) = user_def {
                let mut command = Command::new(
                    SourceStr::from_span(Span::from(&expression), &data),
                    RUN_RULES,
                    Handler::new(|command, data| standard::run(command, data).boxed()),
                    Span::from(&expression),
                );

                let mut args = VecDeque::with_capacity(expression.args.len());
                args.push_back(Argument::new(
                    Value::Var(def.label.clone()),
                    Span::from(&expression),
                ));

                for arg in expression.args {
                    args.push_back(Self::process_arg(data.clone(), defs.clone(), arg).await?);
                }

                command.set_args(args);

                Ok(Value::Command(Box::new(command)))
            } else {
                return Err(RuntimeError::NonExistantCommand {
                    label: expression.name.to_string(),
                }
                .to_exec(Span::from(&expression), data.clone())
                .into());
            }
        }
    }

    fn process_arg<'c>(
        data: Arc<InterpreterData>,
        defs: Arc<RwLock<CommandDefinitions>>,
        arg: Arg<'c>,
    ) -> ProcessResult<'c, Argument, InterpreterError> {
        Box::pin(async move {
            let span = Span::from(&arg);
            match arg.kind {
                ArgKind::Number(number) => {
                    if number.contains('.') {
                        match number.parse::<f64>() {
                            Ok(value) => Ok(Argument::new(Value::Float(value), span)),
                            Err(_) => Err(RuntimeError::FailedParse {
                                value: number.to_string(),
                                valid_types: vec![FslType::Float],
                            }
                            .to_exec(Span::from(&arg), data.clone())
                            .into()),
                        }
                    } else {
                        if let Ok(value) = number.parse::<i64>() {
                            Ok(Argument::new(Value::Int(value), span))
                        } else {
                            if let Ok(value) = number.parse::<f64>() {
                                Ok(Argument::new(Value::Float(value), span))
                            } else {
                                Err(RuntimeError::FailedParse {
                                    value: number.to_string(),
                                    valid_types: vec![FslType::Int, FslType::Int],
                                }
                                .to_exec(span, data.clone())
                                .into())
                            }
                        }
                    }
                }
                ArgKind::String(cow) => match cow {
                    Cow::Borrowed(_) => Ok(Argument::new(
                        Value::Text(SourceStr::Borrowed(data.source.slice(span))),
                        span,
                    )),
                    Cow::Owned(owned) => {
                        Ok(Argument::new(Value::Text(SourceStr::Owned(owned)), span))
                    }
                },
                ArgKind::Keyword(keyword) => match keyword {
                    lexer::TRUE => Ok(Argument::new(Value::Bool(true), span)),
                    lexer::FALSE => Ok(Argument::new(Value::Bool(false), span)),
                    _ => unreachable!("parser should validate keywords"),
                },
                ArgKind::Identifier(_) => Ok(Argument::new(
                    Value::Var(SourceStr::from_span(span, &data)),
                    span,
                )),
                ArgKind::List(list_arg) => {
                    let span = Span::from(&list_arg);
                    let mut list: Vec<Value> = Vec::with_capacity(list_arg.data.len());
                    for arg in list_arg.data {
                        let parsed_arg = Self::process_arg(data.clone(), defs.clone(), arg).await?;
                        list.push(parsed_arg.value);
                    }
                    Ok(Argument::new(Value::from(list), span))
                }
                ArgKind::Map(map) => {
                    let span = Span::from(&map);
                    let mut value_map = HashMap::with_capacity(map.data.len());

                    for (key, value) in map.data {
                        value_map.insert(
                            SourceStr::Borrowed(data.source.slice(Span::from(&key))),
                            Self::process_arg(data.clone(), defs.clone(), value)
                                .await?
                                .value,
                        );
                    }

                    Ok(Argument::new(Value::from(value_map), span))
                }
                ArgKind::Expression(expression) => {
                    let span = Span::from(&expression);
                    let value = Self::process_expression(data, defs, expression).await?;
                    Ok(Argument::new(value, span))
                }
            }
        })
    }
}

#[cfg(test)]
mod interpreter {
    use std::borrow::Cow;

    use bytes::Bytes;

    use crate::FslInterpreter;
    use crate::data::{InterpreterData, InterpreterLimits};
    use crate::error::RuntimeError;
    use crate::libraries::standard::tests::{
        test_interpreter, test_interpreter_embedded, test_interpreter_err_type,
    };
    use crate::source_str::SourceStr;
    use crate::types::command::Command;
    use crate::types::list::List;
    use crate::types::map::Map;
    use crate::types::value::Value;

    #[macro_export]
    macro_rules! assert_runtime_err {
        ($err:ident, $runtime_error:pat_param) => {
            assert!(
                (matches!(
                    $err,
                    $crate::error::InterpreterError::Execution($crate::error::ErrorContext {
                        kind: $runtime_error,
                        ..
                    })
                )) || (matches!(
                    $err,
                    $crate::error::InterpreterError::Runtime($runtime_error)
                ))
            )
        };
    }
    #[macro_export]
    macro_rules! assert_fsl {
        ($code:literal, $expected:literal) => {{
            // 1. Stringify the Rust tokens into a raw string slice at compile time
            let code = $code;

            let mut interpreter = FslInterpreter::new().await;
            interpreter.register_all_libraries().await;
            let result = interpreter
                .interpret(code.to_string(), InterpreterData::default())
                .await;
            println!("=====EXPECTED=====");
            println!($expected);
            println!("");
            println!("=====GOT==========");
            match &result {
                Ok(result) => {
                    println!("{}", result);
                }
                Err(e) => {
                    println!("{}", e.to_string())
                }
            }
            println!("");
            assert!($expected == result.unwrap());
        }};
    }

    async fn test_interpreter_err(code: &str) {
        let result = FslInterpreter::new()
            .await
            .interpret(code.to_string(), InterpreterData::default())
            .await;
        dbg!(&result);
        assert!(result.is_err());
    }

    async fn test_interpreter_not_err(code: &str) {
        let result = FslInterpreter::new()
            .await
            .interpret(code.to_string(), InterpreterData::default())
            .await;
        dbg!(&result);
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn catch_output_overflow() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_output_limit(u16::MAX as usize));
        let code = r#"
            big.store("0000000")
            repeat(10000, print(concat(big, big)))
            "#
        .to_string();
        let result = interpreter.interpret(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::OutputLimitExceeded)
    }

    #[tokio::test]
    async fn catch_memory_limit_embedded() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_memory_limit(u16::MAX as usize));
        let code = r#"
            {big.store("123456789") repeat(1000, big.store(concat(big,big)))}
            {big.store("123456789") repeat(1000, big.store(concat(big,big)))}
        "#
        .to_string();
        let result = interpreter.interpret_embedded_code(code, data).await;
        dbg!(&result);
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::VarMemoryLimitReached)
    }

    #[tokio::test]
    async fn catch_memory_overflow() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_memory_limit(u16::MAX as usize));
        let code = r#"
            big.store("123456789")
            repeat(1000, big.store(concat(big, big)))
            "#
        .to_string();
        let result = interpreter.interpret(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::VarMemoryLimitReached)
    }

    #[tokio::test]
    async fn catch_output_limit_embedded() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_output_limit(12));
        let code = r#"
            {
                print("000111")
                {
                    print("0001111")
                }
            }
        "#
        .to_string();
        let result = interpreter.interpret_embedded_code(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::OutputLimitExceeded)
    }

    // Each block individually fits but combined exceeds limit
    #[tokio::test]
    async fn catch_output_limit_embedded_accumulated() {
        // block 1 outputs 8 chars, block 2 outputs 8 chars, limit is 12
        // neither block alone exceeds, but together they should
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_output_limit(12));
        let code = r#"{print("00001111")}{print("00001111")}"#.to_string();
        let result = interpreter.interpret_embedded_code(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::OutputLimitExceeded)
    }

    // Literal chars outside blocks count toward limit
    #[tokio::test]
    async fn catch_output_limit_embedded_literals() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_output_limit(12));
        let code = r#"aaaaaaaaaaaa{print("x")}"#.to_string(); // 12 literal chars then more
        let result = interpreter.interpret_embedded_code(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::OutputLimitExceeded)
    }

    #[tokio::test]
    async fn catch_loop_limit() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_loop_limit(499));
        let code = r#"
            repeat(500, print("x"))
        "#
        .to_string();
        let result = interpreter.interpret(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::LoopLimitReached)
    }

    #[tokio::test]
    async fn catch_loop_limit_embedded() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_loop_limit(1499));
        let code = r#"
            {repeat(500, print("x"))}
            {repeat(500, print("x"))}
            {repeat(500, print("x"))}
        "#
        .to_string(); // each block alone is under limit but total should exceed
        let result = interpreter.interpret_embedded_code(code, data).await;
        let err = result.unwrap_err();
        assert_runtime_err!(err, RuntimeError::LoopLimitReached)
    }

    // Inner blocks can't bypass loop limit by each running up to the limit
    #[tokio::test]
    async fn catch_loop_limit_embedded_ok() {
        let interpreter = FslInterpreter::new().await;
        let data = InterpreterData::default()
            .with_limits(InterpreterLimits::default().with_loop_limit(1500));
        let code = r#"
            {repeat(500, print("x"))}
            {repeat(500, print("x"))}
            {repeat(500, print("x"))}
        "#
        .to_string(); // each block alone is under limit but total should exceed
        let result = interpreter.interpret_embedded_code(code, data).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn handle_print_overflow() {
        test_interpreter(
            &"print(1000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000)"
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>(),
            "inf",
        )
        .await;
    }

    #[tokio::test]
    async fn handle_int_overflow() {
        test_interpreter_not_err(
            &"print(random_range(
            100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000,
            123400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000000000000000))"
                .chars()
                .filter(|c| !c.is_whitespace())
                .collect::<String>()
        )
        .await;
    }

    #[tokio::test]
    async fn hello_world() {
        test_interpreter("print(\"Hello, world!\")", "Hello, world!").await;
    }

    #[tokio::test]
    async fn single_line_comment() {
        test_interpreter(
            "
            # This will print \"Hello, world!\"
            print(\"Hello, world!\") # comments are completely ignored
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn multi_line_comment() {
        test_interpreter(
            "
            *
            This will print \"Hello, world!\"
            These comments are completely ignored by the interpreter
            they are only here for utility
            *
            print(\"Hello, world!\") * comments are completely ignored *
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_place_comment() {
        test_interpreter_err(
            "
            print(# comment right here \"Hello, world!\") 
            ",
        )
        .await;
    }

    #[tokio::test]
    async fn mixed_comments() {
        test_interpreter(
            "
            *
            This will print \"Hello, world!\"
            These comments are completely ignored by the interpreter
            # single lines inside multis should be ignored
            they are only here for utility
            *
            # multis inside single * lines should be ignored
            print(\"Hello, world!\") * comments are completely ignored *
            ",
            "Hello, world!",
        )
        .await;
    }

    #[tokio::test]
    async fn print_list() {
        test_interpreter(r#"print([1, 2, 3])"#, r#"[1, 2, 3]"#).await;
    }

    #[tokio::test]
    async fn print_var_list() {
        test_interpreter(
            r#"a.store(0) b.store(0) c.store(0) print([a, b, c])"#,
            r#"[0, 0, 0]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_matrix() {
        test_interpreter(
            r#"print([[1, 2, 3], [4, 5, 6], [7, 8, 9]])"#,
            r#"[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_empty_list() {
        test_interpreter(r#"print([])"#, r#"[]"#).await;
    }

    #[tokio::test]
    async fn print_list_with_one_item() {
        test_interpreter(r#"print([1])"#, r#"[1]"#).await;
    }

    #[tokio::test]
    async fn interpret_embedded() {
        test_interpreter_embedded(
            r#"{print("hello", "{print(",", "{print(" world")}")}")}"#,
            "hello, world",
        )
        .await;
    }

    #[tokio::test]
    async fn split_command() {
        test_interpreter(
            r#"print(split("some text to split by whitespace", " "))"#,
            r#"[some, text, to, split, by, whitespace]"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_command() {
        test_interpreter(
            r#"
            a.store("test")
            b.store([1, 2, 3, a])
            print(a.contains("p"), " ")
            print(a.contains("t"), " ")
            print(b.contains(a), " ")
            print(b.contains(1), " ")
            print(b.contains(5), " ")
            print(b.contains("test"))
            "#,
            r#"false true true true false true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_sub_array() {
        test_interpreter(
            r#"
            a.store([[1, 2], [3, 4], [5, 6]])
            print(a.contains([1, 3]))
            print(" ")
            print(a.contains([1, 2]))
            "#,
            r#"false true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn contains_deep_array() {
        test_interpreter(
            r#"
            a.store([[1, 2], [[1, 3], 4], [5, 6]])
            print(a.contains([1, 3]))
            "#,
            r#"false"#,
        )
        .await;
    }

    #[tokio::test]
    async fn clone_command() {
        test_interpreter(
            r#"
            a.store(0)
            b.store(1)
            list.store([a.clone(), b.clone()])
            print(list.index(0), " ", list.index(1))
            "#,
            r#"0 1"#,
        )
        .await;
    }

    #[tokio::test]
    async fn inc_command() {
        test_interpreter(
            r#"
                i.store(0)
                i.inc()
                print(i)
                i.inc()
                print(i)
            "#,
            r#"12"#,
        )
        .await;
    }

    #[tokio::test]
    async fn dec_command() {
        test_interpreter(
            r#"
                i.store(0)
                i.dec()
                print(i)
                i.dec()
                print(i)
            "#,
            r#"-1-2"#,
        )
        .await;
    }

    #[tokio::test]
    async fn var_insert() {
        test_interpreter(
            r#"
                i.store([1, 2])
                i.push(3)
                print(i.index(2))
            "#,
            r#"3"#,
        )
        .await;
    }

    #[tokio::test]
    async fn push_pop() {
        test_interpreter(
            r#"
                text.store("hello")
                text.push("o")
                print(text)
                print("\n")
                text.pop()
                print(text)
            "#,
            "helloo\nhello",
        )
        .await;
    }

    #[tokio::test]
    async fn matrix_manipulation() {
        test_interpreter(
            "
                matrix.store([[1, 2, 3], [\"#\", \"#\", \"#\"], [7, 8, 9]])
                matrix.replace([1, 1], \"X\")
                print(matrix.index(1).index(1))
            ",
            r#"X"#,
        )
        .await;
    }

    #[tokio::test]
    async fn print_iterated_list() {
        test_interpreter(
            r#"

            names.store(["John", "James", "Joseph", "Alexander"])
            i.store(0)
            repeat(names.length(), names.index(i).print(), print("\n"), i.store(i.add(1)))

            "#,
            "John\nJames\nJoseph\nAlexander\n",
        )
        .await;
    }

    #[tokio::test]
    async fn float_vars() {
        test_interpreter(
            r#"

            x.store(2)
            y.store(0.5)
            print(add(x, y))

            "#,
            "2.5",
        )
        .await;
    }

    #[tokio::test]
    async fn float_commands() {
        test_interpreter(
            r#"

            print(add(add(1, 1.5), add(1, 2)))

            "#,
            "5.5",
        )
        .await;
    }

    #[tokio::test]
    async fn invalid_command_arg() {
        test_interpreter_err(
            r#"

            print(add(print(2), print(2)))

            "#,
        )
        .await;
    }

    #[tokio::test]
    async fn nested_list_operations() {
        test_interpreter(
            r#"
        matrix.store([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
        row.store(matrix.index(1))
        print(row.index(2))
        "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn deep_matrix() {
        test_interpreter(
            r#"
        matrix.store([[1, [2, 3, 4], 5], [4, 5, 6], [7, 8, 9]])
        print(matrix.index(0).index(1).index(1))
        "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn conditional_logic_chain() {
        test_interpreter(
            r#"
            x.store(10)
            y.store(20)
            if(x.gt(y),
                then(print("x is greater")),
                else(
                    if(x.lt(y),
                        then(print("y is greater")),
                        else(print("they are equal"))
                    )
                )
            )
        "#,
            "y is greater",
        )
        .await;
    }

    #[tokio::test]
    async fn fibonacci_sequence() {
        test_interpreter(
            r#"
        a.store(0)
        b.store(1)
        i.store(0)
        repeat(10,
            print(a, " "),
            temp.store(a),
            a.store(b),
            b.store(add(temp, b))
        )
        "#,
            "0 1 1 2 3 5 8 13 21 34 ",
        )
        .await;
    }

    #[tokio::test]
    async fn bubble_sort() {
        test_interpreter(
            r#"
        numbers.store([5, 2, 8, 1, 9])
        n.store(numbers.length())
        i.store(0)
        repeat(n,
            j.store(0),
            repeat(sub(n, i, 1),
                if(gt(numbers.index(j), numbers.index(add(j, 1))),
                    then(numbers.swap(j, add(j, 1)))
                ),
                j.store(add(j, 1))
            ),
            i.store(add(i, 1))
        )
        k.store(0)
        repeat(numbers.length(),
            print(numbers.index(k), " "),
            k.store(add(k, 1))
        )
        "#,
            "1 2 5 8 9 ",
        )
        .await;
    }

    #[tokio::test]
    async fn int_equality() {
        test_interpreter(
            r#"

            print(eq(1, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn float_equality() {
        test_interpreter(
            r#"

            print(eq(1.2, 1.2))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1.0))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn int_float_equality() {
        test_interpreter(
            r#"

            print(eq(1.0, 1))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq(2.2, 1))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn string_equality() {
        test_interpreter(
            r#"

            print(eq("h", "h"))

            "#,
            "true",
        )
        .await;
        test_interpreter(
            r#"

            print(eq("e", "h"))

            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn text_search_and_count() {
        test_interpreter(
            r#"
        text.store("hello world hello universe hello")
        count.store(0)
        i.store(0)
        repeat(text.length(),
            if(
                and(
                    text.index(i).eq("h"),
                    if(i.add(1).lt(text.length()),
                        then(text.index(add(i, 1)).eq("e")),
                    )
                ),
                then(count.store(count.add(1)))
            ),
            i.store(i.add(1)),
        )
        print("Found 'he' ", count, " times")
        "#,
            "Found 'he' 3 times",
        )
        .await;
    }

    #[tokio::test]
    async fn list_filter_evens() {
        test_interpreter(
            r#"
        numbers.store([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
        evens.store([])
        i.store(0)
        repeat(numbers.length(),
            num.store(numbers.index(i)),
            if(eq(mod(num, 2), 0),
                then(evens.insert(evens.length(), num))
            ),
            i.store(add(i, 1))
        )
        j.store(0)
        repeat(evens.length(),
            print(evens.index(j), " "),
            j.store(add(j, 1))
        )
        "#,
            "2 4 6 8 10 ",
        )
        .await;
    }

    #[tokio::test]
    async fn factorial_calculator() {
        test_interpreter(
            r#"
        n.store(5)
        result.store(1)
        counter.store(n)
        while(gt(counter, 0),
            result.store(mul(result, counter)),
            counter.store(sub(counter, 1))
        )
        print(n, "! = ", result)
        "#,
            "5! = 120",
        )
        .await;
    }

    #[tokio::test]
    async fn chained_method_calls() {
        test_interpreter(
            r#"
            text.store("  hello world  ")
            result.store(
                text
                    .remove_whitespace()
                    .uppercase()
                    .concat("!!!")
            )
            print(result)
            "#,
            "HELLOWORLD!!!",
        )
        .await;
    }

    #[tokio::test]
    async fn list_contains_search() {
        test_interpreter(
            r#"
        fruits.store(["apple", "banana", "cherry", "date"])
        search.store("cherry")
        found.store(false)
        i.store(0)
        while(and(lt(i, fruits.length()), not(found)),
            if(eq(fruits.index(i), search),
                then(found.store(true)),
            )
                i.store(add(i, 1))
        )
        i.store(i.sub(1))
        if(found,
            then(print("Found ", search, " at index ", i)),
            else(print(search, " not found"))
        )
        "#,
            "Found cherry at index 2",
        )
        .await;
    }

    #[tokio::test]
    async fn sum_of_list() {
        test_interpreter(
            r#"
        numbers.store([10, 20, 30, 40, 50])
        sum.store(0)
        i.store(0)
        repeat(numbers.length(),
            sum.store(add(sum, numbers.index(i))),
            i.store(add(i, 1))
        )
        print("Sum: ", sum)
        "#,
            "Sum: 150",
        )
        .await;
    }

    #[tokio::test]
    async fn find_max_in_list() {
        test_interpreter(
            r#"
        numbers.store([3, 7, 2, 9, 1, 5])
        max.store(numbers.index(0))
        i.store(1)
        repeat(sub(numbers.length(), 1),
            if(numbers.index(i).gt(max),
                then(max.store(numbers.index(i)))
            ),
            i.store(add(i, 1))
        )
        print("Max: ", max)
        "#,
            "Max: 9",
        )
        .await;
    }

    #[tokio::test]
    async fn error_division_by_zero() {
        test_interpreter_err("print(div(10, 0))").await;
    }

    #[tokio::test]
    async fn error_index_out_of_bounds() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        print(list.index(10))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn error_swap_out_of_bounds() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        list.swap(0, 10)
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn bool_keyword_logic() {
        test_interpreter(
            r#"
        a.store(true)
        b.store(false)
        print(and(a, not(b)))
        "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_conditional_with_bools() {
        test_interpreter(
            r#"
        is_valid.store(true)
        is_ready.store(false)
        if(and(is_valid, is_ready),
            then(print("Both true")),
            else(
                if(or(is_valid, is_ready),
                    then(print("At least one true")),
                    else(print("Both false"))
                )
            )
        )
        "#,
            "At least one true",
        )
        .await;
    }

    #[tokio::test]
    async fn mixed_type_concatenation() {
        test_interpreter(
            r#"
        age.store(25)
        name.store("Alice")
        print(concat(name, " is ", age, " years old"))
        "#,
            "Alice is 25 years old",
        )
        .await;
    }

    #[tokio::test]
    async fn list_equality() {
        test_interpreter(
            r#"
        a.store([1, 2, 3])
        b.store([1, 2, 3])
        c.store([1, 2, 4])
        print(eq(a, b), " ", eq(a, c))
        "#,
            "true false",
        )
        .await;
    }

    #[tokio::test]
    async fn empty_list_operations() {
        test_interpreter(
            r#"
        empty.store([])
        print("Length: ", empty.length())
        empty.insert(0, 42)
        print(" After insert: ", empty.index(0))
        "#,
            "Length: 0 After insert: 42",
        )
        .await;
    }

    #[tokio::test]
    async fn string_indexing() {
        test_interpreter(
            r#"
        word.store("hello")
        i.store(0)
        repeat(word.length(),
            print(word.index(i)),
            i.store(add(i, 1))
        )
        "#,
            "hello",
        )
        .await;
    }

    #[tokio::test]
    async fn string_starts_ends_with() {
        test_interpreter(
            r#"
        text.store("hello world")
        print(
            starts_with(text, "hello"), 
            " ", 
            ends_with(text, "world"), 
            " ", 
            starts_with(text, "world")
        )
        "#,
            "true true false",
        )
        .await;
    }

    #[tokio::test]
    async fn capitalize_edge_cases() {
        test_interpreter(
            r#"
        print(
            capitalize("hello"), 
            " ", 
            capitalize("a"), 
            " ", 
            capitalize("")
        )
        "#,
            "Hello A ",
        )
        .await;
    }

    #[tokio::test]
    async fn case_conversion_chain() {
        test_interpreter(
            r#"
        text.store("HeLLo WoRLd")
        print(
            text.lowercase(), 
            " | ", 
            text.uppercase()
        )
        "#,
            "hello world | HELLO WORLD",
        )
        .await;
    }

    #[tokio::test]
    async fn modulus_operations() {
        test_interpreter(
            r#"
        print(mod(10, 3), " ", mod(15, 4), " ", mod(7, 7))
        "#,
            "1 3 0",
        )
        .await;
    }

    #[tokio::test]
    async fn negative_numbers() {
        test_interpreter(
            r#"
        x.store(-5)
        y.store(10)
        print(add(x, y), " ", sub(x, y), " ", mul(x, y))
        "#,
            "5 -15 -50",
        )
        .await;
    }

    #[tokio::test]
    async fn float_division_precision() {
        test_interpreter(
            r#"
        print(div(10, 4), " ", div(10.0, 4))
        "#,
            "2 2.5",
        )
        .await;
    }

    #[tokio::test]
    async fn chained_comparisons() {
        test_interpreter(
            r#"
        x.store(5)
        result.store(
            and(
                gt(x, 0),
                lt(x, 10)
            )
        )
        print(result)
        "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn list_remove_and_length() {
        test_interpreter(
            r#"
        items.store([1, 2, 3, 4, 5])
        items.remove(2)
        print(items.length(), " ", items.index(2))
        "#,
            "4 4",
        )
        .await;
    }

    #[tokio::test]
    async fn list_replace() {
        test_interpreter(
            r#"
        items.store(["a", "b", "c"])
        items.replace(1, "X")
        print(items.index(0), items.index(1), items.index(2))
        "#,
            "aXc",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_while_loops() {
        test_interpreter(
            r#"
        i.store(0)
        total.store(0)
        while(lt(i, 3),
            j.store(0),
            while(lt(j, 2),
                total.store(add(total, 1)),
                j.store(add(j, 1))
            ),
            i.store(add(i, 1))
        )
        print(total)
        "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn command_return_values() {
        test_interpreter(
            r#"
        x.store(5)
        result.store(if(gt(x, 3), then(add(x, 10)), else(mul(x, 2))))
        print(result)
        "#,
            "15",
        )
        .await;
    }

    #[tokio::test]
    async fn repeat_with_multiple_commands() {
        test_interpreter(
            r#"
        sum.store(0)
        product.store(1)
        i.store(1)
        repeat(4,
            sum.store(add(sum, i)),
            product.store(mul(product, i)),
            i.store(add(i, 1))
        )
        print("Sum: ", sum, " Product: ", product)
        "#,
            "Sum: 10 Product: 24",
        )
        .await;
    }

    #[tokio::test]
    async fn zero_repetitions() {
        test_interpreter(
            r#"
            counter.store(0)
            repeat(0,
                counter.store(add(counter, 1))
            )
            print(counter)
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn take_variable() {
        test_interpreter(
            r#"
        x.store(42)
        print(x, " ")
        freed.store(take(x))
        print(freed)
        "#,
            "42 42",
        )
        .await;
    }

    #[tokio::test]
    async fn error_take_nonexistent() {
        test_interpreter_err(
            r#"
        result.store(take(nonexistent))
        print(result)
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn complex_expression_chaining() {
        test_interpreter(
            r#"
        numbers.store([1, 2, 3, 4, 5])
        result.store(
            numbers
                .index(2)
                .add(10)
                .mul(2)
                .sub(6)
        )
        print(result)
        "#,
            "20",
        )
        .await;
    }

    #[tokio::test]
    async fn string_insert() {
        test_interpreter(
            r#"
        text.store("helo")
        text.insert(3, "l")
        print(text)
        "#,
            "hello",
        )
        .await;
    }

    #[tokio::test]
    async fn string_remove() {
        test_interpreter(
            r#"
        text.store("hello")
        text.store(text.remove(2))
        print(text)
        "#,
            "l",
        )
        .await;
    }

    #[tokio::test]
    async fn string_replace() {
        test_interpreter(
            r#"
        text.store("hello")
        text.replace(1, "a")
        print(text)
        "#,
            "hallo",
        )
        .await;
    }

    #[tokio::test]
    async fn error_modulus_by_zero() {
        test_interpreter_err("print(mod(10, 0))").await;
    }

    #[tokio::test]
    async fn error_negative_index() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        print(list.index(-1))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn error_insert_beyond_length() {
        test_interpreter_err(
            r#"
        list.store([1, 2, 3])
        list.store(list.insert(99, 10))
        "#,
        )
        .await;
    }

    #[tokio::test]
    async fn edit_matrix() {
        test_interpreter(
            r#"
        matrix.store([
            [1, 2, 3],
            [4, 5, 6],
            [7, 8, 9]
        ])
        matrix.replace([1, 1], 10)
        matrix.print()
        "#,
            "[[1, 2, 3], [4, 10, 6], [7, 8, 9]]",
        )
        .await;
    }

    #[tokio::test]
    async fn parameter_var_in_function_in_list() {
        test_interpreter(
            r#"
            C_MODS.store(0)
            P_MOD.store(0)
            P_DUR.store(1)
            attacker_mods.store([])
            attacker.store([attacker_mods])
            potion.store([1, 1])
            mods.store([])
            add_potion_modifier.def(potion,
			    mods.store(attacker.index(C_MODS).clone())
			    mods.push([potion.index(P_MOD).clone(), potion.index(P_DUR).clone()])
			    attacker.replace(C_MODS, mods)
            )
            add_potion_modifier([50, 3])
			print("potion: [1, 1]\n", concat("mods: ", mods, "\nattacker_mods: ", attacker.index(C_MODS)))
            "#,
            "potion: [1, 1]\nmods: [[50, 3]]\nattacker_mods: [[50, 3]]",
        )
        .await;
    }

    #[tokio::test]
    async fn index_global_in_local_scope() {
        test_interpreter(
            r#"
	            MIN.store(0)
	            MAX.store(1)
	            HEALTH_POTION_RANGES.store([
		            [-100, 25],
		            [-25, 50],
		            [10, 100]
	            ])

	            get_potion_mod.def(
	                repeat(2,
	                    if(true,
			                then(potion_modifier.store(
			                    HEALTH_POTION_RANGES.index(0).index(MIN)
			                ))
	                    )
	                )
			        potion_modifier.take()
	            )

	            get_potion_mod().print()
            "#,
            "-100",
        )
        .await;
    }

    #[tokio::test]
    async fn multiple_maps() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0
	            ])
	            player.set("name", "player")
	            player.set("health", 5)

	            opponent.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0
	            ])
	            opponent.set("name", "opponent")
	            opponent.set("health", 25)

	            characters.store([player, opponent])

	            print(player.get("health"), "\n")
	            print(opponent.get("health"))
            "#,
            "5\n25",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_map_access() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0,
	            	weapon: [
	            	    type: "sword",
	            	    damage: 100
	            	],
	            ])

	            player.get(["weapon", "type"]).print("\n")
	            player.set(["weapon", "type"], "knife")
	            player.get(["weapon", "type"]).print()
            "#,
            "sword\nknife",
        )
        .await;
    }

    #[tokio::test]
    async fn map_dot_notation() {
        test_interpreter(
            r#"
	            player.store([
	            	name: "blah",
	            	health: 100,
	            	dodge: 0,
	            	strength: 0,
	            	weapon: [
	            	    type: "sword",
	            	    damage: 100
	            	],
	            ])

	            player.weapon.type.get().print("\n")
	            player.weapon.type.set("knife")
	            player.weapon.type.get().print()
            "#,
            "sword\nknife",
        )
        .await;
    }

    #[tokio::test]
    async fn custom_command_calls_custom_command() {
        test_interpreter(
            r#"
            add_two.def(value,
                value.add(2).return()
            )
            add_three.def(value,
                value.add(3).return()
            )

            number.store(0)
            number.store(add_two(add_three(number)))
            number.print()
            "#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn const_map() {
        let err = test_interpreter_err_type(
            r#"
	            player.const([
	            	name: "blah",
	            	health: 100,
	            	weapon: [name: "sword"]
	            ])

	            player.name.set("saber")
            "#,
        )
        .await;

        assert_runtime_err!(err, RuntimeError::AttemptToOverwriteConst { .. })
    }

    #[tokio::test]
    async fn indexing_maps_in_lists() {
        test_interpreter(
            r#"
            action.store([
                id: "id1"
            ])
            action2.store([
                id: "id2"
            ])
            actions.store([[action.clone(), action2.clone()]])
            actions.index(0).index(0).id.get().print()
            "#,
            "id1",
        )
        .await;
    }

    #[tokio::test]
    async fn indexing_map_with_index_command() {
        test_interpreter_err(
            r#"
            map.store([
                tags: [["rust", "cool"]]
            ])
            map.index(0).print()
            "#,
        )
        .await;
    }

    #[tokio::test]
    async fn indexing_deeply_nested_maps_and_lists() {
        test_interpreter(
            r#"
            inner.store([
                tags: [["rust", "cool"]]
            ])
            outer.store([
                data: [[inner.clone()]]
            ])
            outer.data.get().index(0).index(0).tags.get().index(0).index(0).print()
            "#,
            "rust",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_map_set() {
        test_interpreter(
            r#"
            player.store([
                weapons: [
                    sword: [
                        name: "Diamond Sword"
                    ]
                ]
            ])
            player.weapons.sword.name.set("Gold Sword")
            player.weapons.sword.name.get().print()
            "#,
            "Gold Sword",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_order_def() {
        test_interpreter(
            r#"
            test()
            test.def(
                print("it just works")
            )
            "#,
            "it just works",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_order_def_nested() {
        test_interpreter(
            r#"
            if(true,
                then(
                    test(true)
                    test.def(bool,
                        print(bool)
                    )
                )
            )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn basic_recursion() {
        test_interpreter(
            r#"
            recurse.def(n,
            	n.inc()
            	if(not(n.eq(10))
            		then(
            			recurse(n)
            		)
            	)
            	return(n)
            )
            
            n.store(0)
            
            print(recurse(n))
            "#,
            "10",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_def() {
        test_interpreter(
            r#"
            outer.def(
            	n.store(1)
            	inner.def(x,
            		x.inc()
            		x.inc()
            		x.inc()
            	)
            	inner(n)
            	return(n)
            )
            
            print(outer())
            "#,
            "4",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_inner_def() {
        test_interpreter(
            r#"
            outer.def(
            	n.store(1)
            	inner.def(x,
            		x.inc()
            		x.inc()
            		x.inc()
            		inner_inner.def(y,
            		    y.inc()
            		    y.inc()
            		)
            		inner_inner(x)
            	)
            	inner(n)
            	return(n)
            )
            
            print(outer())
            "#,
            "6",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_inner_def_double() {
        // Makes sure def scope is properly cleared after handling nested defs
        test_interpreter(
            r#"
            outer.def(
            	n.store(1)
            	inner.def(x,
            		x.inc()
            		x.inc()
            		x.inc()
            		inner_inner.def(y,
            		    y.inc()
            		    y.inc()
            		)
            		inner_inner(x)
            	)
            	inner(n)
            	return(n)
            )
            
            print(outer())

            outer2.def(
            	n.store(1)
            	inner2.def(x,
            		x.inc()
            		x.inc()
            		x.inc()
            		inner_inner2.def(y,
            		    y.inc()
            		    y.inc()
            		)
            		inner_inner2(x)
            	)
            	inner2(n)
            	return(n)
            )
            
            print(outer2())
            "#,
            "66",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_def_recursion() {
        test_interpreter(
            r#"
            outer.def(
                n.store(0)
                inner.def(x,
                    x.inc()
                    if(not(x.eq(5))
                        then(
                            inner(x)
                        )
                    )
                    return(x)
                )
                return(inner(n))
            )
            print(outer())
            "#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn inner_inner_def_recursion() {
        test_interpreter(
            r#"
            outer.def(
                n.store(0)
                inner.def(x,
                    inner_inner.def(y,
                        y.inc()
                        if(not(y.eq(3))
                            then(
                                inner_inner(y)
                            )
                        )
                        return(y)
                    )
                    return(inner_inner(x))
                )
                return(inner(n))
            )
            print(outer())
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_order_defs() {
        test_interpreter(
            r#"
            outer.def(
                return(inner())
            )
            print(outer())
            inner.def(
                return(1)
            )
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn out_of_order_inner_defs() {
        test_interpreter(
            r#"
            outer.def(
                inner.def(x,
                    return(deep(x))
                )
                n.store(0)
                return(inner(n))
                deep.def(x,
                    x.inc()
                    return(x)
                )
            )
            print(outer())
        "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn conditional_def_not_overwritten() {
        // The def in the else branch should not overwrite the one in then
        // because condition is true, only then should execute
        test_interpreter(
            r#"
            if(true,
                then(
                    run.def(
                        return(1)
                    )
                )
                else(
                    run.def(
                        return(2)
                    )
                )
            )
            print(run())
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn conditional_def_false_branch() {
        test_interpreter(
            r#"
            if(false,
                then(
                    run.def(
                        return(1)
                    )
                )
                else(
                    run.def(
                        return(2)
                    )
                )
            )
            print(run())
            "#,
            "2",
        )
        .await;
    }

    #[tokio::test]
    async fn def_inside_loop_not_redefined_each_iteration() {
        // inner should always behave the same regardless of how many
        // times the loop runs and potentially redefines it
        test_interpreter(
            r#"
            outer.def(
                result.store(0)
                inner.def(x,
                    x.inc()
                    return(x)
                )
                repeat(3,
                    result.store(inner(result))
                )
                return(result)
            )
            print(outer())
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn def_redefined_in_loop_uses_latest() {
        // each iteration redefines run with a different increment
        // the last definition (inc by 3) should win after the loop
        test_interpreter(
            r#"
            i.store(0)
            while(i.lt(3),
                switch(i,
                    case(0,
                        run.def(x, x.inc())
                    )
                    case(1,
                        run.def(x, x.inc() x.inc())
                    )
                    case(2,
                        run.def(x, x.inc() x.inc() x.inc())
                    )
                    fallback()
                )
                i.inc()
            )
            n.store(0)
            print(run(n))
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn sibling_inner_defs_dont_bleed() {
        // a's inner helper increments once, b's increments twice
        // calling a then b should give 1 then 2, not 2 and 2
        test_interpreter(
            r#"
            a.def(
                helper.def(x,
                    x.inc()
                    return(x)
                )
                n.store(0)
                return(helper(n))
            )
            b.def(
                n.store(0)
                return(helper(n))

                # defined out of order but still works
                helper.def(x,
                    x.inc()
                    x.inc()
                    return(x)
                )
            )
            print(a())
            print(b())
            "#,
            "12",
        )
        .await;
    }

    #[tokio::test]
    async fn def_inside_switch_case() {
        test_interpreter(
            r#"
            os.const("linux")
            switch(os,
                case("linux",
                    run.def(
                        return("linux")
                    )
                )
                case("windows",
                    run.def(
                        return("windows")
                    )
                )
                fallback(
                    run.def(
                        return("unknown")
                    )
                )
            )
            print(run())
        "#,
            "linux",
        )
        .await;
    }

    #[tokio::test]
    async fn recursive_inner_doesnt_redefine_on_each_call() {
        // outer is called recursively 3 times
        // inner should behave consistently across all calls
        test_interpreter(
            r#"
            outer.def(n,
                inner.def(x,
                    x.inc()
                    return(x)
                )
                n.store(inner(n))
                if(not(n.eq(3))
                    then(
                        outer(n)
                    )
                )
                return(n)
            )
            n.store(0)
            print(outer(n))
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn def_after_return_not_registered() {
        let err = test_interpreter_err_type(
            r#"
            outer.def(
                return(1)
                unreachable_def.def(
                    return(99)
                )
            )
            unreachable_def()
            "#,
        )
        .await;
        assert_runtime_err!(err, RuntimeError::NonExistantCommand { .. })
    }

    #[tokio::test]
    async fn fsl_macro() {
        assert_fsl!(
            "
            i.store(1)
            if(i.eq(1)
                then(
                    print(true)
                )
            )
            ",
            "true"
        );
    }

    #[tokio::test]
    async fn sizes() {
        dbg!(size_of::<i64>());
        dbg!(size_of::<f64>());
        dbg!(size_of::<bool>());
        dbg!(size_of::<SourceStr>());
        dbg!(size_of::<&'static str>());
        dbg!(size_of::<Cow<'_, str>>());
        dbg!(size_of::<Bytes>());
        dbg!(size_of::<String>());
        dbg!(size_of::<List>());
        dbg!(size_of::<Map>());
        dbg!(size_of::<Box<Command>>());
        dbg!(size_of::<Value>());
    }
}
