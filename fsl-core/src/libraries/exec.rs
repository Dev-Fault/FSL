use std::sync::Arc;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    potential_future, register_async,
    types::{
        command::{Command, CommandSignature, ExpectedArgs},
        value::Value,
    },
};

pub fn register_exec(interpreter: &mut FslInterpreter) {
    register_async!(interpreter, EXEC, EXEC_RULES, exec);
    register_async!(interpreter, SH, SH_RULES, sh);
}

pub const EXEC_RULES: &CommandSignature = &CommandSignature::AnyArgs;
pub const EXEC: &str = "exec";
pub async fn exec(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.take_args();
    let arg = args.pop_front().unwrap();
    let arg_span = arg.span;
    let program = potential_future!(arg.to_text(data.clone())?);
    let args = command.take_args();
    let mut strings = Vec::new();
    for arg in args {
        let string = potential_future!(arg.to_text(data.clone())?);
        strings.push(string);
    }
    let strings: Vec<&str> = strings.iter().map(|cs| &**cs).collect();
    let output = tokio::process::Command::new(&*program)
        .args(strings)
        .output()
        .await
        .map_err(|_| {
            RuntimeError::FailedToRun {
                process: program.to_string(),
            }
            .span(arg_span)
        })?;

    if !output.status.success() {
        let output = String::from_utf8_lossy(&output.stderr);
        return Err(RuntimeError::OutputFailure(output.trim().into()).span(arg_span));
    }

    let output = output.stdout;
    let text = match String::from_utf8(output) {
        Ok(s) => s,
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    };
    Ok(Value::from(text))
}

pub const SH_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::Exactly(1));
pub const SH: &str = "sh";
pub async fn sh(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.take_args();
    let arg = args.pop_front().unwrap();
    let script = potential_future!(arg.to_text(data.clone())?);
    let output = tokio::process::Command::new("sh")
        .arg("-c")
        .arg(&*script)
        .output()
        .await
        .map_err(|_| {
            RuntimeError::FailedToRun {
                process: "sh".into(),
            }
            .span(command.span)
        })?;

    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        // TODO this probably should throw a real error
        eprintln!("{err}");
    }

    let output = output.stdout;
    let text = match String::from_utf8(output) {
        Ok(s) => s,
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    };
    Ok(Value::from(text))
}
