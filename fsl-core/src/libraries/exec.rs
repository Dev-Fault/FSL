use std::{borrow::Cow, sync::Arc};

use tokio_stream::StreamExt;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::{ExecutionError, RuntimeError, ToExecutionError},
    register_command,
    standard::MAYBE_TEXT,
    types::{
        command::{ArgPos, ArgRule, Command, Handler},
        value::{FslValue, Value},
    },
};

use futures::FutureExt;

pub async fn register_exec(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, EXEC, EXEC_RULES, exec);
    register_command!(interpreter, SH, SH_RULES, sh);
}

pub const EXEC_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), MAYBE_TEXT)];
pub const EXEC: &str = "exec";
pub async fn exec<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, ExecutionError<'c>> {
    let mut command = command;
    let mut args = command.take_args();
    let arg = args.pop_front().unwrap();
    let arg_loc = arg.span;
    let program = arg.as_text(data.clone()).await?;
    let args = tokio_stream::iter(args.into_iter());
    let args: Vec<Cow<'c, str>> = args
        .then(|v| v.as_text(data.clone()))
        .collect::<Result<Vec<Cow<'c, str>>, _>>()
        .await?;
    let args: Vec<&str> = args.iter().map(|cs| &**cs).collect();
    let output = tokio::process::Command::new(&*program)
        .args(args)
        .output()
        .await
        .map_err(|_| {
            RuntimeError::FailedToRun {
                process: program.to_string(),
            }
            .to_exec(arg_loc)
        })?;

    if !output.status.success() {
        let output = String::from_utf8_lossy(&output.stderr);
        return Err(RuntimeError::OutputFailure(output.trim().into()).to_exec(arg_loc));
    }

    let output = output.stdout;
    let text = match String::from_utf8(output) {
        Ok(s) => s,
        Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
    };
    Ok(Value::from(text))
}

pub const SH_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const SH: &str = "sh";
pub async fn sh<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, ExecutionError<'c>> {
    let mut command = command;
    let mut args = command.take_args();
    let script = args.pop_front().unwrap().as_text(data).await?;
    let output = tokio::process::Command::new("sh")
        .arg("-c")
        .arg(&*script)
        .output()
        .await
        .map_err(|_| {
            RuntimeError::FailedToRun {
                process: "sh".into(),
            }
            .to_exec(command.span)
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
