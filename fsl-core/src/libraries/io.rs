use std::sync::Arc;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    register_command,
    types::{
        NOT_NONE,
        argument::{ArgPos, ArgRule},
        command::{Command, Handler},
        value::Value,
    },
};

pub async fn register_io(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, SAY, SAY_RULES, say);
    register_command!(interpreter, ASK, ASK_RULES, ask);
}
pub const SAY_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const SAY: &str = "say";
pub async fn say(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = value.to_text(data.clone()).await?;
        output.push_str(&text);
    }

    println!("{output}");

    Ok(Value::None)
}

pub const ASK_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const ASK: &str = "ask";
pub async fn ask(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = value.to_text(data.clone()).await?;
        output.push_str(&text);
    }

    println!("{output}");

    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::from(input.trim().to_string())),
        Err(e) => Err(RuntimeError::Custom(format!("{e}")).span(command.span, data.clone())),
    }
}
