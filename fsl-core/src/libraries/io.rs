use std::sync::Arc;

use crate::{
    FslInterpreter, await_result,
    data::InterpreterData,
    error::{RuntimeError, SpannedError, ToSpannedError},
    register_async,
    types::{
        command::{Command, CommandSignature},
        value::Value,
    },
};

pub fn register_io(interpreter: &mut FslInterpreter) {
    register_async!(interpreter, SAY, SAY_RULES, say);
    register_async!(interpreter, ASK, ASK_RULES, ask);
}

pub const SAY_RULES: &CommandSignature = &CommandSignature::AnyArgs;
pub const SAY: &str = "say";
pub async fn say(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = await_result!(value.to_text(data.clone()))?;
        output.push_str(&text);
    }

    println!("{output}");

    Ok(Value::None)
}

pub const ASK_RULES: &CommandSignature = &CommandSignature::AnyArgs;
pub const ASK: &str = "ask";
pub async fn ask(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = await_result!(value.to_text(data.clone()))?;
        output.push_str(&text);
    }

    println!("{output}");

    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::from(input.trim().to_string())),
        Err(e) => Err(RuntimeError::Custom(format!("{e}")).span(command.span)),
    }
}
