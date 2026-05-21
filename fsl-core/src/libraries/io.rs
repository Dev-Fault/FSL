use std::sync::Arc;

use crate::{
    FslInterpreter,
    data::InterpreterData,
    error::CommandError,
    register_command,
    standard::NOT_NONE,
    types::{
        command::{ArgPos, ArgRule, Command, Handler},
        value::{FslValue, Value},
    },
};

use futures::FutureExt;

pub fn register_io(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, SAY, SAY_RULES, say);
    register_command!(interpreter, ASK, ASK_RULES, ask);
}
pub const SAY_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const SAY: &str = "say";
pub async fn say<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = value.as_text(data.clone()).await?;
        output.push_str(&text);
    }

    println!("{output}");

    Ok(Value::None)
}

pub const ASK_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const ASK: &str = "ask";
pub async fn ask<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = value.as_text(data.clone()).await?;
        output.push_str(&text);
    }

    println!("{output}");

    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::from(input.trim().to_string())),
        Err(e) => Err(CommandError::Custom(format!("{e}"))),
    }
}
