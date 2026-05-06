use std::sync::Arc;

use crate::{
    commands::NOT_NONE,
    data::InterpreterData,
    error::CommandError,
    types::{
        command::{ArgPos, ArgRule, Command},
        value::Value,
    },
};

pub const SAY_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const SAY: &str = "say";
pub async fn say(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
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
pub async fn ask(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    let mut output = String::new();
    for value in values {
        let text = value.as_text(data.clone()).await?;
        output.push_str(&text);
    }

    println!("{output}");

    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::Text(input.trim().to_string())),
        Err(e) => Err(CommandError::Custom(format!("{e}"))),
    }
}
