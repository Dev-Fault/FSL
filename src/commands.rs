use std::{
    collections::{HashMap, VecDeque},
    mem,
    sync::{Arc, atomic::Ordering},
    time::Duration,
};

use async_recursion::async_recursion;
use rand::seq::SliceRandom;

use crate::{
    InterpreterData,
    error::{CommandError, ExecutionError, ValueError},
    types::{
        FslType,
        command::{ArgPos, ArgRule, Command, UserCommand},
        value::Value,
    },
    vars::VarEntry,
};

pub const ALL_TYPES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
    FslType::None,
];

pub const NON_NONE_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const VAR_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const LITERAL_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
];

pub const NUMERIC_TYPES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const WHOLE_NUMBER_TYPES: &[FslType] =
    &[FslType::Int, FslType::Command, FslType::Var, FslType::Text];

pub const INDEX_TYPES: &[FslType] = &[
    FslType::List,
    FslType::Int,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const KEY_TYPES: &[FslType] = &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const ARRAY_TYPES: &[FslType] = &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const LIST_TYPES: &[FslType] = &[FslType::List, FslType::Command, FslType::Var];

pub const MAP_TYPES: &[FslType] = &[FslType::Map, FslType::Command, FslType::Var];

pub const TEXT_TYPES: &[FslType] = &[FslType::Command, FslType::Var, FslType::Text];

pub const LOGIC_TYPES: &[FslType] = &[FslType::Bool, FslType::Command, FslType::Var, FslType::Text];

pub const NO_ARGS: &[ArgRule] = &[ArgRule {
    position: ArgPos::None,
    valid_types: &[],
}];

pub const MATH_RULES: &[ArgRule] = &[
    ArgRule {
        position: ArgPos::AnyFrom(0),
        valid_types: NUMERIC_TYPES,
    },
    ArgRule {
        position: ArgPos::Index(0),
        valid_types: NUMERIC_TYPES,
    },
    ArgRule {
        position: ArgPos::Index(1),
        valid_types: NUMERIC_TYPES,
    },
];

async fn take_if_var(
    value: &mut Value,
    data: Arc<InterpreterData>,
) -> Result<Option<(String, VarEntry)>, ExecutionError> {
    if value.is_type(FslType::Var) {
        let tmp_value = mem::take(value);
        let label = tmp_value.as_var_label(data.clone()).await?;
        let mut var_entry = data.vars.take_entry(&label)?;
        *value = mem::take(&mut var_entry.value);
        Ok(Some((label, var_entry)))
    } else {
        Ok(None)
    }
}

fn update_if_var(
    var: Option<(String, VarEntry)>,
    value: Value,
    data: Arc<InterpreterData>,
) -> Result<(), ValueError> {
    if let Some((label, mut var_entry)) = var {
        var_entry.value = value;
        data.vars.insert_entry(label, var_entry)?;
    }
    Ok(())
}

#[async_recursion]
async fn contains_float(
    values: &VecDeque<Value>,
    data: Arc<InterpreterData>,
) -> Result<bool, CommandError> {
    for value in values {
        match value {
            Value::Float(_) => return Ok(true),
            Value::Text(text) => {
                if text.contains('.') {
                    match text.parse::<f64>() {
                        Ok(_) => return Ok(true),
                        Err(_) => continue,
                    }
                }
            }
            Value::Var(var) => {
                if data.clone().vars.get_var_type(&var) == FslType::Float {
                    return Ok(true);
                }
            }
            Value::Command(command) => {
                return Ok(contains_float(command.get_args(), data).await?);
            }
            _ => {
                continue;
            }
        }
    }
    Ok(false)
}

pub const ADD: &str = "add";
pub async fn add(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let mut sum: f64 = 0.0;
        for value in values {
            sum = sum + value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum: i64 = 0;
        for value in values {
            sum = sum + value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(sum))
    }
}

pub const SUB: &str = "sub";
pub async fn sub(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let mut diff = values.pop_front().unwrap().as_float(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            diff = diff - value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(diff))
    } else {
        let mut diff = values.pop_front().unwrap().as_int(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            diff = diff - value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(diff))
    }
}

pub const MUL: &str = "mul";
pub async fn mul(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let mut product = values.pop_front().unwrap().as_float(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            product = product * value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(product))
    } else {
        let mut product = values.pop_front().unwrap().as_int(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            product = product * value.as_int(data.clone()).await?;
        }
        Ok(Value::Int(product))
    }
}

pub const DIV: &str = "div";
pub async fn div(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let mut quotient = values.pop_front().unwrap().as_float(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            let value = value.as_float(data.clone()).await?;
            if value == 0.0 {
                return Err(CommandError::DivisionByZero);
            };
            quotient = quotient / value;
        }
        Ok(Value::Float(quotient))
    } else {
        let mut quotient = values.pop_front().unwrap().as_int(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            let value = value.as_int(data.clone()).await?;
            if value == 0 {
                return Err(CommandError::DivisionByZero);
            };
            quotient = quotient / value;
        }
        Ok(Value::Int(quotient))
    }
}

pub const MODULUS: &str = "mod";
pub async fn modulus(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut remainder = values.pop_front().unwrap().as_int(data.clone()).await?;
    while let Some(value) = values.pop_front() {
        let value = value.as_int(data.clone()).await?;
        if value == 0 {
            return Err(CommandError::DivisionByZero);
        };
        remainder = remainder % value;
    }
    Ok(Value::Int(remainder))
}

pub const CLAMP_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub const CLAMP: &str = "clamp";
pub async fn clamp(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;
    let arg_1 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;
    let arg_2 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;

    if arg_0.is_type(FslType::Float) {
        let value = arg_0.as_float(data.clone()).await?;
        let min = arg_1.as_float(data.clone()).await?;
        let max = arg_2.as_float(data.clone()).await?;

        if min > max {
            return Err(CommandError::InvalidRange);
        }

        return Ok(Value::Float(value.clamp(min, max)));
    } else {
        let value = arg_0.as_int(data.clone()).await?;
        let min = arg_1.as_int(data.clone()).await?;
        let max = arg_2.as_int(data.clone()).await?;

        if min > max {
            return Err(CommandError::InvalidRange);
        }

        return Ok(Value::Int(value.clamp(min, max)));
    }
}
pub const CLAMP_MIN_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const CLAMP_MIN: &str = "clamp_min";
pub async fn clamp_min(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;
    let arg_1 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;

    if arg_0.is_type(FslType::Float) {
        let value = arg_0.as_float(data.clone()).await?;
        let min = arg_1.as_float(data.clone()).await?;

        if value < min {
            Ok(Value::Float(min))
        } else {
            Ok(Value::Float(value))
        }
    } else {
        let value = arg_0.as_int(data.clone()).await?;
        let min = arg_1.as_int(data.clone()).await?;

        if value < min {
            Ok(Value::Int(min))
        } else {
            Ok(Value::Int(value))
        }
    }
}

pub const CLAMP_MAX_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const CLAMP_MAX: &str = "clamp_max";
pub async fn clamp_max(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;
    let arg_1 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NUMERIC_TYPES)
        .await?;

    if arg_0.is_type(FslType::Float) {
        let value = arg_0.as_float(data.clone()).await?;
        let max = arg_1.as_float(data.clone()).await?;

        if value > max {
            Ok(Value::Float(max))
        } else {
            Ok(Value::Float(value))
        }
    } else {
        let value = arg_0.as_int(data.clone()).await?;
        let max = arg_1.as_int(data.clone()).await?;

        if value > max {
            Ok(Value::Int(max))
        } else {
            Ok(Value::Int(value))
        }
    }
}

pub const PRECISION_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Int]),
];
pub const PRECISION: &str = "precision";
pub async fn precision(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let num = arg_0.as_float(data.clone()).await?;
    let precision = arg_1.as_usize(data).await?;
    let formatted = format!("{:.prec$}", num, prec = precision);

    Ok(Value::Text(formatted))
}

pub const STORE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const STORE: &str = "store";
pub async fn store(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let var = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;
    let value_to_store = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), LITERAL_VALUES)
        .await?;
    let var_label = &var;

    data.vars
        .update_or_create_mut_var(var_label, value_to_store.clone())?;

    Ok(value_to_store)
}

pub const LOCAL_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const LOCAL: &str = "local";
pub async fn local(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let var = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;
    let value_to_store = values.pop_front().unwrap();
    let var_label = &var;

    data.vars.insert_mut_var(
        var_label,
        value_to_store.as_raw(data.clone(), ALL_TYPES).await?,
    )?;

    Ok(data.vars.get_var_value(var_label)?)
}

pub const UPDATE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const UPDATE: &str = "update";
pub async fn update(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let var = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;
    let value_to_store = values.pop_front().unwrap();
    let var_label = &var;

    data.vars.update_var(
        var_label,
        value_to_store.as_raw(data.clone(), ALL_TYPES).await?,
    )?;

    Ok(data.vars.get_var_value(var_label)?)
}

pub const CONST_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const CONST: &str = "const";
pub async fn r#const(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let var = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;
    let value_to_store = values.pop_front().unwrap();
    let var_label = &var;

    data.vars.insert_const_var(
        var_label,
        value_to_store.as_raw(data.clone(), ALL_TYPES).await?,
    )?;

    Ok(data.vars.get_var_value(var_label)?)
}

pub const CLONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ALL_TYPES)];
pub const CLONE: &str = "clone";
pub async fn clone(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let value = command.take_args().pop_front().unwrap();
    Ok(value.as_raw(data, ALL_TYPES).await?)
}

pub const FREE: &str = "free";
pub const FREE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn free(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let arg_0 = command.take_args().pop_front().unwrap();
    let label = arg_0.get_var_label()?;
    match data.vars.remove_var(label)? {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub const PRINT: &str = "print";
pub async fn print(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    if let Some(limit) = data.output_limit {
        for value in values {
            let text = value.as_text(data.clone()).await?;
            // Must be locked after as_text (could require evaluating command that calls print)
            let mut output = data.output.lock().await;
            if text.len() + output.len() > limit {
                return Err(CommandError::OutputLimitExceeded);
            }
            output.push_str(&text);
        }
    } else {
        for value in values {
            let text = value.as_text(data.clone()).await?;
            let mut output = data.output.lock().await;
            output.push_str(&text);
        }
    }
    Ok(Value::None)
}

pub const DEBUG_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub const DEBUG: &str = "debug";
pub async fn debug(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();
    let mut output = String::new();

    for value in values {
        output.push_str(&value.as_text(data.clone()).await?);
    }

    println!("{}", output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &'static [ArgRule] =
    &[ArgRule::new(ArgPos::AnyFrom(0), &[FslType::Command])];
pub const SCOPE: &str = "";
pub async fn scope(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();
    data.vars.push();
    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_command()?.execute(data.clone()).await?;
    }
    data.vars.pop();

    Ok(return_value)
}

pub const EQ_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const EQ: &str = "eq";
pub async fn eq(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let a = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ALL_TYPES)
        .await?;
    let b = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ALL_TYPES)
        .await?;

    Ok(Value::Bool(a.eq(&b)?))
}

pub const GT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const GT: &str = "gt";
pub async fn gt(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let a = values.pop_front().unwrap().as_float(data.clone()).await?;
        let b = values.pop_front().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a > b))
    } else {
        let a = values.pop_front().unwrap().as_int(data.clone()).await?;
        let b = values.pop_front().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a > b))
    }
}

pub const GTOE_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const GTOE: &str = "gtoe";
pub async fn gtoe(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let a = values.pop_front().unwrap().as_float(data.clone()).await?;
        let b = values.pop_front().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a >= b))
    } else {
        let a = values.pop_front().unwrap().as_int(data.clone()).await?;
        let b = values.pop_front().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a >= b))
    }
}

pub const LT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const LT: &str = "lt";
pub async fn lt(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let a = values.pop_front().unwrap().as_float(data.clone()).await?;
        let b = values.pop_front().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a < b))
    } else {
        let a = values.pop_front().unwrap().as_int(data.clone()).await?;
        let b = values.pop_front().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a < b))
    }
}

pub const LTOE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const LTOE: &str = "ltoe";
pub async fn ltoe(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    if contains_float(&values, data.clone()).await? {
        let a = values.pop_front().unwrap().as_float(data.clone()).await?;
        let b = values.pop_front().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a <= b))
    } else {
        let a = values.pop_front().unwrap().as_int(data.clone()).await?;
        let b = values.pop_front().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a <= b))
    }
}

pub const NOT_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), LOGIC_TYPES)];
pub const NOT: &str = "not";
pub async fn not(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let a = values.pop_front().unwrap().as_bool(data.clone()).await?;
    Ok((!a).into())
}

pub const AND_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), LOGIC_TYPES)];
pub const AND: &str = "and";
pub async fn and(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 && value.as_bool(data.clone()).await?;
    }
    Ok(arg_0.into())
}

pub const OR_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), LOGIC_TYPES)];
pub const OR: &str = "or";
pub async fn or(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 || value.as_bool(data.clone()).await?;
    }
    Ok(arg_0.into())
}

pub const IF_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const IF: &str = "if";
pub async fn r#if(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let condition = values.pop_front().unwrap();

    let mut then_command: Option<Value> = None;
    let mut else_if_commands: VecDeque<Value> = VecDeque::new();
    let mut else_command: Option<Value> = None;

    let mut requires_else = false;

    for command in values {
        let label = command.get_command_label().unwrap();
        match label {
            THEN => {
                if then_command.is_some() {
                    return Err(CommandError::MultipleThenCommandsInIf);
                }
                then_command = Some(command)
            }
            ELSE_IF => {
                requires_else = true;
                else_if_commands.push_back(command);
            }
            ELSE => {
                if else_command.is_some() {
                    return Err(CommandError::MultipleThenCommandsInIf);
                }
                else_command = Some(command)
            }
            _ => return Err(CommandError::InvalidCommandInIf),
        }
    }

    let then_command = then_command.ok_or(CommandError::IfMustContainThen)?;

    if requires_else && else_command.is_none() {
        return Err(CommandError::ElseIfMustBePairedWithElse);
    }

    if condition.as_bool(data.clone()).await? == true {
        let result = then_command.as_command()?.execute(data.clone()).await;
        return result;
    } else {
        for else_if_command in else_if_commands {
            let result = else_if_command.as_command()?.execute(data.clone()).await;
            if let Ok(result) = result {
                return Ok(result);
            } else if let Err(CommandError::ConditionFalse) = result {
                continue;
            } else {
                return result;
            }
        }

        if else_command.is_some() {
            let else_command = else_command.unwrap();
            return else_command.as_command()?.execute(data.clone()).await;
        }

        return Ok(Value::None);
    }
}

pub const THEN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), ALL_TYPES)];
pub const THEN: &str = "then";
pub async fn then(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_raw(data.clone(), ALL_TYPES).await?;
    }

    Ok(return_value)
}

pub const ELSE_IF_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), ALL_TYPES),
];
pub const ELSE_IF: &str = "else_if";
pub async fn else_if(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();

    if arg_0.as_bool(data.clone()).await? {
        let mut return_value = Value::None;
        for value in values {
            return_value = value.as_raw(data.clone(), ALL_TYPES).await?;
        }
        return Ok(return_value);
    } else {
        Err(CommandError::ConditionFalse)
    }
}

pub const ELSE_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), ALL_TYPES)];
pub const ELSE: &str = "else";
pub async fn r#else(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_raw(data.clone(), ALL_TYPES).await?;
    }

    Ok(return_value)
}

pub const SWITCH_RULES: &'static [ArgRule] =
    &[ArgRule::new(ArgPos::AnyFrom(0), &[FslType::Command])];
pub const SWITCH: &str = "switch";
pub async fn switch(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let commands = command.take_args();
    let (cases, mut fallback): (VecDeque<Value>, VecDeque<Value>) =
        commands.into_iter().partition(|statement| {
            statement
                .get_command_label()
                .is_some_and(|label| label == CASE)
        });

    if fallback.len() == 1
        && let Some(fallback) = fallback.pop_front()
    {
        for case in cases {
            let result = case.as_command()?.execute(data.clone()).await;
            if let Ok(result) = result {
                return Ok(result);
            }
            if let Err(CommandError::ConditionFalse) = result {
                continue;
            }

            return result;
        }

        let result = fallback.as_command()?.execute(data.clone()).await;

        return result;
    } else {
        return Err(CommandError::SwitchMustHaveSingleFallbackCommand);
    }
}

pub const CASE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), ALL_TYPES),
];
pub const CASE: &str = "case";
pub async fn case(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();

    if arg_0.as_bool(data.clone()).await? {
        let mut return_value = Value::None;
        for value in values {
            return_value = value.as_raw(data.clone(), ALL_TYPES).await?;
        }
        return Ok(return_value);
    } else {
        Err(CommandError::ConditionFalse)
    }
}

pub const FALLBACK_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), ALL_TYPES)];
pub const FALLBACK: &str = "fallback";
pub async fn fallback(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_raw(data.clone(), ALL_TYPES).await?;
    }

    Ok(return_value)
}

pub const WHILE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const WHILE_LOOP: &str = "while";
pub async fn while_command(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let while_condition = values.pop_front().unwrap();

    let mut final_value = Value::None;

    data.loop_depth.fetch_add(1, Ordering::Relaxed);

    'outer: while while_condition.clone().as_bool(data.clone()).await? {
        for command in &values {
            let command = command.clone().as_command()?;
            final_value = command.execute(data.clone()).await?;

            if data.flags.continue_flag.load(Ordering::Relaxed) {
                data.flags.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.flags.break_flag.load(Ordering::Relaxed)
                || data.flags.return_flag.load(Ordering::Relaxed)
            {
                data.flags.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops().await?;
    }

    data.loop_depth.fetch_sub(1, Ordering::Relaxed);

    Ok(final_value)
}

pub const REPEAT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const REPEAT: &str = "repeat";
pub async fn repeat(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let repetitions = values.pop_front().unwrap().as_int(data.clone()).await?;
    let mut final_value = Value::None;

    data.loop_depth.fetch_add(1, Ordering::Relaxed);

    'outer: for _ in 0..repetitions {
        for command in &values {
            let command = command.clone().as_command()?;
            final_value = command.execute(data.clone()).await?;
            if data.flags.continue_flag.load(Ordering::Relaxed) {
                data.flags.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.flags.break_flag.load(Ordering::Relaxed)
                || data.flags.return_flag.load(Ordering::Relaxed)
            {
                data.flags.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops().await?;
    }

    data.loop_depth.fetch_sub(1, Ordering::Relaxed);

    Ok(final_value)
}

/// Performs operation L or T depending on if array Value is List or Text
/// If manipulator returns None returns altered array that was passed in
/// If manipulator returns Some returns the specificed value in the manipulator closure
async fn manipulate_array<L, T>(
    array: Value,
    data: Arc<InterpreterData>,
    list_manipulator: L,
    text_manipulator: T,
) -> Result<Value, CommandError>
where
    L: AsyncFnOnce(&mut Vec<Value>) -> Result<Option<Value>, CommandError>,
    T: AsyncFnOnce(&mut String) -> Result<Option<Value>, CommandError>,
{
    let var_label = if let Ok(label) = array.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let raw = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    if raw.is_type(FslType::List) {
        let mut list = raw.as_list(data.clone()).await?;

        let return_value = list_manipulator(&mut list).await?;

        let list = Value::List(list);

        if let Some(label) = var_label {
            data.vars.update_var(&label, list.clone())?;
        }

        if let Some(return_value) = return_value {
            Ok(return_value)
        } else {
            Ok(list)
        }
    } else if raw.is_type(FslType::Text) {
        let mut text = raw.as_text(data.clone()).await?;

        let return_value = text_manipulator(&mut text).await?;

        let text = Value::Text(text);

        if let Some(label) = var_label {
            data.vars.update_var(&label, text.clone())?;
        }

        if let Some(return_value) = return_value {
            Ok(return_value)
        } else {
            Ok(text)
        }
    } else {
        unreachable!("command arg validation should have handled incorrect arg types")
    }
}

#[async_recursion]
async fn get_index(
    list: &Vec<Value>,
    indices: &[Value],
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    match indices {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [i] => {
            let i = i.clone().as_usize(data.clone()).await?;
            let return_value = list.get(i).cloned();
            Ok(return_value.unwrap_or(Value::None))
        }
        [i, rest @ ..] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get(i) {
                Some(Value::List(inner_list)) => get_index(inner_list, rest, data).await,
                Some(_) => Err(CommandError::ValueError(ValueError::NotAList(format!(
                    "cannot index into value that is not a list",
                )))),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list {:?}", i, list),
                ))),
            }
        }
    }
}

#[async_recursion]
async fn get_mut_index<'a>(
    list: &'a mut Vec<Value>,
    indices: &[Value],
    data: Arc<InterpreterData>,
) -> Result<Option<&'a mut Value>, CommandError> {
    match indices {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [i] => {
            let i = i.clone().as_usize(data.clone()).await?;
            Ok(list.get_mut(i))
        }
        [i, rest @ ..] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get_mut(i) {
                Some(Value::List(inner_list)) => get_mut_index(inner_list, rest, data).await,
                Some(_) => Err(CommandError::ValueError(ValueError::NotAList(format!(
                    "cannot index into value that is not a list",
                )))),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                ))),
            }
        }
    }
}

#[async_recursion]
async fn remove_index<'a>(
    list: &'a mut Vec<Value>,
    indices: &[Value],
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    match indices {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [i] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get(i) {
                Some(_) => Ok(list.remove(i)),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                ))),
            }
        }
        [i, rest @ ..] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get_mut(i) {
                Some(Value::List(inner_list)) => remove_index(inner_list, rest, data).await,
                Some(_) => Err(CommandError::ValueError(ValueError::NotAList(format!(
                    "cannot index into value that is not a list",
                )))),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                ))),
            }
        }
    }
}

#[async_recursion]
async fn insert_at_index(
    list: &mut Vec<Value>,
    indices: &[Value],
    value_to_insert: Value,
    data: Arc<InterpreterData>,
) -> Result<(), CommandError> {
    match indices {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [i] => {
            let i = i.clone().as_usize(data.clone()).await?;
            if i <= list.len() {
                list.insert(i, value_to_insert);
                Ok(())
            } else {
                Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                )))
            }
        }
        [i, rest @ ..] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get_mut(i) {
                Some(Value::List(inner_list)) => {
                    insert_at_index(inner_list, rest, value_to_insert, data).await
                }
                Some(_) => Err(CommandError::ValueError(ValueError::NotAList(format!(
                    "cannot index into value that is not a list",
                )))),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                ))),
            }
        }
    }
}

pub const INDEX_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), INDEX_TYPES),
];
pub const INDEX: &str = "index";
pub async fn index(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    let array = arg_0.as_raw(data.clone(), ARRAY_TYPES).await?;

    if array.is_type(FslType::List) {
        let list = array.as_list(data.clone()).await?;

        let accesor = arg_1
            .as_raw(data.clone(), &[FslType::List, FslType::Int])
            .await?;

        let indices = if accesor.is_type(FslType::List) {
            accesor.as_list(data.clone()).await?
        } else {
            vec![accesor]
        };

        get_index(&list, &indices, data).await
    } else {
        let text = array.as_text(data.clone()).await?;
        let i = arg_1.as_usize(data).await?;
        match text.chars().nth(i) {
            Some(char) => Ok(char.into()),
            None => Err(CommandError::IndexOutOfBounds),
        }
    }
}

#[async_recursion]
async fn get_nested(
    map: &HashMap<String, Value>,
    keys: &[Value],
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    match keys {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [key] => {
            let key = key.clone().as_text(data.clone()).await?;
            let return_value = map.get(&key).cloned();
            Ok(return_value.unwrap_or(Value::None))
        }
        [key, rest @ ..] => {
            let key = key.clone().as_text(data.clone()).await?;
            match map.get(&key) {
                Some(Value::Map(inner_map)) => get_nested(inner_map, rest, data).await,
                Some(_) => Err(CommandError::ValueError(ValueError::NotAMap(format!(
                    "Can't use key \"{}\" to access a value that is not a map",
                    key
                )))),
                None => Err(CommandError::ValueError(ValueError::NonExistantKey(
                    format!("non existant key \"{}\" in map", key),
                ))),
            }
        }
    }
}

pub const GET_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAP_TYPES),
    ArgRule::new(ArgPos::Index(1), KEY_TYPES),
];
pub const GET: &str = "get";
pub async fn get(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    let accesor = arg_1
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    let keys = if accesor.is_type(FslType::List) {
        accesor.as_list(data.clone()).await?
    } else {
        vec![accesor]
    };

    let map = arg_0.as_map(data.clone()).await?;

    get_nested(&map, &keys, data).await
}

#[async_recursion]
async fn set_nested(
    map: &mut HashMap<String, Value>,
    keys: &[Value],
    value: Value,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    match keys {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [key] => {
            let key = key.clone().as_text(data.clone()).await?;
            let return_value = map.insert(key, value);
            Ok(return_value.unwrap_or(Value::None))
        }
        [key, rest @ ..] => {
            let key = key.clone().as_text(data.clone()).await?;
            match map.get_mut(&key) {
                Some(Value::Map(inner_map)) => set_nested(inner_map, rest, value, data).await,
                Some(_) => Err(CommandError::ValueError(ValueError::NotAMap(format!(
                    "Can't use key \"{}\" to access a value that is not a map",
                    key
                )))),
                None => Err(CommandError::ValueError(ValueError::NonExistantKey(
                    format!("non existant key \"{}\" in map", key),
                ))),
            }
        }
    }
}

pub const SET_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAP_TYPES),
    ArgRule::new(ArgPos::Index(1), KEY_TYPES),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub const SET: &str = "set";
pub async fn set(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    let accesor = arg_1
        .as_raw(data.clone(), &[FslType::Text, FslType::List])
        .await?;
    let keys = if accesor.is_type(FslType::List) {
        accesor.as_list(data.clone()).await?
    } else {
        vec![accesor]
    };

    let replacement_value = arg_2.as_raw(data.clone(), NON_NONE_VALUES).await?;

    if arg_0.is_type(FslType::Var) {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut map = arg_0.as_map(data.clone()).await?;

        let return_value = set_nested(&mut map, &keys, replacement_value, data.clone()).await?;

        update_if_var(var, Value::Map(map), data)?;

        Ok(return_value)
    } else {
        let mut map = arg_0.as_map(data.clone()).await?;

        let return_value = set_nested(&mut map, &keys, replacement_value, data.clone()).await?;
        Ok(return_value)
    }
}

pub const LENGTH_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ARRAY_TYPES)];
pub const LENGTH: &str = "length";
pub async fn length(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let array = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ARRAY_TYPES)
        .await?;

    if array.is_type(FslType::List) {
        let list = array.as_list(data).await?;
        Ok((list.len() as i64).into())
    } else {
        let text = array.as_text(data).await?;
        Ok((text.len() as i64).into())
    }
}

pub const REMOVE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), INDEX_TYPES),
];
pub const REMOVE: &str = "remove";
pub async fn remove(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let accesor = arg_1
            .as_raw(data.clone(), &[FslType::List, FslType::Int])
            .await?;

        let indices = if accesor.is_type(FslType::List) {
            accesor.as_list(data.clone()).await?
        } else {
            vec![accesor]
        };

        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut list = arg_0.as_list(data.clone()).await?;

        let return_value = remove_index(&mut list, &indices, data.clone()).await;

        update_if_var(var, Value::List(list), data)?;

        return_value
    } else {
        let i = arg_1.as_usize(data.clone()).await?;
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut text = arg_0.as_text(data.clone()).await?;
        match text.chars().nth(i) {
            Some(_) => {
                let return_value = text.remove(i).to_string();

                update_if_var(var, Value::Text(text), data)?;

                Ok(Value::Text(return_value))
            }
            None => Err(CommandError::IndexOutOfBounds),
        }
    }
}

pub const SWAP_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), INDEX_TYPES),
    ArgRule::new(ArgPos::Index(2), INDEX_TYPES),
];
pub const SWAP: &str = "swap";
pub async fn swap(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap().as_int(data.clone()).await?;
    let arg_2 = values.pop_front().unwrap().as_int(data.clone()).await?;

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut list = arg_0.as_list(data.clone()).await?;

        let a_value = get_index(&list, &[Value::Int(arg_1)], data.clone()).await?;
        let b_value = get_index(&list, &[Value::Int(arg_2)], data.clone()).await?;

        let tmp = get_mut_index(&mut list, &[Value::Int(arg_1)], data.clone()).await?;
        match tmp {
            Some(tmp) => {
                *tmp = b_value;
            }
            None => return Err(CommandError::IndexOutOfBounds),
        }

        let tmp = get_mut_index(&mut list, &[Value::Int(arg_2)], data.clone()).await?;
        match tmp {
            Some(tmp) => {
                *tmp = a_value;
            }
            None => return Err(CommandError::IndexOutOfBounds),
        }

        update_if_var(var, Value::List(list), data)?;

        Ok(Value::None)
    } else {
        let a = arg_1 as usize;
        let b = arg_2 as usize;
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let text = arg_0.as_text(data.clone()).await?;

        let mut chars: Vec<char> = text.chars().collect();

        if a < chars.len() && b < chars.len() {
            chars.swap(a, b);
            let text = chars.iter().collect();
            update_if_var(var, Value::Text(text), data)?;
            Ok(Value::None)
        } else {
            Err(CommandError::IndexOutOfBounds)
        }
    }
}

pub const REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), INDEX_TYPES),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub const REPLACE: &str = "replace";
pub async fn replace(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let accesor = arg_1
            .as_raw(data.clone(), &[FslType::List, FslType::Int])
            .await?;

        let indices = if accesor.is_type(FslType::List) {
            accesor.as_list(data.clone()).await?
        } else {
            vec![accesor]
        };

        let new_value = arg_2.as_raw(data.clone(), NON_NONE_VALUES).await?;

        let var = take_if_var(&mut arg_0, data.clone()).await?;

        let mut list = arg_0.as_list(data.clone()).await?;

        let old_value = get_mut_index(&mut list, &indices, data.clone()).await?;

        let return_value = match old_value {
            Some(old_value) => {
                let return_value = old_value.clone();
                *old_value = new_value;
                return_value
            }
            None => return Err(CommandError::IndexOutOfBounds),
        };

        update_if_var(var, Value::List(list), data.clone())?;

        Ok(return_value)
    } else {
        let i = arg_1.as_usize(data.clone()).await?;
        let new_ch = arg_2.as_text(data.clone()).await?;

        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let text = arg_0.as_text(data.clone()).await?;
        let mut chars: Vec<char> = text.chars().collect();
        let old_ch;
        match chars.get_mut(i) {
            Some(ch) => {
                old_ch = ch.clone();
                *ch = new_ch.chars().nth(0).ok_or(CommandError::InvalidArgument(
                    "text replacement value must be a single character".to_string(),
                ))?;

                let text = chars.iter().collect();
                update_if_var(var, Value::Text(text), data)?;
                Ok(Value::Text(old_ch.to_string()))
            }
            None => Err(CommandError::IndexOutOfBounds),
        }
    }
}

pub const INSERT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), INDEX_TYPES),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub const INSERT: &str = "insert";
pub async fn insert(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let accesor = arg_1
            .as_raw(data.clone(), &[FslType::List, FslType::Int])
            .await?;

        let indices = if accesor.is_type(FslType::List) {
            accesor.as_list(data.clone()).await?
        } else {
            vec![accesor]
        };

        let value_to_insert = arg_2.as_raw(data.clone(), NON_NONE_VALUES).await?;

        let var = take_if_var(&mut arg_0, data.clone()).await?;

        let mut list = arg_0.as_list(data.clone()).await?;

        insert_at_index(&mut list, &indices, value_to_insert, data.clone()).await?;

        update_if_var(var, Value::List(list), data)?;

        Ok(Value::None)
    } else {
        let i = arg_1.as_usize(data.clone()).await?;
        let text_to_insert = arg_2.as_text(data.clone()).await?;

        let var = take_if_var(&mut arg_0, data.clone()).await?;

        let mut text = arg_0.as_text(data.clone()).await?;

        if i <= text.len() {
            text.insert_str(i, &text_to_insert);
        } else {
            return Err(CommandError::IndexOutOfBounds);
        }

        update_if_var(var, Value::Text(text), data)?;

        Ok(Value::None)
    }
}

pub const PUSH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const PUSH: &str = "push";
pub async fn push(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let mut arg_0 = values.pop_front().unwrap();
    let arg_1 = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NON_NONE_VALUES)
        .await?;

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut list = arg_0.as_list(data.clone()).await?;
        list.push(arg_1);
        update_if_var(var, Value::List(list), data)?;
        Ok(Value::None)
    } else {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut text = arg_0.as_text(data.clone()).await?;
        let text_to_push = arg_1.as_text(data.clone()).await?;
        text.push_str(&text_to_push);
        update_if_var(var, Value::Text(text), data)?;
        Ok(Value::None)
    }
}

pub const POP_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ARRAY_TYPES)];
pub const POP: &str = "pop";
pub async fn pop(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let mut arg_0 = values.pop_front().unwrap();

    if arg_0.as_literal_type(data.clone()) == FslType::List {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut list = arg_0.as_list(data.clone()).await?;
        let return_value = list.pop().unwrap_or(Value::None);
        update_if_var(var, Value::List(list), data)?;
        Ok(return_value)
    } else {
        let var = take_if_var(&mut arg_0, data.clone()).await?;
        let mut text = arg_0.as_text(data.clone()).await?;
        let return_value = text
            .pop()
            .map(|c| Value::Text(c.to_string()))
            .unwrap_or(Value::None);
        update_if_var(var, Value::Text(text), data)?;
        Ok(return_value)
    }
}

pub const SEARCH_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(2), TEXT_TYPES),
];
pub const SEARCH_REPLACE: &str = "search_replace";
pub async fn search_replace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap();

    let var = take_if_var(&mut arg_0, data.clone()).await?;

    let from = values.pop_front().unwrap().as_text(data.clone()).await?;
    let to = values.pop_front().unwrap().as_text(data.clone()).await?;

    let input = arg_0.as_text(data.clone()).await?;

    let input = input.replace(&from, &to);

    if var.is_none() {
        Ok(Value::Text(input))
    } else {
        update_if_var(var, Value::Text(input), data)?;
        Ok(Value::None)
    }
}

pub const SLICE_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), LIST_TYPES),
    ArgRule::new(ArgPos::Index(2), TEXT_TYPES),
];
pub const SLICE_REPLACE: &str = "slice_replace";
pub async fn slice_replace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap();

    let var = take_if_var(&mut arg_0, data.clone()).await?;

    let mut range = values.pop_front().unwrap().as_list(data.clone()).await?;
    let with = values.pop_front().unwrap().as_text(data.clone()).await?;

    let mut input = arg_0.as_text(data.clone()).await?;

    if range.len() != 2 {
        return Err(CommandError::IndexOutOfBounds);
    }

    let (from, to) = (
        std::mem::take(&mut range[0]).as_usize(data.clone()).await?,
        std::mem::take(&mut range[1]).as_usize(data.clone()).await?,
    );

    if from > input.len() || to > input.len() || from > to {
        return Err(CommandError::IndexOutOfBounds);
    } else if !input.is_char_boundary(from) || !input.is_char_boundary(to) {
        return Err(CommandError::InvalidArgument(format!(
            "slice of text must lie within char boundries"
        )));
    }

    input.replace_range(from..to, &with);

    if var.is_none() {
        Ok(Value::Text(input))
    } else {
        update_if_var(var, Value::Text(input), data)?;
        Ok(Value::None)
    }
}

pub const REVERSE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ARRAY_TYPES)];
pub const REVERSE: &str = "reverse";
pub async fn reverse(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let array = values.pop_front().unwrap();

    let return_value = manipulate_array(
        array,
        data.clone(),
        async |list| {
            list.reverse();
            Ok(None)
        },
        async |text| {
            let mut chars = text.chars().collect::<Vec<char>>();
            chars.reverse();
            text.clear();
            text.push_str(&chars.iter().collect::<String>());
            Ok(None)
        },
    )
    .await?;

    Ok(return_value)
}

pub const INC_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Var]),
    ArgRule::new(ArgPos::OptionalIndex(1), WHOLE_NUMBER_TYPES),
];
pub const INC: &str = "inc";
pub async fn inc(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let label = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;

    let amount = if let Some(value) = values.pop_front() {
        value.as_int(data.clone()).await?
    } else {
        1
    };

    let mut var_entry = data.vars.take_entry(&label)?;
    let mut int = var_entry.value.as_int(data.clone()).await?;
    int = int + amount;
    var_entry.value = Value::Int(int);
    data.vars.insert_entry(label, var_entry)?;

    Ok(Value::Int(int))
}

pub const DEC_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Var]),
    ArgRule::new(ArgPos::OptionalIndex(1), WHOLE_NUMBER_TYPES),
];
pub const DEC: &str = "dec";
pub async fn dec(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let label = values
        .pop_front()
        .unwrap()
        .as_var_label(data.clone())
        .await?;

    let amount = if let Some(value) = values.pop_front() {
        value.as_int(data.clone()).await?
    } else {
        1
    };

    let mut var_entry = data.vars.take_entry(&label)?;
    let mut int = var_entry.value.as_int(data.clone()).await?;
    int = int - amount;
    var_entry.value = Value::Int(int);
    data.vars.insert_entry(label, var_entry)?;

    Ok(Value::Int(int))
}

pub const CONTAINS_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ARRAY_TYPES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const CONTAINS: &str = "contains";
pub async fn contains(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let array = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NON_NONE_VALUES)
        .await?;
    let item = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NON_NONE_VALUES)
        .await?;
    if let Value::List(list) = array {
        for value in list {
            let raw_value = value.as_raw(data.clone(), NON_NONE_VALUES).await?;
            if let Ok(is_eq) = raw_value.eq(&item)
                && is_eq
            {
                return Ok(Value::Bool(true));
            }
        }
        return Ok(Value::Bool(false));
    } else {
        let text = array.as_text(data.clone()).await?;
        return Ok(Value::Bool(text.contains(&item.as_text(data).await?)));
    }
}

pub const STARTS_WITH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const STARTS_WITH: &str = "starts_with";
pub async fn starts_with(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap().as_text(data.clone()).await?;
    let arg_1 = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(arg_0.starts_with(&arg_1).into())
}

pub const ENDS_WITH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const ENDS_WITH: &str = "ends_with";
pub async fn ends_with(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap().as_text(data.clone()).await?;
    let arg_1 = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(arg_0.ends_with(&arg_1).into())
}

pub const CONCAT_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub const CONCAT: &str = "concat";
pub async fn concat(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let values = command.take_args();

    let mut cat_string = String::new();

    for value in values {
        cat_string.push_str(&value.as_text(data.clone()).await?);
    }

    Ok(cat_string.into())
}

pub const CAPITALIZE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const CAPITALIZE: &str = "capitalize";
pub async fn capitalize(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut text = values.pop_front().unwrap().as_text(data).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        if let Some(ch) = text.get_mut(0..1) {
            ch.make_ascii_uppercase();
        }
        Ok(Value::Text(text))
    }
}

pub const UPPERCASE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const UPPERCASE: &str = "uppercase";
pub async fn uppercase(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.to_uppercase().into())
}

pub const LOWERCASE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const LOWERCASE: &str = "lowercase";
pub async fn lowercase(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.to_lowercase().into())
}

pub const TRIM_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), TEXT_TYPES),
];
pub const TRIM: &str = "trim";
pub async fn trim(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data.clone()).await?;
    let pattern = values.pop_front().unwrap().as_text(data).await?;
    let chars: Vec<char> = pattern.chars().collect();
    Ok(text.trim_matches(chars.as_slice()).into())
}

pub const IS_NUMBER_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ALL_TYPES)];
pub const IS_NUMBER: &str = "is_number";
pub async fn is_number(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    let is_int = value.clone().as_int(data.clone()).await.is_ok();
    let is_float = value.as_float(data.clone()).await.is_ok();
    Ok(Value::Bool(is_int || is_float))
}

pub const IS_NONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ALL_TYPES)];
pub const IS_NONE: &str = "is_none";
pub async fn is_none(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let value = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ALL_TYPES)
        .await?;
    Ok(Value::Bool(value.is_type(FslType::None)))
}

pub const IS_ALPHA_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const IS_ALPHA: &str = "is_alpha";
pub async fn is_alpha(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    if let Ok(text) = value.as_text(data).await {
        let is_alpha = text.chars().all(char::is_alphabetic);

        Ok(Value::Bool(is_alpha))
    } else {
        Ok(Value::Bool(false))
    }
}

pub const IS_ALPHA_EN_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const IS_ALPHA_EN: &str = "is_alpha_en";
pub async fn is_alpha_en(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    if let Ok(text) = value.as_text(data).await {
        const ALPHA: &[char] = &[
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
            'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        ];

        let is_alpha = text
            .chars()
            .all(|c| ALPHA.contains(&c.to_ascii_lowercase()));

        Ok(Value::Bool(is_alpha))
    } else {
        Ok(Value::Bool(false))
    }
}

pub const IS_WHITESPACE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const IS_WHITESPACE: &str = "is_whitespace";
pub async fn is_whitespace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    if let Ok(text) = value.as_text(data).await {
        let is_whitespace = text.chars().all(char::is_whitespace);

        Ok(Value::Bool(is_whitespace))
    } else {
        Ok(Value::Bool(false))
    }
}

pub const REMOVE_WHITESPACE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), TEXT_TYPES)];
pub const REMOVE_WHITESPACE: &str = "remove_whitespace";
pub async fn remove_whitespace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.split_whitespace().collect::<String>().into())
}

pub const SPLIT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), TEXT_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const SPLIT: &str = "split";
pub async fn split(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let text_to_split = values.pop_front().unwrap().as_text(data.clone()).await?;
    let pattern = values.pop_front().unwrap().as_text(data.clone()).await?;
    let mut list: Vec<Value> = Vec::new();

    for split in text_to_split.split(&pattern) {
        if !split.is_empty() {
            list.push(Value::Text(split.to_string()));
        }
    }
    Ok(Value::List(list))
}

pub const RANDOM_RANGE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const RANDOM_RANGE: &str = "random_range";
pub async fn random_range(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    if contains_float(&values, data.clone()).await? {
        let min = values.pop_front().unwrap().as_float(data.clone()).await?;
        let max = values.pop_front().unwrap().as_float(data.clone()).await?;
        if min >= max {
            Err(CommandError::InvalidRange)
        } else if !min.is_finite() || !max.is_finite() {
            Err(CommandError::NonFiniteValue)
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let min = values.pop_front().unwrap().as_int(data.clone()).await?;
        let max = values.pop_front().unwrap().as_int(data.clone()).await?;
        if min >= max {
            Err(CommandError::InvalidRange)
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub const SLEEP: &str = "sleep";
pub const SLEEP_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES)];
pub async fn sleep(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let delay = values.pop_front().unwrap().as_float(data).await?;
    const MAX_DELAY_SECS: f64 = 60.0;
    if delay > MAX_DELAY_SECS {
        Err(CommandError::Custom(
            "sleep time cannot be greater than 60 seconds".into(),
        ))
    } else if !delay.is_finite() {
        Err(CommandError::Custom(
            "sleep time must be a finite number".into(),
        ))
    } else if delay.is_sign_negative() {
        Err(CommandError::Custom(
            "sleep time cannot be a negative number".into(),
        ))
    } else {
        tokio::time::sleep(Duration::from_secs_f64(delay)).await;
        Ok(Value::None)
    }
}

pub const RANDOM_ENTRY_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), LIST_TYPES)];
pub const RANDOM_ENTRY: &str = "random_entry";
pub async fn random_entry(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let list = values.pop_front().unwrap().as_list(data).await?;
    let range = 0..list.len();
    if range.is_empty() {
        Err(CommandError::InvalidRange)
    } else {
        Ok(list[rand::random_range(range)].clone())
    }
}

pub const SHUFFLE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), LIST_TYPES)];
pub const SHUFFLE: &str = "shuffle";
pub async fn shuffle(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let mut list = values.pop_front().unwrap().as_list(data).await?;
    list.shuffle(&mut rand::rng());
    Ok(Value::List(list))
}

pub const DEF_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Var]),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Var, FslType::Command]),
];
pub const DEF: &str = "def";
pub async fn def(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();
    let label = values.pop_front().unwrap().get_var_label()?.to_string();
    let mut parameters: VecDeque<String> = VecDeque::new();
    let mut commands: Vec<Command> = Vec::new();

    let values_len = values.len();
    let mut encountered_command = false;
    for i in 0..values_len {
        if values[i].is_type(FslType::Var) {
            if encountered_command {
                return Err(CommandError::WrongArgOrder(format!(
                    "variables in command definition must come before commands"
                )));
            }
            let var_label = values[i].get_var_label()?.to_string();
            parameters.push_back(var_label);
        } else {
            encountered_command = true;
            let command = values[i].clone().as_command()?;
            commands.push(command);
        }
    }

    let mut user_commands = data.user_commands.lock().await;
    let user_command = UserCommand {
        label: label.clone(),
        parameters,
        commands: commands,
    };
    user_commands.insert(label, user_command);

    Ok(Value::None)
}

fn substitute_args_list(
    values: &mut Vec<Value>,
    var_map: &HashMap<String, Value>,
) -> Result<(), CommandError> {
    for value in values {
        match value {
            Value::List(values) => substitute_args_list(values, var_map)?,
            Value::Map(map) => substitute_args_map(map, var_map)?,
            Value::Var(label) => {
                if let Some(var_value) = var_map.get(label) {
                    *value = var_value.clone();
                }
            }
            Value::Command(command) => substitute_args(command, var_map)?,
            Value::Int(_) => {}
            Value::Float(_) => {}
            Value::Bool(_) => {}
            Value::Text(_) => {}
            Value::None => {}
        }
    }
    Ok(())
}

fn substitute_args_map(
    map: &mut HashMap<String, Value>,
    var_map: &HashMap<String, Value>,
) -> Result<(), CommandError> {
    for (_, value) in map {
        match value {
            Value::Map(map) => substitute_args_map(map, var_map)?,
            Value::List(values) => substitute_args_list(values, var_map)?,
            Value::Var(label) => {
                if let Some(var_value) = var_map.get(label) {
                    *value = var_value.clone();
                }
            }
            Value::Command(command) => substitute_args(command, var_map)?,
            Value::Int(_) => {}
            Value::Float(_) => {}
            Value::Bool(_) => {}
            Value::Text(_) => {}
            Value::None => {}
        }
    }
    Ok(())
}

fn substitute_args(
    command: &mut Command,
    var_map: &HashMap<String, Value>,
) -> Result<(), CommandError> {
    for arg in command.get_args_mut() {
        match arg {
            Value::List(values) => substitute_args_list(values, var_map)?,
            Value::Var(label) => {
                if let Some(var_value) = var_map.get(label) {
                    *arg = var_value.clone()
                }
            }
            Value::Command(_) => {
                let mut inner_command = std::mem::take(arg).as_command()?;
                substitute_args(&mut inner_command, var_map)?;
                *arg = Value::Command(Box::new(inner_command));
            }
            Value::Map(map) => substitute_args_map(map, var_map)?,
            Value::Int(_) => {}
            Value::Float(_) => {}
            Value::Bool(_) => {}
            Value::Text(_) => {}
            Value::None => {}
        }
    }

    Ok(())
}

pub const RUN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub async fn run(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let mut values = command.take_args();

    let command_label = values.pop_front().unwrap().get_var_label()?.to_string();

    let commands_lock = data.user_commands.lock().await;

    let mut var_labels = commands_lock
        .get(&command_label)
        .unwrap()
        .parameters
        .clone();
    let commands = commands_lock.get(&command_label).unwrap().commands.clone();
    drop(commands_lock);

    if values.len() != var_labels.len() {
        return Err(CommandError::WrongArgCount(format!(
            "expected {} args but got {}",
            var_labels.len(),
            values.len()
        )));
    }

    let mut var_map: HashMap<String, Value> = HashMap::new();

    let vars = var_labels.len();
    for _ in 0..vars {
        let mut value = values.pop_front().unwrap();
        match value.as_type() {
            FslType::Int
            | FslType::Float
            | FslType::Bool
            | FslType::Text
            | FslType::None
            | FslType::Var => {}
            FslType::List | FslType::Map | FslType::Command => {
                value = value.as_raw(data.clone(), NON_NONE_VALUES).await?;
            }
        }
        var_map.insert(var_labels.pop_front().unwrap(), value);
    }

    let mut final_value = Value::None;
    data.vars.push();
    for mut command in commands {
        substitute_args(&mut command, &var_map)?;
        final_value = command.execute(data.clone()).await?;
        if data.flags.return_flag.load(Ordering::Relaxed) {
            data.flags.return_flag.store(false, Ordering::Relaxed);
            break;
        }
    }
    data.vars.pop();

    Ok(final_value)
}

pub const BREAK: &str = "break";
pub async fn r#break(_: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    if data.loop_depth.load(Ordering::Relaxed) > 0 {
        data.flags.break_flag.store(true, Ordering::Relaxed);
    } else {
        return Err(CommandError::BreakOutsideLoop);
    }

    Ok(Value::None)
}

pub const CONTINUE: &str = "continue";
pub async fn r#continue(_: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    if data.loop_depth.load(Ordering::Relaxed) > 0 {
        data.flags.continue_flag.store(true, Ordering::Relaxed);
    } else {
        return Err(CommandError::ContinueOutsideLoop);
    }
    Ok(Value::None)
}

pub const RETURN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::OptionalIndex(0), ALL_TYPES)];
pub const RETURN: &str = "return";
pub async fn r#return(command: Command, data: Arc<InterpreterData>) -> Result<Value, CommandError> {
    let return_value = command.take_args().pop_front();

    match return_value {
        Some(value) => {
            let value = value.as_raw(data.clone(), LITERAL_VALUES).await?;
            data.flags.return_flag.store(true, Ordering::Relaxed);
            Ok(value)
        }
        None => {
            data.flags.return_flag.store(true, Ordering::Relaxed);
            Ok(Value::None)
        }
    }
}

pub const EXIT: &str = "exit";
pub async fn exit(_: Command, _: Arc<InterpreterData>) -> Result<Value, CommandError> {
    Err(CommandError::ProgramExited)
}

#[cfg(test)]
pub mod tests {
    use std::time::{Duration, SystemTime};

    use crate::{
        CommandError, FslInterpreter, InterpreterError, InterpreterErrorType, error::ValueError,
    };

    pub async fn test_interpreter(code: &str, expected_output: &str) {
        let result = FslInterpreter::new().interpret(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    pub async fn observe_interpreter(code: &str) {
        let result = FslInterpreter::new().interpret(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);
    }

    pub async fn test_interpreter_embedded(code: &str, expected_output: &str) {
        let result = FslInterpreter::new().interpret_embedded_code(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    pub async fn test_interpreter_err_type(code: &str) -> crate::InterpreterErrorType {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        assert!(result.is_err());
        result.err().unwrap().error_type
    }

    pub async fn interpreter_throws_err(code: &str, err: InterpreterError) -> bool {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        result.is_err_and(|e| {
            dbg!(&e);
            e.error_type == err.error_type
        })
    }

    #[tokio::test]
    async fn add_two_ints() {
        test_interpreter("print(add(1, 2))", "3").await;
    }

    #[tokio::test]
    async fn sub_two_ints() {
        test_interpreter("print(sub(1, 2))", "-1").await;
    }

    #[tokio::test]
    async fn mul_two_ints() {
        test_interpreter("print(mul(1, 2))", "2").await;
    }

    #[tokio::test]
    async fn div_two_ints() {
        test_interpreter("print(div(1, 2))", "0").await;
    }

    #[tokio::test]
    async fn mod_two_ints() {
        test_interpreter("print(mod(8, 2))", "0").await;
    }

    #[tokio::test]
    async fn add_two_floats() {
        test_interpreter("print(add(1.0, 2.0))", "3").await;
    }

    #[tokio::test]
    async fn sub_two_floats() {
        test_interpreter("print(sub(1.0, 2.0))", "-1").await;
    }

    #[tokio::test]
    async fn mul_two_floats() {
        test_interpreter("print(mul(1.0, 2.0))", "2").await;
    }

    #[tokio::test]
    async fn div_two_floats() {
        test_interpreter("print(div(1.0, 2.0))", "0.5").await;
    }

    #[tokio::test]
    async fn mod_two_floats() {
        test_interpreter("print(mod(8.0, 2.0))", "0").await;
    }

    #[tokio::test]
    async fn add_float_and_int() {
        test_interpreter("print(add(1, 2.0))", "3").await;
    }

    #[tokio::test]
    async fn sub_float_and_int() {
        test_interpreter("print(sub(1, 2.0))", "-1").await;
    }

    #[tokio::test]
    async fn mul_float_and_int() {
        test_interpreter("print(mul(1, 2.0))", "2").await;
    }

    #[tokio::test]
    async fn div_float_and_int() {
        test_interpreter("print(div(1, 2.0))", "0.5").await;
    }

    #[tokio::test]
    async fn mod_float_and_int() {
        test_interpreter("print(mod(4, 2.0))", "0").await;
        test_interpreter("print(mod(4.0, 2))", "0").await;
    }

    #[tokio::test]
    async fn div_by_zero() {
        let err = test_interpreter_err_type("print(div(1, 0))").await;
        dbg!(&err);
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::DivisionByZero)
        ));
    }

    #[tokio::test]
    async fn mod_by_zero() {
        let err = test_interpreter_err_type("print(mod(1, 0))").await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::DivisionByZero)
        ));
    }

    #[tokio::test]
    async fn store_var() {
        test_interpreter("number.store(1) print(number)", "1").await;
    }

    #[tokio::test]
    async fn store_command_result_in_var() {
        test_interpreter("number.store(add(1,1)) print(number)", "2").await;
    }

    #[tokio::test]
    async fn store_list_in_var() {
        test_interpreter("numbers.store([1, 2, 3]) print(numbers)", "[1, 2, 3]").await;
    }

    #[tokio::test]
    async fn store_var_in_var() {
        test_interpreter("a.store(1), b.store(a) print(b)", "1").await;
    }

    #[tokio::test]
    async fn clone_var() {
        test_interpreter(
            "a.store([1, 2, 3]) b.store(a), print(b.clone())",
            "[1, 2, 3]",
        )
        .await;
    }

    #[tokio::test]
    async fn free_var() {
        let err = test_interpreter_err_type("a.store(1) print(a) a.free() print(a)").await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantVar(_)))
        ));
    }

    #[tokio::test]
    async fn print_values() {
        test_interpreter(
            r#"a.store(1), print("1", " ", 1, " ", a, " ", [1])"#,
            "1 1 1 [1]",
        )
        .await;
    }

    #[tokio::test]
    async fn commands_in_scope() {
        test_interpreter(r#"(a.store(1), a.inc(), a.print())"#, "2").await;
    }

    #[tokio::test]
    async fn values_in_scope() {
        let err = test_interpreter_err_type(r#"(1)"#).await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::WrongArgType(_))
        ));
    }

    #[tokio::test]
    async fn int_cmp() {
        test_interpreter(r#"print(eq(1, 2))"#, "false").await;
        test_interpreter(r#"print(eq(2, 2))"#, "true").await;

        test_interpreter(r#"print(lt(1, 2))"#, "true").await;
        test_interpreter(r#"print(lt(2, 1))"#, "false").await;
        test_interpreter(r#"print(lt(2, 2))"#, "false").await;

        test_interpreter(r#"print(ltoe(1, 2))"#, "true").await;
        test_interpreter(r#"print(ltoe(2, 1))"#, "false").await;
        test_interpreter(r#"print(ltoe(2, 2))"#, "true").await;

        test_interpreter(r#"print(gt(1, 2))"#, "false").await;
        test_interpreter(r#"print(gt(2, 1))"#, "true").await;
        test_interpreter(r#"print(gt(2, 2))"#, "false").await;

        test_interpreter(r#"print(gtoe(1, 2))"#, "false").await;
        test_interpreter(r#"print(gtoe(2, 1))"#, "true").await;
        test_interpreter(r#"print(gtoe(2, 2))"#, "true").await;
    }

    #[tokio::test]
    async fn float_cmp() {
        test_interpreter(r#"print(eq(1.2, 1.1))"#, "false").await;
        test_interpreter(r#"print(eq(1.1, 1.1))"#, "true").await;

        test_interpreter(r#"print(lt(1.1, 1.2))"#, "true").await;
        test_interpreter(r#"print(lt(2.1, 1.02))"#, "false").await;
        test_interpreter(r#"print(lt(2.0, 2.0))"#, "false").await;

        test_interpreter(r#"print(ltoe(1.1, 1.2))"#, "true").await;
        test_interpreter(r#"print(ltoe(2.1, 1.02))"#, "false").await;
        test_interpreter(r#"print(ltoe(2.0, 2.0))"#, "true").await;

        test_interpreter(r#"print(gt(1.4, 2.2))"#, "false").await;
        test_interpreter(r#"print(gt(2.2, 1.2))"#, "true").await;
        test_interpreter(r#"print(gt(2.0, 2.0))"#, "false").await;

        test_interpreter(r#"print(gtoe(1.4, 2.2))"#, "false").await;
        test_interpreter(r#"print(gtoe(2.2, 1.2))"#, "true").await;
        test_interpreter(r#"print(gtoe(2.0, 2.0))"#, "true").await;
    }

    #[tokio::test]
    async fn text_cmp() {
        test_interpreter(r#"print(eq("text", "ext"))"#, "false").await;
        test_interpreter(r#"print(eq("test", "test"))"#, "true").await;

        test_interpreter(r#"print(lt("1.1", 1.2))"#, "true").await;
        test_interpreter(r#"print(lt("2.1", 1.02))"#, "false").await;
        test_interpreter(r#"print(lt("2.0", 2.0))"#, "false").await;

        test_interpreter(r#"print(gt("1.4", 2.2))"#, "false").await;
        test_interpreter(r#"print(gt("2.2", 1.2))"#, "true").await;
        test_interpreter(r#"print(gt("2.0", 2.0))"#, "false").await;
    }

    #[tokio::test]
    async fn bool_eq() {
        test_interpreter(r#"print(eq(false, false))"#, "true").await;
        test_interpreter(r#"print(eq(true, false))"#, "false").await;
    }

    #[tokio::test]
    async fn int_text_eq() {
        test_interpreter(r#"print(eq(1, "1"))"#, "true").await;
        let err = test_interpreter_err_type(r#"print(eq(1, "a"))"#).await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::FailedParse(_)))
        ));
    }

    #[tokio::test]
    async fn float_text_eq() {
        test_interpreter(r#"print(eq(1.24, "1.24"))"#, "true").await;
        test_interpreter(r#"print(eq(1.123, "1"))"#, "false").await;
    }

    #[tokio::test]
    async fn not_condition() {
        test_interpreter(r#"print(not(true))"#, "false").await;
        test_interpreter(r#"print(not(false))"#, "true").await;
    }

    #[tokio::test]
    async fn and_condition() {
        test_interpreter(r#"print(and(true, false))"#, "false").await;
        test_interpreter(r#"print(and(true, true))"#, "true").await;
    }

    #[tokio::test]
    async fn or_condition() {
        test_interpreter(r#"print(or(true, false))"#, "true").await;
        test_interpreter(r#"print(and(true, true))"#, "true").await;
        test_interpreter(r#"print(and(false, false))"#, "false").await;
    }

    #[tokio::test]
    async fn if_then() {
        test_interpreter(r#"if(true, then(print("true")))"#, "true").await;
        test_interpreter(r#"if(false, then(print("true")))"#, "").await;
    }

    #[tokio::test]
    async fn if_then_else() {
        test_interpreter(
            r#"if(true, then(print("true")), else(print("false")))"#,
            "true",
        )
        .await;
        test_interpreter(
            r#"if(false, then(print("true")), else(print("false")))"#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn while_loop() {
        test_interpreter(
            r#"i.store(0), while(i.lt(3), print(i, " "), i.inc())"#,
            "0 1 2 ",
        )
        .await;
    }

    #[tokio::test]
    async fn repeat() {
        test_interpreter(r#"i.store(0), repeat(3, print(i, " "), i.inc())"#, "0 1 2 ").await;
    }

    #[tokio::test]
    async fn index() {
        test_interpreter(r#"nums.store([1, 2, 3]) nums.index(1).print()"#, "2").await;
        test_interpreter(r#"nums.store("123") nums.index(1).print()"#, "2").await;
    }

    #[tokio::test]
    async fn remove() {
        test_interpreter(r#"nums.store([1, 2, 3]) nums.remove(1).print()"#, "2").await;
        test_interpreter(r#"text.store("text") text.remove(1).print()"#, "e").await;
    }

    #[tokio::test]
    async fn remove_inner() {
        test_interpreter(
            r#"nums.store([1, [2, [3, 4], 5], 6]) nums.remove([1, 1, 0]).print()"#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn remove_inner_var() {
        test_interpreter(
            r#"nums.store([1, [2, [3, 4], 5], 6]) nums.remove([1, 1, 0]) nums.print()"#,
            "[1, [2, [4], 5], 6]",
        )
        .await;
    }

    #[tokio::test]
    async fn length() {
        test_interpreter(r#"nums.store([1, 2, 3]) nums.length().print()"#, "3").await;
        test_interpreter(r#"nums.store("123") nums.length().print()"#, "3").await;
    }

    #[tokio::test]
    async fn swap() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.swap(0, 2) nums.print()"#,
            "[3, 2, 1]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.swap(0, 2) nums.print()"#, "321").await;
    }

    #[tokio::test]
    async fn replace() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.replace(0, 2) nums.print()"#,
            "[2, 2, 3]",
        )
        .await;
        test_interpreter(
            r#"nums.store("123") nums.replace(0, 2) nums.print()"#,
            "223",
        )
        .await;
    }

    #[tokio::test]
    async fn insert() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.insert(0, 0) nums.print()"#,
            "[0, 1, 2, 3]",
        )
        .await;
        test_interpreter(
            r#"nums.store("123") nums.insert(0, 0) nums.print()"#,
            "0123",
        )
        .await;
    }

    #[tokio::test]
    async fn push() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.push(0) nums.print()"#,
            "[1, 2, 3, 0]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.push(0) nums.print()"#, "1230").await;
    }

    #[tokio::test]
    async fn push_matrix() {
        test_interpreter(
            r#"
                nums.store([1, [2, [3, 4], 5], 6])
                tmp.store(nums.index([1, 1]))
                tmp.push(0)
                nums.replace([1, 1], tmp)
                nums.print()
            "#,
            "[1, [2, [3, 4, 0], 5], 6]",
        )
        .await;
    }

    #[tokio::test]
    async fn pop() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.pop().print(" ") nums.print()"#,
            "3 [1, 2]",
        )
        .await;
        test_interpreter(
            r#"nums.store("123") nums.pop().print(" ") nums.print()"#,
            "3 12",
        )
        .await;
    }

    #[tokio::test]
    async fn inc() {
        test_interpreter(r#"i.store(1) i.inc().print()"#, "2").await;
    }

    #[tokio::test]
    async fn inc_by_x() {
        test_interpreter(r#"x.store(5) i.store(1) i.inc(x) i.print()"#, "6").await;
    }

    #[tokio::test]
    async fn dec() {
        test_interpreter(r#"i.store(1) i.dec().print()"#, "0").await;
    }

    #[tokio::test]
    async fn dec_by_x() {
        test_interpreter(r#"x.store(5) i.store(1) i.dec(x) i.print()"#, "-4").await;
    }

    #[tokio::test]
    async fn contains() {
        test_interpreter(r#"nums.store([1, 2, 3]) nums.contains(2).print()"#, "true").await;
        test_interpreter(r#"nums.store([1, 2, 3]) nums.contains(4).print()"#, "false").await;
        test_interpreter(r#"nums.store("123") nums.contains("2").print()"#, "true").await;
        test_interpreter(r#"nums.store("123") nums.contains("0").print()"#, "false").await;
    }

    #[tokio::test]
    async fn starts_with() {
        test_interpreter(r#"starts_with("text", "t").print()"#, "true").await;
        test_interpreter(r#"starts_with("text", "f").print()"#, "false").await;
    }

    #[tokio::test]
    async fn ends_with() {
        test_interpreter(r#"ends_with("text", "t").print()"#, "true").await;
        test_interpreter(r#"ends_with("text", "f").print()"#, "false").await;
    }

    #[tokio::test]
    async fn concat() {
        test_interpreter(r#"concat(1, 2, "3").print()"#, "123").await;
    }

    #[tokio::test]
    async fn capitalize() {
        test_interpreter(r#"capitalize("hey").print()"#, "Hey").await;
    }

    //ﬃ
    #[tokio::test]
    async fn capitalize_non_capitalizable_character() {
        test_interpreter(r#"capitalize("ﬆ").print()"#, "ﬆ").await;
    }

    #[tokio::test]
    async fn capitalize_expandable_character() {
        test_interpreter(r#"capitalize("ﬃ").print()"#, "ﬃ").await;
    }

    #[tokio::test]
    async fn uppercase() {
        test_interpreter(r#"uppercase("hey").print()"#, "HEY").await;
    }

    #[tokio::test]
    async fn lowercase() {
        test_interpreter(r#"lowercase("HEY").print()"#, "hey").await;
    }

    #[tokio::test]
    async fn trim() {
        test_interpreter(r#"trim("\"hey\".", ".\"").print()"#, "hey").await;
    }

    #[tokio::test]
    async fn remove_whitespace() {
        test_interpreter(r#"remove_whitespace(" h e y ").print()"#, "hey").await;
    }

    #[tokio::test]
    async fn split() {
        test_interpreter(r#"split("h e y", " ").print()"#, r#"[h, e, y]"#).await;
    }

    #[tokio::test]
    async fn random_range() {
        test_interpreter(
            r#"n.store(random_range(1, 6)) print(and(n.lt(7), n.gt(0)))"#,
            r#"true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn random_entry() {
        test_interpreter(
            r#"n.store(random_entry([1, 2, 3])) print(and(or(n.eq(1), n.eq(2), n.eq(3)), n.lt(4), n.gt(0)))"#,
            r#"true"#,
        )
        .await;
    }

    #[tokio::test]
    async fn shuffle() {
        observe_interpreter(r#"list.store([1, 2, 3, 4]) print(list, "\n", shuffle(list))"#).await;
    }

    #[tokio::test]
    async fn r#break() {
        test_interpreter(r#"while(true, print("1"), break())"#, r#"1"#).await;
    }

    #[tokio::test]
    async fn break_nested() {
        test_interpreter(
            r#"
                i.store(0)
                repeat(2,
                    repeat(2,
                        if(i.eq(0)
                            then(
                                i.inc()
                                break()
                            )
                        )
                        i.print()
                    )
                )
            "#,
            r#"11"#,
        )
        .await;
    }

    #[tokio::test]
    async fn break_outside_command() {
        let err = test_interpreter_err_type(r#"break()"#).await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::BreakOutsideLoop)
        ));
    }

    #[tokio::test]
    async fn r#continue() {
        test_interpreter(
            r#"
                i.store(0)
                repeat(2,
                    if(i.eq(0)
                        then(
                            i.inc()
                            continue()
                        )
                    )
                    print("1")
                )
            "#,
            r#"1"#,
        )
        .await;
    }

    #[tokio::test]
    async fn continue_deeply_nested() {
        test_interpreter(
            r#"
            i.store(0)
            repeat(2,
                repeat(2,
                    repeat(2,
                        if(i.eq(0)
                            then(
                                i.inc()
                                continue()
                            )
                        )
                        i.print()
                    )
                )
            )
        "#,
            r#"1111111"#,
        )
        .await;
    }

    #[tokio::test]
    async fn continue_outside_command() {
        let err = test_interpreter_err_type(r#"continue()"#).await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ContinueOutsideLoop)
        ));
    }

    #[tokio::test]
    async fn exit() {
        test_interpreter(r#"print("hello") exit() print(" world")"#, "hello").await;
    }

    #[tokio::test]
    async fn custom_command() {
        test_interpreter(
            r#"hey.def(x, y, z, print(x, y, z, "hey")) hey(1, 2, 3)"#,
            "123hey",
        )
        .await;
    }

    #[tokio::test]
    async fn pass_var_to_custom_command() {
        test_interpreter(r#"plus.def(f, f.inc()) i.store(0) i.plus() i.print()"#, "1").await;
    }

    #[tokio::test]
    async fn pass_var_to_custom_command_with_same_name() {
        test_interpreter(r#"plus.def(i, i.inc()) i.store(0) i.plus() i.print()"#, "1").await;
    }

    #[tokio::test]
    async fn custom_command_complex() {
        test_interpreter(
            r#"
                print_box.def(background, size,
                    repeat(size,
                        repeat(size,
                            print(background)
                        )
                        print("\n")
                    )
                )

                print_box("=", 4)
            "#,
            "====\n====\n====\n====\n",
        )
        .await;
    }

    #[tokio::test]
    async fn custom_command_no_args() {
        test_interpreter(
            r#"        
                blank.store("=")
                matrix.store([
	                [blank, blank, blank],
	                [blank, blank, blank],
	                [blank, blank, blank],
                ])
                
                filled_slots.store([])
                
                print_game_state.def(
	                state.store("")
	                i.store(0)
	                j.store(0)
	                repeat(matrix.length()
		                repeat(matrix.index(i).length()
			                state.push(matrix.index(i).index(j))
			                j.inc()
		                ),
		                j.store(0)
		                i.inc()
		                state.push("\n")
	                )
	                print(state)
                )

                print_game_state()
            "#,
            "===\n===\n===\n",
        )
        .await;
    }

    #[tokio::test]
    async fn precision() {
        test_interpreter(r#"print(precision(1.24123, 2))"#, "1.24").await;
    }

    #[tokio::test]
    async fn slice_replace() {
        test_interpreter(r#"slice_replace("hello", [1, 5], "hhhh").print()"#, "hhhhh").await;
    }

    #[tokio::test]
    async fn slice_replace_invalid_index() {
        let err = test_interpreter_err_type(r#"slice_replace("café", [4, 5], "h").print()"#).await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::InvalidArgument(_))
        ));
    }

    #[tokio::test]
    async fn slice_replace_var() {
        test_interpreter(
            r#"text.store("hello") text.slice_replace([1, 5], "hhhh") text.print()"#,
            "hhhhh",
        )
        .await;
    }

    #[tokio::test]
    async fn search_replace() {
        test_interpreter(
            r#"search_replace("this text has not been replaced", "not", "").print()"#,
            "this text has  been replaced",
        )
        .await;
    }

    #[tokio::test]
    async fn search_replace_var() {
        test_interpreter(
            r#"text.store("this text has not been replaced") text.search_replace("not", "") text.print()"#,
            "this text has  been replaced",
        )
        .await;
    }

    #[tokio::test]
    async fn index_matrix() {
        test_interpreter(
            r#"matrix.store([[1,2,3], [3,4,5], [6,7,8]]) matrix.index([1,1]).print(" ") matrix.print()"#,
            "4 [[1, 2, 3], [3, 4, 5], [6, 7, 8]]",
        )
        .await;
    }

    #[tokio::test]
    async fn matrix_index_nested() {
        test_interpreter(
            r#"matrix.store([1,[2,[3,[4,5]]]]) matrix.index([1,1,1,1]).print()"#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn reverse_list() {
        test_interpreter(r#"list.store([1, 2, 3]).reverse().print()"#, "[3, 2, 1]").await;
    }

    #[tokio::test]
    async fn reverse_text() {
        test_interpreter(r#"print("hello".reverse())"#, "olleh").await;
    }

    #[tokio::test]
    async fn is_number() {
        test_interpreter(r#"is_number(1).print()"#, "true").await;
        test_interpreter(r#"is_number(1.012).print()"#, "true").await;
        test_interpreter(r#"is_number(false).print()"#, "false").await;
    }

    #[tokio::test]
    async fn is_none() {
        test_interpreter(r#"is_none(print()).print()"#, "true").await;
        test_interpreter(r#"is_none("").print()"#, "false").await;
    }

    #[tokio::test]
    async fn is_alpha() {
        test_interpreter(r#"is_alpha("apple").print()"#, "true").await;
        test_interpreter(r#"is_alpha("a1pple").print()"#, "false").await;
        test_interpreter(r#"is_alpha("café").print()"#, "true").await;
    }

    #[tokio::test]
    async fn is_alpha_en() {
        test_interpreter(r#"is_alpha_en("apple").print()"#, "true").await;
        test_interpreter(r#"is_alpha_en("a1pple").print()"#, "false").await;
        test_interpreter(r#"is_alpha_en("café").print()"#, "false").await;
    }

    #[tokio::test]
    async fn is_whitespace() {
        test_interpreter(r#"is_whitespace(" ").print()"#, "true").await;
        test_interpreter(r#"is_whitespace("").print()"#, "true").await;
        test_interpreter(r#"is_whitespace(" h e y ").print()"#, "false").await;
    }

    #[tokio::test]
    async fn sleep() {
        let time = SystemTime::now();
        test_interpreter(r#"sleep(0.2)"#, "").await;
        let time = time.elapsed().unwrap();
        assert!(time >= Duration::from_secs_f32(0.2));
    }

    #[tokio::test]
    async fn length_of_bool() {
        let err = test_interpreter_err_type("var.store(true) length(var).print()").await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::InvalidConversion(
                _
            )))
        ))
    }

    #[tokio::test]
    async fn custom_command_with_loop_then_break_in_loop() {
        test_interpreter(
            r#"
                test.def(repeat(2, add(1,2)))
                while(true, test(), break())
                print("done")
            "#,
            "done",
        )
        .await;
    }

    #[tokio::test]
    async fn store_in_result_of_command() {
        test_interpreter(
            r#"
                character_name.store("jake")
                character.store([character_name])
                character.index(0).store("joseph")
                character.index(0).print("\n")
                character_name.print()
            "#,
            "joseph\njoseph",
        )
        .await;
    }

    #[tokio::test]
    async fn return_statement() {
        test_interpreter(
            r#"
                function.def(
                    print(1)
                    return()
                    print(2)
                )
                function()
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn return_statement_nested() {
        test_interpreter(
            r#"
                function.def(
                    repeat(2,
                        repeat(2,
                            repeat(2,
                                repeat(2,
                                    print("4")
                                    return()
                                )
                                print("3")
                            )
                            print("2")
                        )
                        print("1")
                    )
                )
                function()
            "#,
            "4",
        )
        .await;
    }

    #[tokio::test]
    async fn return_outside_command() {
        test_interpreter(
            r#"
                print(1)
                return(5)
                print(2)
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn return_value() {
        test_interpreter(
            r#"
                function.def(
                    return(42)
                    print("never")
                )
                result.store(function())
                print(result)
            "#,
            "42",
        )
        .await;
    }

    #[tokio::test]
    async fn return_if_then() {
        test_interpreter(
            r#"
                function.def(
                    if(true,
                        then(return(1))
                    )
                    print("never")
                )
                print(function())
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn return_nested_functions() {
        test_interpreter(
            r#"
                inner.def(
                    return()
                    print("never")
                )
                outer.def(
                    inner()
                    print("outer continues")
                )
                outer()
            "#,
            "outer continues",
        )
        .await;
    }

    #[tokio::test]
    async fn return_in_expression() {
        test_interpreter(
            r#"
                get_value.def(
                    return(10)
                )
                value.store(get_value())
                print(value.add(1))
            "#,
            "11",
        )
        .await;
    }

    #[tokio::test]
    async fn const_stores_value() {
        test_interpreter(
            r#"
                const(MY_CONST, 42)
                print(MY_CONST)
            "#,
            "42",
        )
        .await;
    }

    #[tokio::test]
    async fn const_cannot_be_overwritten() {
        let err = test_interpreter_err_type(
            r#"
                const(MY_CONST, 42)
                MY_CONST.store(100)
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterErrorType::Command(_)))
    }

    #[tokio::test]
    async fn const_cannot_be_freed() {
        let err = test_interpreter_err_type(
            r#"
                const(MY_CONST, 42)
                MY_CONST.free()
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterErrorType::Command(_)))
    }

    #[tokio::test]
    async fn const_accessible_inside_function() {
        test_interpreter(
            r#"
                const(MY_CONST, 42)
                function.def(
                    print(MY_CONST)
                )
                function()
            "#,
            "42",
        )
        .await;
    }

    #[tokio::test]
    async fn const_cannot_be_overwritten_inside_function() {
        let err = test_interpreter_err_type(
            r#"
                const(MY_CONST, 42)
                function.def(
                    MY_CONST.store(100)
                )
                function()
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterErrorType::Command(_)))
    }

    #[tokio::test]
    async fn const_value_is_not_mutable_via_index() {
        test_interpreter(
            r#"
                THREE.const(3)
                list.store([1, 2, three])
                list.index(2).store(99)
                print(list.index(2), "\n")
                print(THREE)
            "#,
            "99\n3",
        )
        .await;
    }

    #[tokio::test]
    async fn local_vars() {
        test_interpreter(
            r#"
            i.store(0)
            repeat(5,
                i.inc()
            )

            global.def(
                repeat(5,
                    i.inc()
                )
            )

            local_example.def(
                i.local(0)
                repeat(5,
                    i.inc()
                )
            )

            global()
            local_example()
            i.print()
            "#,
            "10",
        )
        .await;
    }

    #[tokio::test]
    async fn local_global() {
        test_interpreter(
            r#"
            i.local(0)

            test.def(
                i.inc()
            )

            test()
            i.print()
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn local_global_local() {
        test_interpreter(
            r#"
            i.local(0)

            test.def(
                i.local(10)
                i.inc()
            )

            test()
            i.print()
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn store_after_local() {
        test_interpreter(
            r#"
            i.store(0)

            test.def(
                i.local(10)
                i.store(12)
                i.inc()
                i.print("\n")
            )

            test()
            i.print()
            "#,
            "11\n12",
        )
        .await;
    }

    #[tokio::test]
    async fn local_const() {
        test_interpreter(
            r#"
            TEST.const(1)

            test.def(
                TEST.const(2)
                TEST.print("\n")
            )

            test()
            TEST.print()
            "#,
            "2\n1",
        )
        .await;
    }

    #[tokio::test]
    async fn store_global_if_outer_var_exists() {
        test_interpreter(
            r#"
            i.store(0)
            repeat(5,
                i.inc()
            )

            global.def(
                repeat(5,
                    i.inc()
                )
            )

            local_test.def(
                i.store(0)
                repeat(5,
                    i.inc()
                )
            )

            global()
            local_test()
            i.print()
            "#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn store_local_if_no_outer_var_exists() {
        let err = test_interpreter_err_type(
            r#"
            test.def(
                i.store(0)
                repeat(5,
                    i.inc()
                )
            )
            test()
            i.print()
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterErrorType::Command(_)))
    }

    #[tokio::test]
    async fn update_local() {
        test_interpreter(
            r#"
                i.store(10)
                test_local.def(
                    i.local(0)
                    i.update(i.add(1))
                    i.print("\n")
                )
                i.update(11)
                test_local()
                i.print()
            "#,
            "1\n11",
        )
        .await;
    }

    #[tokio::test]
    async fn clamp_int() {
        test_interpreter(
            r#"
            print(clamp(5, 0, 3))
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn clamp_float() {
        test_interpreter(
            r#"
            print(clamp(5.2, 0, 3.7))
            "#,
            "3.7",
        )
        .await;
    }

    #[tokio::test]
    async fn clamp_min() {
        test_interpreter(
            r#"
            print(clamp_min(5, 10))
            "#,
            "10",
        )
        .await;
    }

    #[tokio::test]
    async fn clamp_max() {
        test_interpreter(
            r#"
            print(clamp_max(5, 2))
            "#,
            "2",
        )
        .await;
    }

    #[tokio::test]
    async fn clamp_invalid_range() {
        let err = test_interpreter_err_type(
            r#"
            print(clamp(5, 10, 2))
            "#,
        )
        .await;
        assert!(matches!(err, InterpreterErrorType::Command(_)))
    }

    #[tokio::test]
    async fn store_map() {
        test_interpreter(
            r#"
	            player.store([
	                name: "blah",
	                health: 100,
	                dodge: 0,
	                strength: 0
	            ])
	            player.get("name").print()
            "#,
            "blah",
        )
        .await;
    }

    #[tokio::test]
    async fn update_map() {
        test_interpreter(
            r#"
	            player.store([
	                name: "blah",
	                health: 100,
	                dodge: 0,
	                strength: 0
	            ])
	            player.get("name").print("\n")
	            player.set("name", "jake").print("\n")
	            player.get("name").print()
            "#,
            "blah\nblah\njake",
        )
        .await;
    }

    #[tokio::test]
    async fn substitute_map_args() {
        test_interpreter(
            r#"
                create_player.def(name,
	                player.store([
	                    name: name,
	                    health: 100,
	                    dodge: 0,
	                    strength: 0
	                ])
	                player.return()
	            )
	            player.store(create_player("jake"))
	            player.name.get().print()
            "#,
            "jake",
        )
        .await;
    }

    #[tokio::test]
    async fn return_from_loop() {
        test_interpreter(
            r#"
	            test.def(
	            	repeat(5,
	            		return(0)
	            		print("shouldn't happen")
	            	)
	            )

	            test().print()
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn return_in_repeat_in_if() {
        test_interpreter(
            r#"
	            LOW_QUALITY.const("low")
	            MEDIUM_QUALITY.const("medium")
	            HIGH_QUALITY.const("high")

	            QUALITY_THRESHOLDS.const([
	            	[
	            		threshold: 50,
	            		value: LOW_QUALITY,
	            	],
	            	[
	            		threshold: 80,
	            		value: MEDIUM_QUALITY,
	            	],
	            	[
	            		threshold: 100,
	            		value: HIGH_QUALITY,
	            	]
	            ])

	            get_threshold_value.def(thresholds, threshold,
	            	repeat(thresholds.length(),
	            		current_threshold.store(thresholds.index(0).get("threshold"))
	            		if(threshold.ltoe(current_threshold),
	            			then(thresholds.index(0).get("value").return())
	            		)
	            	)
	            )

	            get_threshold_value(QUALITY_THRESHOLDS, 5).print()
            "#,
            "low",
        )
        .await;
    }

    #[tokio::test]
    async fn return_across_multiple_functions_and_loops() {
        test_interpreter(
            r#"
	            HEALTH.const("health")
	            STRENGTH.const("strength")
	            DODGE.const("dodge")

	            LOW_QUALITY.const("low")
	            MEDIUM_QUALITY.const("medium")
	            HIGH_QUALITY.const("high")

	            POTION.const("potion")

	            action_id_counter.store(0)
	            create_action.def(name, type, ability,
	            	action.local([
	            		id: action_id_counter.inc(),
	            		name: name,
	            		type: type,
	            		ability: ability,
	            	]).return()
	            )

	            HEALTH_POTION_QUALITY_RANGES.const([
	            	modifier: [
	            		low: [min: -50, max: 20],
	            		medium: [min: -5, max: 30],
	            		high: [min: 10, max: 50],
	            	],
	            	duration: [
	            		low: [min: 0, max: 0],
	            		medium: [min: 0, max: 0],
	            		high: [min: 0, max: 0],
	            	]
	            ])

	            STRENGTH_POTION_QUALITY_RANGES.const([
	            	modifier: [
	            		low: [min: -15, max: 15],
	            		medium: [min: -5, max: 25],
	            		high: [min: 10, max: 30],
	            	],
	            	duration: [
	            		low: [min: 2, max: 2],
	            		medium: [min: 1, max: 3],
	            		high: [min: 2, max: 4],
	            	]
	            ])

	            DODGE_POTION_QUALITY_RANGES.const([
	            	modifier: [
	            		low: [min: -15, max: 10],
	            		medium: [min: -2, max: 20],
	            		high: [min: 10, max: 50],
	            	],
	            	duration: [
	            		low: [min: 2, max: 2],
	            		medium: [min: 1, max: 3],
	            		high: [min: 2, max: 4],
	            	]
	            ])

	            create_potion_action.def(type, quality,
	            	name.local("test")

	            	if(type.eq(HEALTH),
	            		then(ranges.store(HEALTH_POTION_QUALITY_RANGES))
	            	)
	            	if(type.eq(STRENGTH),
	            		then(ranges.store(STRENGTH_POTION_QUALITY_RANGES))
	            	)
	            	if(type.eq(DODGE),
	            		then(ranges.store(DODGE_POTION_QUALITY_RANGES))
	            	)

	            	min_mod.store(ranges.get(["modifier", quality, "min"]))
	            	max_mod.store(ranges.get(["modifier", quality, "max"]))
	            	min_dur.store(ranges.get(["duration", quality, "min"]))
	            	max_dur.store(ranges.get(["duration", quality, "max"]))
	            	modifier.store(
	            		if(min_mod.lt(max_mod),
	            			then(random_range(min_mod, max_mod))
	            			else(min_mod.clone())
	            		)
	            	)
	            	duration.store(
	            		if(min_dur.lt(max_dur),
	            			then(random_range(min_dur, max_dur))
	            			else(min_dur.clone())
	            		)
	            	)

	            	potion.local([
	            		type: type,
	            		modifier: modifier.free(),
	            		duration: duration.free(),
	            		quality: quality,
	            	])
	            	action.store(create_action(name.free(), POTION, potion.free())).return()
	            )

	            QUALITY_THRESHOLDS.const([
	            	[
	            		threshold: 50,
	            		value: LOW_QUALITY,
	            	],
	            	[
	            		threshold: 80,
	            		value: MEDIUM_QUALITY,
	            	],
	            	[
	            		threshold: 100,
	            		value: HIGH_QUALITY,
	            	]
	            ])

	            get_threshold_value.def(thresholds, threshold,
	            	i.local(0)
	            	repeat(thresholds.length(),
	            		current_threshold.local(thresholds.index(i).get("threshold"))
	            		if(threshold.ltoe(current_threshold),
	            			then(thresholds.index(i).get("value").return())
	            		)
	            		i.inc()
	            	)
	            	return("low")
	            )

	            generate_starter_potions.def(n,
	            	potion_actions.store([])
	            	repeat(n,
	            		type.local(random_entry([HEALTH.clone(), STRENGTH.clone(), DODGE.clone()]))
	            		*return across multiple functions and loops must be fixed*
	            		quality.local(get_threshold_value(QUALITY_THRESHOLDS, random_range(0, 100)))
	            		potion.store(create_potion_action(type, quality))
	            		potion_actions.push(potion.free())
	            	)
	            	return(potion_actions)
	            )

	            actions.store(generate_starter_potions(5))
	            actions.length().print()
            "#,
            "5",
        )
        .await;
    }

    #[tokio::test]
    async fn if_statement() {
        test_interpreter(
            r#"
                if(true,
                    then(
                        print(true)
                    )
                )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn exit_inside_if() {
        test_interpreter(
            r#"
                if(true,
                    then(
                        print(true)
                        exit()
                    )
                )
                print(false)
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn then_with_multiple_values() {
        test_interpreter(
            r#"
                print(
                    if(true,
                        then(
                            print(0)
                            print(1)
                            print(2)
                            3
                        )
                    )
                )
            "#,
            "0123",
        )
        .await;
    }

    #[tokio::test]
    async fn false_if_statement() {
        test_interpreter(
            r#"
                if(false,
                    then(
                        print(true)
                    )
                )
            "#,
            "",
        )
        .await;
    }

    #[tokio::test]
    async fn if_else_statement() {
        test_interpreter(
            r#"
                if(false,
                    then(
                        print(true)
                    )
                    else(
                        print(false)
                    )
                )
            "#,
            "false",
        )
        .await;
    }

    #[tokio::test]
    async fn else_if_statement() {
        test_interpreter(
            r#"
                if(false,
                    then(
                        print(true)
                    )
                    else_if(true,
                        print("else_if")
                    )
                    else(
                        print(false)
                    )
                )
            "#,
            "else_if",
        )
        .await;
    }

    #[tokio::test]
    async fn else_missing() {
        assert!(
            interpreter_throws_err(
                r#"
                    if(false,
                        then(
                            print(true)
                        )
                        else_if(true,
                            print("else_if")
                        )
                    )
                "#,
                InterpreterErrorType::Command(CommandError::ElseIfMustBePairedWithElse).into()
            )
            .await
        );
    }

    #[tokio::test]
    async fn then_outside_if() {
        test_interpreter(
            r#"
                then(
                    print(true)
                )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn else_if_outside_if() {
        test_interpreter(
            r#"
                else_if(true,
                    print(true)
                )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn else_outside_if() {
        test_interpreter(
            r#"
                else(print(true))
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_if() {
        test_interpreter(
            r#"
                    if(true,
                        then(
                            if(true,
                                then(
                                    if(true,
                                        then(
                                            print(true)
                                        )
                                    )
                                )
                            )
                        )
                    )
                "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_if_else() {
        test_interpreter(
            r#"
            if(true,
                then(
                    if(false,
                        then(
                            print("false")
                        )
                        else(
                            print("true")
                        )
                    )
                )
            )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_if_else_2() {
        test_interpreter(
            r#"
            secret_number.store(5)
		    guess.store(5)
		    if(guess.is_number()
		    	then(
		    		if(guess.lt(secret_number)
		    			then(
		    				print("Too low, guess again.")
		    			)
		    			else_if(guess.gt(secret_number)
		    			    debug("shouldn't be here")
		    				print("Too high, guess again.")
		    			)
		    			else(
		    				print("You win")
		    				exit()
		    			)
		    		)
		    	)
		    	else(
		    		print("That's not a valid number")
		    	)
		    )
            "#,
            "You win",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_if_2() {
        test_interpreter(
            r#"
        test.def(
            if(true,
                then(
                    print("test")
                )
            )
        )

        while(true,
            if(true,
                then(
                    if(true,
                        then(
                            test()
                        )
                    )
                )
            )
            exit()
        )
        "#,
            "test",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_else_if_chain() {
        test_interpreter(
            r#"
            if(false,
                then(print("if"))
                else_if(false,
                    print("else_if_1")
                )
                else_if(false,
                    print("else_if_2")
                )
                else_if(true,
                    print("else_if_3")
                )
                else_if(false,
                    print("else_if_4")
                )
                else(print("else"))
            )
        "#,
            "else_if_3",
        )
        .await;
    }

    #[tokio::test]
    async fn nested_if_else_if_chain() {
        test_interpreter(
            r#"
            if(false,
                then(print("outer_if"))
                else_if(false,
                    if(false,
                        then(print("wrong"))
                        else_if(true,
                            print("inner_else_if")
                        )
                        else(print("wrong"))
                    )
                )
                else_if(true,
                    if(false,
                        then(print("wrong"))
                        else_if(false,
                            print("wrong")
                        )
                        else_if(true,
                            print("correct")
                        )
                        else(print("wrong"))
                    )
                )
                else(print("wrong"))
            )
        "#,
            "correct",
        )
        .await;
    }

    #[tokio::test]
    async fn else_if_chain_first_true() {
        test_interpreter(
            r#"
            if(false,
                then(print("wrong"))
                else_if(true,
                    print("else_if_1")
                )
                else_if(true,
                    print("else_if_2")
                )
                else_if(true,
                    print("else_if_3")
                )
                else(print("wrong"))
            )
        "#,
            "else_if_1",
        )
        .await;
    }

    #[tokio::test]
    async fn lone_then_after_if() {
        test_interpreter(
            r#"
            if(true,
                then(print("a"))
            )
            then(print("b"))
            "#,
            "ab",
        )
        .await;
    }

    #[tokio::test]
    async fn sequential_ifs_after_false() {
        test_interpreter(
            r#"
            if(false,
                then(print("wrong"))
            )
            if(true,
                then(print("correct"))
            )
        "#,
            "correct",
        )
        .await;
    }

    #[tokio::test]
    async fn switch_statement() {
        test_interpreter(
            r#"
                switch(
                    case(true,
                        print(0)
                    )
                    case(false,
                        print(1)
                    )
                    fallback(
                        print(2)
                    )
                )
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn case_with_multiple_values() {
        test_interpreter(
            r#"
                print(
                    switch(
                        case(true,
                            print(0)
                            print(1)
                            print(2)
                            3
                        )
                        case(false,
                            print(1)
                        )
                        fallback(
                            print(2)
                        )
                    )
                )
            "#,
            "0123",
        )
        .await;
    }

    #[tokio::test]
    async fn switch_statement_second_case() {
        test_interpreter(
            r#"
                switch(
                    case(false,
                        print(0)
                    )
                    case(true,
                        print(1)
                    )
                    fallback(
                        print(2)
                    )
                )
            "#,
            "1",
        )
        .await;
    }

    #[tokio::test]
    async fn switch_statement_fallback() {
        test_interpreter(
            r#"
                switch(
                    case(false,
                        print(0)
                    )
                    case(false,
                        print(1)
                    )
                    fallback(
                        print(2)
                    )
                )
            "#,
            "2",
        )
        .await;
    }

    #[tokio::test]
    async fn switch_statement_no_fallback() {
        let err = test_interpreter_err_type(
            r#"
                switch(
                    case(false,
                        print(0)
                    )
                    case(false,
                        print(1)
                    )
                )
            "#,
        )
        .await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::SwitchMustHaveSingleFallbackCommand)
        ))
    }

    #[tokio::test]
    async fn switch_statement_no_case() {
        test_interpreter(
            r#"
                switch(
                    fallback(
                        print(0)
                    )
                )
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn case_outside_switch() {
        test_interpreter(
            r#"
                case(true,
                    print(0)
                )
            "#,
            "0",
        )
        .await;
    }

    #[tokio::test]
    async fn fallback_oustide_of_switch() {
        test_interpreter(
            r#"
                fallback(
                    print(0)
                )
            "#,
            "0",
        )
        .await;
    }
}
