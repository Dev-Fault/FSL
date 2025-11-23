use std::{
    collections::{HashMap, VecDeque},
    sync::{Arc, atomic::Ordering},
};

use async_recursion::async_recursion;

use crate::{
    ErrorContext, FslError, InterpreterData,
    types::{
        FslType,
        command::{ArgPos, ArgRule, Command, UserCommand},
        value::Value,
    },
};

pub const ALL_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
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
    FslType::Var,
    FslType::Command,
];

pub const VAR_VALUES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Var,
];

pub const NUMERIC_TYPES: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

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

#[async_recursion]
async fn contains_float(
    values: &VecDeque<Value>,
    data: Arc<InterpreterData>,
) -> Result<bool, FslError> {
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
                if data
                    .clone()
                    .vars
                    .get_value(&var)?
                    .as_raw(data.clone())
                    .await?
                    .is_type(FslType::Float)
                {
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
pub async fn add(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
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
pub async fn sub(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
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
pub async fn mul(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
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
pub async fn div(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    if contains_float(&values, data.clone()).await? {
        let mut quotient = values.pop_front().unwrap().as_float(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            let value = value.as_float(data.clone()).await?;
            if value == 0.0 {
                return Err(FslError::DivisionByZero(ErrorContext::new(
                    DIV.into(),
                    "".into(),
                )));
            };
            quotient = quotient / value;
        }
        Ok(Value::Float(quotient))
    } else {
        let mut quotient = values.pop_front().unwrap().as_int(data.clone()).await?;
        while let Some(value) = values.pop_front() {
            let value = value.as_int(data.clone()).await?;
            if value == 0 {
                return Err(FslError::DivisionByZero(ErrorContext::new(
                    DIV.into(),
                    "".into(),
                )));
            };
            quotient = quotient / value;
        }
        Ok(Value::Int(quotient))
    }
}

pub const MODULUS: &str = "mod";
pub async fn modulus(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let mut remainder = values.pop_front().unwrap().as_int(data.clone()).await?;
    while let Some(value) = values.pop_front() {
        let value = value.as_int(data.clone()).await?;
        if value == 0 {
            return Err(FslError::DivisionByZero(ErrorContext::new(
                MODULUS.into(),
                "".into(),
            )));
        };
        remainder = remainder % value;
    }
    Ok(Value::Int(remainder))
}

pub const PRECISION_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Int]),
];
pub const PRECISION: &str = "precision";
pub async fn precision(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let num = arg_0.as_float(data.clone()).await?;
    let precision = arg_1.as_int(data).await?;
    let formatted = format!("{:.prec$}", num, prec = precision as usize);

    Ok(Value::Text(formatted))
}

pub const STORE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), VAR_VALUES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Var]),
];
pub const STORE: &str = "store";
pub async fn store(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let label = arg_0.get_var_label()?;

    match arg_1 {
        Value::Var(var) => {
            let value = data.vars.get_value(&var)?.as_raw(data.clone()).await?;
            data.vars.insert_value(label, value)?;
        }
        Value::Command(command) => {
            data.clone()
                .vars
                .insert_value(label, command.execute(data.clone()).await?)?;
        }
        Value::None => {
            return Err(FslError::CannotStoreValueInVar(format!(
                "Cannot store {} in a var",
                FslType::None.as_str()
            )));
        }
        _ => {
            data.vars.insert_value(label, arg_1)?;
        }
    }
    Ok(data.vars.get_value(label)?)
}

pub const CLONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub const CLONE: &str = "clone";
pub async fn clone(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let value = command
        .take_args()
        .pop_front()
        .unwrap()
        .get_var_value(data)?;
    Ok(value.clone())
}

pub const FREE: &str = "free";
pub const FREE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn free(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let arg_0 = command.take_args().pop_front().unwrap();
    let label = arg_0.get_var_label()?;
    match data.vars.remove_value(label) {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub const PRINT: &str = "print";
pub async fn print(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let values = command.take_args();
    let mut output = String::new();

    for value in values {
        output.push_str(&value.as_text(data.clone()).await?);
    }

    let mut std_out = data.output.lock().await;
    std_out.push_str(&output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &'static [ArgRule] =
    &[ArgRule::new(ArgPos::AnyFrom(0), &[FslType::Command])];
pub const SCOPE: &str = "";
pub async fn scope(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let values = command.take_args();
    for value in values {
        value.as_command()?.execute(data.clone()).await?;
    }

    Ok(Value::None)
}

pub const EQ_RULES: &'static [ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NON_NONE_VALUES),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const EQ: &str = "eq";
pub async fn eq(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let a = values.pop_front().unwrap().as_raw(data.clone()).await?;
    let b = values.pop_front().unwrap().as_raw(data.clone()).await?;

    Ok(Value::Bool(a.eq(&b)?))
}

pub const GT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const GT: &str = "gt";
pub async fn gt(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
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

pub const LT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const LT: &str = "lt";
pub async fn lt(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
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

pub const NOT_RULES: &[ArgRule; 1] = &[ArgRule::new(ArgPos::Index(0), LOGIC_TYPES)];
pub const NOT: &str = "not";
pub async fn not(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let a = values.pop_front().unwrap().as_bool(data.clone()).await?;
    Ok((!a).into())
}

pub const AND_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), LOGIC_TYPES)];
pub const AND: &str = "and";
pub async fn and(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 && value.as_bool(data.clone()).await?;
    }
    Ok(arg_0.into())
}

pub const OR_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), LOGIC_TYPES)];
pub const OR: &str = "or";
pub async fn or(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 || value.as_bool(data.clone()).await?;
    }
    Ok(arg_0.into())
}

pub const IF_THEN_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
];
pub const IF_THEN: &str = "if_then";
pub async fn if_then(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    if arg_0.as_bool(data.clone()).await? {
        let result = arg_1.as_command()?.execute(data).await?;
        Ok(result)
    } else {
        Ok(Value::None)
    }
}

pub const IF_THEN_ELSE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::Index(1), &[FslType::Command]),
    ArgRule::new(ArgPos::Index(2), &[FslType::Command]),
];
pub const IF_THEN_ELSE: &str = "if_then_else";
pub async fn if_then_else(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    if arg_0.as_bool(data.clone()).await? {
        let result = arg_1.as_command()?.execute(data).await?;
        Ok(result)
    } else {
        let result = arg_2.as_command()?.execute(data).await?;
        Ok(result)
    }
}

pub const WHILE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), LOGIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const WHILE_LOOP: &str = "while";
pub async fn while_command(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let while_condition = values.pop_front().unwrap();

    let mut final_value = Value::None;

    data.inside_loop.store(true, Ordering::Relaxed);

    'outer: while while_condition.clone().as_bool(data.clone()).await? {
        for command in &values {
            let command = command.clone().as_command()?;
            final_value = command.execute(data.clone()).await?;

            if data.continue_flag.load(Ordering::Relaxed) {
                data.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.break_flag.load(Ordering::Relaxed) {
                data.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops(WHILE_LOOP).await?;
    }

    data.inside_loop.store(false, Ordering::Relaxed);

    Ok(final_value)
}

pub const REPEAT_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const REPEAT: &str = "repeat";
pub async fn repeat(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let repetitions = values.pop_front().unwrap().as_int(data.clone()).await?;
    let mut final_value = Value::None;

    data.inside_loop.store(true, Ordering::Relaxed);

    'outer: for _ in 0..repetitions {
        for command in &values {
            let command = command.clone().as_command()?;
            final_value = command.execute(data.clone()).await?;
            if data.continue_flag.load(Ordering::Relaxed) {
                data.continue_flag.store(false, Ordering::Relaxed);
                continue 'outer;
            }
            if data.break_flag.load(Ordering::Relaxed) {
                data.break_flag.store(false, Ordering::Relaxed);
                break 'outer;
            }
        }
        data.increment_loops(REPEAT).await?;
    }

    data.inside_loop.store(false, Ordering::Relaxed);

    Ok(final_value)
}

pub const INDEX_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const INDEX: &str = "index";
pub async fn index(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    let array = arg_0.as_raw(data.clone()).await?;

    if array.is_type(FslType::List) {
        let list = array.as_list(data.clone()).await?;
        let i = arg_1.as_int(data.clone()).await?;
        match list.get(i as usize) {
            Some(value) => Ok(value.clone().as_raw(data).await?),
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                INDEX.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            ))),
        }
    } else {
        let text = array.as_text(data.clone()).await?;
        let i = arg_1.as_int(data).await?;
        match text.chars().nth(i as usize) {
            Some(char) => Ok(char.into()),
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                INDEX.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            ))),
        }
    }
}

pub const REMOVE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const REMOVE: &str = "remove";
pub async fn remove(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;
    let i = arg_1.as_int(data.clone()).await? as usize;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        match list.get(i) {
            Some(_) => {
                list.remove(i);
                let list = Value::List(list);
                if let Some(label) = var_label {
                    data.vars.insert_value(&label, list.clone())?;
                }
                Ok(list)
            }
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                REMOVE.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            ))),
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        match text.chars().nth(i) {
            Some(_) => {
                text.remove(i);
                let text = Value::Text(text);
                if let Some(label) = var_label {
                    data.vars.insert_value(&label, text.clone())?;
                }
                Ok(text)
            }
            None => Err(FslError::OutOfBounds(ErrorContext::new(
                REMOVE.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            ))),
        }
    }
}

pub const LENGTH_RULES: [ArgRule; 1] = [ArgRule::new(
    ArgPos::Index(0),
    &[FslType::List, FslType::Text],
)];
pub const LENGTH: &str = "length";
pub async fn length(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let array = values.pop_front().unwrap().as_raw(data.clone()).await?;

    if array.is_type(FslType::List) {
        let list = array.as_list(data).await?;
        Ok((list.len() as i64).into())
    } else {
        let text = array.as_text(data).await?;
        Ok((text.len() as i64).into())
    }
}

pub const SWAP_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(2), NUMERIC_TYPES),
];
pub const SWAP: &str = "swap";
pub async fn swap(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;
    let a = arg_1.as_int(data.clone()).await? as usize;
    let b = arg_2.as_int(data.clone()).await? as usize;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        if list.get(a).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", a, list),
            )))
        } else if list.get(b).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of list: {:?}", b, list),
            )))
        } else {
            let tmp = list[a].clone();
            list[a] = list[b].clone();
            list[b] = tmp;
            let list = Value::List(list);
            if let Some(label) = var_label {
                data.vars.insert_value(&label, list.clone())?;
            }
            Ok(list)
        }
    } else {
        let text = array.as_text(data.clone()).await?;

        if text.chars().nth(a).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", a, text),
            )))
        } else if text.chars().nth(b).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                SWAP.into(),
                format!("index {} was outside the bounds of text: {}", b, text),
            )))
        } else {
            let mut text = text.chars().collect::<Vec<char>>();
            let tmp = text[a].clone();
            text[a] = text[b].clone();
            text[b] = tmp;
            let text = Value::Text(text.iter().collect::<String>().into());
            if let Some(label) = var_label {
                data.vars.insert_value(&label, text.clone())?;
            }
            Ok(text)
        }
    }
}

pub const REPLACE_RULES: [ArgRule; 3] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Int]),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub const REPLACE: &str = "replace";
pub async fn replace(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;
    let i = arg_1.as_int(data.clone()).await? as usize;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let replacement = arg_2.as_raw(data.clone()).await?;
        if list.get(i).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                REPLACE.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            )))
        } else {
            list[i] = replacement;
            let list = Value::List(list);
            if let Some(label) = var_label {
                data.vars.insert_value(&label, list.clone())?;
            }
            Ok(list)
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let replacement = arg_2.as_text(data.clone()).await?;

        if text.chars().nth(i).is_none() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                REPLACE.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            )))
        } else {
            text.replace_range(i..i + 1, &replacement);
            let text = Value::Text(text);
            if let Some(label) = var_label {
                data.vars.insert_value(&label, text.clone())?;
            }
            Ok(text)
        }
    }
}

pub const SEARCH_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(2), &[FslType::Text]),
];
pub const SEARCH_REPLACE: &str = "search_replace";
pub async fn search_replace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let input = values.pop_front().unwrap().as_text(data.clone()).await?;
    let from = values.pop_front().unwrap().as_text(data.clone()).await?;
    let to = values.pop_front().unwrap().as_text(data).await?;

    let input = input.replace(&from, &to);

    Ok(Value::Text(input))
}

pub const SLICE_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::List]),
    ArgRule::new(ArgPos::Index(2), &[FslType::Text]),
];
pub const SLICE_REPLACE: &str = "slice_replace";
pub async fn slice_replace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let mut input = values.pop_front().unwrap().as_text(data.clone()).await?;
    let mut range = values.pop_front().unwrap().as_list(data.clone()).await?;
    let with = values.pop_front().unwrap().as_text(data.clone()).await?;

    if range.len() != 2 {
        return Err(FslError::WrongNumberOfArgs(ErrorContext::new(
            format!("{}", SLICE_REPLACE),
            format!("list should contain two items, min and max values for range"),
        )));
    }

    let (from, to) = (
        std::mem::take(&mut range[0]).as_int(data.clone()).await? as usize,
        std::mem::take(&mut range[1]).as_int(data).await? as usize,
    );

    if from > input.len() || to > input.len() || from > to {
        return Err(FslError::OutOfBounds(ErrorContext::new(
            SLICE_REPLACE.into(),
            format!("{} or {} was outside the bounds of text", from, to,),
        )));
    }

    input.replace_range(from..to, &with);

    Ok(Value::Text(input))
}

pub const INSERT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(2), NON_NONE_VALUES),
];
pub const INSERT: &str = "insert";
pub async fn insert(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let arg_2 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;
    let i = arg_1.as_int(data.clone()).await? as usize;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let to_insert = arg_2.as_raw(data.clone()).await?;

        if i > list.len() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                INSERT.into(),
                format!("index {} was outside the bounds of list: {:?}", i, list),
            )))
        } else {
            list.insert(i, to_insert);
            let list = Value::List(list);
            if let Some(label) = var_label {
                data.vars.insert_value(&label, list.clone())?;
            }
            Ok(list)
        }
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let to_insert = arg_2.as_text(data.clone()).await?;

        if i > text.len() {
            Err(FslError::OutOfBounds(ErrorContext::new(
                INSERT.into(),
                format!("index {} was outside the bounds of text: {}", i, text),
            )))
        } else {
            text.insert_str(i, &to_insert);
            let text = Value::Text(text);
            if let Some(label) = var_label {
                data.vars.insert_value(&label, text.clone())?;
            }
            Ok(text)
        }
    }
}

pub const PUSH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const PUSH: &str = "push";
pub async fn push(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;
        let to_push = arg_1.as_raw(data.clone()).await?;

        list.push(to_push);
        let list = Value::List(list);
        if let Some(label) = var_label {
            data.vars.insert_value(&label, list.clone())?;
        }
        Ok(list)
    } else {
        let mut text = array.as_text(data.clone()).await?;
        let to_push = arg_1.as_text(data.clone()).await?;

        text.push_str(&to_push);
        let text = Value::Text(text);
        if let Some(label) = var_label {
            data.vars.insert_value(&label, text.clone())?;
        }
        Ok(text)
    }
}

pub const POP_RULES: &[ArgRule] = &[ArgRule::new(
    ArgPos::Index(0),
    &[FslType::List, FslType::Text],
)];
pub const POP: &str = "pop";
pub async fn pop(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let arg_0 = values.pop_front().unwrap();

    let var_label = if let Ok(label) = arg_0.get_var_label() {
        Some(label.to_string())
    } else {
        None
    };

    let array = arg_0.as_raw(data.clone()).await?;

    if array.is_type(FslType::List) {
        let mut list = array.as_list(data.clone()).await?;

        let ret_value = list.pop();
        let list = Value::List(list);
        if let Some(label) = var_label {
            data.vars.insert_value(&label, list)?;
        }
        Ok(ret_value.unwrap_or(Value::None))
    } else {
        let mut text = array.as_text(data.clone()).await?;

        let ret_value = text.pop();
        let text = Value::Text(text);
        if let Some(label) = var_label {
            data.vars.insert_value(&label, text)?;
        }
        match ret_value {
            Some(ch) => Ok(Value::Text(ch.to_string())),
            None => Ok(Value::None),
        }
    }
}

pub const INC_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub const INC: &str = "inc";
pub async fn inc(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let values = command.take_args();
    let n = values[0].clone().as_int(data.clone()).await?;
    let new_value = Value::Int(n + 1);
    data.vars
        .insert_value(&values[0].get_var_label()?, new_value.clone())?;
    Ok(new_value)
}

pub const DEC_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub const DEC: &str = "dec";
pub async fn dec(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let values = command.take_args();
    let n = values[0].clone().as_int(data.clone()).await?;
    let new_value = Value::Int(n - 1);
    data.vars
        .insert_value(&values[0].get_var_label()?, new_value.clone())?;
    Ok(new_value)
}

pub const CONTAINS_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::List, FslType::Text]),
    ArgRule::new(ArgPos::Index(1), NON_NONE_VALUES),
];
pub const CONTAINS: &str = "contains";
pub async fn contains(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let a = values.pop_front().unwrap().as_raw(data.clone()).await?;
    let b = values.pop_front().unwrap().as_raw(data.clone()).await?;
    if let Value::List(list) = a {
        for value in list {
            match value.as_raw(data.clone()).await?.eq(&b) {
                Ok(is_eq) => {
                    if is_eq {
                        return Ok(Value::Bool(true));
                    }
                }
                Err(_) => {}
            }
        }
        return Ok(Value::Bool(false));
    } else {
        let text = a.as_text(data.clone()).await?;
        return Ok(Value::Bool(text.contains(&b.as_text(data).await?)));
    }
}

pub const STARTS_WITH_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const STARTS_WITH: &str = "starts_with";
pub async fn starts_with(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap().as_text(data.clone()).await?;
    let arg_1 = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(arg_0.starts_with(&arg_1).into())
}

pub const ENDS_WITH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const ENDS_WITH: &str = "ends_with";
pub async fn ends_with(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap().as_text(data.clone()).await?;
    let arg_1 = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(arg_0.ends_with(&arg_1).into())
}

pub const CONCAT_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub const CONCAT: &str = "concat";
pub async fn concat(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let values = command.take_args();

    let mut cat_string = String::new();

    for value in values {
        cat_string.push_str(&value.as_text(data.clone()).await?);
    }

    Ok(cat_string.into())
}

pub const CAPITALIZE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub const CAPITALIZE: &str = "capitalize";
pub async fn capitalize(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    if text.len() < 1 {
        Ok("".into())
    } else {
        Ok(format!("{}{}", text[0..1].to_uppercase(), &text[1..]).into())
    }
}

pub const UPPERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub const UPPERCASE: &str = "uppercase";
pub async fn uppercase(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.to_uppercase().into())
}

pub const LOWERCASE_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub const LOWERCASE: &str = "lowercase";
pub async fn lowercase(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.to_lowercase().into())
}

pub const REMOVE_WHITESPACE_RULES: [ArgRule; 1] =
    [ArgRule::new(ArgPos::Index(0), &[FslType::Text])];
pub const REMOVE_WHITESPACE: &str = "remove_whitespace";
pub async fn remove_whitespace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.split_whitespace().collect::<String>().into())
}

pub const SPLIT_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const SPLIT: &str = "split";
pub async fn split(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let text_to_split = values.pop_front().unwrap().as_text(data.clone()).await?;
    let pattern = values.pop_front().unwrap().as_text(data.clone()).await?;
    let split = text_to_split
        .split(&pattern)
        .map(|s| Value::Text(s.to_string()))
        .collect::<Vec<Value>>();
    Ok(Value::List(split))
}

pub const RANDOM_RANGE_RULES: [ArgRule; 2] = [
    ArgRule::new(ArgPos::Index(0), NUMERIC_TYPES),
    ArgRule::new(ArgPos::Index(1), NUMERIC_TYPES),
];
pub const RANDOM_RANGE: &str = "random_range";
pub async fn random_range(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    if contains_float(&values, data.clone()).await? {
        let min = values.pop_front().unwrap().as_float(data.clone()).await?;
        let max = values.pop_front().unwrap().as_float(data.clone()).await?;
        if min >= max {
            Err(FslError::InvalidRange(ErrorContext::new(
                RANDOM_RANGE.into(),
                "min must be greater than max".into(),
            )))
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let min = values.pop_front().unwrap().as_int(data.clone()).await?;
        let max = values.pop_front().unwrap().as_int(data.clone()).await?;
        if min >= max {
            Err(FslError::InvalidRange(ErrorContext::new(
                RANDOM_RANGE.into(),
                "min must be greater than max".into(),
            )))
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub const RANDOM_ENTRY_RULES: [ArgRule; 1] = [ArgRule::new(ArgPos::Index(0), &[FslType::List])];
pub const RANDOM_ENTRY: &str = "random_entry";
pub async fn random_entry(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let list = values.pop_front().unwrap().as_list(data).await?;
    Ok(list[rand::random_range(0..list.len())].clone())
}

pub const DEF_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Text]),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Var, FslType::Command]),
];
pub const DEF: &str = "def";
pub async fn def(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();
    let label = values.pop_front().unwrap().get_var_label()?.to_string();
    let mut local_vars: VecDeque<String> = VecDeque::new();
    let mut commands: Vec<Command> = Vec::new();

    let values_len = values.len();
    let mut encountered_command = false;
    for i in 0..values_len {
        if values[i].is_type(FslType::Var) {
            if encountered_command {
                return Err(FslError::WrongOrderOfArgs(ErrorContext::new(
                    DEF.into(),
                    format!("argument labels to command must come before inner commands"),
                )));
            }
            let var_label = values[i].get_var_label()?.to_string();
            local_vars.push_back(var_label);
        } else {
            encountered_command = true;
            let command = values[i].clone().as_command()?;
            commands.push(command);
        }
    }

    let mut user_commands = data.user_commands.lock().await;
    let user_command = UserCommand {
        label: label.clone(),
        vars: local_vars,
        commands: commands,
    };
    user_commands.insert(label.clone(), user_command);

    Ok(Value::None)
}

fn substitute_args(
    command: &mut Command,
    var_map: &HashMap<String, Value>,
) -> Result<(), FslError> {
    for arg in command.get_args_mut() {
        if arg.is_type(FslType::Var)
            && let Some(value) = var_map.get(arg.get_var_label()?)
        {
            *arg = value.clone();
        } else if arg.is_type(FslType::Command) {
            let mut inner_command = std::mem::take(arg).as_command()?;
            substitute_args(&mut inner_command, var_map)?;
            *arg = Value::Command(inner_command);
        } else {
        }
    }

    Ok(())
}

pub const RUN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NON_NONE_VALUES)];
pub async fn run(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    let mut values = command.take_args();

    let command_label = values.pop_front().unwrap().get_var_label()?.to_string();

    let commands_lock = data.user_commands.lock().await;

    let mut var_labels = commands_lock.get(&command_label).unwrap().vars.clone();
    let commands = commands_lock.get(&command_label).unwrap().commands.clone();
    drop(commands_lock);

    if values.len() != var_labels.len() {
        return Err(FslError::WrongNumberOfArgs(ErrorContext::new(
            command_label,
            format!(
                "expected {} args but got {}",
                var_labels.len(),
                values.len()
            ),
        )));
    }

    let mut var_map: HashMap<String, Value> = HashMap::new();

    let vars = var_labels.len();
    for _ in 0..vars {
        let mut value = values.pop_front().unwrap();
        if value.is_type(FslType::Command) || value.is_type(FslType::List) {
            value = values.pop_front().unwrap().as_raw(data.clone()).await?;
        };
        var_map.insert(var_labels.pop_front().unwrap(), value);
    }

    let mut final_value = Value::None;
    for mut command in commands {
        substitute_args(&mut command, &var_map)?;
        final_value = command.execute(data.clone()).await?;
    }

    Ok(final_value)
}

pub const BREAK: &str = "break";
pub async fn break_command(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    if data.inside_loop.load(Ordering::Relaxed) {
        data.break_flag
            .store(true, std::sync::atomic::Ordering::Relaxed);
    } else {
        return Err(FslError::BreakCalledOutsideLoop);
    }

    Ok(Value::None)
}

pub const CONTINUE: &str = "continue";
pub async fn continue_command(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, FslError> {
    if data.inside_loop.load(Ordering::Relaxed) {
        data.continue_flag
            .store(true, std::sync::atomic::Ordering::Relaxed);
    } else {
        return Err(FslError::ContinueCalledOutsideLoop);
    }
    Ok(Value::None)
}

pub const EXIT: &str = "exit";
pub async fn exit(command: Command, data: Arc<InterpreterData>) -> Result<Value, FslError> {
    return Err(FslError::ProgramExited);
}

#[cfg(test)]
mod tests {
    use crate::{FslError, FslInterpreter};

    async fn test_interpreter(code: &str, expected_output: &str) {
        let result = FslInterpreter::new().interpret(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    async fn test_interpreter_embedded(code: &str, expected_output: &str) {
        let result = FslInterpreter::new().interpret_embedded_code(code).await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    async fn test_interpreter_err(code: &str) -> FslError {
        let result = FslInterpreter::new().interpret(code).await;
        dbg!(&result);
        assert!(result.is_err());
        result.err().unwrap()
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
        let err = test_interpreter_err("print(div(1, 0))").await;
        assert!(matches!(err, FslError::DivisionByZero(_)));
    }

    #[tokio::test]
    async fn mod_by_zero() {
        let err = test_interpreter_err("print(mod(1, 0))").await;
        assert!(matches!(err, FslError::DivisionByZero(_)));
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
        let err = test_interpreter_err("a.store(1) print(a) a.free() print(a)").await;
        assert!(matches!(err, FslError::NonExistantVar(_)));
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
        let err = test_interpreter_err(r#"(1)"#).await;
        assert!(matches!(err, FslError::WrongTypeOfArgs(_)));
    }

    #[tokio::test]
    async fn int_cmp() {
        test_interpreter(r#"print(eq(1, 2))"#, "false").await;
        test_interpreter(r#"print(eq(2, 2))"#, "true").await;

        test_interpreter(r#"print(lt(1, 2))"#, "true").await;
        test_interpreter(r#"print(lt(2, 1))"#, "false").await;
        test_interpreter(r#"print(lt(2, 2))"#, "false").await;

        test_interpreter(r#"print(gt(1, 2))"#, "false").await;
        test_interpreter(r#"print(gt(2, 1))"#, "true").await;
        test_interpreter(r#"print(gt(2, 2))"#, "false").await;
    }

    #[tokio::test]
    async fn float_cmp() {
        test_interpreter(r#"print(eq(1.2, 1.1))"#, "false").await;
        test_interpreter(r#"print(eq(1.1, 1.1))"#, "true").await;

        test_interpreter(r#"print(lt(1.1, 1.2))"#, "true").await;
        test_interpreter(r#"print(lt(2.1, 1.02))"#, "false").await;
        test_interpreter(r#"print(lt(2.0, 2.0))"#, "false").await;

        test_interpreter(r#"print(gt(1.4, 2.2))"#, "false").await;
        test_interpreter(r#"print(gt(2.2, 1.2))"#, "true").await;
        test_interpreter(r#"print(gt(2.0, 2.0))"#, "false").await;
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
        let err = test_interpreter_err(r#"print(eq(1, "a"))"#).await;
        assert!(matches!(err, FslError::FailedValueParse(_)));
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
        test_interpreter(r#"if_then(true, print("true"))"#, "true").await;
        test_interpreter(r#"if_then(false, print("true"))"#, "").await;
    }

    #[tokio::test]
    async fn if_then_else() {
        test_interpreter(
            r#"if_then_else(true, print("true"), print("false"))"#,
            "true",
        )
        .await;
        test_interpreter(
            r#"if_then_else(false, print("true"), print("false"))"#,
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
        test_interpreter(r#"nums.store([1, 2, 3]) nums.remove(1).print()"#, "[1, 3]").await;
        test_interpreter(r#"text.store("text") text.remove(1).print()"#, "txt").await;
    }

    #[tokio::test]
    async fn length() {
        test_interpreter(r#"nums.store([1, 2, 3]) nums.length().print()"#, "3").await;
        test_interpreter(r#"nums.store("123") nums.length().print()"#, "3").await;
    }

    #[tokio::test]
    async fn swap() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.swap(0, 2).print()"#,
            "[3, 2, 1]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.swap(0, 2).print()"#, "321").await;
    }

    #[tokio::test]
    async fn replace() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.replace(0, 2).print()"#,
            "[2, 2, 3]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.replace(0, 2).print()"#, "223").await;
    }

    #[tokio::test]
    async fn insert() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.insert(0, 0).print()"#,
            "[0, 1, 2, 3]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.insert(0, 0).print()"#, "0123").await;
    }

    #[tokio::test]
    async fn push() {
        test_interpreter(
            r#"nums.store([1, 2, 3]) nums.push(0).print()"#,
            "[1, 2, 3, 0]",
        )
        .await;
        test_interpreter(r#"nums.store("123") nums.push(0).print()"#, "1230").await;
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
    async fn dec() {
        test_interpreter(r#"i.store(1) i.dec().print()"#, "0").await;
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

    #[tokio::test]
    async fn uppercase() {
        test_interpreter(r#"uppercase("hey").print()"#, "HEY").await;
    }

    #[tokio::test]
    async fn lowercase() {
        test_interpreter(r#"lowercase("HEY").print()"#, "hey").await;
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
    async fn r#break() {
        test_interpreter(r#"while(true, print("1"), break())"#, r#"1"#).await;
    }

    #[tokio::test]
    async fn break_nested() {
        test_interpreter(
            r#"i.store(0) repeat(2, repeat(2, if_then(i.eq(0), (i.inc(), break()) ), i.print() ))"#,
            r#"11"#,
        )
        .await;
    }

    #[tokio::test]
    async fn break_outside_command() {
        let err = test_interpreter_err(r#"break()"#).await;
        assert!(matches!(err, FslError::BreakCalledOutsideLoop));
    }

    #[tokio::test]
    async fn r#continue() {
        test_interpreter(
            r#"i.store(0) repeat(2, if_then(i.eq(0), (i.inc(), continue())), print("1"))"#,
            r#"1"#,
        )
        .await;
    }

    #[tokio::test]
    async fn continue_deeply_nested() {
        test_interpreter(
        r#"i.store(0) repeat(2, repeat(2, repeat(2, if_then(i.eq(0), (i.inc(), continue()) ), i.print() )))"#,
        r#"1111111"#,
    )
    .await;
    }

    #[tokio::test]
    async fn continue_outside_command() {
        let err = test_interpreter_err(r#"continue()"#).await;
        assert!(matches!(err, FslError::ContinueCalledOutsideLoop));
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
    async fn search_replace() {
        test_interpreter(
            r#"search_replace("this text has not been replaced", "not", "").print()"#,
            "this text has  been replaced",
        )
        .await;
    }
}
