use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
    mem,
    sync::{Arc, atomic::Ordering},
    time::Duration,
};

use async_recursion::async_recursion;
use rand::seq::SliceRandom;
use tokio_stream::StreamExt;

use crate::{
    FslInterpreter, InterpreterData,
    error::{CommandError, ExecutionError, ValueError},
    register_command,
    types::{
        FslType,
        command::{ArgPos, ArgRule, Command, Handler},
        value::{FslMap, Value},
    },
    vars::VarEntry,
};

use futures::FutureExt;

pub const ANY: &[FslType] = &[
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

pub const NOT_NONE: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const STORABLE: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
    FslType::Var,
    FslType::Command,
];

pub const LITERAL: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Bool,
    FslType::Text,
    FslType::List,
    FslType::Map,
];

pub const MAYBE_NUMBER: &[FslType] = &[
    FslType::Int,
    FslType::Float,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const NUMBER: &[FslType] = &[FslType::Int, FslType::Float, FslType::Text];

pub const MAYBE_INT: &[FslType] = &[FslType::Int, FslType::Command, FslType::Var, FslType::Text];

pub const MAYBE_KEY: &[FslType] = &[
    FslType::List,
    FslType::Int,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const KEY: &[FslType] = &[FslType::List, FslType::Int, FslType::Text];

pub const MAYBE_LIST_KEY: &[FslType] = &[
    FslType::List,
    FslType::Int,
    FslType::Command,
    FslType::Var,
    FslType::Text,
];

pub const LIST_KEY: &[FslType] = &[FslType::List, FslType::Int];

pub const MAYBE_MAP_KEY: &[FslType] =
    &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const MAP_KEY: &[FslType] = &[FslType::List, FslType::Text];

pub const MAYBE_INDEXABLE: &[FslType] =
    &[FslType::List, FslType::Command, FslType::Var, FslType::Text];

pub const INDEXABLE: &[FslType] = &[FslType::List, FslType::Text];

pub const MAYBE_LIST: &[FslType] = &[FslType::List, FslType::Command, FslType::Var];

pub const MAYBE_MAP: &[FslType] = &[FslType::Map, FslType::Command, FslType::Var];

pub const MAYBE_COLLECTION: &[FslType] = &[
    FslType::Map,
    FslType::List,
    FslType::Text,
    FslType::Command,
    FslType::Var,
];

pub const COLLECTION: &[FslType] = &[FslType::Map, FslType::List, FslType::Text];

pub const MAYBE_TEXT: &[FslType] = &[FslType::Command, FslType::Var, FslType::Text];

pub const MAYBE_BOOL: &[FslType] = &[FslType::Bool, FslType::Command, FslType::Var, FslType::Text];

pub const NO_ARGS: &[ArgRule] = &[ArgRule::none()];

pub const MATH_RULES: &[ArgRule] = &[
    ArgRule {
        position: ArgPos::AnyFrom(0),
        valid_types: MAYBE_NUMBER,
    },
    ArgRule {
        position: ArgPos::Index(0),
        valid_types: MAYBE_NUMBER,
    },
    ArgRule {
        position: ArgPos::Index(1),
        valid_types: MAYBE_NUMBER,
    },
];

pub fn register_std(interpreter: &mut FslInterpreter) {
    register_command!(interpreter, ADD, MATH_RULES, add);
    register_command!(interpreter, SUB, MATH_RULES, sub);
    register_command!(interpreter, MUL, MATH_RULES, mul);
    register_command!(interpreter, DIV, MATH_RULES, div);
    register_command!(interpreter, MODULUS, MATH_RULES, modulus);
    register_command!(interpreter, CLAMP, CLAMP_RULES, clamp);
    register_command!(interpreter, CLAMP_MIN, CLAMP_MIN_RULES, clamp_min);
    register_command!(interpreter, CLAMP_MAX, CLAMP_MAX_RULES, clamp_max);
    register_command!(interpreter, PRECISION, PRECISION_RULES, precision);
    register_command!(interpreter, STORE, STORE_RULES, store);
    register_command!(interpreter, CONST, CONST_RULES, r#const);
    register_command!(interpreter, LOCAL, LOCAL_RULES, local);
    register_command!(interpreter, UPDATE, UPDATE_RULES, update);
    register_command!(interpreter, CLONE, CLONE_RULES, clone);
    register_command!(interpreter, TAKE, TAKE_RULES, take);
    register_command!(interpreter, PRINT, PRINT_RULES, print);
    register_command!(interpreter, ARGS, ARGS_RULES, args);
    register_command!(interpreter, DEBUG, DEBUG_RULES, debug);
    register_command!(interpreter, SCOPE, SCOPE_RULES, scope);
    register_command!(interpreter, EQ, EQ_RULES, eq);
    register_command!(interpreter, GT, GT_RULES, gt);
    register_command!(interpreter, GTOE, GTOE_RULES, gtoe);
    register_command!(interpreter, LT, LT_RULES, lt);
    register_command!(interpreter, LTOE, LTOE_RULES, ltoe);
    register_command!(interpreter, NOT, NOT_RULES, not);
    register_command!(interpreter, AND, AND_RULES, and);
    register_command!(interpreter, OR, OR_RULES, or);
    register_command!(interpreter, IF, IF_RULES, r#if);
    register_command!(interpreter, THEN, BLOCK_RULES, block);
    register_command!(interpreter, ELSE_IF, BLOCK_RULES, block);
    register_command!(interpreter, ELSE, BLOCK_RULES, block);
    register_command!(interpreter, SWITCH, SWITCH_RULES, switch);
    register_command!(interpreter, CASE, BLOCK_RULES, block);
    register_command!(interpreter, FALLBACK, BLOCK_RULES, block);
    register_command!(interpreter, WHILE_LOOP, WHILE_RULES, while_command);
    register_command!(interpreter, REPEAT, REPEAT_RULES, repeat);
    register_command!(interpreter, INDEX, INDEX_RULES, index);
    register_command!(interpreter, GET, GET_RULES, get);
    register_command!(interpreter, SET, SET_RULES, set);
    register_command!(interpreter, LENGTH, LENGTH_RULES, length);
    register_command!(interpreter, SWAP, SWAP_RULES, swap);
    register_command!(interpreter, INSERT, INSERT_RULES, insert);
    register_command!(interpreter, REMOVE, REMOVE_RULES, remove);
    register_command!(interpreter, PUSH, PUSH_RULES, push);
    register_command!(interpreter, POP, POP_RULES, pop);
    register_command!(interpreter, REPLACE, REPLACE_RULES, replace);
    register_command!(
        interpreter,
        SLICE_REPLACE,
        SLICE_REPLACE_RULES,
        slice_replace
    );
    register_command!(
        interpreter,
        SEARCH_REPLACE,
        SEARCH_REPLACE_RULES,
        search_replace
    );
    register_command!(interpreter, REVERSE, REVERSE_RULES, reverse);
    register_command!(interpreter, INC, INC_RULES, inc);
    register_command!(interpreter, DEC, DEC_RULES, dec);
    register_command!(interpreter, CONTAINS, CONTAINS_RULES, contains);
    register_command!(interpreter, STARTS_WITH, STARTS_WITH_RULES, starts_with);
    register_command!(interpreter, ENDS_WITH, ENDS_WITH_RULES, ends_with);
    register_command!(interpreter, CONCAT, CONCAT_RULES, concat);
    register_command!(interpreter, CAPITALIZE, CAPITALIZE_RULES, capitalize);
    register_command!(interpreter, UPPERCASE, UPPERCASE_RULES, uppercase);
    register_command!(interpreter, LOWERCASE, LOWERCASE_RULES, lowercase);
    register_command!(interpreter, TRIM, TRIM_RULES, trim);
    register_command!(
        interpreter,
        TRIM_WHITESPACE,
        TRIM_WHITESPACE_RULES,
        trim_whitespace
    );
    register_command!(interpreter, IS_NUMBER, IS_NUMBER_RULES, is_number);
    register_command!(interpreter, IS_NONE, IS_NONE_RULES, is_none);
    register_command!(interpreter, IS_ALPHA, IS_ALPHA_RULES, is_alpha);
    register_command!(interpreter, IS_ALPHA_EN, IS_ALPHA_EN_RULES, is_alpha_en);
    register_command!(
        interpreter,
        IS_WHITESPACE,
        IS_WHITESPACE_RULES,
        is_whitespace
    );
    register_command!(
        interpreter,
        REMOVE_WHITESPACE,
        REMOVE_WHITESPACE_RULES,
        remove_whitespace
    );
    register_command!(interpreter, SPLIT, SPLIT_RULES, split);
    register_command!(interpreter, RANDOM_RANGE, RANDOM_RANGE_RULES, random_range);
    register_command!(interpreter, SLEEP, SLEEP_RULES, sleep);
    register_command!(interpreter, RANDOM_ENTRY, RANDOM_ENTRY_RULES, random_entry);
    register_command!(interpreter, SHUFFLE, SHUFFLE_RULES, shuffle);
    register_command!(interpreter, DEF, DEF_RULES, def);
    register_command!(interpreter, EXIT, NO_ARGS, exit);
    register_command!(interpreter, BREAK, NO_ARGS, r#break);
    register_command!(interpreter, CONTINUE, NO_ARGS, r#continue);
    register_command!(interpreter, RETURN, RETURN_RULES, r#return);
}

/// If value is var label replaces it with it's actual inner value and returns label + var entry otherwise returns None
pub async fn take_if_var<'c>(
    value: &mut Value<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Option<(Cow<'c, str>, VarEntry<'c>)>, ExecutionError> {
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

/// If value is var updates var entry and returns Value::Var otherwise returns value that was passed in
pub fn update_if_var<'c>(
    var: Option<(Cow<'c, str>, VarEntry<'c>)>,
    value: Value<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, ValueError> {
    if let Some((label, mut var_entry)) = var {
        var_entry.value = value;
        data.vars.insert_entry(label.clone(), var_entry)?;
        return Ok(Value::Var(label));
    }
    Ok(value)
}

fn contains_float(values: &[Value]) -> bool {
    values.iter().any(|v| v.is_type(FslType::Float))
}

pub const ADD: &str = "add";
pub async fn add<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    if contains_float(&values) {
        let mut sum: f64 = 0.0;
        for value in values {
            let value = value.as_float(data.clone()).await?;
            sum = sum + value;
        }
        Ok(Value::Float(sum))
    } else {
        let mut sum: i64 = 0;
        for value in values {
            let value = value.as_int(data.clone()).await?;
            sum = sum.checked_add(value).ok_or(CommandError::Overflow)?;
        }
        Ok(Value::Int(sum))
    }
}

pub const SUB: &str = "sub";
pub async fn sub<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    let contains_float = contains_float(&values);
    let mut iter = values.into_iter();
    if contains_float {
        let mut diff = iter.next().unwrap().as_float(data.clone()).await?;
        for value in iter {
            diff = diff - value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(diff))
    } else {
        let mut diff = iter.next().unwrap().as_int(data.clone()).await?;
        for value in iter {
            let value = value.as_int(data.clone()).await?;
            diff = diff.checked_sub(value).ok_or(CommandError::Overflow)?;
        }
        Ok(Value::Int(diff))
    }
}

pub const MUL: &str = "mul";
pub async fn mul<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    let contains_float = contains_float(&values);
    let mut iter = values.into_iter();
    if contains_float {
        let mut product = iter.next().unwrap().as_float(data.clone()).await?;
        for value in iter {
            product = product * value.as_float(data.clone()).await?;
        }
        Ok(Value::Float(product))
    } else {
        let mut product = iter.next().unwrap().as_int(data.clone()).await?;
        for value in iter {
            let value = value.as_int(data.clone()).await?;
            product = product.checked_mul(value).ok_or(CommandError::Overflow)?;
        }
        Ok(Value::Int(product))
    }
}

pub const DIV: &str = "div";
pub async fn div<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    let contains_float = contains_float(&values);
    let mut iter = values.into_iter();
    if contains_float {
        let mut quotient = iter.next().unwrap().as_float(data.clone()).await?;
        for value in iter {
            let value = value.as_float(data.clone()).await?;
            if value == 0.0 {
                return Err(CommandError::DivisionByZero);
            };
            quotient = quotient / value;
        }
        Ok(Value::Float(quotient))
    } else {
        let mut quotient = iter.next().unwrap().as_int(data.clone()).await?;
        for value in iter {
            let value = value.as_int(data.clone()).await?;
            if value == 0 {
                return Err(CommandError::DivisionByZero);
            };
            quotient = quotient.checked_div(value).ok_or(CommandError::Overflow)?;
        }
        Ok(Value::Int(quotient))
    }
}

pub const MODULUS: &str = "mod";
pub async fn modulus<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(2), MAYBE_NUMBER),
];
pub const CLAMP: &str = "clamp";
pub async fn clamp<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let to_clamp = values.pop_front().unwrap().as_number(data.clone()).await?;
    let min = values.pop_front().unwrap();
    let max = values.pop_front().unwrap();

    match to_clamp {
        Value::Int(to_clamp) => {
            let min = min.as_int(data.clone()).await?;
            let max = max.as_int(data.clone()).await?;

            if min > max {
                return Err(CommandError::InvalidRange);
            }

            return Ok(Value::Int(to_clamp.clamp(min, max)));
        }
        Value::Float(to_clamp) => {
            let min = min.as_float(data.clone()).await?;
            let max = max.as_float(data.clone()).await?;

            if min > max {
                return Err(CommandError::InvalidRange);
            }

            return Ok(Value::Float(to_clamp.clamp(min, max)));
        }
        _ => unreachable!("already checked to_clamp was number with as_number"),
    }
}
pub const CLAMP_MIN_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const CLAMP_MIN: &str = "clamp_min";
pub async fn clamp_min<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let to_clamp = values.pop_front().unwrap().as_number(data.clone()).await?;
    let min = values.pop_front().unwrap();

    match to_clamp {
        Value::Int(to_clamp) => {
            let min = min.as_int(data.clone()).await?;

            if to_clamp < min {
                Ok(Value::Int(min))
            } else {
                Ok(Value::Int(to_clamp))
            }
        }
        Value::Float(to_clamp) => {
            let min = min.as_float(data.clone()).await?;

            if to_clamp < min {
                Ok(Value::Float(min))
            } else {
                Ok(Value::Float(to_clamp))
            }
        }
        _ => unreachable!("already checked to_clamp was number with as_number"),
    }
}

pub const CLAMP_MAX_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const CLAMP_MAX: &str = "clamp_max";
pub async fn clamp_max<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let to_clamp = values.pop_front().unwrap().as_number(data.clone()).await?;
    let max = values.pop_front().unwrap();

    match to_clamp {
        Value::Int(to_clamp) => {
            let max = max.as_int(data.clone()).await?;

            if to_clamp > max {
                Ok(Value::Int(max))
            } else {
                Ok(Value::Int(to_clamp))
            }
        }
        Value::Float(to_clamp) => {
            let max = max.as_float(data.clone()).await?;

            if to_clamp > max {
                Ok(Value::Float(max))
            } else {
                Ok(Value::Float(to_clamp))
            }
        }
        _ => unreachable!("already checked to_clamp was number with as_number"),
    }
}

pub const PRECISION_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_INT),
];
pub const PRECISION: &str = "precision";
pub async fn precision<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let arg_0 = values.pop_front().unwrap();
    let arg_1 = values.pop_front().unwrap();
    let num = arg_0.as_float(data.clone()).await?;
    let precision = arg_1.as_usize(data).await?;
    let formatted = format!("{:.prec$}", num, prec = precision);

    Ok(Value::Text(Cow::Owned(formatted)))
}

pub const STORE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), STORABLE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const STORE: &str = "store";
pub async fn store<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let var = values.pop_front().unwrap();
    let var = var.as_var_label(data.clone()).await?;

    let value_to_store = values.pop_front().unwrap();
    let value_to_store = value_to_store.as_raw(data.clone(), ANY).await?;

    data.vars.create_or_update(&var, value_to_store)?;

    Ok(data.vars.get(&var)?)
}

pub const LOCAL_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), STORABLE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const LOCAL: &str = "local";
pub async fn local<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let var = values.pop_front().unwrap();
    let var = var.as_var_label(data.clone()).await?;

    let value_to_store = values.pop_front().unwrap();
    let value_to_store = value_to_store.as_raw(data.clone(), ANY).await?;

    data.vars.insert(&var, value_to_store)?;

    Ok(data.vars.get(&var)?)
}

pub const UPDATE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), STORABLE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const UPDATE: &str = "update";
pub async fn update<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let var = values.pop_front().unwrap();
    let var = var.as_var_label(data.clone()).await?;

    let value_to_store = values.pop_front().unwrap();
    let value_to_store = value_to_store.as_raw(data.clone(), ANY).await?;
    let var_label = &var;

    data.vars.update_var(var_label, value_to_store)?;

    Ok(data.vars.get(var_label)?)
}

pub const CONST_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), STORABLE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const CONST: &str = "const";
pub async fn r#const<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let var = values.pop_front().unwrap();
    let var = var.as_var_label(data.clone()).await?;

    let value_to_store = values.pop_front().unwrap();
    let value_to_store = value_to_store.as_raw(data.clone(), ANY).await?;
    let var_label = &var;

    data.vars.insert_const(var_label, value_to_store)?;

    Ok(data.vars.get(var_label)?)
}

pub const CLONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ANY)];
pub const CLONE: &str = "clone";
pub async fn clone<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let value = command.take_args().pop_front().unwrap();
    Ok(value.as_raw(data, ANY).await?)
}

pub const TAKE: &str = "take";
pub const TAKE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn take<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let arg_0 = command.take_args().pop_front().unwrap();
    let label = arg_0.get_var_label()?;
    match data.vars.remove(&label)? {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub const REF: &str = "ref";
pub const REF_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), &[FslType::Var])];
pub async fn r#ref<'c>(
    command: Command<'c>,
    _: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let value = command.take_args().pop_front().unwrap();
    Ok(Value::Var(value.get_var_label()?))
}

pub const PRINT_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const PRINT: &str = "print";
pub async fn print<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();

    if let Some(limit) = data.limits.max_output_len {
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

pub const ARGS_RULES: &'static [ArgRule] = NO_ARGS;
pub const ARGS: &str = "args";
pub async fn args<'c>(
    _: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut input = data.args.lock().await;
    let arg_list = Value::List(std::mem::take(&mut input));
    Ok(arg_list)
}

pub const DEBUG_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const DEBUG: &str = "debug";
pub async fn debug<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();
    let mut output = String::new();

    for value in values {
        output.push_str(&value.as_text(data.clone()).await?);
    }

    dbg!(output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), ANY)];
pub const SCOPE: &str = "";
pub async fn scope<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();
    data.vars.push();
    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_raw(data.clone(), ANY).await?;
    }
    data.vars.pop();

    Ok(return_value)
}

pub const EQ_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), NOT_NONE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const EQ: &str = "eq";
pub async fn eq<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let a = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ANY)
        .await?;
    let b = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ANY)
        .await?;

    Ok(Value::Bool(a.eq(&b)?))
}

pub const GT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const GT: &str = "gt";
pub async fn gt<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let mut values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    if contains_float(&values) {
        let b = values.pop().unwrap().as_float(data.clone()).await?;
        let a = values.pop().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a > b))
    } else {
        let b = values.pop().unwrap().as_int(data.clone()).await?;
        let a = values.pop().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a > b))
    }
}

pub const GTOE_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const GTOE: &str = "gtoe";
pub async fn gtoe<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let mut values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    if contains_float(&values) {
        let b = values.pop().unwrap().as_float(data.clone()).await?;
        let a = values.pop().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a >= b))
    } else {
        let b = values.pop().unwrap().as_int(data.clone()).await?;
        let a = values.pop().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a >= b))
    }
}

pub const LT_RULES: &[ArgRule; 2] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const LT: &str = "lt";
pub async fn lt<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let mut values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    if contains_float(&values) {
        let b = values.pop().unwrap().as_float(data.clone()).await?;
        let a = values.pop().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a < b))
    } else {
        let b = values.pop().unwrap().as_int(data.clone()).await?;
        let a = values.pop().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a < b))
    }
}

pub const LTOE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const LTOE: &str = "ltoe";
pub async fn ltoe<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let mut values = values
        .then(|v| v.as_number(data.clone()))
        .collect::<Result<Vec<Value>, _>>()
        .await?;
    if contains_float(&values) {
        let b = values.pop().unwrap().as_float(data.clone()).await?;
        let a = values.pop().unwrap().as_float(data.clone()).await?;

        Ok(Value::Bool(a <= b))
    } else {
        let b = values.pop().unwrap().as_int(data.clone()).await?;
        let a = values.pop().unwrap().as_int(data.clone()).await?;

        Ok(Value::Bool(a <= b))
    }
}

pub const NOT_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_BOOL)];
pub const NOT: &str = "not";
pub async fn not<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let a = values.pop_front().unwrap().as_bool(data.clone()).await?;
    Ok(Value::from(!a))
}

pub const AND_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), MAYBE_BOOL)];
pub const AND: &str = "and";
pub async fn and<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 && value.as_bool(data.clone()).await?;
    }
    Ok(Value::from(arg_0))
}

pub const OR_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), MAYBE_BOOL)];
pub const OR: &str = "or";
pub async fn or<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut arg_0 = values.pop_front().unwrap().as_bool(data.clone()).await?;
    for value in values {
        arg_0 = arg_0 || value.as_bool(data.clone()).await?;
    }
    Ok(Value::from(arg_0))
}

pub const IF_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_BOOL),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const IF: &str = "if";
pub async fn r#if<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let condition = values.pop_front().unwrap();

    let mut then_command: Option<Value> = None;
    let mut else_ifs: VecDeque<Value> = VecDeque::new();
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
                else_ifs.push_back(command);
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
        for else_if in else_ifs {
            let mut else_if = else_if.as_command()?;
            let condition = else_if.pop_front_arg().unwrap();
            let condition = condition.as_bool(data.clone()).await?;

            if condition == true {
                return else_if.execute(data.clone()).await;
            } else {
                continue;
            }
        }

        if else_command.is_some() {
            let else_command = else_command.unwrap();
            return else_command.as_command()?.execute(data.clone()).await;
        }

        return Ok(Value::None);
    }
}

pub const SWITCH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), ANY),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const SWITCH: &str = "switch";
pub async fn switch<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut args = command.take_args();
    let expression = args.pop_front().unwrap();
    let expression = expression.as_raw(data.clone(), ANY).await?;
    let commands = args;

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
            let mut case = case.as_command()?;
            let value = case.pop_front_arg().unwrap();
            let value = value.as_raw(data.clone(), ANY).await?;

            if value == expression {
                return case.execute(data.clone()).await;
            } else {
                continue;
            }
        }

        let result = fallback.as_command()?.execute(data.clone()).await;

        return result;
    } else {
        return Err(CommandError::SwitchMustHaveSingleFallbackCommand);
    }
}

pub const BLOCK_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), ANY)];
pub const CASE: &str = "case";
pub const FALLBACK: &str = "fallback";
pub const THEN: &str = "then";
pub const ELSE_IF: &str = "else_if";
pub const ELSE: &str = "else";
pub async fn block<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();

    let mut return_value = Value::None;
    for value in values {
        return_value = value.as_raw(data.clone(), ANY).await?;
    }
    return Ok(return_value);
}

pub const WHILE_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_BOOL),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const WHILE_LOOP: &str = "while";
pub async fn while_command<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::AnyFrom(1), &[FslType::Command]),
];
pub const REPEAT: &str = "repeat";
pub async fn repeat<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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

#[async_recursion]
async fn get_index<'c>(
    list: &Vec<Value<'c>>,
    indices: &[Value<'c>],
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
async fn get_mut_index<'c, 'a>(
    list: &'a mut Vec<Value<'c>>,
    indices: &[Value<'c>],
    data: Arc<InterpreterData<'c>>,
) -> Result<&'a mut Value<'c>, CommandError> {
    match indices {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [i] => {
            let i = i.clone().as_usize(data.clone()).await?;
            match list.get_mut(i) {
                Some(i) => Ok(i),
                None => Err(CommandError::ValueError(ValueError::IndexOutOfBounds(
                    format!("index {} was not present in list", i),
                ))),
            }
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
async fn remove_index<'c, 'a>(
    list: &'a mut Vec<Value<'c>>,
    indices: &[Value<'c>],
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
async fn insert_at_index<'c>(
    list: &mut Vec<Value<'c>>,
    indices: &[Value<'c>],
    value_to_insert: Value<'c>,
    data: Arc<InterpreterData<'c>>,
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
    ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE),
    ArgRule::new(ArgPos::Index(1), MAYBE_LIST_KEY),
];
pub const INDEX: &str = "index";
pub async fn index<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let array = values.pop_front().unwrap();
    let i = values.pop_front().unwrap();

    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let i = i.as_usize(data).await?;
            match text.chars().nth(i) {
                Some(char) => Ok(char.into()),
                None => Err(CommandError::IndexOutOfBounds),
            }
        }
        Value::List(list) => {
            let key = i.as_key(data.clone(), MAYBE_LIST_KEY).await?;
            get_index(&list, &key, data).await
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

#[async_recursion]
async fn get_nested<'c>(
    map: &FslMap<'c>,
    keys: &[Value<'c>],
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    match keys {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [key] => {
            let key = key.clone().as_text(data.clone()).await?;
            let return_value = map.get(&*key).cloned();
            Ok(return_value.unwrap_or(Value::None))
        }
        [key, rest @ ..] => {
            let key = key.clone().as_text(data.clone()).await?;
            match map.get(&*key) {
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
    ArgRule::new(ArgPos::Index(0), MAYBE_MAP),
    ArgRule::new(ArgPos::Index(1), MAYBE_MAP_KEY),
];
pub const GET: &str = "get";
pub async fn get<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let map = values.pop_front().unwrap();
    let key = values.pop_front().unwrap();
    let key = key.as_key(data.clone(), MAP_KEY).await?;
    let map = map.as_map(data.clone()).await?;

    get_nested(&map, &key, data).await
}

#[async_recursion]
async fn set_nested<'c>(
    map: &mut FslMap<'c>,
    keys: &[Value<'c>],
    value: Value<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    match keys {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [key] => {
            let key = key.clone().as_text(data.clone()).await?;
            if let Some(_) = map.get(&*key) {
                let return_value = map.insert(key, value);
                Ok(return_value.unwrap_or(Value::None))
            } else {
                Err(CommandError::ValueError(ValueError::NonExistantKey(
                    format!("non existant key \"{}\" in map", key),
                )))
            }
        }
        [key, rest @ ..] => {
            let key = key.clone().as_text(data.clone()).await?;
            match map.get_mut(&*key) {
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

#[async_recursion]
async fn remove_nested<'c>(
    map: &mut FslMap<'c>,
    keys: &[Value<'c>],
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    match keys {
        [] => Err(CommandError::ValueError(ValueError::NotAMap(
            "".to_string(),
        ))),
        [key] => {
            let key = key.clone().as_text(data.clone()).await?;
            let return_value = map.remove(&*key);
            Ok(return_value.unwrap_or(Value::None))
        }
        [key, rest @ ..] => {
            let key = key.clone().as_text(data.clone()).await?;
            match map.get_mut(&*key) {
                Some(Value::Map(inner_map)) => remove_nested(inner_map, rest, data).await,
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

#[async_recursion]
async fn insert_nested<'c>(
    map: &mut FslMap<'c>,
    keys: &[Value<'c>],
    value: Value<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
            match map.get_mut(&*key) {
                Some(Value::Map(inner_map)) => insert_nested(inner_map, rest, value, data).await,
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
    ArgRule::new(ArgPos::Index(0), MAYBE_MAP),
    ArgRule::new(ArgPos::Index(1), MAYBE_MAP_KEY),
    ArgRule::new(ArgPos::Index(2), NOT_NONE),
];
pub const SET: &str = "set";
pub async fn set<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut map = values.pop_front().unwrap();
    let key = values.pop_front().unwrap();
    let value = values.pop_front().unwrap();

    let key = key.as_key(data.clone(), MAP_KEY).await?;

    let value = value.as_raw(data.clone(), NOT_NONE).await?;

    let var = take_if_var(&mut map, data.clone()).await?;

    let mut map = map.as_map(data.clone()).await?;

    let return_value = set_nested(&mut map, &key, value, data.clone()).await?;

    update_if_var(var, Value::Map(map), data)?;

    Ok(return_value)
}

pub const LENGTH_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE)];
pub const LENGTH: &str = "length";
pub async fn length<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let array = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), INDEXABLE)
        .await?;

    match array {
        Value::Text(text) => Ok(Value::from(text.len())),
        Value::List(list) => Ok(Value::from(list.len())),
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const REMOVE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_COLLECTION),
    ArgRule::new(ArgPos::Index(1), MAYBE_LIST_KEY),
];
pub const REMOVE: &str = "remove";
pub async fn remove<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut array = values.pop_front().unwrap();
    let key = values.pop_front().unwrap();

    let key = key.as_key(data.clone(), KEY).await?;

    let var = take_if_var(&mut array, data.clone()).await?;

    let array = array.as_raw(data.clone(), COLLECTION).await?;

    match array {
        Value::Text(text) => {
            let mut key = key;
            if key.len() > 1 {
                return Err(CommandError::IndexOutOfBounds);
            }
            let Some(i) = key.pop() else {
                return Err(CommandError::IndexOutOfBounds);
            };
            let i = i.as_usize(data.clone()).await?;

            match text.chars().nth(i) {
                Some(_) => {
                    let mut text = text.into_owned();
                    let return_value = text.remove(i).to_string();
                    let text = Value::from(std::mem::take(&mut text));

                    update_if_var(var, text, data)?;

                    Ok(Value::from(return_value))
                }
                None => Err(CommandError::IndexOutOfBounds),
            }
        }
        Value::List(mut list) => {
            let return_value = remove_index(&mut list, &key, data.clone()).await;

            update_if_var(var, Value::List(list), data)?;

            return_value
        }
        Value::Map(mut map) => {
            let return_value = remove_nested(&mut map, &key, data.clone()).await;

            update_if_var(var, Value::Map(map), data)?;

            return_value
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const SWAP_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE),
    ArgRule::new(ArgPos::Index(1), MAYBE_LIST_KEY),
    ArgRule::new(ArgPos::Index(2), MAYBE_LIST_KEY),
];
pub const SWAP: &str = "swap";
pub async fn swap<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();

    let i = values.pop_front().unwrap();
    let j = values.pop_front().unwrap();

    let var = take_if_var(&mut array, data.clone()).await?;

    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let i = i.as_int(data.clone()).await?;
            let j = j.as_int(data.clone()).await?;
            let a = i as usize;
            let b = j as usize;

            let mut chars: Vec<char> = text.chars().collect();

            if a < chars.len() && b < chars.len() {
                chars.swap(a, b);
                let text: String = chars.iter().collect();
                let return_value = update_if_var(var, Value::from(text), data)?;
                Ok(return_value)
            } else {
                Err(CommandError::IndexOutOfBounds)
            }
        }
        Value::List(mut list) => {
            let i = i
                .as_key(data.clone(), &[FslType::List, FslType::Int])
                .await?;
            let j = j
                .as_key(data.clone(), &[FslType::List, FslType::Int])
                .await?;

            let a_value = get_index(&list, &i, data.clone()).await?;
            let b_value = get_index(&list, &j, data.clone()).await?;

            let a_swap = get_mut_index(&mut list, &i, data.clone()).await?;
            *a_swap = b_value;
            let b_swap = get_mut_index(&mut list, &j, data.clone()).await?;
            *b_swap = a_value;

            let return_value = update_if_var(var, Value::List(list), data)?;

            Ok(return_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE),
    ArgRule::new(ArgPos::Index(1), MAYBE_LIST_KEY),
    ArgRule::new(ArgPos::Index(2), NOT_NONE),
];
pub const REPLACE: &str = "replace";
pub async fn replace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();
    let key = values.pop_front().unwrap();
    let value = values.pop_front().unwrap();
    let value = value.as_raw(data.clone(), ANY).await?;

    let key = key.as_key(data.clone(), LIST_KEY).await?;

    let var = take_if_var(&mut array, data.clone()).await?;

    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let mut key = key;
            if key.len() > 1 {
                return Err(CommandError::IndexOutOfBounds);
            }
            let Some(i) = key.pop() else {
                return Err(CommandError::IndexOutOfBounds);
            };
            let i = i.as_usize(data.clone()).await?;
            let new_ch = value.as_text(data.clone()).await?;

            let mut chars: Vec<char> = text.chars().collect();
            let old_ch;
            match chars.get_mut(i) {
                Some(ch) => {
                    old_ch = ch.clone();
                    *ch = new_ch.chars().nth(0).ok_or(CommandError::InvalidArgument(
                        "text replacement value must be a single character".to_string(),
                    ))?;

                    let text: String = chars.iter().collect();
                    update_if_var(var, Value::from(text), data)?;
                    Ok(Value::Text(Cow::Owned(old_ch.to_string())))
                }
                None => Err(CommandError::IndexOutOfBounds),
            }
        }
        Value::List(mut list) => {
            let new_value = value.as_raw(data.clone(), NOT_NONE).await?;

            let old_value = get_mut_index(&mut list, &key, data.clone()).await?;

            let return_value = old_value.clone();
            *old_value = new_value;

            update_if_var(var, Value::List(list), data.clone())?;

            Ok(return_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const INSERT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_COLLECTION),
    ArgRule::new(ArgPos::Index(1), MAYBE_KEY),
    ArgRule::new(ArgPos::Index(2), NOT_NONE),
];
pub const INSERT: &str = "insert";
pub async fn insert<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();
    let key = values.pop_front().unwrap();
    let value = values.pop_front().unwrap();
    let value = value.as_raw(data.clone(), ANY).await?;

    let key = key.as_key(data.clone(), KEY).await?;

    let var = take_if_var(&mut array, data.clone()).await?;

    let array = array.as_raw(data.clone(), COLLECTION).await?;

    match array {
        Value::Text(text) => {
            let mut key = key;
            if key.len() > 1 {
                return Err(CommandError::IndexOutOfBounds);
            }
            let Some(i) = key.pop() else {
                return Err(CommandError::IndexOutOfBounds);
            };
            let i = i.as_usize(data.clone()).await?;

            let text_to_insert = value.as_text(data.clone()).await?;

            let mut text = text.into_owned();
            if i <= text.len() {
                text.insert_str(i, &text_to_insert);
            } else {
                return Err(CommandError::IndexOutOfBounds);
            }

            let return_value = update_if_var(var, Value::from(std::mem::take(&mut text)), data)?;

            Ok(return_value)
        }
        Value::List(mut list) => {
            let value_to_insert = value.as_raw(data.clone(), NOT_NONE).await?;

            insert_at_index(&mut list, &key, value_to_insert, data.clone()).await?;

            let return_value = update_if_var(var, Value::List(list), data)?;

            Ok(return_value)
        }
        Value::Map(mut map) => {
            let value_to_insert = value.as_raw(data.clone(), NOT_NONE).await?;

            insert_nested(&mut map, &key, value_to_insert, data.clone()).await?;

            let return_value = update_if_var(var, Value::Map(map), data)?;

            Ok(return_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const PUSH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const PUSH: &str = "push";
pub async fn push<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();
    let value = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NOT_NONE)
        .await?;

    let var = take_if_var(&mut array, data.clone()).await?;

    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let text_to_push = value.as_text(data.clone()).await?;
            let mut text = text.into_owned();
            text.push_str(&text_to_push);

            let return_value = update_if_var(var, Value::from(text), data)?;
            Ok(return_value)
        }
        Value::List(mut list) => {
            list.push(value);

            let return_value = update_if_var(var, Value::List(list), data)?;
            Ok(return_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const POP_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE)];
pub const POP: &str = "pop";
pub async fn pop<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();
    let var = take_if_var(&mut array, data.clone()).await?;
    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let mut text = text.into_owned();
            let popped_text = text
                .pop()
                .map(|c| Value::from(c.to_string()))
                .unwrap_or(Value::None);

            update_if_var(var, Value::from(text), data)?;
            Ok(popped_text)
        }
        Value::List(mut list) => {
            let popped_value = list.pop().unwrap_or(Value::None);

            update_if_var(var, Value::List(list), data)?;
            Ok(popped_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const SEARCH_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(2), MAYBE_TEXT),
];
pub const SEARCH_REPLACE: &str = "search_replace";
pub async fn search_replace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut string = values.pop_front().unwrap();

    let to_replace = values.pop_front().unwrap().as_text(data.clone()).await?;
    let with = values.pop_front().unwrap().as_text(data.clone()).await?;

    let var = take_if_var(&mut string, data.clone()).await?;

    let string = string.as_text(data.clone()).await?;

    let input = string.replace(&*to_replace, &with);

    let return_value = update_if_var(var, Value::from(input), data)?;
    Ok(return_value)
}

pub const SLICE_REPLACE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), MAYBE_LIST),
    ArgRule::new(ArgPos::Index(2), MAYBE_TEXT),
];
pub const SLICE_REPLACE: &str = "slice_replace";
pub async fn slice_replace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let mut string = values.pop_front().unwrap();

    let mut range = values.pop_front().unwrap().as_list(data.clone()).await?;
    let with = values.pop_front().unwrap().as_text(data.clone()).await?;

    let var = take_if_var(&mut string, data.clone()).await?;

    let string = string.as_text(data.clone()).await?;

    if range.len() != 2 {
        return Err(CommandError::IndexOutOfBounds);
    }

    let (from, to) = (
        std::mem::take(&mut range[0]).as_usize(data.clone()).await?,
        std::mem::take(&mut range[1]).as_usize(data.clone()).await?,
    );

    if from > string.len() || to > string.len() || from > to {
        return Err(CommandError::IndexOutOfBounds);
    } else if !string.is_char_boundary(from) || !string.is_char_boundary(to) {
        return Err(CommandError::InvalidArgument(format!(
            "slice of text must lie within char boundries"
        )));
    }

    let mut input = string.into_owned();
    input.replace_range(from..to, &with);

    let return_value = update_if_var(var, Value::from(input), data)?;
    Ok(return_value)
}

pub const REVERSE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_INDEXABLE)];
pub const REVERSE: &str = "reverse";
pub async fn reverse<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let mut array = values.pop_front().unwrap();
    let var = take_if_var(&mut array, data.clone()).await?;
    let array = array
        .as_raw(data.clone(), &[FslType::List, FslType::Text])
        .await?;

    match array {
        Value::Text(text) => {
            let text = text.chars().rev().collect();

            let return_value = update_if_var(var, Value::Text(text), data)?;
            Ok(return_value)
        }
        Value::List(mut list) => {
            list.reverse();

            let return_value = update_if_var(var, Value::List(list), data)?;
            Ok(return_value)
        }
        _ => unreachable!("as_raw should enforce array is List or Text"),
    }
}

pub const INC_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), &[FslType::Var]),
    ArgRule::new(ArgPos::OptionalIndex(1), MAYBE_INT),
];
pub const INC: &str = "inc";
pub async fn inc<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
    ArgRule::new(ArgPos::OptionalIndex(1), MAYBE_INT),
];
pub const DEC: &str = "dec";
pub async fn dec<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
    ArgRule::new(ArgPos::Index(0), MAYBE_COLLECTION),
    ArgRule::new(ArgPos::Index(1), NOT_NONE),
];
pub const CONTAINS: &str = "contains";
pub async fn contains<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let collection = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), COLLECTION)
        .await?;
    let item = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), NOT_NONE)
        .await?;

    match collection {
        Value::Text(text) => {
            let item = &item.as_text(data).await?;
            Ok(Value::Bool(text.contains(&**item)))
        }
        Value::List(list) => {
            let iter = tokio_stream::iter(list.into_iter());
            let list = iter
                .then(|v| v.as_raw(data.clone(), NOT_NONE))
                .collect::<Result<Vec<_>, _>>()
                .await?;
            Ok(Value::Bool(list.contains(&item)))
        }
        Value::Map(map) => {
            let key = item.as_text(data).await?;
            Ok(Value::Bool(map.contains_key(&*key)))
        }
        _ => unreachable!("as_raw should enforce type"),
    }
}

pub const STARTS_WITH_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const STARTS_WITH: &str = "starts_with";
pub async fn starts_with<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let string = values.pop_front().unwrap().as_text(data.clone()).await?;
    let value = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(Value::from(string.starts_with(&*value)))
}

pub const ENDS_WITH_RULES: &'static [ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const ENDS_WITH: &str = "ends_with";
pub async fn ends_with<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let string = values.pop_front().unwrap().as_text(data.clone()).await?;
    let value = values.pop_front().unwrap().as_text(data.clone()).await?;
    Ok(Value::from(string.ends_with(&*value)))
}

pub const CONCAT_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub const CONCAT: &str = "concat";
pub async fn concat<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = command.take_args();

    let mut cat_string = String::new();

    for value in values {
        cat_string.push_str(&value.as_text(data.clone()).await?);
    }

    Ok(cat_string.into())
}

pub const CAPITALIZE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const CAPITALIZE: &str = "capitalize";
pub async fn capitalize<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    let mut chars = text.chars();
    let text = if let Some(ch) = chars.next() {
        Cow::Owned(ch.to_uppercase().collect::<String>() + chars.as_str())
    } else {
        text
    };
    Ok(Value::Text(text))
}

pub const UPPERCASE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const UPPERCASE: &str = "uppercase";
pub async fn uppercase<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(Value::from(text.to_uppercase()))
}

pub const LOWERCASE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const LOWERCASE: &str = "lowercase";
pub async fn lowercase<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(Value::from(text.to_lowercase()))
}

pub const TRIM_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), MAYBE_TEXT),
];
pub const TRIM: &str = "trim";
pub async fn trim<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data.clone()).await?;
    let pattern = values.pop_front().unwrap().as_text(data).await?;
    let chars: Vec<char> = pattern.chars().collect();
    let trimmed = match text {
        Cow::Borrowed(s) => Cow::Borrowed(s.trim_matches(chars.as_slice())),
        Cow::Owned(s) => Cow::Owned(s.trim_matches(chars.as_slice()).to_owned()),
    };
    Ok(Value::from(trimmed))
}

pub const TRIM_WHITESPACE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const TRIM_WHITESPACE: &str = "trim_whitespace";
pub async fn trim_whitespace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data.clone()).await?;
    let trimmed = match text {
        Cow::Borrowed(s) => Cow::Borrowed(s.trim()),
        Cow::Owned(s) => Cow::Owned(s.trim().to_owned()),
    };
    Ok(Value::from(trimmed))
}

pub const IS_NUMBER_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ANY)];
pub const IS_NUMBER: &str = "is_number";
pub async fn is_number<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap().as_number(data).await;
    Ok(Value::Bool(value.is_ok()))
}

pub const IS_NONE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), ANY)];
pub const IS_NONE: &str = "is_none";
pub async fn is_none<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let value = values
        .pop_front()
        .unwrap()
        .as_raw(data.clone(), ANY)
        .await?;
    Ok(Value::Bool(value.is_type(FslType::None)))
}

pub const IS_ALPHA_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const IS_ALPHA: &str = "is_alpha";
pub async fn is_alpha<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    if let Ok(text) = value.as_text(data).await {
        let is_alpha = text.chars().all(char::is_alphabetic);

        Ok(Value::Bool(is_alpha))
    } else {
        Ok(Value::Bool(false))
    }
}

pub const IS_ALPHA_EN_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const IS_ALPHA_EN: &str = "is_alpha_en";
pub async fn is_alpha_en<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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

pub const IS_WHITESPACE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const IS_WHITESPACE: &str = "is_whitespace";
pub async fn is_whitespace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let value = values.pop_front().unwrap();
    if let Ok(text) = value.as_text(data).await {
        let is_whitespace = text.chars().all(char::is_whitespace);

        Ok(Value::Bool(is_whitespace))
    } else {
        Ok(Value::Bool(false))
    }
}

pub const REMOVE_WHITESPACE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_TEXT)];
pub const REMOVE_WHITESPACE: &str = "remove_whitespace";
pub async fn remove_whitespace<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text = values.pop_front().unwrap().as_text(data).await?;
    Ok(text.split_whitespace().collect::<String>().into())
}

pub const SPLIT_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_TEXT),
    ArgRule::new(ArgPos::Index(1), &[FslType::Text]),
];
pub const SPLIT: &str = "split";
pub async fn split<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let text_to_split = values.pop_front().unwrap().as_text(data.clone()).await?;
    let pattern = values.pop_front().unwrap().as_text(data.clone()).await?;
    let mut list: Vec<Value> = Vec::new();

    for split in text_to_split.split(&*pattern) {
        if !split.is_empty() {
            list.push(Value::from(split.to_string()));
        }
    }
    Ok(Value::List(list))
}

pub const RANDOM_RANGE_RULES: &[ArgRule] = &[
    ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER),
    ArgRule::new(ArgPos::Index(1), MAYBE_NUMBER),
];
pub const RANDOM_RANGE: &str = "random_range";
pub async fn random_range<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let values = tokio_stream::iter(command.take_args());
    let mut values = values
        .then(|v| v.as_raw(data.clone(), NUMBER))
        .collect::<Result<Vec<Value>, _>>()
        .await?;

    if contains_float(&values) {
        let max = values.pop().unwrap().as_float(data.clone()).await?;
        let min = values.pop().unwrap().as_float(data.clone()).await?;
        if min >= max {
            Err(CommandError::InvalidRange)
        } else if !min.is_finite() || !max.is_finite() {
            Err(CommandError::NonFiniteValue)
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    } else {
        let max = values.pop().unwrap().as_int(data.clone()).await?;
        let min = values.pop().unwrap().as_int(data.clone()).await?;
        if min >= max {
            Err(CommandError::InvalidRange)
        } else {
            Ok(rand::random_range(min..=max).into())
        }
    }
}

pub const SLEEP: &str = "sleep";
pub const SLEEP_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_NUMBER)];
pub async fn sleep<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let delay = values.pop_front().unwrap().as_float(data).await?;
    if !delay.is_finite() {
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

pub const RANDOM_ENTRY_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_LIST)];
pub const RANDOM_ENTRY: &str = "random_entry";
pub async fn random_entry<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let list = values.pop_front().unwrap().as_list(data).await?;
    let range = 0..list.len();
    if range.is_empty() {
        Err(CommandError::InvalidRange)
    } else {
        Ok(list[rand::random_range(range)].clone())
    }
}

pub const SHUFFLE_RULES: &[ArgRule] = &[ArgRule::new(ArgPos::Index(0), MAYBE_LIST)];
pub const SHUFFLE: &str = "shuffle";
pub async fn shuffle<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
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
pub async fn def<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();
    let label = values.pop_front().unwrap().get_var_label()?;
    let mut parameters: VecDeque<Cow<'c, str>> = VecDeque::new();
    let mut commands: Vec<Command> = Vec::new();

    let values_len = values.len();
    let mut encountered_command = false;
    for i in 0..values_len {
        match values[i] {
            Value::Var(ref label) => {
                if encountered_command {
                    return Err(CommandError::WrongArgOrder(format!(
                        "variables in command definition must come before commands"
                    )));
                }
                parameters.push_back(label.clone());
            }
            Value::Command(ref command) => {
                encountered_command = true;
                commands.push(*command.clone());
            }
            _ => {
                return Err(CommandError::InvalidArgument(
                    "def must only contain parameter labels followed by commands".into(),
                ));
            }
        }
    }

    let def = data
        .find_user_def(&label)
        .await
        .expect("definitions should be pre declared");

    def.define(parameters, commands).await;

    Ok(Value::None)
}

fn alias_parameter<'c>(parameter: &mut Value<'c>, aliases: &HashMap<Cow<'c, str>, Cow<'c, str>>) {
    match parameter {
        Value::List(values) => {
            for value in values.iter_mut() {
                alias_parameter(value, aliases);
            }
        }
        Value::Map(map) => {
            for value in map.values_mut() {
                alias_parameter(value, aliases);
            }
        }
        Value::Command(command) => {
            for arg in command.get_args_mut() {
                alias_parameter(arg, aliases);
            }
        }
        Value::Var(var) => match aliases.get(var) {
            Some(alias) => *parameter = Value::Var(alias.clone()),
            None => {}
        },
        _ => {}
    }
}

pub const RUN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::AnyFrom(0), NOT_NONE)];
pub async fn run<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let mut values = command.take_args();

    let command_label = values.pop_front().unwrap().get_var_label()?;

    {
        let mut call_stack = data.user_call_stack.lock().await;
        call_stack.push(command_label.clone());
    }

    let (mut parameter_labels, commands) = {
        let def = data
            .find_user_def(&command_label)
            .await
            .expect("command should be defined");

        let parameter_labels = def.parameters.lock().await.clone();
        let commands = def.commands.lock().await.clone();
        (parameter_labels, commands)
    };

    if values.len() != parameter_labels.len() {
        return Err(CommandError::WrongArgCount(format!(
            "expected {} args but got {}",
            parameter_labels.len(),
            values.len()
        )));
    }

    data.vars.push();

    let mut aliases: HashMap<Cow<'c, str>, Cow<'c, str>> = HashMap::new();
    let parameters = parameter_labels.len();
    for _ in 0..parameters {
        let mut value = values.pop_front().unwrap();
        let parameter = parameter_labels.pop_front().unwrap();
        if let Value::Var(var) = value {
            aliases.insert(parameter, var);
        } else {
            value = value.as_raw_unchecked(data.clone()).await?;
            data.vars.insert(&parameter, value)?;
        }
    }

    let mut final_value = Value::None;
    for mut command in commands {
        let args = command.get_args_mut();
        for arg in args {
            alias_parameter(arg, &aliases);
        }
        final_value = command.execute(data.clone()).await?;
        if data.flags.return_flag.load(Ordering::Relaxed) {
            data.flags.return_flag.store(false, Ordering::Relaxed);
            break;
        }
    }
    data.vars.pop();

    {
        let mut call_stack = data.user_call_stack.lock().await;
        call_stack.pop();
    }

    Ok(final_value)
}

pub const BREAK: &str = "break";
pub async fn r#break<'c>(
    _: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    if data.loop_depth.load(Ordering::Relaxed) > 0 {
        data.flags.break_flag.store(true, Ordering::Relaxed);
    } else {
        return Err(CommandError::BreakOutsideLoop);
    }
    Ok(Value::None)
}

pub const CONTINUE: &str = "continue";
pub async fn r#continue<'c>(
    _: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    if data.loop_depth.load(Ordering::Relaxed) > 0 {
        data.flags.continue_flag.store(true, Ordering::Relaxed);
    } else {
        return Err(CommandError::ContinueOutsideLoop);
    }
    Ok(Value::None)
}

pub const RETURN_RULES: &'static [ArgRule] = &[ArgRule::new(ArgPos::OptionalIndex(0), ANY)];
pub const RETURN: &str = "return";
pub async fn r#return<'c>(
    command: Command<'c>,
    data: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    let return_value = command.take_args().pop_front();

    match return_value {
        Some(value) => {
            let value = value.as_raw_unchecked(data.clone()).await?;
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
pub async fn exit<'c>(
    _: Command<'c>,
    _: Arc<InterpreterData<'c>>,
) -> Result<Value<'c>, CommandError> {
    Err(CommandError::ProgramExited)
}

#[cfg(test)]
pub mod tests {
    use std::time::{Duration, SystemTime};

    use crate::{
        CommandError, FslInterpreter, InterpreterError, InterpreterErrorType,
        data::InterpreterData, error::ValueError,
    };

    pub async fn test_interpreter(code: &str, expected_output: &str) {
        let result = FslInterpreter::new()
            .interpret(code, InterpreterData::default())
            .await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    pub async fn observe_interpreter(code: &str) {
        let result = FslInterpreter::new()
            .interpret(code, InterpreterData::default())
            .await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);
    }

    pub async fn test_interpreter_embedded(code: &str, expected_output: &str) {
        let result = FslInterpreter::new()
            .interpret_embedded_code(code, InterpreterData::default())
            .await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    pub async fn test_interpreter_err_type(code: &str) -> crate::InterpreterErrorType {
        let result = FslInterpreter::new()
            .interpret(code, InterpreterData::default())
            .await;
        dbg!(&result);
        assert!(result.is_err());
        result.err().unwrap().error_type
    }

    pub async fn interpreter_throws_err(code: &str, err: InterpreterError) -> bool {
        let result = FslInterpreter::new()
            .interpret(code, InterpreterData::default())
            .await;
        dbg!(&result);
        result.is_err_and(|e| {
            dbg!(&e);
            e.error_type == err.error_type
        })
    }

    pub async fn interpreter_results_in_error(code: &str) -> bool {
        let result = FslInterpreter::new()
            .interpret(code, InterpreterData::default())
            .await;
        dbg!(&result);
        result.is_err()
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
    async fn ambiguous_add() {
        test_interpreter(
            r#"
                two.def(return(2))
                three_point_four.def(return(3.4))
                print(add(two(), three_point_four()))
            "#,
            "5.4",
        )
        .await;
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
        test_interpreter("a.store(1) b.store(a) print(b)", "1").await;
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
    async fn take_var() {
        let err = test_interpreter_err_type("a.store(1) print(a) a.take() print(a)").await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantVar(_)))
        ));
    }

    #[tokio::test]
    async fn print_values() {
        test_interpreter(
            r#"a.store(1) print("1", " ", 1, " ", a, " ", [1])"#,
            "1 1 1 [1]",
        )
        .await;
    }

    #[tokio::test]
    async fn commands_in_scope() {
        test_interpreter(r#"(a.store(1), a.inc(), a.print())"#, "2").await;
    }

    #[tokio::test]
    async fn scope_vars() {
        let err = test_interpreter_err_type(
            r#"
            (
                inner.local(0)
            )
            print(inner)
            "#,
        )
        .await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantVar(_)))
        ))
    }

    #[tokio::test]
    async fn scope_returns() {
        test_interpreter(
            r#"
            (1,2,3).print()
            "#,
            "3",
        )
        .await;
    }

    #[tokio::test]
    async fn values_in_scope() {
        test_interpreter(r#"(1)"#, "").await;
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
            r#"i.store(0) while(i.lt(3), print(i, " "), i.inc())"#,
            "0 1 2 ",
        )
        .await;
    }

    #[tokio::test]
    async fn repeat() {
        test_interpreter(r#"i.store(0) repeat(3, print(i, " "), i.inc())"#, "0 1 2 ").await;
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
        test_interpreter(
            r#"player.store([name: "player", health: 100]) player.remove("name").print()"#,
            "player",
        )
        .await;
        test_interpreter(
            r#"player.store([name: "player", health: 100, weapons: [sword: [name: "sword"] ] ]) player.weapons.sword.remove().name.get().print()"#,
            "sword",
        )
        .await;
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
    async fn matrix_swap() {
        test_interpreter(
            r#"
                matrix.store([[1, 2], [3, 4]])
                matrix.swap([0, 0], [1, 0])
                matrix.print()
            "#,
            "[[3, 2], [1, 4]]",
        )
        .await;
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
        test_interpreter(
            r#"player.store([name: "player", health: 100]) player.contains("name").print()"#,
            "true",
        )
        .await;
        test_interpreter(
            r#"player.store([name: "player", health: 100]) player.contains("weapon").print()"#,
            "false",
        )
        .await;
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
        assert!(interpreter_results_in_error(r#"capitalize("ﬆ").print()"#).await == false)
    }

    #[tokio::test]
    async fn capitalize_expandable_character() {
        assert!(interpreter_results_in_error(r#"capitalize("ﬃ").print()"#).await == false)
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
    async fn trim_whitespace() {
        test_interpreter(r#"trim_whitespace(" hey ").print()"#, "hey").await;
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
    async fn def_passing_by_value() {
        test_interpreter(
            r#"
            create_obj.def(x, y,
                obj.store([
                    x: x,
                    y: y,
                ]).return()
            )

            obj.store(create_obj(2, 3))
            obj.x.get().print()
            "#,
            "2",
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
    async fn reverse_var() {
        test_interpreter(
            r#"hello.store("hello") print(hello.reverse()) print(hello)"#,
            "olleholleh",
        )
        .await;
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
    async fn const_cannot_be_took() {
        let err = test_interpreter_err_type(
            r#"
                const(MY_CONST, 42)
                MY_CONST.take()
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
                list.store([1, 2, THREE])
                list.replace(2, 99)
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
            "13\n0",
        )
        .await;
    }

    #[tokio::test]
    async fn store_before_local() {
        test_interpreter(
            r#"
            i.store(0)

            test.def(
                i.store(12)
                i.local(10)
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
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantVar(_)))
        ))
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
    async fn insert_map() {
        test_interpreter(
            r#"
	            player.store([
	                health: 100,
	                dodge: 0,
	                strength: 0
	            ])
	            player.insert("name", "player")
	            player.get("name").print("\n")
	            player.set("name", "jake").print("\n")
	            player.get("name").print()
            "#,
            "player\nplayer\njake",
        )
        .await;
    }

    #[tokio::test]
    async fn set_none_map() {
        let err = test_interpreter_err_type(
            r#"
	            player.store([
	                health: 100,
	                dodge: 0,
	                strength: 0
	            ])
	            player.set("name", "jake").print("\n")
            "#,
        )
        .await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantKey(_)))
        ))
    }

    #[tokio::test]
    async fn substitute_map_args() {
        test_interpreter(
            r#"
                create_player.def(name,
	                player.store([
	                    name: name.take(),
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
    async fn substitute_list_args() {
        test_interpreter(
            r#"
                create_player.def(name,
	                player.store([
	                    name.take(),
	                    100,
	                    0,
	                    0,
	                ])
	                player.return()
	            )
	            player.store(create_player("jake"))
	            player.index(0).print()
            "#,
            "jake",
        )
        .await;
    }

    #[tokio::test]
    async fn substitute_command_args() {
        test_interpreter(
            r#"
                print_name.def(name,
	                print(name)
	            )
	            print_name("jake")
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
	            		modifier: modifier.take(),
	            		duration: duration.take(),
	            		quality: quality,
	            	])
	            	action.store(create_action(name.take(), POTION, potion.take())).return()
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
	            		potion_actions.push(potion.take())
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
                switch("hello".index(1),
                    case("h",
                        print(0)
                    )
                    case("e",
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
    async fn case_with_multiple_values() {
        test_interpreter(
            r#"
                print(
                    switch(0,
                        case(0,
                            print(0)
                            print(1)
                            print(2)
                            3
                        )
                        case(1,
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
                switch(add(1,1),
                    case(1,
                        print(0)
                    )
                    case(2,
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
                switch(true,
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
                switch(true,
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
                switch(true,
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
    async fn nested_switch() {
        test_interpreter(
            r#"
                switch(
                    switch(true,
                        case(true, 1)
                        fallback(2)
                    ),
                    case(1, print("correct"))
                    fallback(print("wrong"))
                )
            "#,
            "correct",
        )
        .await;
    }

    #[tokio::test]
    async fn switch_multiple_fallbacks() {
        let err = test_interpreter_err_type(
            r#"
                switch(true,
                    case(false, print("wrong"))
                    fallback(print("first"))
                    fallback(print("second"))
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

    #[tokio::test]
    async fn keys_with_commands() {
        test_interpreter(
            r#"
                index([1, add(1, 1), 3], [add(1, 0)]).print()
            "#,
            "2",
        )
        .await;
    }

    #[tokio::test]
    async fn invalid_key() {
        let err = test_interpreter_err_type(
            r#"
                index([1, add(1, 1), 3], ["one"]).print()
            "#,
        )
        .await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::FailedParse(_))),
        ))
    }

    #[tokio::test]
    async fn out_of_scope_var() {
        let err = test_interpreter_err_type(
            r#"
                (
                    test.store("stuff")
                )
                test.print()
            "#,
        )
        .await;
        assert!(matches!(
            err,
            InterpreterErrorType::Command(CommandError::ValueError(ValueError::NonExistantVar(_))),
        ))
    }
}
