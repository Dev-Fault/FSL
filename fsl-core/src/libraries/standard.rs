use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
    time::Duration,
};

use rand::seq::SliceRandom;

use crate::{
    DEF, DEF_RULES, FslInterpreter, InterpreterData,
    data::UserDeclaration,
    def,
    error::{RuntimeError, SpanError, SpannedError, ToSpannedError},
    execute_command, potential_future,
    potential_futures::SpannedPotentialFutureResult,
    register_async, register_sync,
    source_str::SourceStr,
    span::Span,
    types::{
        ANY, COLLECTION, INDEXABLE, MATH_RULES, NO_ARGS, NUMBER, ValueType,
        argument::{Argument, Key},
        command::{ArgPos, ArgRule, Command, CommandSignature, ExpectedArgs},
        list::List,
        map::FslMap,
        value::{Number, Value},
    },
    vars::Var,
};

pub fn register_std(interpreter: &mut FslInterpreter) {
    // core
    register_sync!(interpreter, NO_OP, NO_OP_RULES, no_op);
    register_sync!(interpreter, ADD, MATH_RULES, add);
    register_sync!(interpreter, SUB, MATH_RULES, sub);
    register_sync!(interpreter, MUL, MATH_RULES, mul);
    register_sync!(interpreter, DIV, MATH_RULES, div);
    register_sync!(interpreter, MODULUS, MATH_RULES, modulus);
    register_sync!(interpreter, STORE, STORE_RULES, store);
    register_sync!(interpreter, CONST, CONST_RULES, r#const);
    register_sync!(interpreter, LOCAL, LOCAL_RULES, local);
    register_sync!(interpreter, GET, GET_RULES, get);
    register_sync!(interpreter, SET, SET_RULES, set);
    register_sync!(interpreter, CLONE, CLONE_RULES, clone);
    register_sync!(interpreter, TAKE, TAKE_RULES, take);
    register_sync!(interpreter, ARGS, ARGS_RULES, args);
    register_sync!(interpreter, EQ, EQ_RULES, eq);
    register_sync!(interpreter, GT, GT_RULES, gt);
    register_sync!(interpreter, GTOE, GTOE_RULES, gtoe);
    register_sync!(interpreter, LT, LT_RULES, lt);
    register_sync!(interpreter, LTOE, LTOE_RULES, ltoe);
    register_sync!(interpreter, NOT, NOT_RULES, not);
    register_sync!(interpreter, AND, AND_RULES, and);
    register_sync!(interpreter, OR, OR_RULES, or);
    register_sync!(interpreter, INDEX, INDEX_RULES, index);
    register_sync!(interpreter, LENGTH, LENGTH_RULES, length);
    register_sync!(interpreter, SWAP, SWAP_RULES, swap);
    register_sync!(interpreter, INSERT, INSERT_RULES, insert);
    register_sync!(interpreter, REMOVE, REMOVE_RULES, remove);
    register_sync!(interpreter, PUSH, PUSH_RULES, push);
    register_sync!(interpreter, POP, POP_RULES, pop);
    register_sync!(interpreter, REPLACE, REPLACE_RULES, replace);
    register_async!(interpreter, WHILE_LOOP, WHILE_RULES, while_command);
    register_async!(interpreter, REPEAT, REPEAT_RULES, repeat);
    register_async!(interpreter, FOR_EACH, FOR_EACH_RULES, for_each);
    register_sync!(interpreter, EXIT, NO_ARGS, exit);
    register_sync!(interpreter, BREAK, NO_ARGS, r#break);
    register_sync!(interpreter, CONTINUE, NO_ARGS, r#continue);
    register_async!(interpreter, IF, IF_RULES, r#if);
    register_async!(interpreter, THEN, BLOCK_RULES, block);
    register_async!(interpreter, ELSE_IF, BLOCK_RULES, block);
    register_async!(interpreter, ELSE, BLOCK_RULES, block);
    register_async!(interpreter, SWITCH, SWITCH_RULES, switch);
    register_async!(interpreter, CASE, BLOCK_RULES, block);
    register_async!(interpreter, FALLBACK, BLOCK_RULES, block);
    register_async!(interpreter, DEF, DEF_RULES, def);
    register_async!(interpreter, SCOPE, SCOPE_RULES, scope);
    register_async!(interpreter, RETURN, RETURN_RULES, r#return);
    register_async!(interpreter, PRINT, PRINT_RULES, print);
    register_sync!(interpreter, CONCAT, CONCAT_RULES, concat);
    // std
    register_sync!(interpreter, ASSIGN, ASSIGN_RULES, assign);
    register_sync!(interpreter, UPDATE, UPDATE_RULES, update);
    register_sync!(interpreter, CLAMP, CLAMP_RULES, clamp);
    register_sync!(interpreter, CLAMP_MIN, CLAMP_MIN_RULES, clamp_min);
    register_sync!(interpreter, CLAMP_MAX, CLAMP_MAX_RULES, clamp_max);
    register_sync!(interpreter, PRECISION, PRECISION_RULES, precision);
    register_async!(interpreter, DEBUG, DEBUG_RULES, debug);
    register_sync!(interpreter, INC, INC_RULES, inc);
    register_sync!(interpreter, DEC, DEC_RULES, dec);
    register_sync!(interpreter, SCALE, SCALE_RULES, scale);
    register_sync!(interpreter, REDUCE, REDUCE_RULES, reduce);
    register_sync!(interpreter, CONTAINS, CONTAINS_RULES, contains);
    register_sync!(interpreter, PREPEND, PREPEND_RULES, prepend);
    register_sync!(interpreter, REVERSE, REVERSE_RULES, reverse);
    register_sync!(interpreter, STARTS_WITH, STARTS_WITH_RULES, starts_with);
    register_sync!(interpreter, ENDS_WITH, ENDS_WITH_RULES, ends_with);
    register_sync!(interpreter, CAPITALIZE, CAPITALIZE_RULES, capitalize);
    register_sync!(interpreter, UPPERCASE, UPPERCASE_RULES, uppercase);
    register_sync!(interpreter, LOWERCASE, LOWERCASE_RULES, lowercase);
    register_sync!(interpreter, TRIM, TRIM_RULES, trim);
    register_sync!(
        interpreter,
        TRIM_WHITESPACE,
        TRIM_WHITESPACE_RULES,
        trim_whitespace
    );
    register_sync!(
        interpreter,
        REMOVE_WHITESPACE,
        REMOVE_WHITESPACE_RULES,
        remove_whitespace
    );
    register_sync!(interpreter, IS_NUMBER, IS_NUMBER_RULES, is_number);
    register_sync!(interpreter, IS_NONE, IS_NONE_RULES, is_none);
    register_sync!(interpreter, IS_ALPHA, IS_ALPHA_RULES, is_alpha);
    register_sync!(interpreter, IS_ALPHA_EN, IS_ALPHA_EN_RULES, is_alpha_en);
    register_sync!(
        interpreter,
        IS_WHITESPACE,
        IS_WHITESPACE_RULES,
        is_whitespace
    );
    register_sync!(interpreter, SPLIT, SPLIT_RULES, split);
    register_sync!(interpreter, RANDOM_RANGE, RANDOM_RANGE_RULES, random_range);
    register_async!(interpreter, SLEEP, SLEEP_RULES, sleep);
    register_async!(interpreter, STOPWATCH, STOPWATCH_RULES, stopwatch);
    register_sync!(interpreter, RANDOM_ENTRY, RANDOM_ENTRY_RULES, random_entry);
    register_sync!(interpreter, SHUFFLE, SHUFFLE_RULES, shuffle);
    register_sync!(
        interpreter,
        RANGE_REPLACE,
        RANGE_REPLACE_RULES,
        range_replace
    );
    register_sync!(
        interpreter,
        SEARCH_REPLACE,
        SEARCH_REPLACE_RULES,
        search_replace
    );
}

pub const ADD: &str = "add";
pub fn add(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let mut int: i64 = 0;
    let mut float: f64 = 0.0;
    let mut is_float: bool = false;
    for arg in args.iter_mut() {
        let arg_span = arg.span;
        let value = arg.take_value();
        match value {
            Value::Int(i) => {
                int = int
                    .checked_add(i)
                    .ok_or(RuntimeError::Overflow)
                    .span(arg_span)?;
            }
            Value::Float(f) => {
                float += f;
                is_float = true;
            }
            Value::Text(str) => match FslInterpreter::parse_number(&*str).span(arg_span)? {
                Value::Int(i) => {
                    int = int
                        .checked_add(i)
                        .ok_or(RuntimeError::Overflow)
                        .span(arg_span)?;
                }
                Value::Float(f) => {
                    float += f;
                    is_float = true
                }
                _ => unreachable!("parse number only returns int, float"),
            },
            _ => return Err(value.conversion_err(NUMBER).span(arg_span)),
        }
    }
    if is_float {
        Ok(Value::Float((int as f64) + float))
    } else {
        Ok(Value::Int(int))
    }
}

pub const SUB: &str = "sub";
pub fn sub(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    let first = args[0].take_value();
    let (mut int, mut float, mut is_float, mut switched) = match first {
        Value::Int(i) => (i, 0.0, false, false),
        Value::Float(f) => (0, f, true, true),
        Value::Text(ref s) => match FslInterpreter::parse_number(s).span(args[0].span)? {
            Value::Int(i) => (i, 0.0, false, false),
            Value::Float(f) => (0, f, true, true),
            _ => unreachable!(),
        },
        _ => return Err(first.conversion_err(NUMBER).span(args[0].span)),
    };

    for arg in args.iter_mut().skip(1) {
        let arg_span = arg.span;
        let value = arg.take_value();
        match value {
            Value::Int(i) => {
                if !switched {
                    int = int
                        .checked_sub(i)
                        .ok_or(RuntimeError::Overflow)
                        .span(arg_span)?;
                } else {
                    float -= i as f64;
                }
            }
            Value::Float(f) => {
                if !switched {
                    float = int as f64;
                }
                float -= f;
                is_float = true;
                switched = true;
            }
            Value::Text(str) => match FslInterpreter::parse_number(&*str).span(arg_span)? {
                Value::Int(i) => {
                    if !switched {
                        int = int
                            .checked_sub(i)
                            .ok_or(RuntimeError::Overflow)
                            .span(arg_span)?;
                    } else {
                        float -= i as f64;
                    }
                }
                Value::Float(f) => {
                    if !switched {
                        float = int as f64;
                    }
                    float -= f;
                    is_float = true;
                    switched = true;
                }
                _ => unreachable!("parse number only returns int, float"),
            },
            _ => return Err(value.conversion_err(NUMBER).span(arg_span)),
        }
    }
    if is_float {
        Ok(Value::Float(float))
    } else {
        Ok(Value::Int(int))
    }
}
pub const MUL: &str = "mul";
pub fn mul(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    let first = args[0].take_value();
    let (mut int, mut float, mut is_float, mut switched) = match first {
        Value::Int(i) => (i, 0.0, false, false),
        Value::Float(f) => (0, f, true, true),
        Value::Text(ref s) => match FslInterpreter::parse_number(s).span(args[0].span)? {
            Value::Int(i) => (i, 0.0, false, false),
            Value::Float(f) => (0, f, true, true),
            _ => unreachable!(),
        },
        _ => return Err(first.conversion_err(NUMBER).span(args[0].span)),
    };

    let handle_int = |i: i64,
                      int: &mut i64,
                      float: &mut f64,
                      switched: bool,
                      span: Span|
     -> Result<(), SpannedError> {
        if !switched {
            *int = int
                .checked_mul(i)
                .ok_or(RuntimeError::Overflow)
                .span(span)?;
        } else {
            *float *= i as f64;
        }
        Ok(())
    };

    let handle_float = |f: f64,
                        int: i64,
                        float: &mut f64,
                        is_float: &mut bool,
                        switched: &mut bool|
     -> Result<(), SpannedError> {
        if !*switched {
            *float = int as f64;
        }
        *float *= f;
        *is_float = true;
        *switched = true;
        Ok(())
    };

    for arg in args.iter_mut().skip(1) {
        let span = arg.span;
        let value = arg.take_value();
        match value {
            Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
            Value::Float(f) => handle_float(f, int, &mut float, &mut is_float, &mut switched)?,
            Value::Text(str) => match FslInterpreter::parse_number(&*str).span(span)? {
                Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
                Value::Float(f) => handle_float(f, int, &mut float, &mut is_float, &mut switched)?,
                _ => unreachable!(),
            },
            _ => return Err(value.conversion_err(NUMBER).span(span)),
        }
    }
    if is_float {
        Ok(Value::Float(float))
    } else {
        Ok(Value::Int(int))
    }
}

pub const DIV: &str = "div";
pub fn div(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    let first = args[0].take_value();
    let (mut int, mut float, mut is_float, mut switched) = match first {
        Value::Int(i) => (i, 0.0, false, false),
        Value::Float(f) => (0, f, true, true),
        Value::Text(ref s) => match FslInterpreter::parse_number(s).span(args[0].span)? {
            Value::Int(i) => (i, 0.0, false, false),
            Value::Float(f) => (0, f, true, true),
            _ => unreachable!(),
        },
        _ => return Err(first.conversion_err(NUMBER).span(args[0].span)),
    };

    let handle_int = |i: i64,
                      int: &mut i64,
                      float: &mut f64,
                      switched: bool,
                      span: Span|
     -> Result<(), SpannedError> {
        if i == 0 {
            return Err(RuntimeError::DivisionByZero.span(span));
        }
        if !switched {
            *int = int
                .checked_div(i)
                .ok_or(RuntimeError::Overflow)
                .span(span)?;
        } else {
            *float /= i as f64;
        }
        Ok(())
    };

    let handle_float = |f: f64,
                        int: i64,
                        float: &mut f64,
                        is_float: &mut bool,
                        switched: &mut bool,
                        span: Span|
     -> Result<(), SpannedError> {
        if f == 0.0 {
            return Err(RuntimeError::DivisionByZero.span(span));
        }
        if !*switched {
            *float = int as f64;
        }
        *float /= f;
        *is_float = true;
        *switched = true;
        Ok(())
    };

    for arg in args.iter_mut().skip(1) {
        let span = arg.span;
        let value = arg.take_value();
        match value {
            Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
            Value::Float(f) => {
                handle_float(f, int, &mut float, &mut is_float, &mut switched, span)?
            }
            Value::Text(str) => match FslInterpreter::parse_number(&*str).span(span)? {
                Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
                Value::Float(f) => {
                    handle_float(f, int, &mut float, &mut is_float, &mut switched, span)?
                }
                _ => unreachable!(),
            },
            _ => return Err(value.conversion_err(NUMBER).span(span)),
        }
    }
    if is_float {
        Ok(Value::Float(float))
    } else {
        Ok(Value::Int(int))
    }
}

pub const MODULUS: &str = "mod";
pub fn modulus(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    let first = args[0].take_value();
    let (mut int, mut float, mut is_float, mut switched) = match first {
        Value::Int(i) => (i, 0.0, false, false),
        Value::Float(f) => (0, f, true, true),
        Value::Text(ref s) => match FslInterpreter::parse_number(s).span(args[0].span)? {
            Value::Int(i) => (i, 0.0, false, false),
            Value::Float(f) => (0, f, true, true),
            _ => unreachable!(),
        },
        _ => return Err(first.conversion_err(NUMBER).span(args[0].span)),
    };

    let handle_int = |i: i64,
                      int: &mut i64,
                      float: &mut f64,
                      switched: bool,
                      span: Span|
     -> Result<(), SpannedError> {
        if i == 0 {
            return Err(RuntimeError::DivisionByZero.span(span));
        }
        if !switched {
            *int %= i;
        } else {
            *float %= i as f64;
        }
        Ok(())
    };

    let handle_float = |f: f64,
                        int: i64,
                        float: &mut f64,
                        is_float: &mut bool,
                        switched: &mut bool,
                        span: Span|
     -> Result<(), SpannedError> {
        if f == 0.0 {
            return Err(RuntimeError::DivisionByZero.span(span));
        }
        if !*switched {
            *float = int as f64;
        }
        *float %= f;
        *is_float = true;
        *switched = true;
        Ok(())
    };

    for arg in args.iter_mut().skip(1) {
        let span = arg.span;
        let value = arg.take_value();
        match value {
            Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
            Value::Float(f) => {
                handle_float(f, int, &mut float, &mut is_float, &mut switched, span)?
            }
            Value::Text(str) => match FslInterpreter::parse_number(&*str).span(span)? {
                Value::Int(i) => handle_int(i, &mut int, &mut float, switched, span)?,
                Value::Float(f) => {
                    handle_float(f, int, &mut float, &mut is_float, &mut switched, span)?
                }
                _ => unreachable!(),
            },
            _ => return Err(value.conversion_err(NUMBER).span(span)),
        }
    }
    if is_float {
        Ok(Value::Float(float))
    } else {
        Ok(Value::Int(int))
    }
}

pub const CLAMP_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..3))]);
pub const CLAMP: &str = "clamp";
pub fn clamp(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let command = command;
    let mut args = command.args;

    match args[0].into_number()? {
        Number::Int(i) => {
            let min = args[1].into_int()?;
            let max = args[2].into_int()?;

            if min > max {
                return Err(RuntimeError::InvalidRange.span(command.span));
            }

            Ok(Value::Int(i.clamp(min, max)))
        }
        Number::Float(f) => {
            let min = args[1].into_float()?;
            let max = args[2].into_float()?;

            if min > max {
                return Err(RuntimeError::InvalidRange.span(command.span));
            }

            Ok(Value::Float(f.clamp(min, max)))
        }
    }
}

pub const CLAMP_MIN_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const CLAMP_MIN: &str = "clamp_min";
pub fn clamp_min(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let command = command;
    let mut args = command.args;

    match args[0].into_number()? {
        Number::Int(i) => {
            let min = args[1].into_int()?;

            if i < min {
                Ok(Value::Int(min))
            } else {
                Ok(Value::Int(i))
            }
        }
        Number::Float(f) => {
            let min = args[1].into_float()?;

            if f < min {
                Ok(Value::Float(min))
            } else {
                Ok(Value::Float(f))
            }
        }
    }
}

pub const CLAMP_MAX_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const CLAMP_MAX: &str = "clamp_max";
pub fn clamp_max(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let command = command;
    let mut args = command.args;

    match args[0].into_number()? {
        Number::Int(i) => {
            let max = args[1].into_int()?;

            if i > max {
                Ok(Value::Int(max))
            } else {
                Ok(Value::Int(i))
            }
        }
        Number::Float(f) => {
            let max = args[1].into_float()?;

            if f > max {
                Ok(Value::Float(max))
            } else {
                Ok(Value::Float(f))
            }
        }
    }
}

pub const PRECISION_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const PRECISION: &str = "precision";
pub fn precision(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let num = args[0].into_float()?;
    let precision = args[1].into_usize()?;
    let formatted = format!("{:.prec$}", num, prec = precision);

    Ok(Value::Text(SourceStr::Owned(formatted)))
}

pub const STORE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const STORE: &str = "store";
pub fn store(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[0].into_var()?;

    data.vars
        .write()
        .store(&var, Var::Mut(args[1].take_value()))
        .span(args[0].span)?;

    Ok(Value::Var(var))
}

pub const ASSIGN_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Literal(ArgPos::Index(0)),
    ArgRule::Mutable(ArgPos::Index(1)),
]);
pub const ASSIGN: &str = "assign";
pub fn assign(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[1].into_var()?;

    data.vars
        .write()
        .store(&var, Var::Mut(args[0].take_value()))
        .span(args[1].span)?;

    Ok(Value::Var(var))
}

pub const LOCAL_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const LOCAL: &str = "local";
pub fn local(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[0].into_var()?;

    data.vars
        .write()
        .insert(&var, Var::Mut(args[1].take_value()))
        .span(args[0].span)?;

    Ok(Value::Var(var))
}

pub const UPDATE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const UPDATE: &str = "update";
pub fn update(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[0].into_var()?;

    data.vars
        .write()
        .replace(&var, args[1].take_value())
        .span(args[0].span)?;

    Ok(Value::Var(var))
}

pub const CONST_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const CONST: &str = "const";
pub fn r#const(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[0].into_var()?;

    data.vars
        .write()
        .insert(&var, Var::Const(args[1].take_value()))
        .span(args[0].span)?;

    Ok(Value::Var(var))
}

pub const CLONE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Index(0))]);
pub const CLONE: &str = "clone";
pub fn clone(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    Ok(args[0].take_value())
}

pub const TAKE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const TAKE: &str = "take";
pub fn take(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let var = args[0].into_var()?;
    match data.vars.write().remove(&var).span(args[0].span)? {
        Some(value) => Ok(value),
        None => Ok(Value::None),
    }
}

pub const PRINT_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Raw(ArgPos::AnyFrom(0))]);
pub const PRINT: &str = "print";
pub async fn print(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    if let Some(limit) = data.limits.max_output_len {
        for arg in args.iter_mut() {
            let text = potential_future!(arg.to_text(data.clone())?);
            // Must be locked after as_text (could require evaluating command that calls print)
            let mut output = data.output.lock();
            if text.len() + output.len() > limit {
                return Err(RuntimeError::OutputLimitExceeded.span(command.span));
            }
            output.push_str(&text);
        }
    } else {
        for arg in args.iter_mut() {
            let text = potential_future!(arg.to_text(data.clone())?);
            let mut output = data.output.lock();
            output.push_str(&text);
        }
    }
    Ok(Value::None)
}

pub const ARGS_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::None);
pub const ARGS: &str = "args";
pub fn args(_: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let input = data.args.lock();
    let arg_list = Value::from(input.clone());
    Ok(arg_list)
}

pub const DEBUG_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Raw(ArgPos::AnyFrom(0))]);
pub const DEBUG: &str = "debug";
pub async fn debug(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let mut output = String::new();

    for arg in args.iter_mut() {
        output.push_str(&potential_future!(arg.to_text(data.clone())?));
    }

    dbg!(output);
    Ok(Value::None)
}

pub const SCOPE_RULES: &CommandSignature = &CommandSignature::AnyArgs;
pub const SCOPE: &str = "";
pub async fn scope(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    data.vars.write().push();

    let mut return_value = Value::None;
    for value in args.iter_mut() {
        return_value = potential_future!(value.to_inner(data.clone())?);
    }

    data.vars.write().pop();

    Ok(return_value)
}

pub const NO_OP_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::None);
pub const NO_OP: &str = "no_op";
pub fn no_op(_: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    Ok(Value::None)
}

pub const EQ_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const EQ: &str = "eq";
pub fn eq(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let a = args[0].take_value();
    let b = args[1].take_value();

    Ok(Value::Bool(a.equal(&b, data.clone()).span(command.span)?))
}

pub const SOFT_EQ_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const SOFT_EQ: &str = "soft_eq";
pub fn soft_eq(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let a = args[0].take_value();
    let b = args[1].take_value();

    Ok(Value::Bool(
        a.soft_equal(&b, data.clone()).span(command.span)?,
    ))
}

pub const GT_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const GT: &str = "gt";
pub fn gt(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    match (args[0].into_number()?, args[1].into_number()?) {
        (Number::Int(a), Number::Int(b)) => Ok(Value::from(a > b)),
        (Number::Int(a), Number::Float(b)) => Ok(Value::from(a as f64 > b)),
        (Number::Float(a), Number::Int(b)) => Ok(Value::from(a > b as f64)),
        (Number::Float(a), Number::Float(b)) => Ok(Value::from(a > b)),
    }
}

pub const GTOE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const GTOE: &str = "gtoe";
pub fn gtoe(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    match (args[0].into_number()?, args[1].into_number()?) {
        (Number::Int(a), Number::Int(b)) => Ok(Value::from(a >= b)),
        (Number::Int(a), Number::Float(b)) => Ok(Value::from(a as f64 >= b)),
        (Number::Float(a), Number::Int(b)) => Ok(Value::from(a >= b as f64)),
        (Number::Float(a), Number::Float(b)) => Ok(Value::from(a >= b)),
    }
}

pub const LT_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const LT: &str = "lt";
pub fn lt(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    match (args[0].into_number()?, args[1].into_number()?) {
        (Number::Int(a), Number::Int(b)) => Ok(Value::from(a < b)),
        (Number::Int(a), Number::Float(b)) => Ok(Value::from((a as f64) < b)),
        (Number::Float(a), Number::Int(b)) => Ok(Value::from(a < b as f64)),
        (Number::Float(a), Number::Float(b)) => Ok(Value::from(a < b)),
    }
}

pub const LTOE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Range(0..2))]);
pub const LTOE: &str = "ltoe";
pub fn ltoe(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    match (args[0].into_number()?, args[1].into_number()?) {
        (Number::Int(a), Number::Int(b)) => Ok(Value::from(a <= b)),
        (Number::Int(a), Number::Float(b)) => Ok(Value::from(a as f64 <= b)),
        (Number::Float(a), Number::Int(b)) => Ok(Value::from(a <= b as f64)),
        (Number::Float(a), Number::Float(b)) => Ok(Value::from(a <= b)),
    }
}

pub const NOT_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::Index(0))]);
pub const NOT: &str = "not";
pub fn not(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let a = args[0].into_bool()?;
    Ok(Value::from(!a))
}

pub const AND_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::AnyFrom(0))]);
pub const AND: &str = "and";
pub fn and(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let args = &mut command.args.iter_mut();
    let mut return_value = args.next().unwrap().into_bool()?;
    for arg in args {
        return_value = return_value && arg.into_bool()?;
    }
    Ok(Value::from(return_value))
}

pub const OR_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::AnyFrom(0))]);
pub const OR: &str = "or";
pub fn or(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let args = &mut command.args.iter_mut();
    let mut return_value = args.next().unwrap().into_bool()?;
    for arg in args {
        return_value = return_value || arg.into_bool()?;
    }
    Ok(Value::from(return_value))
}

pub const IF_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(2));
pub const IF: &str = "if";
pub async fn r#if(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let condition = potential_future!(args[0].to_bool(data.clone())?);

    let mut then_command: Option<Argument> = None;
    let mut else_ifs: VecDeque<Argument> = VecDeque::new();
    let mut else_command: Option<Argument> = None;

    let mut requires_else = false;

    let command_count = args.len() - 1;
    for mut command in args.into_iter().skip(1) {
        let label = command.as_command_label(data.clone()).unwrap();
        match &*label {
            THEN => {
                if then_command.is_some() {
                    return Err(RuntimeError::MultipleThenCommandsInIf.span(command.span));
                }
                then_command = Some(command)
            }
            ELSE_IF => {
                requires_else = true;
                else_ifs.push_back(command);
            }
            ELSE => {
                if else_command.is_some() {
                    return Err(RuntimeError::MultipleElseCommandsInIf.span(command.span));
                }
                else_command = Some(command)
            }
            _ => {
                if command_count == 1 {
                    if condition {
                        let command = potential_future!(command.to_command(data.clone())?);
                        return execute_command!(command, data);
                    } else {
                        return Ok(Value::None);
                    }
                } else {
                    return Err(RuntimeError::InvalidCommandInIf.span(command.span));
                }
            }
        }
    }

    let mut then_command =
        then_command.ok_or(RuntimeError::IfMustContainThen.span(command.span))?;

    if requires_else && else_command.is_none() {
        return Err(RuntimeError::ElseIfMustBePairedWithElse.span(command.span));
    }

    if condition {
        let then_command = potential_future!(then_command.to_command(data.clone())?);
        execute_command!(then_command, data)
    } else {
        for else_if in &mut else_ifs {
            let mut else_if = potential_future!(else_if.to_command(data.clone())?);
            let condition = &mut else_if.args[0];
            let condition = potential_future!(condition.to_bool(data.clone())?);

            if condition {
                return execute_command!(else_if, data);
            } else {
                continue;
            }
        }

        if else_command.is_some() {
            let mut else_command = else_command.unwrap();
            let else_command = potential_future!(else_command.to_command(data.clone())?);
            return execute_command!(else_command, data);
        }

        Ok(Value::None)
    }
}

pub const SWITCH_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(2));
pub const SWITCH: &str = "switch";
pub async fn switch(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    let expression = potential_future!(args[0].to_inner_checked(ANY, data.clone())?);

    let mut cases: VecDeque<Argument> = VecDeque::new();
    let mut fallback: VecDeque<Argument> = VecDeque::new();
    for mut command in args.into_iter().skip(1) {
        let label = command.as_command_label(data.clone())?;
        match &*label {
            CASE => cases.push_back(command),
            FALLBACK => fallback.push_back(command),
            _ => {
                return Err(RuntimeError::InvalidCommandInSwitch.span(command.span));
            }
        }
    }

    if fallback.len() == 1
        && let Some(mut fallback) = fallback.pop_front()
    {
        for case in &mut cases {
            let case_span = case.span;
            let mut case = potential_future!(case.to_command(data.clone())?);
            let arg = &mut case.args[0];
            let arg = potential_future!(arg.to_inner(data.clone())?);

            if arg.equal(&expression, data.clone()).span(case_span)? {
                return execute_command!(case, data);
            } else {
                continue;
            }
        }

        let fallback = potential_future!(fallback.to_command(data.clone())?);
        execute_command!(fallback, data)
    } else {
        Err(RuntimeError::SwitchMustHaveSingleFallbackCommand.span(command.span))
    }
}

pub const BLOCK_RULES: &CommandSignature = &CommandSignature::AnyArgs;
pub const CASE: &str = "case";
pub const FALLBACK: &str = "fallback";
pub const THEN: &str = "then";
pub const ELSE_IF: &str = "else_if";
pub const ELSE: &str = "else";
pub async fn block(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    let mut return_value = Value::None;
    for value in args.iter_mut() {
        return_value = potential_future!(value.to_inner_checked(ANY, data.clone())?);
    }
    Ok(return_value)
}

pub const WHILE_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(2));
pub const WHILE_LOOP: &str = "while";
pub async fn while_command(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let condition = args.next().unwrap();
    let mut final_value = Value::None;

    let mut commands = Vec::with_capacity(args.len());
    for arg in args {
        commands.push(potential_future!(arg.to_command(data.clone())?));
    }

    data.inc_loop_depth();

    'outer: while potential_future!(condition.clone().to_bool(data.clone())?) {
        for command in &commands {
            final_value = execute_command!(command.clone(), data.clone())?;

            if data.get_break_flag() || data.get_return_flag() {
                data.set_break_flag(false);
                break 'outer;
            }
            if data.get_continue_flag() {
                data.set_continue_flag(false);
                continue 'outer;
            }
        }
        data.inc_total_loops().span(command.span)?;
    }

    data.dec_loop_depth();

    Ok(final_value)
}

pub const REPEAT_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(2));
pub const REPEAT: &str = "repeat";
pub async fn repeat(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let repetitions = potential_future!(args.next().unwrap().to_int(data.clone())?);
    let mut final_value = Value::None;

    let mut commands = Vec::with_capacity(args.len());
    for arg in args {
        commands.push(potential_future!(arg.to_command(data.clone())?));
    }

    data.inc_loop_depth();

    'outer: for _ in 0..repetitions {
        for command in &commands {
            final_value = execute_command!(command.clone(), data.clone())?;

            if data.get_break_flag() || data.get_return_flag() {
                data.set_break_flag(false);
                break 'outer;
            }
            if data.get_continue_flag() {
                data.set_continue_flag(false);
                continue 'outer;
            }
        }
        data.inc_total_loops().span(command.span)?;
    }

    data.dec_loop_depth();

    Ok(final_value)
}

pub const FOR_EACH_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Mutable(ArgPos::Index(1)),
    ArgRule::Raw(ArgPos::AnyFrom(2)),
]);
pub const FOR_EACH: &str = "for_each";
pub async fn for_each(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    let item = args.next().unwrap();
    let item_span = item.span;
    let item = item.into_var()?;

    let mut commands: Vec<Command> = Vec::with_capacity(args.len());
    for arg in args {
        let command = potential_future!(arg.to_command(data.clone())?);
        commands.push(command);
    }

    let mut final_value = Value::None;

    enum Either {
        Text(SourceStr),
        List(List),
    }

    let mut array = arg.with(data.clone(), |value, span| match value {
        Value::Text(source_str) => Ok(Either::Text(source_str.clone())),
        Value::List(list) => Ok(Either::List(list.clone())),
        _ => Err(value.conversion_err(&[ValueType::Text, ValueType::List])).span(span),
    })?;

    data.vars.write().push();

    match array {
        Either::Text(ref mut source_str) => {
            let original_len = source_str.len();
            let mut chars: Vec<(usize, Value)> = source_str
                .char_indices()
                .map(|(i, ch)| (i, Value::from(ch)))
                .collect();
            let mut owned = std::mem::take(source_str).into_owned_string();
            let mut offset = 0;

            let mut update_str = |i: usize| -> Result<(), SpannedError> {
                let replacement = data
                    .vars
                    .write()
                    .remove(&item)
                    .span(item_span)?
                    .unwrap_or(Value::None);
                let replacement = replacement.into_str().span(item_span)?;
                let i = (i as isize + offset) as usize;
                let c = owned.chars().nth(i).unwrap();
                owned.replace_range(i..i + c.len_utf8(), &replacement);
                offset = owned.len() as isize - original_len as isize;
                Ok(())
            };

            'outer: for (i, element) in &mut chars {
                data.vars
                    .write()
                    .insert(&item, Var::Mut(std::mem::take(element)))
                    .span(item_span)?;

                for command in &commands {
                    final_value = execute_command!(command.clone(), data.clone())?;

                    if data.get_break_flag() || data.get_return_flag() {
                        data.set_break_flag(false);
                        update_str(*i)?;
                        break 'outer;
                    } else if data.get_continue_flag() {
                        update_str(*i)?;
                        data.set_continue_flag(false);
                        continue 'outer;
                    }
                }

                update_str(*i)?;
            }
            *source_str = SourceStr::Owned(owned);
        }
        Either::List(ref mut list) => {
            let update_element = |element: &mut Value| -> Result<(), SpannedError> {
                let replacement = data
                    .vars
                    .write()
                    .remove(&item)
                    .span(item_span)?
                    .unwrap_or(Value::None);
                *element = replacement;
                Ok(())
            };
            'outer: for element in list.iter_mut() {
                data.vars
                    .write()
                    .insert(&item, Var::Mut(std::mem::take(element)))
                    .span(item_span)?;

                for command in &commands {
                    final_value = execute_command!(command.clone(), data.clone())?;

                    if data.get_break_flag() || data.get_return_flag() {
                        update_element(element)?;
                        data.set_break_flag(false);
                        break 'outer;
                    } else if data.get_continue_flag() {
                        update_element(element)?;
                        data.set_continue_flag(false);
                        continue 'outer;
                    }
                }

                update_element(element)?;
            }
        }
    }

    data.vars.write().pop();

    arg.with_mut(data, |value, _| match value {
        Value::List(list) => match array {
            Either::Text(_) => unreachable!(),
            Either::List(new_list) => {
                *list = new_list;
                Ok(())
            }
        },
        Value::Text(source_str) => match array {
            Either::Text(new_str) => {
                *source_str = new_str;
                Ok(())
            }
            Either::List(_) => unreachable!(),
        },
        _ => unreachable!(),
    })?;

    Ok(final_value)
}

pub const INDEX_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const INDEX: &str = "index";
pub fn index(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();
    let i = args.next().unwrap();

    array.with(data.clone(), |value, span| match value {
        Value::Text(text) => {
            let (i_span, i) = (i.span, i.into_usize()?);
            match text.chars().nth(i) {
                Some(char) => Ok(char.into()),
                None => Err(RuntimeError::IndexOutOfBounds.span(i_span)),
            }
        }
        Value::List(list) => {
            let (i_span, i) = (i.span, i.into_keys()?);
            list.get_nested_clone(&i, i_span)
        }
        _ => Err(value.conversion_err(INDEXABLE).span(span))?,
    })
}

pub const GET_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Literal(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::OptionalIndex(1)),
]);
pub const GET: &str = "get";
pub fn get(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;
    if args.len() == 1 {
        args[0].with(data, |value, _| Ok(value.clone()))
    } else {
        let mut args = args.iter_mut();
        let arg = args.next().unwrap();
        let key = args.next().unwrap();
        arg.with(data.clone(), |value, span| match value {
            Value::Map(map) => {
                let (key_span, key) = (key.span, key.into_keys()?);
                let get = map.get_nested_clone(&key, key_span)?;
                Ok(get)
            }
            Value::List(list) => {
                let (i_span, i) = (key.span, key.into_keys()?);
                let get = list.get_nested_clone(&i, i_span)?;
                Ok(get)
            }
            _ => Err(value.conversion_err(&[ValueType::Map]).span(span)),
        })
    }
}

pub const SET_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::OptionalIndex(1)),
    ArgRule::Literal(ArgPos::OptionalIndex(2)),
]);
pub const SET: &str = "set";
pub fn set(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let command_label = command.label();
    let mut args = command.args.iter_mut();

    if args.len() == 2 {
        let old = args.next().unwrap();
        let new = args.next().unwrap();

        let new = new.take_value();
        old.with_mut(data.clone(), |value, _| {
            *value = new;
            Ok(())
        })?;

        Ok(old.take_value())
    } else if args.len() == 3 {
        let arg = args.next().unwrap();
        let key = args.next().unwrap();
        let to_set = args.next().unwrap();

        arg.with_mut(data.clone(), |value, span| match value {
            Value::Map(map) => {
                let key = key.into_keys()?;
                let to_set = map.set_nested(&key, to_set.take_value(), span)?;
                Ok(to_set)
            }
            Value::List(list) => {
                let key = key.into_keys()?;
                let to_set = list.set_nested(&key, to_set.take_value(), span)?;
                Ok(to_set)
            }
            _ => Err(value.conversion_err(&[ValueType::Map]).span(span)),
        })
    } else {
        Err(RuntimeError::WrongArgCount {
            command_label: command_label.to_string(),
            expected: ExpectedArgs::AtLeast(2),
            got: args.len(),
        }
        .span(command.span))
    }
}

pub const LENGTH_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const LENGTH: &str = "length";
pub fn length(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut args = command.args;

    args[0].with(data.clone(), |value, span| match value {
        Value::Text(text) => Ok(Value::from(text.len())),
        Value::List(list) => Ok(Value::from(list.len())),
        _ => Err(value.conversion_err(INDEXABLE).span(span)),
    })
}

pub const REMOVE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::OptionalIndex(1)),
]);
pub const REMOVE: &str = "remove";
pub fn remove(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();

    if args.len() == 1 {
        let arg = args.next().unwrap();
        let (mut root, indexer) = arg.into_indexer(data.clone())?;
        root.with_mut(data.clone(), |value, span| match value {
            Value::Text(source_str) => {
                let mut text = std::mem::take(source_str).into_owned_string();
                let i = indexer.to_text_indexer().span(span)?;
                let removed = text.remove(i);
                *source_str = SourceStr::Owned(text);
                Ok(Value::from(removed))
            }
            Value::List(list) => {
                let indexer = indexer.to_list_indexer().span(span)?;
                let removed = list.remove_nested(&indexer, span)?;
                Ok(removed)
            }
            Value::Map(map) => {
                let indexer = indexer.to_map_indexer().span(span)?;
                let removed = map.remove_nested(&indexer, span)?;
                Ok(removed)
            }
            _ => Err(value.conversion_err(&COLLECTION).span(span)),
        })
    } else {
        let collection = args.next().unwrap();
        let indexer = args.next().unwrap();
        collection.with_mut(data.clone(), |value, span| match value {
            Value::Text(source_str) => {
                let mut text = std::mem::take(source_str).into_owned_string();
                let removed = text.remove(indexer.into_usize()?);
                *source_str = SourceStr::Owned(text);
                Ok(Value::from(removed))
            }
            Value::List(list) => {
                let removed = list.remove_nested(&indexer.into_keys()?, span)?;
                Ok(removed)
            }
            Value::Map(map) => {
                let removed = map.remove_nested(&indexer.into_keys()?, span)?;
                Ok(removed)
            }
            _ => Err(value.conversion_err(&COLLECTION).span(span)),
        })
    }
}

pub const SWAP_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
    ArgRule::Literal(ArgPos::Index(2)),
]);
pub const SWAP: &str = "swap";
pub fn swap(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();
    let a_pos = args.next().unwrap();
    let b_pos = args.next().unwrap();

    array.with_mut(data.clone(), |value, span| match value {
        Value::Text(text) => {
            let text = std::mem::take(text).into_owned_string();
            let mut chars: Vec<char> = text.chars().collect();
            let (a_pos_span, a_pos) = (a_pos.span, a_pos.into_usize()?);
            let (b_pos_span, b_pos) = (b_pos.span, (b_pos.into_usize()?));
            chars
                .get(a_pos)
                .ok_or(RuntimeError::IndexOutOfBounds.span(a_pos_span))?;
            chars
                .get(b_pos)
                .ok_or(RuntimeError::IndexOutOfBounds.span(b_pos_span))?;
            chars.swap(a_pos, b_pos);
            *value = Value::from(chars.into_iter().collect::<String>());
            Ok(())
        }
        Value::List(list) => {
            let (a_pos_span, a_pos) = (a_pos.span, a_pos.into_keys()?);
            let (b_pos_span, b_pos) = (b_pos.span, b_pos.into_keys()?);
            let mut tmp = std::mem::take(list.get_nested_mut(&a_pos, span)?);
            let b = list.get_nested_mut(&b_pos, a_pos_span)?;
            std::mem::swap(b, &mut tmp);
            let a = list.get_nested_mut(&a_pos, b_pos_span)?;
            std::mem::swap(a, &mut tmp);
            Ok(())
        }
        _ => Err(value.conversion_err(INDEXABLE).span(span)),
    })?;

    Ok(array.take_value())
}

pub const REPLACE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
    ArgRule::Literal(ArgPos::Index(2)),
]);
pub const REPLACE: &str = "replace";
pub fn replace(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let collection = args.next().unwrap();
    let indexer = args.next().unwrap();
    let replacement = args.next().unwrap();

    collection.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let mut text = std::mem::take(source_str).into_owned_string();
            let (i_span, i) = (indexer.span, (indexer.into_usize()?));
            let replacement = replacement.into_str()?;

            if !text.is_char_boundary(i) {
                Err(RuntimeError::IndexOutOfBounds.span(i_span))
            } else {
                let old_ch = text[i..]
                    .chars()
                    .next()
                    .ok_or(RuntimeError::IndexOutOfBounds.span(i_span))?;
                text.replace_range(i..i + old_ch.len_utf8(), &replacement);
                *source_str = SourceStr::Owned(text);
                Ok(Value::from(old_ch))
            }
        }
        Value::List(list) => {
            let (i_span, i) = (indexer.span, indexer.into_keys()?);
            let replacement = replacement.take_value();
            let current = list.get_nested_mut(&i, i_span)?;
            let old = std::mem::replace(current, replacement);
            Ok(old)
        }
        Value::Map(map) => {
            let (i_span, i) = (indexer.span, indexer.into_keys()?);
            let replacement = replacement.take_value();
            let current = map.get_nested_mut(&i, i_span)?;
            let old = std::mem::replace(current, replacement);
            Ok(old)
        }
        _ => Err(value.conversion_err(COLLECTION).span(span)),
    })
}

pub const INSERT_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
    ArgRule::Literal(ArgPos::Index(2)),
]);
pub const INSERT: &str = "insert";
pub fn insert(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let collection = args.next().unwrap();
    let indexer = args.next().unwrap();
    let to_insert = args.next().unwrap();

    collection.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let mut text = std::mem::take(source_str).into_owned_string();
            let (i_span, i) = (indexer.span, (indexer.into_usize()?));
            let to_insert = to_insert.into_str()?;

            if !text.is_char_boundary(i) {
                Err(RuntimeError::IndexOutOfBounds.span(i_span))
            } else {
                text.insert_str(i, &to_insert.into_owned_string());
                *source_str = SourceStr::Owned(text);
                Ok(())
            }
        }
        Value::List(list) => {
            let (i_span, i) = (indexer.span, indexer.into_keys()?);
            let to_insert = to_insert.take_value();
            list.insert_nested(&i, to_insert, i_span)?;
            Ok(())
        }
        Value::Map(map) => {
            let (i_span, i) = (indexer.span, indexer.into_keys()?);
            let to_insert = to_insert.take_value();
            map.insert_nested(&i, to_insert, i_span)?;
            Ok(())
        }
        _ => Err(value.conversion_err(COLLECTION).span(span)),
    })?;
    Ok(collection.take_value())
}

pub const PUSH_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const PUSH: &str = "push";
pub fn push(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();
    let to_push = args.next().unwrap();

    array.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let mut text = std::mem::take(source_str).into_owned_string();
            let ch = to_push.into_str()?;
            text.push_str(&ch);
            *source_str = SourceStr::Owned(text);
            Ok(())
        }
        Value::List(list) => {
            let to_push = to_push.take_value();
            list.push(to_push);
            Ok(())
        }
        _ => Err(value.conversion_err(INDEXABLE).span(span)),
    })?;
    Ok(array.take_value())
}

pub const POP_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const POP: &str = "pop";
pub fn pop(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();

    array.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let mut text = std::mem::take(source_str).into_owned_string();
            let popped = text.pop();
            *source_str = SourceStr::Owned(text);
            Ok(popped.map(Value::from).unwrap_or(Value::None))
        }
        Value::List(list) => {
            let popped = list.pop();
            Ok(popped.unwrap_or(Value::None))
        }

        _ => Err(value.conversion_err(INDEXABLE).span(span)),
    })
}

pub const SEARCH_REPLACE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
    ArgRule::Literal(ArgPos::Index(2)),
]);
pub const SEARCH_REPLACE: &str = "search_replace";
pub fn search_replace(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    let to_replace = args.next().unwrap().into_str()?;
    let with = args.next().unwrap().into_str()?;
    text.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let string = source_str.replace(&*to_replace, &with);
            *value = Value::from(string);
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const RANGE_REPLACE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
    ArgRule::Literal(ArgPos::Index(2)),
]);
pub const RANGE_REPLACE: &str = "range_replace";
pub fn range_replace(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    let range = args.next().unwrap().into_keys()?;
    let with = args.next().unwrap().into_str()?;
    let range = match (range.get(0), range.get(1)) {
        (Some(Key::Index(l)), Some(Key::Index(r))) => *l..*r,
        _ => {
            return Err(RuntimeError::InvalidRange.span(command.span));
        }
    };

    text.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            if !source_str.is_char_boundary(range.start) || !source_str.is_char_boundary(range.end)
            {
                return Err(RuntimeError::InvalidArgument(
                    "slice of text must lie within char boundries".to_string(),
                )
                .span(command.span));
            }
            let mut string = source_str.to_string();
            string.replace_range(range, &with);
            *value = Value::from(string);
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;

    Ok(text.take_value())
}

pub const REVERSE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const REVERSE: &str = "reverse";
pub fn reverse(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();

    array.with_mut(data.clone(), |value, span| match value {
        Value::Text(source_str) => {
            let string: String = source_str.chars().rev().collect();
            *value = Value::from(string);
            Ok(())
        }
        Value::List(list) => {
            list.reverse();
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text, ValueType::List])).span(span),
    })?;
    Ok(array.take_value())
}

pub const SHUFFLE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const SHUFFLE: &str = "shuffle";
pub fn shuffle(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();

    array.with_mut(data.clone(), |value, span| match value {
        Value::List(list) => {
            list.shuffle(&mut rand::rng());
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::List])).span(span),
    })?;
    Ok(array.take_value())
}

pub const INC_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::OptionalIndex(1)),
]);
pub const INC: &str = "inc";
pub fn inc(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();

    let amount = if let Some(value) = args.next() {
        value.into_int()?
    } else {
        1
    };

    arg.with_mut(data.clone(), |value, span| match value {
        Value::Int(value) => {
            *value += amount;
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Int]).span(span)),
    })?;
    Ok(arg.take_value())
}

pub const DEC_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::OptionalIndex(1)),
]);
pub const DEC: &str = "dec";
pub fn dec(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();

    let amount = if let Some(value) = args.next() {
        value.into_int()?
    } else {
        1
    };

    arg.with_mut(data.clone(), |value, span| match value {
        Value::Int(value) => {
            *value -= amount;
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Int]).span(span)),
    })?;
    Ok(arg.take_value())
}

pub const SCALE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const SCALE: &str = "scale";
pub fn scale(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    let value = args.next().unwrap();
    let amount = value.into_float()?;

    arg.with_mut(data.clone(), |value, span| match value {
        Value::Int(i) => {
            *value = Value::from((*i as f64) * amount);
            Ok(())
        }
        Value::Float(f) => {
            *f *= amount;
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Int]).span(span)),
    })?;
    Ok(arg.take_value())
}

pub const REDUCE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const REDUCE: &str = "reduce";
pub fn reduce(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    let value = args.next().unwrap();
    let amount = value.into_float()?;

    arg.with_mut(data.clone(), |value, span| match value {
        Value::Int(i) => {
            *value = Value::from((*i as f64) / amount);
            Ok(())
        }
        Value::Float(f) => {
            *f /= amount;
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Int]).span(span)),
    })?;
    Ok(arg.take_value())
}

pub const CONTAINS_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const CONTAINS: &str = "contains";
pub fn contains(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let collection = args.next().unwrap();
    let item = args.next().unwrap();

    collection.with(data.clone(), |value, span| match value {
        Value::Text(source_str) => Ok(Value::Bool(source_str.contains(&*item.into_str()?))),
        Value::List(list) => Ok(Value::Bool(list.contains(&item.take_value()))),
        Value::Map(map) => Ok(Value::Bool(map.contains_key(&item.into_str()?))),
        _ => Err(value.conversion_err(&[ValueType::Text, ValueType::List, ValueType::Map]))
            .span(span),
    })
}

pub const STARTS_WITH_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const STARTS_WITH: &str = "starts_with";
pub fn starts_with(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();
    let item = args.next().unwrap();
    array.with(data.clone(), |value, span| match value {
        Value::Text(source_str) => Ok(Value::Bool(source_str.starts_with(&*item.into_str()?))),
        Value::List(list) => {
            let item = item.take_value();
            match item {
                Value::List(values) => Ok(Value::Bool(list.starts_with(&values.take()))),
                _ => Ok(Value::Bool(list.starts_with(&[item]))),
            }
        }
        _ => Err(value.conversion_err(&[ValueType::Text, ValueType::List])).span(span),
    })
}

pub const ENDS_WITH_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const ENDS_WITH: &str = "ends_with";
pub fn ends_with(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let array = args.next().unwrap();
    let item = args.next().unwrap();
    array.with(data.clone(), |value, span| match value {
        Value::Text(source_str) => Ok(Value::Bool(source_str.ends_with(&*item.into_str()?))),
        Value::List(list) => {
            let item = item.take_value();
            match item {
                Value::List(values) => Ok(Value::Bool(list.ends_with(&values.take()))),
                _ => Ok(Value::Bool(list.ends_with(&[item]))),
            }
        }
        _ => Err(value.conversion_err(&[ValueType::Text, ValueType::List])).span(span),
    })
}

pub const CONCAT_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::AnyFrom(0))]);
pub const CONCAT: &str = "concat";
pub fn concat(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let args = command.args.iter_mut();

    let mut cat_string = String::new();

    for value in args {
        cat_string.push_str(&value.into_str()?);
    }

    Ok(cat_string.into())
}

pub const PREPEND_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Literal(ArgPos::AnyFrom(0))]);
pub const PREPEND: &str = "prepend";
pub fn prepend(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let end = args.next().unwrap().into_str()?;

    let mut cat_string = String::new();

    for value in args {
        cat_string.push_str(&value.into_str()?);
    }

    cat_string.push_str(&end);

    Ok(cat_string.into())
}

pub const CAPITALIZE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const CAPITALIZE: &str = "capitalize";
pub fn capitalize(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            let mut chars = source_str.chars();
            if let Some(ch) = chars.next() {
                let string: String = ch.to_uppercase().collect::<String>() + chars.as_str();
                *value = Value::from(string);
            }
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const UPPERCASE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const UPPERCASE: &str = "uppercase";
pub fn uppercase(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            *value = Value::from(source_str.to_uppercase());
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const LOWERCASE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const LOWERCASE: &str = "lowercase";
pub fn lowercase(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            *value = Value::from(source_str.to_lowercase());
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const TRIM_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const TRIM: &str = "trim";
pub fn trim(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    let pattern = args.next().unwrap().into_str()?;
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            let chars: Vec<char> = pattern.chars().collect();
            let string = source_str.trim_matches(chars.as_slice()).to_string();
            *value = Value::from(string);
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const TRIM_WHITESPACE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const TRIM_WHITESPACE: &str = "trim_whitespace";
pub fn trim_whitespace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            let string = source_str.trim().to_string();
            *value = Value::from(string);
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const SPLIT_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Mutable(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const SPLIT: &str = "split";
pub fn split(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    let pattern = args.next().unwrap().into_str()?;
    text.with(data, |value, span| match value {
        Value::Text(source_str) => {
            let split: Vec<_> = source_str
                .split(&*pattern)
                .map(|s| Value::from(s.to_string()))
                .collect();
            Ok(Value::List(List::Resolved(Arc::new(split))))
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })
}

pub const REMOVE_WHITESPACE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const REMOVE_WHITESPACE: &str = "remove_whitespace";
pub fn remove_whitespace(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with_mut(data, |value, span| match value {
        Value::Text(source_str) => {
            let string = source_str.split_whitespace().collect::<String>();
            *value = Value::Text(SourceStr::Owned(string));
            Ok(())
        }
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })?;
    Ok(text.take_value())
}

pub const IS_ALPHA_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const IS_ALPHA: &str = "is_alpha";
pub fn is_alpha(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with(data, |value, span| match value {
        Value::Text(source_str) => Ok(Value::from(source_str.chars().all(char::is_alphabetic))),
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })
}

pub const IS_ALPHA_EN_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::Exactly(1));
pub const IS_ALPHA_EN: &str = "is_alpha_en";
pub fn is_alpha_en(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    const ALPHA: &[char] = &[
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    ];

    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with(data, |value, span| match value {
        Value::Text(source_str) => Ok(Value::from(
            source_str
                .chars()
                .all(|ch| ALPHA.contains(&ch.to_ascii_lowercase())),
        )),
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })
}

pub const IS_WHITESPACE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const IS_WHITESPACE: &str = "is_whitespace";
pub fn is_whitespace(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let text = args.next().unwrap();
    text.with(data, |value, span| match value {
        Value::Text(source_str) => Ok(Value::from(source_str.chars().all(char::is_whitespace))),
        _ => Err(value.conversion_err(&[ValueType::Text])).span(span),
    })
}

pub const IS_NUMBER_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const IS_NUMBER: &str = "is_number";
pub fn is_number(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    arg.with(data, |value, _| match value {
        Value::Int(_) => Ok(Value::from(true)),
        Value::Float(_) => Ok(Value::from(true)),
        Value::Text(text) => Ok(Value::from(FslInterpreter::parse_number(&text).is_ok())),
        _ => Ok(Value::from(false)),
    })
}

pub const IS_NONE_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const IS_NONE: &str = "is_none";
pub fn is_none(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    arg.with(data, |value, _| match value {
        Value::None => Ok(Value::from(true)),
        _ => Ok(Value::from(false)),
    })
}

pub const RANDOM_RANGE_RULES: &CommandSignature = &CommandSignature::Positional(&[
    ArgRule::Literal(ArgPos::Index(0)),
    ArgRule::Literal(ArgPos::Index(1)),
]);
pub const RANDOM_RANGE: &str = "random_range";
pub fn random_range(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let min = args.next().unwrap().into_number()?;
    let max = args.next().unwrap().into_number()?;

    match (min, max) {
        (Number::Int(min), Number::Int(max)) => Ok(Value::from(rand::random_range(min..=max))),
        (Number::Int(min), Number::Float(max)) => {
            Ok(Value::from(rand::random_range((min as f64)..=max)))
        }
        (Number::Float(min), Number::Int(max)) => {
            Ok(Value::from(rand::random_range(min..=(max as f64))))
        }
        (Number::Float(min), Number::Float(max)) => Ok(Value::from(rand::random_range(min..=max))),
    }
}

pub const RANDOM_ENTRY_RULES: &CommandSignature =
    &CommandSignature::Positional(&[ArgRule::Mutable(ArgPos::Index(0))]);
pub const RANDOM_ENTRY: &str = "random_entry";
pub fn random_entry(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let list = args.next().unwrap();
    list.with(data, |value, span| match value {
        Value::List(list) => {
            let item = list[rand::random_range(0..list.len())].clone();
            Ok(Value::from(item))
        }
        _ => Err(value.conversion_err(&[ValueType::List])).span(span),
    })
}

pub const SLEEP_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::Exactly(1));
pub const SLEEP: &str = "sleep";
pub async fn sleep(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let arg = args.next().unwrap();
    let arg_loc = arg.span;
    let delay = potential_future!(arg.to_float(data.clone())?);
    if !delay.is_finite() {
        Err(RuntimeError::NonFiniteValue.span(command.span))
    } else if delay.is_sign_negative() {
        Err(
            RuntimeError::InvalidArgument("sleep time cannot be a negative number".into())
                .span(arg_loc),
        )
    } else if delay > Duration::MAX.as_secs_f64() {
        Err(RuntimeError::InvalidArgument(format!(
            "sleep time cannot exceed {} seconds",
            Duration::MAX.as_secs_f64()
        ))
        .span(arg_loc))
    } else {
        tokio::time::sleep(Duration::from_secs_f64(delay)).await;
        Ok(Value::None)
    }
}

pub const STOPWATCH_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtLeast(1));
pub const STOPWATCH: &str = "stopwatch";
pub async fn stopwatch(
    command: Command,
    data: Arc<InterpreterData>,
) -> Result<Value, SpannedError> {
    let mut command = command;
    let args = command.args.iter_mut();
    let start = std::time::Instant::now();
    let mut final_value = Value::None;
    for command in args {
        let command = potential_future!(command.to_command(data.clone())?);
        final_value = execute_command!(command, data.clone())?;
    }
    let elapsed = start.elapsed().as_secs_f64();
    let return_value = Value::from(FslMap::from([
        (SourceStr::Static("value"), final_value),
        (SourceStr::Static("elapsed"), Value::from(elapsed)),
    ]));
    Ok(return_value)
}

fn alias_parameter(
    parameter: &mut Value,
    aliases: &HashMap<SourceStr, SourceStr>,
    data: Arc<InterpreterData>,
) {
    match parameter {
        Value::List(values) => {
            for value in values.iter_mut() {
                alias_parameter(value, aliases, data.clone());
            }
        }
        Value::Map(map) => {
            for value in map.values_mut() {
                alias_parameter(value, aliases, data.clone());
            }
        }
        Value::Command(command) => {
            for arg in command.args.iter_mut() {
                let mut value = arg.take_value();
                alias_parameter(&mut value, &aliases, data.clone());
                arg.replace_value(value);
            }
        }
        Value::Var(var) => {
            if let Some(alias) = aliases.get(var) {
                *parameter = Value::Var(alias.clone())
            }
        }
        _ => {}
    }
}

pub const RUN_RULES: CommandSignature = CommandSignature::AnyArgs;
pub async fn run(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();

    let command_label = args.next().unwrap().as_var_label(data.clone())?;

    data.vars.write().push();
    data.push_def(command_label.clone());

    let Some(UserDeclaration {
        definition: Some(def),
        ..
    }) = data.find_def(&command_label)
    else {
        return Err(RuntimeError::NonExistantCommand {
            label: command_label.into_owned_string(),
        })
        .span(command.span);
    };

    let (mut parameter_labels, commands) = { (def.parameters, def.commands) };

    if args.len() != parameter_labels.len() {
        return Err(RuntimeError::WrongArgCount {
            command_label: command_label.to_string(),
            expected: ExpectedArgs::Exactly(parameter_labels.len()),
            got: args.len(),
        }
        .span(command.span));
    }

    let mut final_value = Value::None;
    if parameter_labels.len() != 0 {
        let mut aliases: HashMap<SourceStr, SourceStr> = HashMap::new();
        let parameters = parameter_labels.len();
        for _ in 0..parameters {
            let arg = args.next().unwrap();
            let arg_span = arg.span;
            let parameter = parameter_labels.pop_front().unwrap();
            let value = potential_future!(arg.into_value(data.clone())?);
            match value {
                Value::Var(var) => {
                    aliases.insert(parameter, var);
                }
                _ => {
                    let value =
                        potential_future!(value.to_inner(data.clone()).span_future(arg_span)?);
                    data.vars
                        .write()
                        .insert(&parameter, Var::Mut(value))
                        .span(command.span)?;
                }
            }
        }
        for mut command in commands {
            let args = command.args.iter_mut();
            for arg in args {
                let mut value = arg.take_value();
                alias_parameter(&mut value, &aliases, data.clone());
                arg.replace_value(value);
            }
            final_value = execute_command!(command, data.clone())?;
            if data.get_return_flag() {
                data.set_return_flag(false);
                break;
            }
        }
    } else {
        for command in commands {
            final_value = execute_command!(command, data.clone())?;
            if data.get_return_flag() {
                data.set_return_flag(false);
                break;
            }
        }
    }

    data.vars.write().pop();
    data.pop_def();

    Ok(final_value)
}

pub const BREAK: &str = "break";
pub fn r#break(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    if data.loop_depth() > 0 {
        data.set_break_flag(true);
    } else {
        return Err(RuntimeError::BreakOutsideLoop.span(command.span));
    }
    Ok(Value::None)
}

pub const CONTINUE: &str = "continue";
pub fn r#continue(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    if data.loop_depth() > 0 {
        data.set_continue_flag(true);
    } else {
        return Err(RuntimeError::ContinueOutsideLoop.span(command.span));
    }
    Ok(Value::None)
}

pub const RETURN_RULES: &CommandSignature = &CommandSignature::Count(ExpectedArgs::AtMost(1));
pub const RETURN: &str = "return";
pub async fn r#return(command: Command, data: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    let mut command = command;
    let mut args = command.args.iter_mut();
    let return_value = args.next();

    match return_value {
        Some(arg) => {
            let arg = potential_future!(arg.to_inner(data.clone())?);
            data.set_return_flag(true);
            Ok(arg)
        }
        None => {
            data.set_return_flag(true);
            Ok(Value::None)
        }
    }
}

pub const EXIT: &str = "exit";
pub fn exit(command: Command, _: Arc<InterpreterData>) -> Result<Value, SpannedError> {
    Err(RuntimeError::ProgramExited.span(command.span))
}

#[cfg(test)]
pub mod tests {
    use std::time::{Duration, SystemTime};

    use crate::{
        FslInterpreter, InterpreterError, RuntimeError, assert_fsl, assert_runtime_err,
        data::InterpreterData,
    };

    pub async fn test_interpreter(code: &str, expected_output: &str) {
        let mut interpreter = FslInterpreter::new();
        interpreter.register_all_libraries();
        let result = interpreter
            .interpret(code.to_string(), InterpreterData::default())
            .await;

        match &result {
            Ok(o) => println!("Result:\n{}", o),
            Err(e) => println!("Result:\n{}", e.to_string()),
        }

        println!("\n");
        let result = result.unwrap();
        println!("EXPECTED:\n{}\n", expected_output);
        println!("\nGOT:\n{}\n", &result);

        assert!(result == expected_output);
    }

    pub async fn observe_interpreter(code: &str) {
        let result = FslInterpreter::new()
            .interpret(code.to_string(), InterpreterData::default())
            .await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);
    }

    pub async fn test_interpreter_embedded(code: &str, expected_output: &str) {
        let result = FslInterpreter::new()
            .interpret_embedded_code(code.to_string(), InterpreterData::default())
            .await;

        println!("DEBUG");
        dbg!(&result);

        let result = result.unwrap();
        println!("PRETTY PRINT");
        println!("{}", &result);

        assert!(result == expected_output);
    }

    pub async fn test_interpreter_err_type(code: &str) -> InterpreterError {
        let result = FslInterpreter::new()
            .interpret(code.to_string(), InterpreterData::default())
            .await;
        dbg!(&result);
        assert!(result.is_err());
        result.err().unwrap()
    }

    pub async fn interpreter_throws_err(code: &str, err: InterpreterError) -> bool {
        let result = FslInterpreter::new()
            .interpret(code.to_string(), InterpreterData::default())
            .await;
        dbg!(&result);
        result.is_err_and(|e| {
            dbg!(&e);
            e == err
        })
    }

    pub async fn interpreter_results_in_error(code: &str) -> bool {
        let result = FslInterpreter::new()
            .interpret(code.to_string(), InterpreterData::default())
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
        assert_runtime_err!(err, RuntimeError::DivisionByZero)
    }

    #[tokio::test]
    async fn mod_by_zero() {
        let err = test_interpreter_err_type("print(mod(1, 0))").await;
        assert_runtime_err!(err, RuntimeError::DivisionByZero)
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
            "a.store([1, 2, 3]) b.store(a) print(b.clone())",
            "[1, 2, 3]",
        )
        .await;
    }

    #[tokio::test]
    async fn clone_var_param() {
        test_interpreter(
            r#"
                string.store("hello")
                test.def(s,
                    s.push("o")
                    return(s)
                )
                test(string.clone()).print("\n")
                string.print()
            "#,
            "helloo\nhello",
        )
        .await;
    }

    #[tokio::test]
    async fn take_var() {
        let err = test_interpreter_err_type("a.store(1) print(a) a.take() print(a)").await;
        assert_runtime_err!(err, RuntimeError::NonExistantVar { .. })
    }

    #[tokio::test]
    async fn take_params() {
        test_interpreter(
            r#"
                person.store([])
                make_person.def(name,
                    person.local([name: name.take()]).return()
                )
                person.update(make_person("jake"))
                person.print()
            "#,
            "[name: jake]",
        )
        .await;
    }

    #[tokio::test]
    async fn pass_in_taken() {
        test_interpreter(
            r#"
                person.store([])
                make_person.def(name,
                    debug(name)
                    person.local([name: name.take()]).return()
                )
                make_jake.def(
                    name.local("jake")
                    say("local name: ", name)
                    person.update(
                        make_person(name.take())
                    )
                )
                make_jake()
                person.print()
            "#,
            "[name: jake]",
        )
        .await;
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
        assert_runtime_err!(err, RuntimeError::NonExistantVar { .. })
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
        assert_runtime_err!(err, RuntimeError::InvalidConversion { .. })
    }

    #[tokio::test]
    async fn list_eq() {
        test_interpreter(
            r#"
                x.store([1, 2, 3])
                y.store([1, 2, 3])
                x.eq(y).print(" ")
                x.push(4)
                x.eq(y).print()
            "#,
            "true false",
        )
        .await;
        let err = test_interpreter_err_type(r#"print(eq(1, "a"))"#).await;
        assert_runtime_err!(err, RuntimeError::InvalidConversion { .. })
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
    async fn for_each_list() {
        test_interpreter(
            r#"
                list.store([1, 2, 3])
                list.for_each(n,
                    n.update(n.mul(2))
                )
                list.print()

            "#,
            "[2, 4, 6]",
        )
        .await;
    }

    #[tokio::test]
    async fn for_each_return() {
        test_interpreter(
            r#"
                list.store([1, 2, 3])
                func.def(
                    list.for_each(n,
                        n.update(n.mul(2))
                        return()
                    )
                )
                func()
                list.print()

            "#,
            "[2, 2, 3]",
        )
        .await;
    }

    #[tokio::test]
    async fn for_each_return_text() {
        test_interpreter(
            r#"
                text.store("hello")
                func.def(
                    text.for_each(char,
                        char.update("l")
                        return()
                    )
                )
                func()
                text.print()

            "#,
            "lello",
        )
        .await;
    }

    #[tokio::test]
    async fn for_each_mutation() {
        test_interpreter(
            r#"
                list.store([1, 2, 3])
                func.def(
                    list.for_each(n,
                        list.push(n)
                    )
                )
                func()
                list.print()

            "#,
            "[1, 2, 3]",
        )
        .await;
    }

    #[tokio::test]
    async fn for_each_return_value() {
        test_interpreter(
            r#"
                list.store([10, 2, 3])
                func.def(
                    list.for_each(n,
                        n.update(n.mul(2))
                        return(n)
                    )
                )
                func().print()

            "#,
            "20",
        )
        .await;
    }

    #[tokio::test]
    async fn for_each_text() {
        test_interpreter(
            r#"
                text.store("ñbéñbcñbé")
                text.for_each(c,
                    switch(c,
                        case("ñ",
                            c.update("n")
                        )
                        case("é",
                            c.update("e")
                        )
                        fallback()
                    )
                )
                text.print()

            "#,
            "nbenbcnbe",
        )
        .await;
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
            r#"
                player.store([name: "player", health: 100])
                player.remove("name").print()
            "#,
            "player",
        )
        .await;
    }

    #[tokio::test]
    async fn remove_nested_map() {
        test_interpreter(
            r#"
                player.store([name: "player", health: 100, weapons: [sword: [name: "sword"] ] ])
                player.weapons.sword.remove().name.print()
            "#,
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
    async fn remove_accessor_with_command() {
        assert_fsl!(
            r#"
                list.store([1, 2, 3])
                list.def(list, list.return())
                list(list).0.remove().print()
            "#,
            "1"
        )
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
    async fn scale() {
        test_interpreter(r#"i.store(1) i.scale(1.5).print()"#, "1.5").await;
    }

    #[tokio::test]
    async fn reduce() {
        test_interpreter(r#"i.store(3) i.reduce(2).print()"#, "1.5").await;
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
    async fn starts_with_text_1() {
        test_interpreter(r#"starts_with("text", "t").print()"#, "true").await;
    }

    #[tokio::test]
    async fn starts_with_text_2() {
        test_interpreter(r#"starts_with("text", "f").print()"#, "false").await;
    }

    #[tokio::test]
    async fn starts_with_list_1() {
        test_interpreter(r#"starts_with([1, 2, 3], 1).print()"#, "true").await;
    }

    #[tokio::test]
    async fn starts_with_list_2() {
        test_interpreter(r#"starts_with([1, 2, 3], 2).print()"#, "false").await;
    }

    #[tokio::test]
    async fn starts_with_list_3() {
        test_interpreter(r#"starts_with([1, 2, 3], [1, 2]).print()"#, "true").await;
    }

    #[tokio::test]
    async fn starts_with_list_4() {
        test_interpreter(r#"starts_with([1, 2, 3], ["d", 2]).print()"#, "false").await;
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
    async fn prepend() {
        test_interpreter(
            r#"
                "example".prepend("-> ").print()
            "#,
            "-> example",
        )
        .await;
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
        assert_runtime_err!(err, RuntimeError::BreakOutsideLoop)
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
        assert_runtime_err!(err, RuntimeError::ContinueOutsideLoop)
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
    async fn range_replace() {
        test_interpreter(r#"range_replace("hello", [1, 5], "hhhh").print()"#, "hhhhh").await;
    }

    #[tokio::test]
    async fn range_replace_invalid_index() {
        let err = test_interpreter_err_type(r#"range_replace("café", [4, 5], "h").print()"#).await;
        assert_runtime_err!(err, RuntimeError::InvalidArgument(_))
    }

    #[tokio::test]
    async fn range_replace_var() {
        test_interpreter(
            r#"text.store("hello") text.range_replace([1, 5], "hhhh") text.print()"#,
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
    async fn is_number_text() {
        test_interpreter(r#"is_number("1").print()"#, "true").await;
        test_interpreter(r#"is_number("1.0").print()"#, "true").await;
        test_interpreter(r#"is_number("d").print()"#, "false").await;
        test_interpreter(r#"is_number("1.0.0").print()"#, "false").await;
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
        assert_runtime_err!(err, RuntimeError::InvalidConversion { .. })
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
        assert_runtime_err!(err, RuntimeError::AttemptToOverwriteConst { .. })
    }

    #[tokio::test]
    async fn const_cannot_be_taken() {
        let err = test_interpreter_err_type(
            r#"
                const(MY_CONST, 42)
                MY_CONST.take()
            "#,
        )
        .await;
        assert_runtime_err!(err, RuntimeError::AttemptToOverwriteConst { .. })
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
        assert_runtime_err!(err, RuntimeError::AttemptToOverwriteConst { .. })
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
        assert_runtime_err!(err, RuntimeError::NonExistantVar { .. })
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
    async fn update_constructor() {
        test_interpreter(
            r#"
                person.store([])
                create_person.def(name,
                    person.local([name: name]).return()
                )
                test.def(
                    person.update(
                        create_person("jake")
                    )
                )
                test()
                person.name.get().print()
            "#,
            "jake",
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
        assert_runtime_err!(err, RuntimeError::InvalidRange)
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
        assert_runtime_err!(err, RuntimeError::NonExistantKey { .. })
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
    async fn if_statement_without_then() {
        test_interpreter(
            r#"
                if(true,
                    print(true)
                )
                if(false,
                    print(false)
                )
            "#,
            "true",
        )
        .await;
    }

    #[tokio::test]
    async fn if_statement_without_then_with_scope() {
        test_interpreter(
            r#"
                if(true,
                    (
                        print("1")
                        print("2")
                        print("3")
                    )
                )
            "#,
            "123",
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
        let err = test_interpreter_err_type(
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
        )
        .await;
        assert_runtime_err!(err, RuntimeError::ElseIfMustBePairedWithElse)
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
        assert_runtime_err!(err, RuntimeError::SwitchMustHaveSingleFallbackCommand)
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
        assert_runtime_err!(err, RuntimeError::SwitchMustHaveSingleFallbackCommand)
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
    async fn invalid_key_list() {
        let err = test_interpreter_err_type(
            r#"
                index([1, add(1, 1), 3], ["one"]).print()
            "#,
        )
        .await;
        assert_runtime_err!(err, RuntimeError::NotAccessible { .. })
    }

    #[tokio::test]
    async fn invalid_key_map() {
        let err = test_interpreter_err_type(
            r#"
                get([test: 1], 1).print()
            "#,
        )
        .await;
        assert_runtime_err!(err, RuntimeError::NotAccessible { .. })
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
        assert_runtime_err!(err, RuntimeError::NonExistantVar { .. })
    }

    #[tokio::test]
    async fn stopwatch() {
        test_interpreter(
            r#"
                result.store(
                    stopwatch(
                        add(1, 2)
                        div(3, 4)
                        sub(1, 1)
                    )
                )
                result.value.get().print()
                if(result.elapsed.get().gt(0), print(1))
            "#,
            "01",
        )
        .await;
    }

    #[tokio::test]
    async fn construct_local() {
        test_interpreter(
            r#"
	            FULL_HEALTH.const(100)
	            DEFAULT_REGEN.const(0)
	            DEFAULT_STRENGTH.const(0)
	            DEFAULT_DODGE.const(0)
	            DEFAULT_SPEED.const(1)
	            DEFAULT_LUCK.const(0)
	            DEFAULT_ARMOR.const(0)

	            *modifer = [value: 5, duration: 5]*
	            create_character.def(name, actions, gold, image,
	            	character.local([
	            		name: name,
	            		health: FULL_HEALTH,
	            		regen: DEFAULT_REGEN,
	            		dodge: DEFAULT_DODGE,
	            		strength: DEFAULT_STRENGTH,
	            		speed: DEFAULT_SPEED,
	            		luck: DEFAULT_LUCK,
	            		armor: DEFAULT_ARMOR,
	            		actions: actions,
	            		gold: gold,
	            		image: image,
	            		modifiers: [
	            			health: [],
	            			regen: [],
	            			dodge: [],
	            			strength: [],
	            			speed: [],
	            			luck: [],
	            			armor: [],
	            		],
	            	]).return()
	            )


	        create_opponent.def(
	        	debug(
	        		create_character(
	        			"test",
	        			[],
	        			random_range(50, 100),
	        			"image.jpg",
	        		)
	        	)
	        )

	        create_opponent()
	        print("done")
            "#,
            "done",
        )
        .await;
    }
}
