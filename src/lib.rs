use core::fmt;
use std::{collections::HashMap, ops::Range, sync::Arc};

use rand::random_range;

use crate::types::{ArgRange, ArgRule, Command, Error, FslType, Value, VarMap};

mod types;

pub const ADD_LABEL: &str = "add";
pub const SUB_LABEL: &str = "sub";
pub const MUL_LABEL: &str = "mul";
pub const DIV_LABEL: &str = "div";
pub const MOD_LABEL: &str = "mod";

const NUMBER_TYPES: &[FslType] = &[FslType::Int, FslType::Float, FslType::Command, FslType::Var];

trait ContainsFloat {
    fn contains_float(&self) -> bool;
}

impl ContainsFloat for Vec<Value> {
    fn contains_float(&self) -> bool {
        for value in self {
            match value {
                Value::Float(_) => {
                    return true;
                }
                _ => {
                    continue;
                }
            }
        }
        false
    }
}

pub type CommandMap = HashMap<String, Command>;

struct FslInterpreter {
    std_out: String,
    std_commands: CommandMap,
    custom_commands: CommandMap,
    var_map: VarMap,
}

impl FslInterpreter {
    pub fn new() -> Self {
        Self {
            std_out: String::new(),
            std_commands: Self::construct_std_commands(),
            custom_commands: CommandMap::new(),
            var_map: VarMap::new(),
        }
    }

    fn construct_std_commands() -> CommandMap {
        let mut lib = HashMap::new();

        let math_rules = vec![
            ArgRule::new(ArgRange::Infinite, NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(1), NUMBER_TYPES.into()),
        ];

        let add = Command::new(ADD_LABEL, math_rules.clone(), Arc::new(Self::add));
        lib.insert(add.get_label(), add);

        let sub = Command::new(SUB_LABEL, math_rules.clone(), Arc::new(Self::sub));
        lib.insert(sub.get_label(), sub);

        let mul = Command::new(MUL_LABEL, math_rules.clone(), Arc::new(Self::mul));
        lib.insert(mul.get_label(), mul);

        let div = Command::new(DIV_LABEL, math_rules.clone(), Arc::new(Self::div));
        lib.insert(div.get_label(), div);

        let modulus = Command::new(MOD_LABEL, math_rules.clone(), Arc::new(Self::modulus));
        lib.insert(modulus.get_label(), modulus);

        let repeat_rules = vec![
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(1), vec![FslType::Command]),
        ];
        let repeat = Command::new("repeat", repeat_rules, Arc::new(Self::repeat));
        lib.insert(repeat.get_label(), repeat);

        lib
    }

    fn add(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        if values.contains_float() {
            let mut sum: f64 = 0.0;
            for value in values {
                sum = sum + value.as_float(vars)?;
            }
            Ok(Value::Float(sum))
        } else {
            let mut sum: i64 = 0;
            for value in values {
                sum = sum + value.as_int(vars)?;
            }
            Ok(Value::Int(sum))
        }
    }

    fn sub(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        if values.contains_float() {
            let mut diff = values[0].as_float(vars)?;
            for value in &values[1..values.len()] {
                diff = diff - value.as_float(vars)?;
            }
            Ok(Value::Float(diff))
        } else {
            let mut diff = values[0].as_int(vars)?;
            for value in &values[1..values.len()] {
                diff = diff - value.as_int(vars)?;
            }
            Ok(Value::Int(diff))
        }
    }

    fn mul(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        if values.contains_float() {
            let mut product = values[0].as_float(vars)?;
            for value in &values[1..values.len()] {
                product = product * value.as_float(vars)?;
            }
            Ok(Value::Float(product))
        } else {
            let mut product = values[0].as_int(vars)?;
            for value in &values[1..values.len()] {
                product = product * value.as_int(vars)?;
            }
            Ok(Value::Int(product))
        }
    }

    fn div(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        if values.contains_float() {
            let mut quotient = values[0].as_float(vars)?;
            for value in &values[1..values.len()] {
                let value = value.as_float(vars)?;
                if value == 0.0 {
                    return Err("division by zero".to_string());
                };
                quotient = quotient / value;
            }
            Ok(Value::Float(quotient))
        } else {
            let mut quotient = values[0].as_int(vars)?;
            for value in &values[1..values.len()] {
                let value = value.as_int(vars)?;
                if value == 0 {
                    return Err("division by zero".to_string());
                };
                quotient = quotient / value;
            }
            Ok(Value::Int(quotient))
        }
    }

    fn modulus(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let mut remainder = values[0].as_int(vars)?;
        for value in &values[1..values.len()] {
            let value = value.as_int(vars)?;
            if value == 0 {
                return Err("division by zero".to_string());
            };
            remainder = remainder % value;
        }
        Ok(Value::Int(remainder))
    }

    fn store(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let mut values = values.clone();
        let var = values.pop().unwrap();

        todo!();
    }

    fn clone(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn print(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn eq(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn gt(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn lt(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn not(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok((!values[0].as_bool()?).into())
    }

    fn and(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok((values[0].as_bool()? && values[1].as_bool()?).into())
    }

    fn or(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok((values[0].as_bool()? || values[1].as_bool()?).into())
    }

    fn if_then(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn if_then_else(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn while_loop(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn repeat(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let repetitions = values[0].as_int(vars)?;
        let command = values[1].as_command()?;
        let mut final_value = Value::None;
        for i in 0..repetitions {
            final_value = command.execute(vars)?;
        }

        Ok(final_value)
    }

    fn index_of(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn length_of(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn swap_at(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn insert_at(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn remove_at(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn replace_at(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        todo!()
    }

    fn starts_with(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[0]
            .as_text()?
            .starts_with(values[1].as_text()?)
            .into())
    }

    fn ends_with(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[0].as_text()?.ends_with(values[1].as_text()?).into())
    }

    fn concat(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let mut cat_string = String::new();

        for value in values {
            cat_string.push_str(value.as_text()?);
        }

        Ok(cat_string.into())
    }

    fn capitalize(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let text = values[0].as_text()?;
        if text.len() < 1 {
            Ok("".into())
        } else {
            Ok(format!("{}{}", text[0..1].to_uppercase(), text[1..].to_uppercase()).into())
        }
    }

    fn upper(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[0].as_text()?.to_uppercase().into())
    }

    fn lower(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[0].as_text()?.to_lowercase().into())
    }

    fn remove_whitespace(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[0]
            .as_text()?
            .split_whitespace()
            .collect::<String>()
            .into())
    }

    fn nl(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok("\n".into())
    }

    fn random_range(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        if values.contains_float() {
            let min = values[0].as_float(vars)?;
            let max = values[1].as_float(vars)?;
            if min >= max {
                Err("min must be greater than max".to_string())
            } else {
                Ok(random_range(min..max).into())
            }
        } else {
            let min = values[0].as_int(vars)?;
            let max = values[1].as_int(vars)?;
            if min >= max {
                Err("min must be greater than max".to_string())
            } else {
                Ok(random_range(min..max).into())
            }
        }
    }

    fn random_entry(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        Ok(values[random_range(0..values.len())].clone())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const NUMBER_TYPES: &[FslType] =
        &[FslType::Int, FslType::Float, FslType::Command, FslType::Var];

    fn get_add() -> Command {
        FslInterpreter::construct_std_commands()
            .get(ADD_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_sub() -> Command {
        FslInterpreter::construct_std_commands()
            .get(SUB_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_mul() -> Command {
        FslInterpreter::construct_std_commands()
            .get(MUL_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_div() -> Command {
        FslInterpreter::construct_std_commands()
            .get(DIV_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_mod() -> Command {
        FslInterpreter::construct_std_commands()
            .get(MOD_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_repeat() -> Command {
        FslInterpreter::construct_std_commands()
            .get("repeat")
            .cloned()
            .unwrap()
    }

    fn test_math(
        args: &Vec<Value>,
        command: &mut Command,
        expected_value: Value,
        vars: Option<&VarMap>,
    ) {
        let args = args.clone();
        let no_vars = VarMap::new();
        let vars = match vars {
            Some(vars) => vars,
            None => &no_vars,
        };
        match expected_value {
            Value::None => {
                // Should be error
                command.set_args(args);
                let output = command.execute(&vars);
                dbg!(&output);
                match output {
                    Ok(_) => panic!(),
                    Err(e) => {
                        dbg!(e);
                    }
                }
            }
            _ => {
                command.set_args(args);
                let output = command.execute(&vars).unwrap();
                dbg!(&output);
                assert!(output == expected_value);
            }
        }
    }

    #[test]
    fn math_two_ints() {
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_add(),
            Value::Int(9),
            None,
        );
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_sub(),
            Value::Int(1),
            None,
        );
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_mul(),
            Value::Int(20),
            None,
        );
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_div(),
            Value::Int(1),
            None,
        );
        test_math(
            &vec![5.into(), 4.into()],
            &mut get_mod(),
            Value::Int(1),
            None,
        );
    }

    #[test]
    fn math_two_floats() {
        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_add(),
            Value::Float(9.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_sub(),
            Value::Float(1.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_mul(),
            Value::Float(20.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_div(),
            Value::Float(1.25),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.0.into()],
            &mut get_mod(),
            Value::Int(1),
            None,
        );
    }

    #[test]
    fn math_int_and_float() {
        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_add(),
            Value::Float(9.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_sub(),
            Value::Float(1.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_mul(),
            Value::Float(20.0),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_div(),
            Value::Float(1.25),
            None,
        );

        test_math(
            &vec![5.0.into(), 4.into()],
            &mut get_mod(),
            Value::Int(1),
            None,
        );
    }

    #[test]
    fn math_no_args() {
        test_math(&vec![], &mut get_add(), Value::None, None);
        test_math(&vec![], &mut get_sub(), Value::None, None);
        test_math(&vec![], &mut get_mul(), Value::None, None);
        test_math(&vec![], &mut get_div(), Value::None, None);
        test_math(&vec![], &mut get_mod(), Value::None, None);
    }

    #[test]
    fn math_one_arg() {
        test_math(&vec![1.into()], &mut get_add(), Value::None, None);
        test_math(&vec![1.into()], &mut get_sub(), Value::None, None);
        test_math(&vec![1.into()], &mut get_mul(), Value::None, None);
        test_math(&vec![1.into()], &mut get_div(), Value::None, None);
        test_math(&vec![1.into()], &mut get_mod(), Value::None, None);
    }

    #[test]
    fn math_wrong_type() {
        test_math(
            &vec!["1".into(), Value::Int(1)],
            &mut get_add(),
            Value::None,
            None,
        );

        test_math(
            &vec!["1".into(), Value::Int(1)],
            &mut get_sub(),
            Value::None,
            None,
        );

        test_math(
            &vec!["1".into(), Value::Int(1)],
            &mut get_mul(),
            Value::None,
            None,
        );

        test_math(
            &vec!["1".into(), Value::Int(1)],
            &mut get_div(),
            Value::None,
            None,
        );

        test_math(
            &vec!["1".into(), Value::Int(1)],
            &mut get_mod(),
            Value::None,
            None,
        );
    }

    #[test]
    fn math_many_args() {
        let args = vec![5.0.into(), 3.into(), 3.into(), 100.into(), 57.into()];
        test_math(&args, &mut get_add(), Value::Float(168.0), None);
        test_math(&args, &mut get_sub(), Value::Float(-158.0), None);
        test_math(&args, &mut get_mul(), Value::Float(256500.0), None);
        test_math(
            &args,
            &mut get_div(),
            Value::Float(9.746588693957115e-5),
            None,
        );
        test_math(&args, &mut get_mod(), Value::Int(2), None);
    }

    #[test]
    fn math_vars() {
        let mut vars = VarMap::new();
        vars.insert_value("one", &1.into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(&args, &mut get_add(), Value::Int(2), Some(&vars));
        test_math(&args, &mut get_sub(), Value::Int(0), Some(&vars));
        test_math(&args, &mut get_mul(), Value::Int(1), Some(&vars));
        test_math(&args, &mut get_div(), Value::Int(1), Some(&vars));
        test_math(&args, &mut get_mod(), Value::Int(0), Some(&vars));
    }

    #[test]
    fn math_wrong_var() {
        let mut vars = VarMap::new();
        vars.insert_value("one", &"1".into());
        let var = Value::Var("one".to_string());
        let args = vec![var.clone(), var.clone()];
        test_math(&args, &mut get_add(), Value::None, Some(&vars));
        test_math(&args, &mut get_sub(), Value::None, Some(&vars));
        test_math(&args, &mut get_mul(), Value::None, Some(&vars));
        test_math(&args, &mut get_div(), Value::None, Some(&vars));
        test_math(&args, &mut get_mod(), Value::None, Some(&vars));
    }

    #[test]
    fn math_commands() {
        let interpreter = FslInterpreter::new();
        let args = vec![5.into(), 4.into()];
        let mut command = interpreter.std_commands.get("add").cloned().unwrap();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![command.clone().into(), command.clone().into()];

        test_math(&args, &mut get_add(), Value::Int(18), None);
        test_math(&args, &mut get_sub(), Value::Int(0), None);
        test_math(&args, &mut get_mul(), Value::Int(81), None);
        test_math(&args, &mut get_div(), Value::Int(1), None);
        test_math(&args, &mut get_mod(), Value::Int(0), None);
    }

    #[test]
    fn division_by_zero() {
        let args = vec![1.into(), 0.into()];
        test_math(&args, &mut get_div(), Value::None, None);
        test_math(&args, &mut get_mod(), Value::None, None);
    }

    #[test]
    fn repeat_add() {
        let vars = VarMap::new();
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command)];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute(&vars).unwrap();
        dbg!(&output);
        assert!(output == Value::Int(9));
    }

    #[test]
    #[should_panic]
    fn repeat_wrong_repititions() {
        let vars = VarMap::new();
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec!["5".into(), Value::Command(command)];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute(&vars);
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    #[should_panic]
    fn repeat_wrong_thing_to_repeat() {
        let vars = VarMap::new();
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), 5.into()];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute(&vars);
        dbg!(&output);
        output.unwrap();
    }

    #[test]
    #[should_panic]
    fn repeat_too_many_args() {
        let vars = VarMap::new();
        let args = vec![5.into(), 4.into()];
        let mut command = get_add();
        command.set_args(args);
        let command = Arc::new(command);

        let args = vec![5.into(), Value::Command(command), 5.into()];
        let mut command = get_repeat();
        command.set_args(args);
        let output = command.execute(&vars);
        dbg!(&output);
        output.unwrap();
    }
}
