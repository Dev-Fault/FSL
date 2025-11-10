use core::fmt;
use std::{collections::HashMap, ops::Range, sync::Arc};

type Error = String;

#[derive(Debug, Clone, PartialEq, Eq)]
enum FslType {
    Int,
    Float,
    Text,
    Bool,
    List,
    Var,
    Command,
    None,
}

impl FslType {
    fn as_str(&self) -> &str {
        match self {
            FslType::Int => "int",
            FslType::Float => "float",
            FslType::Text => "text",
            FslType::Bool => "bool",
            FslType::List => "list",
            FslType::Var => "var",
            FslType::Command => "command",
            FslType::None => "none",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum ArgCount {
    Finite(usize),
    Infinite,
}

#[derive(Debug, Clone)]
enum ArgRange {
    Index(usize),
    Range(Range<usize>),
    Infinite,
}

#[derive(Debug, Clone)]
struct ArgRule {
    range: ArgRange,
    fsl_types: Vec<FslType>,
}

impl ArgRule {
    fn new(range: ArgRange, fsl_types: Vec<FslType>) -> Self {
        Self { range, fsl_types }
    }
}

type CommandFn = dyn Fn(Vec<Value>, &VarMap) -> Result<Value, Error> + Send + Sync;
type Executor = Arc<CommandFn>;

pub struct Command {
    label: String,
    arg_rules: Vec<ArgRule>,
    args: Vec<Value>,
    executor: Executor,
}

impl Command {
    fn new(label: &str, arg_rules: Vec<ArgRule>, executor: Executor) -> Self {
        Self {
            label: label.to_string(),
            arg_rules,
            args: vec![],
            executor,
        }
    }

    fn set_args(&mut self, args: Vec<Value>) {
        self.args = args;
    }

    fn validate_arg_range(&self, arg_rule: &ArgRule, range: &Range<usize>) -> Result<(), Error> {
        for (i, arg) in self.args[range.start..range.end].iter().enumerate() {
            let fsl_type = arg.as_type();
            if !arg_rule.fsl_types.contains(&fsl_type) {
                return Err(format!(
                    "Arg {} of command {} cannot be of type {}\nValid types are {:?}",
                    i,
                    self.label,
                    fsl_type.as_str(),
                    arg_rule.fsl_types
                ));
            }
        }
        Ok(())
    }

    fn execute(&self, vars: &VarMap) -> Result<Value, Error> {
        let mut max_args = 0;
        for arg_rule in &self.arg_rules {
            match &arg_rule.range {
                ArgRange::Index(i) => {
                    max_args = if max_args < (*i + 1) {
                        *i + 1
                    } else {
                        max_args
                    };
                    let range = *i..*i + 1;
                    match self.args.get(*i) {
                        Some(_) => {
                            self.validate_arg_range(arg_rule, &range)?;
                        }
                        None => {
                            return Err(format!(
                                "Arg {} of command {} must be present and be of type {:?}",
                                i, self.label, arg_rule.fsl_types
                            ));
                        }
                    }
                }
                ArgRange::Range(range) => {
                    max_args = if max_args < range.end {
                        range.end
                    } else {
                        max_args
                    };
                    if self.args.len() < range.start {
                        return Err(format!(
                            "Command {} must have at least {} arguments and only {} were given",
                            self.label,
                            range.start,
                            self.args.len(),
                        ));
                    } else if self.args.len() > range.end {
                        return Err(format!(
                            "Command {} must have no more than {} arguments and {} were given",
                            self.label,
                            range.end,
                            self.args.len(),
                        ));
                    } else {
                        self.validate_arg_range(arg_rule, range)?;
                    }
                }
                ArgRange::Infinite => {
                    max_args = usize::MAX;
                    let range = Range::from(0..self.args.len());
                    self.validate_arg_range(arg_rule, &range)?;
                }
            }
        }

        if self.args.len() > max_args {
            return Err(format!(
                "Command {} expected {} args but got {}",
                self.label,
                max_args,
                self.args.len()
            ));
        }

        Ok((self.executor)(self.args.clone(), vars)?)
    }
}

impl PartialEq for Command {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

impl fmt::Debug for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Command")
            .field("label", &self.label)
            .finish()
    }
}

impl Clone for Command {
    fn clone(&self) -> Self {
        Self {
            label: self.label.clone(),
            arg_rules: self.arg_rules.clone(),
            args: self.args.clone(),
            executor: self.executor.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i64),
    Float(f64),
    Text(String),
    Bool(bool),
    List(Vec<Value>),
    Var(String),
    Command(Arc<Command>),
    None,
}

struct VarMap(HashMap<String, Value>);

impl VarMap {
    pub fn new() -> Self {
        Self { 0: HashMap::new() }
    }

    pub fn insert_value(&mut self, label: &str, value: &Value) {
        self.0.insert(label.to_string(), value.clone());
    }

    pub fn get_value(&self, label: &str) -> Result<Value, Error> {
        match self.0.get(label) {
            Some(value) => {
                if value.is_type(FslType::Var) {
                    return self.get_value(&value.get_var_label()?);
                } else {
                    Ok(value.clone())
                }
            }
            None => Err(format!("tried to get value of non existant var {}", label)),
        }
    }
}

impl Value {
    pub fn as_type(&self) -> FslType {
        match self {
            Value::Int(_) => FslType::Int,
            Value::Float(_) => FslType::Float,
            Value::Text(_) => FslType::Text,
            Value::Bool(_) => FslType::Bool,
            Value::List(_) => FslType::List,
            Value::Var(_) => FslType::Var,
            Value::Command(_) => FslType::Command,
            Value::None => FslType::None,
        }
    }

    pub fn is_type(&self, fsl_type: FslType) -> bool {
        return self.as_type() == fsl_type;
    }

    pub fn as_int(&self, vars: &VarMap) -> Result<i64, Error> {
        match self {
            Value::Int(value) => Ok(value.clone()),
            Value::Float(value) => Ok(value.clone() as i64),
            Value::Var(label) => vars.get_value(label)?.as_int(vars),
            Value::Command(command) => command.execute(vars)?.as_int(vars),
            _ => Err("invalid Int conversion".into()),
        }
    }

    pub fn as_float(&self, vars: &VarMap) -> Result<f64, Error> {
        match self {
            Value::Int(value) => Ok(value.clone() as f64),
            Value::Float(value) => Ok(value.clone()),
            Value::Var(label) => vars.get_value(label)?.as_float(vars),
            Value::Command(command) => command.execute(vars)?.as_float(vars),
            _ => Err("invalid Int conversion".into()),
        }
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        if let Value::Text(value) = self {
            Ok(value)
        } else {
            Err("invalid Text conversion".into())
        }
    }

    pub fn as_bool(&self) -> Result<bool, Error> {
        if let Value::Bool(value) = self {
            Ok(value.clone())
        } else {
            Err("invalid Bool conversion".into())
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>, Error> {
        if let Value::List(value) = self {
            Ok(value)
        } else {
            Err("invalid Text conversion".into())
        }
    }

    pub fn as_command(&self) -> Result<Arc<Command>, Error> {
        if let Value::Command(command) = self {
            Ok(command.clone())
        } else {
            Err("invalid Command conversion".into())
        }
    }

    pub fn get_var_label(&self) -> Result<String, Error> {
        if let Value::Var(value) = self {
            Ok(value.clone())
        } else {
            Err("tried to get label of non Var type".into())
        }
    }
}

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

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Float(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Text(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Text(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<Arc<Command>> for Value {
    fn from(value: Arc<Command>) -> Self {
        Value::Command(value.clone())
    }
}

pub const ADD_LABEL: &str = "add";
pub const SUB_LABEL: &str = "sub";
pub const MUL_LABEL: &str = "mul";
pub const DIV_LABEL: &str = "div";
pub const MOD_LABEL: &str = "mod";

const NUMBER_TYPES: &[FslType] = &[FslType::Int, FslType::Float, FslType::Command, FslType::Var];

pub type CommandMap = HashMap<String, Command>;

struct FslInterpreter {
    std_commands: CommandMap,
    custom_commands: CommandMap,
    var_map: VarMap,
}

impl FslInterpreter {
    pub fn new() -> Self {
        Self {
            std_commands: Self::construct_std_lib(),
            custom_commands: CommandMap::new(),
            var_map: VarMap::new(),
        }
    }

    fn construct_std_lib() -> CommandMap {
        let mut lib = HashMap::new();

        let math_rules = vec![
            ArgRule::new(ArgRange::Infinite, NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(1), NUMBER_TYPES.into()),
        ];

        let add = Command::new(ADD_LABEL, math_rules.clone(), Arc::new(Self::add));
        lib.insert(add.label.clone(), add);

        let sub = Command::new(SUB_LABEL, math_rules.clone(), Arc::new(Self::sub));
        lib.insert(sub.label.clone(), sub);

        let mul = Command::new(MUL_LABEL, math_rules.clone(), Arc::new(Self::mul));
        lib.insert(mul.label.clone(), mul);

        let div = Command::new(DIV_LABEL, math_rules.clone(), Arc::new(Self::div));
        lib.insert(div.label.clone(), div);

        let modulus = Command::new(MOD_LABEL, math_rules.clone(), Arc::new(Self::modulus));
        lib.insert(modulus.label.clone(), modulus);

        let repeat_rules = vec![
            ArgRule::new(ArgRange::Index(0), NUMBER_TYPES.into()),
            ArgRule::new(ArgRange::Index(1), vec![FslType::Command]),
        ];
        let repeat = Command::new("repeat", repeat_rules, Arc::new(Self::repeat));
        lib.insert(repeat.label.clone(), repeat);

        lib
    }

    pub fn add(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
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

    pub fn sub(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
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

    pub fn mul(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
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

    pub fn div(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
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

    pub fn modulus(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
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

    pub fn repeat(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let repetitions = values[0].as_int(vars)?;
        let command = values[1].as_command()?;
        let mut final_value = Value::None;
        for i in 0..repetitions {
            final_value = command.execute(vars)?;
        }

        Ok(final_value)
    }

    pub fn store(values: Vec<Value>, vars: &VarMap) -> Result<Value, Error> {
        let mut values = values.clone();
        let var = values.pop().unwrap();

        todo!();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const NUMBER_TYPES: &[FslType] =
        &[FslType::Int, FslType::Float, FslType::Command, FslType::Var];

    fn get_add() -> Command {
        FslInterpreter::construct_std_lib()
            .get(ADD_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_sub() -> Command {
        FslInterpreter::construct_std_lib()
            .get(SUB_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_mul() -> Command {
        FslInterpreter::construct_std_lib()
            .get(MUL_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_div() -> Command {
        FslInterpreter::construct_std_lib()
            .get(DIV_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_mod() -> Command {
        FslInterpreter::construct_std_lib()
            .get(MOD_LABEL)
            .cloned()
            .unwrap()
    }

    fn get_repeat() -> Command {
        FslInterpreter::construct_std_lib()
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
