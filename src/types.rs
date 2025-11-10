use core::fmt;
use std::{collections::HashMap, ops::Range, sync::Arc};

pub type Error = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FslType {
    Int,
    Float,
    Text,
    Bool,
    List,
    Var,
    Command,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Text(String),
    Bool(bool),
    List(Vec<Value>),
    Var(String),
    Command(Arc<Command>),
    None,
}

pub struct VarMap(HashMap<String, Value>);

#[derive(Debug, Clone)]
pub enum ArgRange {
    Index(usize),
    Range(Range<usize>),
    Infinite,
}

#[derive(Debug, Clone)]
pub struct ArgRule {
    range: ArgRange,
    fsl_types: Vec<FslType>,
}

type CommandFn = dyn Fn(Vec<Value>, &VarMap) -> Result<Value, Error> + Send + Sync;
type Executor = Arc<CommandFn>;

pub struct Command {
    label: String,
    arg_rules: Vec<ArgRule>,
    args: Vec<Value>,
    executor: Executor,
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

impl ArgRule {
    pub fn new(range: ArgRange, fsl_types: Vec<FslType>) -> Self {
        Self { range, fsl_types }
    }
}

impl Command {
    pub fn new(label: &str, arg_rules: Vec<ArgRule>, executor: Executor) -> Self {
        Self {
            label: label.to_string(),
            arg_rules,
            args: vec![],
            executor,
        }
    }

    pub fn get_label(&self) -> String {
        self.label.clone()
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
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

    pub fn execute(&self, vars: &VarMap) -> Result<Value, Error> {
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
