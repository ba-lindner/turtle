use std::fmt::Display;

use super::Keyword;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValType {
    Any,
    Number,
    String,
    Boolean,
}

impl ValType {
    pub fn default(&self) -> Value {
        match self {
            ValType::Any => panic!("i don't know what to do here"),
            ValType::Number => Value::Number(0.0),
            ValType::String => Value::String(String::new()),
            ValType::Boolean => Value::Boolean(false),
        }
    }

    pub fn parse(kw: Keyword) -> Option<Self> {
        match kw {
            Keyword::Num => Some(Self::Number),
            Keyword::String => Some(Self::String),
            Keyword::Bool => Some(Self::Boolean),
            _ => None,
        }
    }
}

impl Display for ValType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValType::Any => write!(f, "unknown"),
            ValType::Number => write!(f, "num"),
            ValType::String => write!(f, "string"),
            ValType::Boolean => write!(f, "bool"),
        }
    }
}

impl From<f64> for ValType {
    fn from(_: f64) -> Self {
        Self::Number
    }
}

impl From<String> for ValType {
    fn from(_: String) -> Self {
        Self::String
    }
}

impl From<&str> for ValType {
    fn from(_: &str) -> Self {
        Self::String
    }
}

impl From<bool> for ValType {
    fn from(_: bool) -> Self {
        Self::Boolean
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
}

impl Value {
    pub fn val_type(&self) -> ValType {
        match self {
            Value::Number(_) => ValType::Number,
            Value::String(_) => ValType::String,
            Value::Boolean(_) => ValType::Boolean,
        }
    }

    pub fn convert(&self, to: ValType) -> Value {
        match to {
            ValType::Number => self.num().into(),
            ValType::String => self.string().into(),
            ValType::Boolean => self.bool().into(),
            ValType::Any => self.clone(),
        }
    }

    pub fn num(&self) -> f64 {
        match self {
            Value::Number(val) => *val,
            Value::String(s) => s.parse().unwrap_or_default(),
            Value::Boolean(b) => *b as i32 as f64,
        }
    }

    pub fn string(&self) -> String {
        match self {
            Value::Number(val) => val.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
        }
    }

    pub fn bool(&self) -> bool {
        match self {
            Value::Number(val) => *val != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Boolean(b) => *b,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{num}"),
            Value::String(s) => write!(f, "'{s}'"),
            Value::Boolean(b) => write!(f, "{b}"),
        }
    }
}

impl From<&Value> for f64 {
    fn from(value: &Value) -> Self {
        value.num()
    }
}

impl From<&Value> for String {
    fn from(value: &Value) -> Self {
        value.string()
    }
}

impl<'s> From<&'s Value> for &'s str {
    fn from(value: &'s Value) -> &'s str {
        match value {
            Value::Number(_) => unimplemented!("should actually never be reached"),
            Value::String(s) => s,
            Value::Boolean(b) => {
                if *b {
                    "true"
                } else {
                    "false"
                }
            }
        }
    }
}

impl From<&Value> for bool {
    fn from(value: &Value) -> Self {
        value.bool()
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}
