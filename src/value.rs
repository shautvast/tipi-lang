use crate::DATE_FORMAT_TIMEZONE;
use crate::errors::ValueError;
use chrono::{DateTime, Utc};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Shl, Shr, Sub};

#[derive(Debug, Clone)]
pub struct Object {
    pub(crate) definition: String,
    pub(crate) fields: Vec<(String, Value)>,
}

#[derive(Debug, Clone)]
pub enum Value {
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
    String(String),
    Char(char),
    Bool(bool),
    DateTime(Box<DateTime<Utc>>),
    Enum,
    List(Vec<Value>),
    Map(HashMap<Value, Value>),
    ObjectType(Box<Object>),
    Error(String),
    Void,
}

pub(crate) fn string(v: impl Into<String>) -> Value {
    Value::String(v.into())
}

pub(crate) fn _i64(v: impl Into<i64>) -> Value {
    Value::I64(v.into())
}

pub(crate) fn u64(v: impl Into<u64>) -> Value {
    Value::U64(v.into())
}

pub(crate) fn bool(v: impl Into<bool>) -> Value {
    Value::Bool(v.into())
}

impl Value {
    pub fn cast_u32(self) -> Result<Self, ValueError> {
        match self {
            Value::U32(v) => Ok(Value::U32(v)),
            Value::U64(v) => Ok(Value::U32(v as u32)),
            Value::I32(v) => Ok(Value::U32(v as u32)),
            Value::I64(v) => Ok(Value::U32(v as u32)),
            Value::F32(v) => Ok(Value::U32(v as u32)),
            Value::F64(v) => Ok(Value::U32(v as u32)),
            _ => Err(ValueError::IllegalCast),
        }
    }

    pub fn cast_u64(self) -> Result<Self, ValueError> {
        match self {
            Value::U32(v) => Ok(Value::U64(v as u64)),
            Value::U64(v) => Ok(Value::U64(v)),
            Value::I32(v) => Ok(Value::U64(v as u64)),
            Value::I64(v) => Ok(Value::U64(v as u64)),
            Value::F32(v) => Ok(Value::U64(v as u64)),
            Value::F64(v) => Ok(Value::U64(v as u64)),
            _ => Err(ValueError::IllegalCast),
        }
    }

    pub fn cast_usize(self) -> Result<usize, ValueError> {
        match self {
            Value::U32(v) => Ok(v as usize),
            Value::U64(v) => Ok(v as usize),
            Value::I32(v) => Ok(v as usize),
            Value::I64(v) => Ok(v as usize),
            Value::F32(v) => Ok(v as usize),
            Value::F64(v) => Ok(v as usize),
            _ => Err(ValueError::IllegalCast),
        }
    }

    pub fn cast_i32(self) -> Result<Self, ValueError> {
        match self {
            Value::U32(v) => Ok(Value::I32(v as i32)),
            Value::U64(v) => Ok(Value::I32(v as i32)),
            Value::I32(v) => Ok(Value::I32(v)),
            Value::I64(v) => Ok(Value::I32(v as i32)),
            Value::F32(v) => Ok(Value::I32(v as i32)),
            Value::F64(v) => Ok(Value::I32(v as i32)),
            _ => Err(ValueError::IllegalCast),
        }
    }

    pub fn cast_f32(self) -> Result<Self, ValueError> {
        match self {
            Value::U32(v) => Ok(Value::F32(v as f32)),
            Value::U64(v) => Ok(Value::F32(v as f32)),
            Value::I32(v) => Ok(Value::F32(v as f32)),
            Value::I64(v) => Ok(Value::F32(v as f32)),
            Value::F32(v) => Ok(Value::F32(v)),
            Value::F64(v) => Ok(Value::F32(v as f32)),
            _ => Err(ValueError::IllegalCast),
        }
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Value {
        Value::I32(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Value {
        Value::I64(v)
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Value {
        Value::U32(v)
    }
}
impl From<u64> for Value {
    fn from(v: u64) -> Value {
        Value::U64(v)
    }
}
impl From<f32> for Value {
    fn from(v: f32) -> Value {
        Value::F32(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Value {
        Value::F64(v)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Value {
        Value::String(v.to_string())
    }
}

impl From<String> for Value {
    fn from(v: String) -> Value {
        Value::String(v)
    }
}

impl From<char> for Value {
    fn from(v: char) -> Value {
        Value::Char(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Value {
        Value::Bool(v)
    }
}

impl From<DateTime<Utc>> for Value {
    fn from(v: DateTime<Utc>) -> Value {
        Value::DateTime(Box::new(v))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::U32(v) => write!(f, "{}", v),
            Value::U64(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::I32(v) => write!(f, "{}", v),
            Value::I64(v) => write!(f, "{}", v),
            Value::F32(v) => write!(f, "{}", v),
            Value::F64(v) => write!(f, "{}", v),
            Value::Char(v) => write!(f, "{}", v),
            Value::DateTime(v) => write!(f, "{}", v.format(DATE_FORMAT_TIMEZONE)),
            Value::Enum => write!(f, "enum"),
            Value::ObjectType(o) => write!(f, "{}: {:?}", o.definition, o.fields),
            Value::List(v) => {
                for i in &v[0..v.len() - 1] {
                    write!(f, "{}, ", i)?;
                }
                write!(f, "{}", v[v.len() - 1])
            }
            Value::Map(map) => to_string(f, map),
            Value::Error(v) => write!(f, "{}", v),
            Value::Void => write!(f, "()"),
        }
    }
}

fn to_string(f: &mut Formatter, map: &HashMap<Value, Value>) -> std::fmt::Result {
    f.write_str("{")?;
    let mut first = true;
    for (k, v) in map {
        if !first {
            f.write_str(", ")?;
        }
        f.write_str(&k.to_string())?;
        f.write_str(": ")?;
        f.write_str(&v.to_string())?;
        first = false;
    }
    f.write_str("}")
}

impl Neg for &Value {
    type Output = Result<Value, ValueError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::I32(i) => Ok(Value::I32(-i)),
            Value::I64(i) => Ok(Value::I64(-i)),
            Value::F32(i) => Ok(Value::F32(-i)),
            Value::F64(i) => Ok(Value::F64(-i)),
            _ => Err(ValueError::Some("Cannot negate")),
        }
    }
}

impl Add<&Value> for &Value {
    type Output = Result<Value, ValueError>;

    fn add(self, rhs: &Value) -> Self::Output {
        if let Value::List(s) = self {
            let mut copy = s.clone();
            copy.push(rhs.clone());
            Ok(Value::List(copy))
        } else if let Value::List(rhs) = rhs {
            let mut copy = rhs.clone();
            copy.push(self.clone());
            Ok(Value::List(copy))
        } else {
            match (self, rhs) {
                (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a + b)),
                (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a + b)),
                (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a + b)),
                (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a + b)),
                (Value::F32(a), Value::F32(b)) => Ok(Value::F32(a + b)),
                (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a + b)),
                (Value::String(s), Value::I32(i)) => Ok(format!("{}{}", s, i).into()),
                (Value::String(s), Value::I64(i)) => Ok(format!("{}{}", s, i).into()),
                (Value::String(s), Value::U32(u)) => Ok(Value::String(format!("{}{}", s, u))),
                (Value::String(s), Value::U64(u)) => Ok(Value::String(format!("{}{}", s, u))),
                (Value::String(s), Value::F32(f)) => Ok(Value::String(format!("{}{}", s, f))),
                (Value::String(s), Value::F64(f)) => Ok(Value::String(format!("{}{}", s, f))),
                (Value::String(s), Value::Bool(b)) => Ok(Value::String(format!("{}{}", s, b))),
                (Value::String(s), Value::Char(c)) => Ok(Value::String(format!("{}{}", s, c))),
                (Value::String(s1), Value::String(s2)) => {
                    let mut s = String::with_capacity(s1.len() + s2.len());
                    s.push_str(s1.as_str());
                    s.push_str(s2.as_str());
                    Ok(Value::String(s))
                }
                (Value::String(s1), Value::Map(m)) => Ok(Value::String(format!("{}{:?}", s1, m))),
                //enum?
                _ => Err(ValueError::Some("Cannot add")),
            }
        }
    }
}

impl Sub<&Value> for &Value {
    type Output = Result<Value, ValueError>;

    fn sub(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a - b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a - b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a - b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a - b)),
            (Value::F32(a), Value::F32(b)) => Ok(Value::F32(a - b)),
            (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a - b)),
            //enum?
            _ => Err(ValueError::Some("Cannot subtract")),
        }
    }
}

impl Mul<&Value> for &Value {
    type Output = Result<Value, ValueError>;

    fn mul(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a * b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a * b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a * b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a * b)),
            (Value::F32(a), Value::F32(b)) => Ok(Value::F32(a * b)),
            (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a * b)),
            _ => Err(ValueError::Some("Cannot multiply")),
        }
    }
}

impl Div<&Value> for &Value {
    type Output = Result<Value, ValueError>;

    fn div(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a / b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a / b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a / b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a / b)),
            (Value::F32(a), Value::F32(b)) => Ok(Value::F32(a / b)),
            (Value::F64(a), Value::F64(b)) => Ok(Value::F64(a / b)),
            _ => Err(ValueError::Some("Cannot divide")),
        }
    }
}

impl BitAnd<&Value> for &Value {
    type Output = Result<Value, ValueError>;
    fn bitand(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a & b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a & b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a & b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a & b)),
            _ => Err(ValueError::Some("Cannot do bitwise-and on")),
        }
    }
}

impl BitOr<&Value> for &Value {
    type Output = Result<Value, ValueError>;
    fn bitor(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a | b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a | b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a | b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a | b)),
            _ => Err(ValueError::Some("Cannot do bitwise-or on")),
        }
    }
}

impl BitXor<&Value> for &Value {
    type Output = Result<Value, ValueError>;
    fn bitxor(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a ^ b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a ^ b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a ^ b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a ^ b)),
            _ => Err(ValueError::Some("Cannot do bitwise-xor on")),
        }
    }
}

impl Not for &Value {
    type Output = Result<Value, ValueError>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            Value::I32(i32) => Ok(Value::I32(!i32)),
            Value::I64(i64) => Ok(Value::I64(!i64)),
            Value::U32(u32) => Ok(Value::U32(!u32)),
            Value::U64(u64) => Ok(Value::U64(!u64)),
            _ => Err(ValueError::Some("Cannot calculate not")),
        }
    }
}

impl Shl<&Value> for &Value {
    type Output = Result<Value, ValueError>;
    fn shl(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a << b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a << b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a << b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a << b)),
            _ => Err(ValueError::Some("Cannot shift left on")),
        }
    }
}

impl Shr<&Value> for &Value {
    type Output = Result<Value, ValueError>;
    fn shr(self, rhs: &Value) -> Self::Output {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Ok(Value::I32(a >> b)),
            (Value::I64(a), Value::I64(b)) => Ok(Value::I64(a >> b)),
            (Value::U32(a), Value::U32(b)) => Ok(Value::U32(a >> b)),
            (Value::U64(a), Value::U64(b)) => Ok(Value::U64(a >> b)),
            _ => Err(ValueError::Some("Cannot shift right on")),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => a == b,
            (Value::I64(a), Value::I64(b)) => a == b,
            (Value::U32(a), Value::U32(b)) => a == b,
            (Value::U64(a), Value::U64(b)) => a == b,
            (Value::F32(a), Value::F32(b)) => a == b,
            (Value::F64(a), Value::F64(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::DateTime(a), Value::DateTime(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => {
                let mut equal = true;
                for (k, v) in a.iter() {
                    if !b.contains_key(k) || b.get(k).unwrap() != v {
                        //safe unwrap
                        equal = false;
                        break;
                    }
                }
                equal
            }
            // TODO objects
            (Value::Void, Value::Void) => true,
            _ => false, //?
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Value::I32(a), Value::I32(b)) => Some(a.partial_cmp(b)?),
            (Value::I64(a), Value::I64(b)) => Some(a.partial_cmp(b)?),
            (Value::U32(a), Value::U32(b)) => Some(a.partial_cmp(b)?),
            (Value::U64(a), Value::U64(b)) => Some(a.partial_cmp(b)?),
            (Value::F32(a), Value::F32(b)) => Some(a.partial_cmp(b)?),
            (Value::F64(a), Value::F64(b)) => Some(a.partial_cmp(b)?),
            (Value::String(a), Value::String(b)) => Some(a.partial_cmp(b)?),
            (Value::Char(a), Value::Char(b)) => Some(a.partial_cmp(b)?),
            (Value::DateTime(a), Value::DateTime(b)) => Some(a.partial_cmp(b)?),
            _ => None,
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);

        // Then hash the fields
        match self {
            Value::I32(i32) => i32.hash(state),
            Value::I64(i64) => i64.hash(state),
            Value::U32(u32) => u32.hash(state),
            Value::U64(u64) => u64.hash(state),
            Value::F32(f32) => f32.to_bits().hash(state),
            Value::F64(f64) => f64.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::Char(c) => c.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::DateTime(d) => d.hash(state),
            Value::List(l) => l.hash(state),
            _ => {}
        }
    }
}

// impl Ord for Value {
//     fn cmp(&self, rhs: &Self) -> Ordering {
//         self.partial_cmp(rhs).unwrap()
//     }
// }
