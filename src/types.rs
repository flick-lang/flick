use std::fmt;
use crate::ast::FuncProto;

/// An enum to store the built-in Flick types, like `void`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// Variable-size int type, with `width` bits.
    Int(IntType),
    Bool,
    Void,
    Func(FuncProto),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int_type) => write!(f, "{}", int_type),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Func(func_type) => write!(f, "{}", func_type),
        }
    }
}

/// An enum to store the built-in int type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct IntType {
    pub signed: bool,
    pub width: u32,
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.signed {
            true => write!(f, "i{}", self.width),
            false => write!(f, "u{}", self.width),
        }
    }
}


