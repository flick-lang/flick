use std::fmt;

/// An enum to store the built-in Flick types, like `void`
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// Variable-size int type, with `width` bits.
    Int(IntType),
    Void,
    Func(FuncType),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int_type) => write!(f, "{}", int_type),
            Self::Void => write!(f, "void"),
            Self::Func(func_type) => write!(f, "{}", func_type),
        }
    }
}

/// An enum to store the built-in int type
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct IntType {
    pub width: u32,
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "i{}", self.width)
    }
}

/// An enum to store the built-in function type
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncType {
    pub param_types: Vec<Type>,
    pub return_type: Box<Type>,
}

impl fmt::Display for FuncType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let comma_seperated = self
            .param_types
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "fn ({}) {}", comma_seperated, self.return_type)
    }
}
