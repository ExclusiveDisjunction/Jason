use super::base::{ASTNode, ASTRawNode, TreeOrderTraversal};
use crate::calc::{VariableData, VariableUnion, CalcResult, CalcError, calc_error::{IndexOutOfRangeError, UndefinedError}};

/// Represents a single number via `VariableUnion`. This is a leaf node.
pub struct ConstExpr {
    value: VariableUnion
}
impl ASTNode for ConstExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Ok(self.value.clone())
    }

    fn print_self(&self, _: TreeOrderTraversal) -> String {
        format!("{}", &self.value)
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "const-expr {}", self.value.get_type())
    }
}
impl From<VariableUnion> for ConstExpr {
    fn from(value: VariableUnion) -> Self {
        Self {
            value
        }
    }
}
impl ConstExpr {
    pub fn new(with: VariableUnion) -> Self {
        Self {
            value: with
        }
    }
}

/// Represents a variable along a specific dimension. Upon evaluation, it will pull data out of the `on` parameter. This is a leaf node.
pub struct VariableExpr {
    symbol: char,
    along: usize
}
impl ASTNode for VariableExpr {
    fn evaluate(&self, on: &[VariableUnion]) -> CalcResult<VariableUnion> {
        if self.along >= on.len() {
            Err(CalcError::Index(IndexOutOfRangeError::new(self.along)))
        }
        else {
            Ok(on[self.along].clone())
        }
    }

    fn print_self(&self, _: TreeOrderTraversal) -> String  {
        format!("{}", &self.symbol)
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var-expr '{}' along: {}", self.symbol, self.along)
    }
}
impl VariableExpr {
    pub fn new(symbol: char, along: usize) -> Self {
        Self {
            symbol,
            along
        }
    }
}

/// Represents a leaf node yet to be processed. 
pub struct RawLeafExpr {
    contents: String
}
impl ASTNode for RawLeafExpr {
    fn evaluate(&self, _: &[VariableUnion]) -> CalcResult<VariableUnion> {
        Err(UndefinedError::new("raw expressions have no evaluation").into())
    }

    fn print_self(&self, _: TreeOrderTraversal) -> String {
        self.contents.clone()
    }
    fn debug_print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "raw-leaf-expr '{}'", &self.contents)
    }
}
impl From<String> for RawLeafExpr {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
impl From<RawLeafExpr> for String {
    fn from(value: RawLeafExpr) -> Self {
        value.contents
    }
}
impl ASTRawNode for RawLeafExpr {
    fn get_contents(&self) -> &str {
        &self.contents
    }
    fn set_contents(&mut self, new: String) {
        self.contents = new    
    }
}
impl RawLeafExpr {
    pub fn new<T>(value: T) -> Self where T: ToString {
        Self {
            contents: value.to_string()
        }
    }
}