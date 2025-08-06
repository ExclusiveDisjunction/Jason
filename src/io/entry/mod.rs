pub mod base;
pub mod var;
pub mod func;

pub use base::{IOEntry, IOStorage};
pub use var::{VariableEntryType, VariableEntry};
pub use func::{FunctionEntryBase, FunctionEntry, ImplFunctionEntry};