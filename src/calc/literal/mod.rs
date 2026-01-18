pub mod core;
pub mod numeric;
pub mod composite;
pub mod sequence;
pub mod lit;
pub mod lit_ref;
// pub mod union_mut;

pub use lit::Literal;
pub use lit_ref::LiteralReference;
// pub use union_mut::VariableUnionMut;