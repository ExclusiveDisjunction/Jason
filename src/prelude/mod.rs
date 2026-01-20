pub mod name;
pub mod types;

pub use name::{Name, NameRef, VerifiedPath, VerifiedPathRef, PathValidationError};
pub use types::FlatType;

pub use exdisj::version::Version;

pub const JASON_CURRENT_VERSION: Version = Version::new(1, 0, 0);