pub mod name;
pub mod ops;
pub mod tree;
pub mod types;

/// This module defines identification for packages and resources. Package identification is handled with `NumericalPackageID` and `PackageID`, while resources (entries and functions) are handled with `NumericalResourceID` and `ResourceID`.
/// `NumericalPackageID` and `NumericalResourceID` are both stored inside of objects in the data structures. They are used to uniquely identify a structure.
/// `PackageID`, `ResourceID`, `ResourceKind`, and `ResourceLocator` are all used to locate objects within the data structures. They come in a few major varieties:
///     1. Numerical - Stores a specific `u32` number, but no name.
///     2. Strong - Stores a name and a specific `u32` number. Used for *resolved* lookups.
///     3. Weak - Just a name. This may or may not point to a resource, and must be *resolved* before they can be used.
///
/// Additionally, the `PackageID` enum provided the members, `Any`, `Usr`, and `Std`. These three are special cases, and are used for faster lookup via the `Session` object.
pub mod id;

pub use id::{PackageID, PackageIDRef, Identifier, LocatorParsingError, ResourceKind};

pub use name::{Name, NameRef, VerifiedPath, VerifiedPathRef, PathValidationError};