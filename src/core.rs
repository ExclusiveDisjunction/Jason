pub mod errors;
pub mod logger;
pub mod version;

pub use version::Version;
pub use logger::{Logger, LoggerLevel, logging};

pub fn is_string_whitespace(target: &str) -> bool {
    for char in target.chars() {
        if !char.is_whitespace() { return false; }
    }

    true
}
