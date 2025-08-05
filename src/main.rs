//use std::io::{stdin, stdout, Write};
//use expr::repr::basic_tree_functionality;

pub mod calc;
pub mod expr;
pub mod io;
pub mod core;

use crate::core::JASON_CURRENT_VERSION;

fn main() -> Result<(), String> {
    println!(" Welcome to Jason ");
    println!("------------------");
    println!("  Version: {}\n", JASON_CURRENT_VERSION);

    Ok(())
}
