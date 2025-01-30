use calc::functions::function_base::test_func_display;
//use std::io::{stdin, stdout, Write};
use expr::repr::basic_tree_functionality;

use crate::core::version::JASON_CURRENT_VERSION;
use crate::core::logger::{logging, LoggerLevel};

pub mod core;
pub mod calc;
pub mod expr;

fn main() -> Result<(), String> {
    println!(" Welcome to Jason ");
    println!("------------------");
    println!("  Version: {}\n", JASON_CURRENT_VERSION);

    let logger_level: LoggerLevel = if cfg!(debug_assertions) {
        LoggerLevel::Debug
    } else {
        LoggerLevel::Info
    };

    if let Err(e) = logging.open("run.log", logger_level) {
        eprintln!("Error in creating log: {}", &e);
        return Err(e.to_string());
    }

    log_info!("Starting Jason v. {}", JASON_CURRENT_VERSION);

    println!("Starting Jason services (configurations, session)...");

    log_info!("Reading configurations...");
    log_info!("Configurations read.");

    log_info!("Starting session...");
    log_info!("Session loaded.");

    log_info!("Starting IO thread & timer thread...");
    log_info!("Threads initialized");

    println!("All services loaded!\n");

    log_info!("Starting user input mode.");

    if logger_level == LoggerLevel::Debug {
        println!("Notice: Debugging mode activated.");
    }

    println!("Starting testing");
    test_func_display();

    /*
    println!("Jason CLI mode: If you need help, type 'help' or 'help [command]' to review topics about a list of commands, info about a command.\n");

    let mut input: String = String::new();
    let cin = stdin();
    let mut cout = stdout();
    loop {
        print!("> ");
        if let Err(e) = cout.flush() {
            eprintln!("Could not flush stdout {}", &e);
            return Err(e.to_string());
        }

        if let Err(e) = cin.read_line(&mut input) {
            eprintln!("Could not read from stdin {}", &e);
            return Err(e.to_string());
        }

        input = input.trim().to_lowercase();
        if input == "quit" || input == "exit" {
            println!("Saving...");
            println!("Saved. Goodbye!\n");

            break;
        }
    }
     */

    log_info!("Jason completed, exit code 0.");
    logging.close();

    Ok(())
}
