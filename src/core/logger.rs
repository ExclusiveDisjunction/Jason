use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;

use super::errors::{Error, ArgumentValueError, OperationError};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum LoggerLevel {
    None = 0,
    Debug = 1,
    Info = 2,
    Warning = 3,
    Error = 4,
    Critical = 5 
}
impl Debug for LoggerLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::None => "NONE",
                Self::Debug => "DEBUG",
                Self::Info => "INFO",
                Self::Warning => "WARNING",
                Self::Error => "ERROR",
                Self::Critical => "CRITICAL"
            }
        )
    }
}

pub struct LoggerData {
    file: Option<File>,
    state: LoggerLevel, //Currently being used
    level: LoggerLevel //The level of the logger (what we will accept)
}
impl Default for LoggerData {
    fn default() -> Self {
        Self {
            file: None,
            state: LoggerLevel::None,
            level: LoggerLevel::None
        }
    }
}
impl LoggerData {
    pub fn open(&mut self, path: &str, level: LoggerLevel) -> Result<(), Error> {
        if self.is_open() {
            Err( OperationError::new("open", "already open").into() )
        } else if level == LoggerLevel::None {
            Err( ArgumentValueError::new("level", &LoggerLevel::None).into() )
        } else {
            self.file = match std::fs::File::create(path) {
                Ok(f) => Some(f),
                Err(e) => return Err(e.into())
            };

            self.state = LoggerLevel::None;
            self.level = level;
            Ok(())
        }
    }
    pub fn close(&mut self) {
        self.file = None;
        self.state = LoggerLevel::None;
        self.level = LoggerLevel::None;
    }
    pub fn is_open(&self) -> bool {
        self.file.is_some()
    }
    pub fn open_level(&self) -> LoggerLevel {
        self.level
    }
    fn get_file(&mut self) -> &mut File {
        if self.file.is_none() {
            panic!("get file called when the file stored in the log is None");
        }

        self.file.as_mut().unwrap()
    }
    fn is_in_log(&self) -> bool {
        self.state != LoggerLevel::None && self.file.is_some()
    }
    fn curr_log_ignored(&self) -> bool {
       self.state < self.level || self.file.is_none()
    }

    pub fn write(&mut self, obj: &impl Debug) -> Result<(), Error> {
        if !self.is_open() {
            Err( OperationError::new("write", "no file currently open").into() )
        }
        else if !self.is_in_log() {
            Err( OperationError::new("write", "not in writing mode").into() )
        }
        else {
            if !self.curr_log_ignored() {
                let t_file: &mut File = self.file.as_mut().unwrap();
                if let Err(e) = t_file.write(format!("{:?}", obj).as_bytes()) {
                    return Err(e.into());
                }
            }

            Ok(())
        }
    }
    pub fn start_log(&mut self, level: LoggerLevel) -> Result<(), Error> {
        if !self.is_open() {
            return Err( OperationError::new("start log", "log not open").into() );
        }
        else if level == LoggerLevel::None {
            return Err( OperationError::new("start log", format!("cannot start a log at level {:?}", LoggerLevel::None)).into() );
        }
        else if self.is_in_log() {
            return Err( OperationError::new("start log", format!("log already started at level {:?}", &self.state)).into() );
        }

        self.state = level;
        if !self.curr_log_ignored() {
            if let Err(e) =  self.get_file().write(format!("{:?} {:?} ", chrono::Local::now(), level).as_bytes()) {
                return Err(e.into());
            }
        }

        Ok(())
    }
    pub fn end_log(&mut self) -> Result<(), Error> {
        if !self.is_open() {
            return Err( OperationError::new("end log", "log not open").into() );
        }
        else if !self.is_in_log() {
            return Err( OperationError::new("end log", "not in log").into() );
        }

        if let Err(e) = self.get_file().write("\n".as_bytes()) {
            return Err(e.into());
        }

        self.state = LoggerLevel::None;
        Ok(())
    }
}

pub struct Logger {
    data: Arc<Mutex<LoggerData>>
}
impl Default for Logger {
    fn default() -> Self {
        Self::new()
    }
}
impl Logger {
    pub fn new() -> Self {
        Self {
            data: Arc::new(Mutex::new(LoggerData::default()))
        }
    }

    pub fn open(&self, path: &str, level: LoggerLevel) -> Result<(), Error> {
        let mut data = self.data.lock().unwrap();
        data.open(path, level)
    }
    pub fn close(&self) {
        let mut data = self.data.lock().unwrap();
        data.close()
    }
    pub fn is_open(&self) -> bool {
        let data = self.data.lock().unwrap();
        data.is_open()
    }
    pub fn open_level(&self) -> LoggerLevel {
        let data = self.data.lock().unwrap();
        data.open_level()
    }

    pub fn write(&self, obj: &impl Debug) -> Result<(), Error> {
        let mut data = self.data.lock().unwrap();
        data.write(obj)
    }
    pub fn start_log(&self, level: LoggerLevel) -> Result<(), Error> {
        let mut data = self.data.lock().unwrap();
        data.start_log(level)
    }
    pub fn end_log(&self) -> Result<(), Error> {
        let mut data = self.data.lock().unwrap();
        data.end_log()
    }

}

lazy_static! {
    pub static ref logging: Logger = Logger::new();
}

#[macro_export]
macro_rules! logger_write {
    ($level: expr, $($arg:tt)*) => {
        {
            if $crate::core::logger::logging.is_open() { //Do nothing, so that standard error is not flooded with 'not open' errors.
                #[allow(unreachable_patterns)]
                let true_level: $crate::core::logger::LoggerLevel = match $level {
                    $crate::core::logger::LoggerLevel::None => panic!("cannot record information about a None log"),
                    $crate::core::logger::LoggerLevel::Debug => $crate::core::logger::LoggerLevel::Debug,
                    $crate::core::logger::LoggerLevel::Info => $crate::core::logger::LoggerLevel::Info,
                    $crate::core::logger::LoggerLevel::Warning => $crate::core::logger::LoggerLevel::Warning,
                    $crate::core::logger::LoggerLevel::Error => $crate::core::logger::LoggerLevel::Error,
                    $crate::core::logger::LoggerLevel::Critical => $crate::core::logger::LoggerLevel::Critical,
                    _ => panic!("the type {:?} cannot be interpreted as a valid `LoggerLevel` instance", $level)
                };
                if true_level >= $crate::core::logger::logging.open_level() {
                    let contents: String = format!($($arg)*);

                    match $crate::core::logger::logging.start_log(true_level) {
                        Ok(_) => {
                            match $crate::core::logger::logging.write(&contents) {
                                Ok(_) => {
                                    if let Err(e) = $crate::core::logger::logging.end_log() {
                                        eprintln!("log error: '{:?}'. closing log", e);
                                        $crate::core::logger::logging.close();
                                    }
                                },
                                Err(e) => {
                                    eprintln!("log error: '{:?}'. closing log", e);
                                    $crate::core::logger::logging.close();
                                }
                            }
                        },
                        Err(e) => {
                            eprintln!("log error: '{:?}'. closing log", e);
                            $crate::core::logger::logging.close();
                        }
                    }
                }
            }
        }
    };
}
#[macro_export]
macro_rules! log_debug {
    ($($arg:tt)*) => {
        {
            use $crate::logger_write;
            logger_write!($crate::core::logger::LoggerLevel::Debug, $($arg)*)
        }
    }
}
#[macro_export]
macro_rules! log_info {
    ($($arg:tt)*) => {
        {
            use $crate::logger_write;
            logger_write!($crate::core::logger::LoggerLevel::Info, $($arg)*)
        }
    }
}
#[macro_export]
macro_rules! log_warning {
    ($($arg:tt)*) => {
        {
            use $crate::logger_write;
            logger_write!($crate::core::logger::LoggerLevel::Warning, $($arg)*)
        }
    }
}
#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        {
            use $crate::logger_write;
            logger_write!($crate::core::logger::LoggerLevel::Error, $($arg)*)
        }
    }
}
#[macro_export]
macro_rules! log_critical {
    ($($arg:tt)*) => {
        {
            use $crate::logger_write;
            logger_write!($crate::core::logger::LoggerLevel::Critical, $($arg)*)
        }
    }
}

#[test]
fn test_logger_write() {
    if let Err(e) = logging.open("tmp.log", LoggerLevel::Debug) {
        panic!("unable to open log because '{:?}'", e);
    }

    logger_write!(LoggerLevel::Debug, "hello");
    logger_write!(LoggerLevel::Info, "hello");
    logger_write!(LoggerLevel::Warning, "hello");
    logger_write!(LoggerLevel::Error, "hello");
    logger_write!(LoggerLevel::Critical, "hello");

    log_debug!("hello 2");
    log_info!("hello 2");
    log_warning!("hello 2");
    log_error!("hello 2");
    log_critical!("hello 2");

    logging.close();
    assert!(logging.write(&"hello".to_string()).is_err())
}