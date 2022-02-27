use std::rc::Rc;

use tsc_rust::{execute_command_line, get_sys, Debug_, LogLevel, LoggingHost, System};

fn main() {
    Debug_.set_logging_host(Some(Rc::new(LoggingHostConcrete::new())));
    let sys = get_sys();
    execute_command_line(sys, sys.args());
}

struct LoggingHostConcrete {}

impl LoggingHostConcrete {
    pub fn new() -> Self {
        Self {}
    }
}

impl LoggingHost for LoggingHostConcrete {
    fn log(&self, _level: LogLevel, s: &str) {
        let sys = get_sys();
        sys.write(&format!("{}{}", s /*|| ""*/, sys.new_line()));
    }
}
