use regex::Regex;
use std::rc::Rc;

use std::io;
use typescript_rust::{execute_command_line, get_sys, Debug_, LogLevel, LoggingHost};

fn main() -> io::Result<()> {
    Debug_.set_logging_host(Some(Rc::new(LoggingHostConcrete::new())));

    if Debug_.is_debugging() {
        Debug_.enable_debug_info();
    }

    let sys = get_sys();

    if
    /*sys.tryEnableSourceMapsForHost &&*/
    Regex::new(r#"(?i)^development$"#)
        .unwrap()
        // TODO: does using NODE_ENV make sense?
        .is_match(&sys.get_environment_variable("NODE_ENV"))
    {
        sys.try_enable_source_maps_for_host();
    }

    // if (ts.sys.setBlocking) {
    sys.set_blocking();
    // }

    execute_command_line(sys.clone(), |_| {}, sys.args())?;

    Ok(())
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
