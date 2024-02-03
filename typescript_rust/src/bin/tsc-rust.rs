use std::{io, rc::Rc};

use regex::Regex;
use typescript_rust::{
    execute_command_line, get_sys, static_arena, AllArenas, Debug_, HasArena, InArena, LogLevel,
    LoggingHost,
};

fn main() -> io::Result<()> {
    Debug_.set_logging_host(Some(Rc::new(LoggingHostConcrete::new())));

    if Debug_.is_debugging() {
        Debug_.enable_debug_info();
    }

    let arena = &*static_arena();

    let sys = get_sys(arena);

    if
    /*sys.tryEnableSourceMapsForHost &&*/
    Regex::new(r#"(?i)^development$"#)
        .unwrap()
        // TODO: does using NODE_ENV make sense?
        .is_match(&sys.ref_(arena).get_environment_variable("NODE_ENV"))
    {
        sys.ref_(arena).try_enable_source_maps_for_host();
    }

    // if (ts.sys.setBlocking) {
    sys.ref_(arena).set_blocking();
    // }

    execute_command_line(sys.clone(), |_| {}, sys.ref_(arena).args(), arena)?;

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
        let sys = get_sys(self);
        sys.ref_(self).write(&format!(
            "{}{}",
            s, /*|| ""*/
            sys.ref_(self).new_line()
        ));
    }
}

impl HasArena for LoggingHostConcrete {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
