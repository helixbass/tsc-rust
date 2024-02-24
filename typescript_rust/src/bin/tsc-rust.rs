use std::io;

use id_arena::Id;
use regex::Regex;
use typescript_rust::{
    execute_command_line, get_sys, impl_has_arena, AllArenas, Debug_, HasArena, InArena, LogLevel,
    LoggingHost,
};

fn main() -> io::Result<()> {
    let ref arena = AllArenas::default();

    Debug_.set_logging_host(Some(LoggingHostConcrete::new(arena)));

    if Debug_.is_debugging() {
        Debug_.enable_debug_info();
    }

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

struct LoggingHostConcrete {
    arena: *const AllArenas,
}

impl LoggingHostConcrete {
    pub fn new(arena: &impl HasArena) -> Id<Box<dyn LoggingHost>> {
        arena.alloc_logging_host(Box::new(Self {
            arena: arena.arena(),
        }))
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

impl_has_arena!(LoggingHostConcrete);
