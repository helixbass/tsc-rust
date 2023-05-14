use gc::{Finalize, Gc, Trace};

use crate::{
    find_argument, LanguageServiceMode, LogLevel, Logger, ServerCancellationToken,
    StartSessionOptions,
};

fn find_argument_string_array(arg_name: &str) -> Vec<String> {
    let arg = find_argument(arg_name);
    if arg.is_none() {
        return vec![];
    }
    let arg = arg.unwrap();
    arg.split(",")
        .filter(|name| !name.is_empty())
        .map(ToOwned::to_owned)
        .collect()
}

pub fn get_log_level(level: Option<&str>) -> Option<LogLevel> {
    if let Some(level) = level {
        let l = level.to_lowercase();
        for name in LogLevel::all() {
            if
            /*isNaN(+name) &&*/
            l == name.as_str().to_lowercase() {
                return Some(name);
            }
        }
    }
    None
}

pub struct StartInput {
    pub args: Vec<String>,
    pub logger: Gc<Box<dyn Logger>>,
    pub cancellation_token: Gc<Box<dyn ServerCancellationToken>>,
    pub server_mode: Option<LanguageServiceMode>,
    pub unknown_server_mode: Option<String>,
    pub start_session: Gc<Box<dyn StartSession>>,
}

pub trait StartSession: Trace + Finalize {
    fn call(
        &self,
        something: StartSessionOptions,
        something_else: Gc<Box<dyn Logger>>,
        something_else_else: Gc<Box<dyn ServerCancellationToken>>,
    );
}

pub fn start(
    StartInput {
        args,
        logger,
        cancellation_token,
        server_mode,
        unknown_server_mode,
        start_session: start_server,
    }: StartInput,
    platform: &str,
) {
    unimplemented!()
}
