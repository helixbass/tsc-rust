use std::rc::Rc;

use crate::{
    HostCancellationToken, ITypingsInstaller, LanguageServiceMode, Logger,
    ProjectServiceEventHandler, ServerHost,
};

pub type RequestId = usize;

pub trait ServerCancellationToken: HostCancellationToken {
    fn set_request(&self, request_id: RequestId);
    fn reset_request(&self, request_id: RequestId);
}

pub struct SessionOptions {
    pub host: Rc<dyn ServerHost>,
    pub cancellation_token: Rc<dyn ServerCancellationToken>,
    pub use_single_inferred_project: bool,
    pub use_inferred_project_per_project_root: bool,
    pub typings_installer: Rc<dyn ITypingsInstaller>,
    pub byte_length: Rc<dyn Fn(&str, Option<&str>) -> usize>,
    pub hrtime: Rc<dyn Fn(Option<&[usize]>) -> Vec<usize>>,
    pub logger: Rc<dyn Logger>,
    pub can_use_events: bool,
    pub event_handler: Option<ProjectServiceEventHandler>,
    pub suppress_diagnostic_events: Option<bool>,
    pub syntax_only: Option<bool>,
    pub server_mode: Option<LanguageServiceMode>,
    pub throttle_wait_milliseconds: Option<usize>,
    pub no_get_err_on_background_update: Option<bool>,

    pub global_plugins: Option<Vec<String>>,
    pub plugin_probe_locations: Option<Vec<String>>,
    pub allow_local_plugin_loads: Option<bool>,
    pub types_map_location: Option<String>,
}
