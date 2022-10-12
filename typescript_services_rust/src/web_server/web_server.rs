use crate::LanguageServiceMode;

pub struct StartSessionOptions {
    pub global_plugins: Option<Vec<String>>,
    pub plugin_probe_locations: Option<Vec<String>>,
    pub allow_local_plugin_loads: Option<bool>,
    pub use_single_inferred_project: bool,
    pub use_inferred_project_per_project_root: bool,
    pub suppress_diagnostic_events: Option<bool>,
    pub no_get_err_on_background_update: Option<bool>,
    pub syntax_only: Option<bool>,
    pub server_mode: Option<LanguageServiceMode>,
}
