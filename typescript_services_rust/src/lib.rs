#![allow(non_upper_case_globals)]

mod js_typing;
mod rust_helpers;
mod server;
mod services;
mod tsserver;
mod web_server;

use js_typing::shared::find_argument;
pub use rust_helpers::sys::os_platform;
pub use server::{
    editor_services::{ProjectService, ProjectServiceEventHandler},
    project::ProjectInterface,
    session::ServerCancellationToken,
    types::ServerHost,
    typings_cache::ITypingsInstaller,
    utilities_public::{LogLevel, Logger},
};
pub use services::{
    services::{get_default_compiler_options, NodeServicesInterface},
    types::{ApplyCodeActionCommandResult, HostCancellationToken, LanguageServiceMode},
    utilities::{
        get_meaning_from_declaration, is_jump_statement_target, is_label_name,
        is_label_of_labeled_statement, SemanticMeaning,
    },
};
pub use tsserver::{
    node_server::initialize_node_system,
    server::{start, StartInput},
};
pub use web_server::web_server::StartSessionOptions;
