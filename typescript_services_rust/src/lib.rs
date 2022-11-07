mod js_typing;
mod rust_helpers;
mod server;
mod services;
mod tsserver;
mod web_server;

use js_typing::shared::find_argument;

pub use rust_helpers::sys::os_platform;

pub use server::editor_services::{ProjectService, ProjectServiceEventHandler};
pub use server::project::ProjectInterface;
pub use server::session::ServerCancellationToken;
pub use server::types::ServerHost;
pub use server::typings_cache::ITypingsInstaller;
pub use server::utilities_public::{LogLevel, Logger};

pub use services::services::get_default_compiler_options;
pub use services::types::{
    ApplyCodeActionCommandResult, HostCancellationToken, LanguageServiceMode,
};

pub use tsserver::node_server::initialize_node_system;
pub use tsserver::server::{start, StartInput};

pub use web_server::web_server::StartSessionOptions;
