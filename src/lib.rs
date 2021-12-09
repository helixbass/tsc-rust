#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;

pub use compiler::command_line_parser::parse_command_line;
pub use compiler::core::{for_each, last_or_undefined};
pub use compiler::factory::base_node_factory::BaseNodeFactory;
pub use compiler::factory::node_factory::create_node_factory;
pub use compiler::parser::create_source_file;
pub use compiler::path::{normalize_path, to_path};
pub use compiler::program::create_program;
pub use compiler::scanner::{create_scanner, Scanner};
pub use compiler::sys::{get_sys, System};
pub use compiler::types::{
    BaseNode, CharacterCodes, CompilerHost, CreateProgramOptions, Diagnostic, EmptyStatement,
    ExitStatus, ModuleResolutionHost, Node, NodeArray, NodeFactory, ParsedCommandLine, Path,
    Program, SourceFile, Statement, StructureIsReused, SyntaxKind,
};
pub use compiler::utilities::object_allocator;
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
