#[macro_use]
extern crate lazy_static;

mod compiler;
mod execute_command_line;
mod rust_helpers;

pub use compiler::checker::create_type_checker;
pub use compiler::command_line_parser::parse_command_line;
pub use compiler::core::{concatenate, for_each, last_or_undefined};
pub use compiler::core_public::SortedArray;
pub use compiler::debug::Debug_;
pub use compiler::diagnostic_information_map_generated::Diagnostics;
pub use compiler::factory::base_node_factory::BaseNodeFactory;
pub use compiler::factory::node_factory::create_node_factory;
pub use compiler::parser::create_source_file;
pub use compiler::path::{normalize_path, to_path};
pub use compiler::program::create_program;
pub use compiler::scanner::{create_scanner, Scanner};
pub use compiler::sys::{get_sys, System};
pub use compiler::types::{
    BaseIntrinsicType, BaseLiteralLikeNode, BaseNode, BaseType, BinaryExpression, CharacterCodes,
    CompilerHost, CreateProgramOptions, Diagnostic, DiagnosticCategory, DiagnosticCollection,
    DiagnosticMessage, DiagnosticWithDetachedLocation, DiagnosticWithLocation, EmptyStatement,
    ExitStatus, Expression, ExpressionStatement, FreshableIntrinsicType, Identifier, IntrinsicType,
    LiteralLikeNode, ModuleResolutionHost, Node, NodeArray, NodeArrayOrVec, NodeFactory,
    NodeInterface, NumericLiteral, ParsedCommandLine, Path, PrefixUnaryExpression, Program,
    RelationComparisonResult, SourceFile, Statement, StructureIsReused, SyntaxKind, Type,
    TypeChecker, TypeFlags,
};
pub use compiler::utilities::{
    create_detached_diagnostic, create_diagnostic_collection, get_binary_operator_precedence,
    object_allocator, OperatorPrecedence,
};
pub use compiler::watch::emit_files_and_report_errors_and_get_exit_status;
pub use execute_command_line::execute_command_line::execute_command_line;
pub use rust_helpers::is_same_variant;
