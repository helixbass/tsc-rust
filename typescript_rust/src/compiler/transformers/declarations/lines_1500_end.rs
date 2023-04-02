use std::{
    borrow::{Borrow, Cow},
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    mem, ptr,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};

use super::TransformDeclarations;
use crate::{
    add_related_info_rc, are_option_gcs_equal, can_produce_diagnostics, contains_comparer,
    create_diagnostic_for_node, create_empty_exports,
    create_get_symbol_accessibility_diagnostic_for_node, create_unparsed_source_file,
    declaration_name_to_string, filter, gc_cell_ref_mut_unwrapped, gc_cell_ref_unwrapped,
    get_comment_range, get_directory_path,
    get_external_module_import_equals_declaration_expression,
    get_external_module_name_from_declaration, get_factory, get_leading_comment_ranges,
    get_leading_comment_ranges_of_node, get_name_of_declaration, get_original_node_id,
    get_output_paths_for, get_parse_tree_node, get_relative_path_to_directory_or_url,
    get_resolved_external_module_name, get_set_accessor_value_parameter, get_source_file_of_node,
    get_text_of_node, get_this_parameter, get_trailing_comment_ranges, has_effective_modifier,
    has_extension, has_jsdoc_nodes, is_any_import_syntax, is_binding_pattern, is_class_declaration,
    is_export_assignment, is_external_module, is_external_module_indicator,
    is_external_module_reference, is_external_or_common_js_module, is_function_declaration,
    is_function_like, is_import_declaration, is_import_equals_declaration,
    is_index_signature_declaration, is_interface_declaration, is_json_source_file,
    is_late_visibility_painted_statement, is_mapped_type_node, is_module_declaration,
    is_omitted_expression, is_set_accessor_declaration, is_source_file, is_source_file_js,
    is_source_file_not_json, is_string_literal, is_string_literal_like, is_type_alias_declaration,
    is_unparsed_source, last, length, map, map_defined, maybe_concatenate, maybe_filter,
    maybe_for_each, maybe_for_each_bool, maybe_map, module_specifiers, needs_scope_marker,
    normalize_slashes, path_contains_node_modules, path_is_relative, push_if_unique_gc,
    set_comment_range_rc, set_text_range_node_array, skip_trivia, some, starts_with,
    string_contains, to_file_name_lower_case, to_path, transform_nodes, visit_node, visit_nodes,
    with_synthetic_factory, AllAccessorDeclarations, BaseNodeFactorySynthetic, CommentRange,
    CompilerOptions, Debug_, Diagnostic, Diagnostics, EmitHost, EmitResolver, FileReference,
    FunctionLikeDeclarationInterface, GetSymbolAccessibilityDiagnostic,
    GetSymbolAccessibilityDiagnosticInterface, HasInitializerInterface, HasStatementsInterface,
    HasTypeInterface, LiteralLikeNodeInterface, ModifierFlags,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, NamedDeclarationInterface, Node,
    NodeArray, NodeBuilderFlags, NodeFactory, NodeId, NodeInterface, NonEmpty, ReadonlyTextRange,
    ScriptReferenceHost, SingleNodeOrVecNode, SourceFileLike, Symbol, SymbolAccessibility,
    SymbolAccessibilityDiagnostic, SymbolAccessibilityResult, SymbolFlags, SymbolInterface,
    SymbolTracker, SyntaxKind, TextRange, TransformationContext, TransformationResult, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
};

pub(super) fn mask_modifiers(
    node: &Node,
    modifier_mask: Option<ModifierFlags>,
    modifier_additions: Option<ModifierFlags>,
) -> Vec<Gc<Node /*Modifier*/>> {
    unimplemented!()
}

pub(super) fn can_have_literal_initializer(node: &Node) -> bool {
    unimplemented!()
}
