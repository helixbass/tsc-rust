#![allow(non_upper_case_globals)]

use regex::{Captures, Regex};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::ptr;
use std::rc::Rc;

use super::{
    wrap_symbol_tracker_to_report_for_context, NodeBuilderContext, RcOrReferenceToDynSymbolTracker,
};
use crate::{
    every, find, find_ancestor, get_effective_return_type_node, get_effective_type_annotation_node,
    get_emit_script_target, get_first_identifier, get_line_and_character_of_position,
    get_name_of_declaration, get_object_flags, get_source_file_of_node, get_text_of_node,
    is_entity_name, is_entity_name_expression, is_exports_identifier,
    is_expression_with_type_arguments, is_function_like_declaration, is_get_accessor_declaration,
    is_identifier, is_identifier_start, is_identifier_text, is_in_js_file, is_in_jsdoc,
    is_indexed_access_type_node, is_jsdoc_all_type, is_jsdoc_construct_signature,
    is_jsdoc_function_type, is_jsdoc_index_signature, is_jsdoc_non_nullable_type,
    is_jsdoc_nullable_type, is_jsdoc_optional_type, is_jsdoc_type_literal, is_jsdoc_unknown_type,
    is_jsdoc_variadic_type, is_literal_import_type_node, is_module_exports_access_expression,
    is_module_identifier, is_qualified_name, is_single_or_double_quote, is_string_literal,
    is_tuple_type_node, is_type_reference_node, length, map, map_defined, maybe_map,
    node_is_synthesized, null_transformation_context, set_emit_flags, set_original_node,
    set_text_range, some, starts_with, unescape_leading_underscores,
    using_single_line_string_writer, visit_each_child, visit_node, visit_nodes,
    with_synthetic_factory_and_factory, CharacterCodes, Debug_, EmitFlags, EmitTextWriter,
    HasTypeArgumentsInterface, HasTypeInterface, HasTypeParametersInterface, InternalSymbolName,
    LiteralType, NamedDeclarationInterface, Node, NodeArray, NodeBuilder, NodeInterface, Number,
    ObjectFlags, ReadonlyTextRange, Signature, SignatureDeclarationInterface, Symbol,
    SymbolAccessibility, SymbolFlags, SymbolInterface, SymbolTable, SymbolTracker, SyntaxKind,
    Type, TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface, TypePredicate, VisitResult,
    __String, for_each_entry_bool, NodeBuilderFlags,
};

impl NodeBuilder {
    pub(super) fn get_effective_dot_dot_dot_for_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
    ) -> Option<Rc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_name_for_jsdoc_function_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
        index: usize,
    ) -> Option<Rc<Node>> {
        unimplemented!()
    }

    pub(super) fn symbol_table_to_declaration_statements_(
        &self,
        symbol_table: &SymbolTable,
        context: &NodeBuilderContext,
        bundled: Option<bool>,
    ) -> Option<Vec<Rc<Node /*Statement*/>>> {
        unimplemented!()
    }
}

impl TypeChecker {
    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        if let Some(writer) = writer {
            self.type_predicate_to_string_worker(
                type_predicate,
                enclosing_declaration,
                flags,
                writer.clone(),
            );
            RefCell::borrow(&writer).get_text()
        } else {
            using_single_line_string_writer(|writer| {
                self.type_predicate_to_string_worker(
                    type_predicate,
                    enclosing_declaration,
                    flags,
                    writer,
                )
            })
        }
    }
}
