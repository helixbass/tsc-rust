#![allow(non_upper_case_globals)]

use gc::Gc;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::NodeBuilderContext;
use crate::{
    using_single_line_string_writer, EmitTextWriter, Node, NodeBuilder, SymbolTable, TypeChecker,
    TypeFormatFlags, TypePredicate,
};

impl NodeBuilder {
    pub(super) fn get_effective_dot_dot_dot_for_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_name_for_jsdoc_function_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
        index: usize,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn symbol_table_to_declaration_statements_(
        &self,
        symbol_table: &SymbolTable,
        context: &NodeBuilderContext,
        bundled: Option<bool>,
    ) -> Option<Vec<Gc<Node /*Statement*/>>> {
        unimplemented!()
    }
}

impl TypeChecker {
    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<dyn EmitTextWriter>>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        if let Some(writer) = writer {
            self.type_predicate_to_string_worker(
                type_predicate,
                enclosing_declaration,
                flags,
                writer.clone(),
            );
            writer.get_text()
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
