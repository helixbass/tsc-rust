#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use crate::{
    TypeNode, UnionOrIntersectionType, UnionOrIntersectionTypeInterface, UnionType,
    VariableDeclaration, VariableStatement, __String, bind_source_file, chain_diagnostic_messages,
    create_diagnostic_collection, create_diagnostic_for_node,
    create_diagnostic_for_node_from_message_chain, create_printer, create_symbol_table,
    create_text_writer, declaration_name_to_string, escape_leading_underscores, every, factory,
    first_defined, first_or_undefined, for_each, get_effective_initializer,
    get_effective_type_annotation_node, get_first_identifier, get_name_of_declaration,
    get_object_flags, get_source_file_of_node, get_synthetic_factory, has_dynamic_name,
    has_initializer, is_binding_element, is_external_or_common_js_module, is_identifier_text,
    is_object_literal_expression, is_private_identifier, is_property_assignment,
    is_property_declaration, is_property_signature, is_variable_declaration, node_is_missing,
    object_allocator, unescape_leading_underscores, using_single_line_string_writer, ArrayTypeNode,
    BaseInterfaceType, BaseIntrinsicType, BaseLiteralType, BaseNodeFactorySynthetic,
    BaseObjectType, BaseType, BaseUnionOrIntersectionType, CharacterCodes, Debug_, Diagnostic,
    DiagnosticCollection, DiagnosticMessage, DiagnosticMessageChain, Diagnostics, EmitHint,
    EmitTextWriter, Expression, ExpressionStatement, FreshableIntrinsicType,
    HasExpressionInitializerInterface, InterfaceDeclaration, InterfaceType, IntrinsicType,
    KeywordTypeNode, LiteralLikeNode, LiteralLikeNodeInterface, LiteralType, LiteralTypeInterface,
    NamedDeclarationInterface, Node, NodeInterface, Number, NumberLiteralType, NumericLiteral,
    ObjectFlags, ObjectFlagsTypeInterface, ObjectLiteralExpression, PrefixUnaryExpression,
    PrinterOptions, PropertyAssignment, PropertySignature, RelationComparisonResult,
    ResolvableTypeInterface, ResolvedTypeInterface, SourceFile, Statement, StringLiteralType,
    Symbol, SymbolFlags, SymbolFormatFlags, SymbolTable, SymbolTracker, SyntaxKind, Ternary, Type,
    TypeChecker, TypeCheckerHost, TypeElement, TypeFlags, TypeInterface, TypeReferenceNode,
    UnionReduction, VariableLikeDeclarationInterface,
};

impl TypeChecker {
    pub(crate) fn create_error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        if let Some(location) = location {
            Rc::new(create_diagnostic_for_node(location, message).into())
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn error<TNode: NodeInterface>(
        &self,
        location: Option<&TNode>,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.create_error(location, message);
        self.diagnostics().add(diagnostic.clone());
        diagnostic
    }

    pub(crate) fn error_and_maybe_suggest_await<TNode: NodeInterface>(
        &self,
        location: &TNode,
        message: &DiagnosticMessage,
    ) -> Rc<Diagnostic> {
        let diagnostic = self.error(Some(location), message);
        diagnostic
    }

    pub(crate) fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        let symbol = (self.Symbol)(flags | SymbolFlags::Transient, name);
        symbol
    }

    pub(crate) fn merge_symbol_table(
        &self,
        target: &mut SymbolTable,
        source: &SymbolTable,
        unidirectional: Option<bool>,
    ) {
        let unidirectional = unidirectional.unwrap_or(false);
        for (id, source_symbol) in source {
            let target_symbol = target.get(id);
            let value = if let Some(target_symbol) = target_symbol {
                unimplemented!()
            } else {
                source_symbol.clone()
            };
            target.insert(id.clone(), value);
        }
    }

    pub(crate) fn is_global_source_file(&self, node: &Node) -> bool {
        node.kind() == SyntaxKind::SourceFile && true
    }

    pub(crate) fn get_symbol(
        &self,
        symbols: &SymbolTable,
        name: &__String,
        meaning: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        if meaning != SymbolFlags::None {
            let symbol = self.get_merged_symbol(symbols.get(name).map(Clone::clone));
            if let Some(symbol) = symbol {
                if symbol.flags().intersects(meaning) {
                    return Some(symbol);
                }
            }
        }
        None
    }

    pub(crate) fn resolve_name<TLocation: NodeInterface, TNameArg: Into<ResolveNameNameArg>>(
        &self,
        location: Option<&TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        let exclude_globals = exclude_globals.unwrap_or(false);
        self.resolve_name_helper(
            location,
            name,
            meaning,
            name_not_found_message,
            name_arg,
            is_use,
            exclude_globals,
            TypeChecker::get_symbol,
        )
    }

    pub(crate) fn resolve_name_helper<
        TLocation: NodeInterface,
        TNameArg: Into<ResolveNameNameArg>,
    >(
        &self,
        location: Option<&TLocation>,
        name: &__String,
        meaning: SymbolFlags,
        name_not_found_message: Option<DiagnosticMessage>,
        name_arg: Option<TNameArg>,
        is_use: bool,
        exclude_globals: bool,
        lookup: fn(&TypeChecker, &SymbolTable, &__String, SymbolFlags) -> Option<Rc<Symbol>>,
    ) -> Option<Rc<Symbol>> {
        let mut location: Option<Rc<Node>> = location.map(|node| node.node_wrapper());
        let mut result: Option<Rc<Symbol>> = None;
        let mut last_location: Option<Rc<Node>> = None;

        while let Some(location_unwrapped) = location {
            if location_unwrapped.maybe_locals().is_some()
                && !self.is_global_source_file(&*location_unwrapped)
            {
                unimplemented!()
            }
            last_location = Some(location_unwrapped.clone());
            location = location_unwrapped.maybe_parent();
        }

        if result.is_none() {
            if let Some(last_location) = last_location {
                Debug_.assert(last_location.kind() == SyntaxKind::SourceFile, None);
            }

            if !exclude_globals {
                result = lookup(self, &self.globals(), name, meaning);
            }
        }

        result
    }
}

pub(crate) enum ResolveNameNameArg {
    Node(Rc<Node>),
    __String(__String),
}

impl From<Rc<Node>> for ResolveNameNameArg {
    fn from(node: Rc<Node>) -> Self {
        ResolveNameNameArg::Node(node)
    }
}

impl From<__String> for ResolveNameNameArg {
    fn from(string: __String) -> Self {
        ResolveNameNameArg::__String(string)
    }
}
