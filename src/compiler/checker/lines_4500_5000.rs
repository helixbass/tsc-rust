#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::ptr;
use std::rc::Rc;

use crate::{
    add_synthetic_leading_comment, append_if_unique_rc, contains_rc, create_printer,
    create_text_writer, default_maximum_truncation_length, every, factory, filter,
    get_first_identifier, get_object_flags, get_source_file_of_node, get_text_of_node,
    get_trailing_semicolon_deferring_writer, has_syntactic_modifier, id_text, is_binding_element,
    is_expression, is_expression_with_type_arguments_in_class_extends_clause,
    is_external_or_common_js_module, is_identifier_text, is_import_type_node, is_in_js_file,
    is_late_visibility_painted_statement, is_module_with_string_literal_name,
    is_type_reference_node, is_variable_declaration, is_variable_statement, map,
    no_truncation_maximum_truncation_length, pseudo_big_int_to_string, set_emit_flags, symbol_name,
    synthetic_factory, using_single_line_string_writer, Debug_, EmitFlags, EmitHint,
    EmitTextWriter, IndexInfo, KeywordTypeNode, ModifierFlags, Node, NodeArray, NodeBuilderFlags,
    NodeFlags, NodeInterface, ObjectFlags, PrinterOptionsBuilder, ScriptTarget, Signature,
    SignatureKind, Symbol, SymbolAccessibility, SymbolFlags, SymbolFormatFlags, SymbolId,
    SymbolInterface, SymbolTable, SymbolTracker, SymbolVisibilityResult, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeFormatFlags, TypeId, TypeInterface, TypePredicate,
};

impl TypeChecker {
    pub(super) fn has_non_global_augmentation_external_module_symbol(
        &self,
        declaration: &Node,
    ) -> bool {
        is_module_with_string_literal_name(declaration)
            || declaration.kind() == SyntaxKind::SourceFile
                && is_external_or_common_js_module(declaration)
    }

    pub(super) fn has_visible_declarations(
        &self,
        symbol: &Symbol,
        should_compute_aliases_to_make_visible: bool,
    ) -> Option<SymbolVisibilityResult> {
        let aliases_to_make_visible: RefCell<
            Option<Vec<Rc<Node /*LateVisibilityPaintedStatement*/>>>,
        > = RefCell::new(None);
        if !every(
            &filter(symbol.maybe_declarations().as_deref(), |d: &Rc<Node>| {
                d.kind() != SyntaxKind::Identifier
            })
            .unwrap_or_else(|| vec![]),
            |d: &Rc<Node>, _| {
                self.get_is_declaration_visible(
                    symbol,
                    should_compute_aliases_to_make_visible,
                    &aliases_to_make_visible,
                    d,
                )
            },
        ) {
            return None;
        }
        Some(SymbolVisibilityResult {
            accessibility: SymbolAccessibility::Accessible,
            aliases_to_make_visible: aliases_to_make_visible.into_inner(),
            error_symbol_name: None,
            error_node: None,
        })
    }

    pub(super) fn get_is_declaration_visible(
        &self,
        symbol: &Symbol,
        should_compute_aliases_to_make_visible: bool,
        aliases_to_make_visible: &RefCell<Option<Vec<Rc<Node>>>>,
        declaration: &Node, /*Declaration*/
    ) -> bool {
        if !self.is_declaration_visible(declaration) {
            let any_import_syntax = self.get_any_import_syntax(declaration);
            if matches!(
                any_import_syntax.as_ref(),
                Some(any_import_syntax) if !has_syntactic_modifier(any_import_syntax, ModifierFlags::Export) && self.is_declaration_visible(&any_import_syntax.parent())
            ) {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    any_import_syntax.as_ref().unwrap(),
                );
            } else if is_variable_declaration(declaration)
                && is_variable_statement(&declaration.parent().parent())
            {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    &declaration.parent().parent(),
                );
            } else if is_late_visibility_painted_statement(declaration)
                && !has_syntactic_modifier(declaration, ModifierFlags::Export)
                && self.is_declaration_visible(&declaration.parent())
            {
                return self.add_visible_alias(
                    should_compute_aliases_to_make_visible,
                    aliases_to_make_visible,
                    declaration,
                    declaration,
                );
            } else if symbol.flags().intersects(SymbolFlags::Alias)
                && is_binding_element(declaration)
                && is_in_js_file(Some(declaration))
            {
                let declaration_parent_parent = declaration
                    .maybe_parent()
                    .and_then(|parent| parent.maybe_parent());
                if let Some(declaration_parent_parent) = declaration_parent_parent {
                    if is_variable_declaration(&declaration_parent_parent) {
                        let declaration_parent_parent_parent_parent = declaration_parent_parent
                            .maybe_parent()
                            .and_then(|parent| parent.maybe_parent());
                        if let Some(declaration_parent_parent_parent_parent) =
                            declaration_parent_parent_parent_parent
                        {
                            if is_variable_statement(&declaration_parent_parent_parent_parent)
                                && !has_syntactic_modifier(
                                    &declaration_parent_parent_parent_parent,
                                    ModifierFlags::Export,
                                )
                            {
                                let declaration_parent_parent_parent_parent_parent =
                                    declaration_parent_parent_parent_parent.maybe_parent();
                                if let Some(declaration_parent_parent_parent_parent_parent) =
                                    declaration_parent_parent_parent_parent_parent
                                {
                                    if self.is_declaration_visible(
                                        &declaration_parent_parent_parent_parent_parent,
                                    ) {
                                        return self.add_visible_alias(
                                            should_compute_aliases_to_make_visible,
                                            aliases_to_make_visible,
                                            declaration,
                                            &declaration_parent_parent_parent_parent,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return false;
        }

        true
    }

    pub(super) fn add_visible_alias(
        &self,
        should_compute_aliases_to_make_visible: bool,
        aliases_to_make_visible: &RefCell<Option<Vec<Rc<Node>>>>,
        declaration: &Node,        /*Declaration*/
        aliasing_statement: &Node, /*LateVisibilityPaintedStatement*/
    ) -> bool {
        if should_compute_aliases_to_make_visible {
            self.get_node_links(declaration).borrow_mut().is_visible = Some(true);
            let mut aliases_to_make_visible = aliases_to_make_visible.borrow_mut();
            if aliases_to_make_visible.is_none() {
                *aliases_to_make_visible = Some(vec![]);
            }
            append_if_unique_rc(
                aliases_to_make_visible.as_mut().unwrap(),
                &aliasing_statement.node_wrapper(),
            );
        }
        true
    }

    pub(super) fn is_entity_name_visible(
        &self,
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) -> SymbolVisibilityResult {
        let meaning: SymbolFlags;
        if entity_name.parent().kind() == SyntaxKind::TypeQuery
            || is_expression_with_type_arguments_in_class_extends_clause(&entity_name.parent())
            || entity_name.parent().kind() == SyntaxKind::ComputedPropertyName
        {
            meaning = SymbolFlags::Value | SymbolFlags::ExportValue;
        } else if matches!(
            entity_name.kind(),
            SyntaxKind::QualifiedName | SyntaxKind::PropertyAccessExpression
        ) || entity_name.parent().kind() == SyntaxKind::ImportEqualsDeclaration
        {
            meaning = SymbolFlags::Namespace;
        } else {
            meaning = SymbolFlags::Type;
        }

        let first_identifier = get_first_identifier(entity_name);
        let symbol = self.resolve_name_(
            Some(enclosing_declaration),
            &first_identifier.as_identifier().escaped_text,
            meaning,
            None,
            Option::<Rc<Node>>::None,
            false,
            None,
        );
        if matches!(symbol.as_ref(), Some(symbol) if symbol.flags().intersects(SymbolFlags::TypeParameter) && meaning.intersects(SymbolFlags::Type))
        {
            return SymbolVisibilityResult {
                accessibility: SymbolAccessibility::Accessible,
                aliases_to_make_visible: None,
                error_symbol_name: None,
                error_node: None,
            };
        }

        symbol
            .and_then(|symbol| self.has_visible_declarations(&symbol, true))
            .unwrap_or_else(|| SymbolVisibilityResult {
                accessibility: SymbolAccessibility::NotAccessible,
                aliases_to_make_visible: None,
                error_symbol_name: Some(get_text_of_node(&first_identifier, None).into_owned()),
                error_node: Some(first_identifier),
            })
    }

    pub(super) fn symbol_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(SymbolFormatFlags::AllowAnyNodeKind);
        let mut node_flags = NodeBuilderFlags::IgnoreErrors;
        if flags.intersects(SymbolFormatFlags::UseOnlyExternalAliasing) {
            node_flags |= NodeBuilderFlags::UseOnlyExternalAliasing;
        }
        if flags.intersects(SymbolFormatFlags::WriteTypeParametersOrArguments) {
            node_flags |= NodeBuilderFlags::WriteTypeParametersInQualifiedName;
        }
        if flags.intersects(SymbolFormatFlags::UseAliasDefinedOutsideCurrentScope) {
            node_flags |= NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope;
        }
        if flags.intersects(SymbolFormatFlags::DoNotIncludeSymbolChain) {
            node_flags |= NodeBuilderFlags::DoNotIncludeSymbolChain;
        }
        let builder = if flags.intersects(SymbolFormatFlags::AllowAnyNodeKind) {
            NodeBuilder::symbol_to_expression
        } else {
            NodeBuilder::symbol_to_entity_name
        };
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let symbol_to_string_worker = |writer: Rc<RefCell<dyn EmitTextWriter>>| {
            let entity = builder(
                &self.node_builder,
                self,
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                enclosing_declaration.as_deref(),
                Some(node_flags),
                None,
            )
            .unwrap();
            let entity: Rc<Node> = entity.into();
            let mut printer = if matches!(enclosing_declaration.as_ref(), Some(enclosing_declaration) if enclosing_declaration.kind() == SyntaxKind::SourceFile)
            {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .never_ascii_escape(Some(true))
                        .build()
                        .unwrap(),
                )
            } else {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .build()
                        .unwrap(),
                )
            };
            let source_file = enclosing_declaration
                .as_deref()
                .and_then(|enclosing_declaration| {
                    get_source_file_of_node(Some(enclosing_declaration))
                });
            printer.write_node(EmitHint::Unspecified, &entity, source_file, writer);
            // writer
        };
        if let Some(writer) = writer {
            symbol_to_string_worker(writer.clone());
            RefCell::borrow(&writer).get_text()
        } else {
            using_single_line_string_writer(symbol_to_string_worker)
        }
    }

    pub(super) fn signature_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        if let Some(writer) = writer {
            self.signature_to_string_worker(
                signature,
                enclosing_declaration,
                flags,
                kind,
                writer.clone(),
            );
            RefCell::borrow(&writer).get_text()
        } else {
            using_single_line_string_writer(|writer: Rc<RefCell<dyn EmitTextWriter>>| {
                self.signature_to_string_worker(
                    signature,
                    enclosing_declaration,
                    flags,
                    kind,
                    writer,
                )
            })
        }
    }

    pub(super) fn signature_to_string_worker<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: TypeFormatFlags,
        kind: Option<SignatureKind>,
        writer: Rc<RefCell<dyn EmitTextWriter>>,
    ) {
        let sig_output: SyntaxKind;
        if flags.intersects(TypeFormatFlags::WriteArrowStyleSignature) {
            sig_output = if matches!(kind, Some(SignatureKind::Construct)) {
                SyntaxKind::ConstructorType
            } else {
                SyntaxKind::FunctionType
            };
        } else {
            sig_output = if matches!(kind, Some(SignatureKind::Construct)) {
                SyntaxKind::ConstructSignature
            } else {
                SyntaxKind::CallSignature
            };
        }
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let sig = self.node_builder.signature_to_signature_declaration(
            signature,
            sig_output,
            enclosing_declaration.as_deref(),
            Some(
                self.to_node_builder_flags(Some(flags))
                    | NodeBuilderFlags::IgnoreErrors
                    | NodeBuilderFlags::WriteTypeParametersInQualifiedName,
            ),
            None,
        );
        let mut printer = create_printer(
            PrinterOptionsBuilder::default()
                .remove_comments(Some(true))
                .omit_trailing_semicolon(Some(true))
                .build()
                .unwrap(),
        );
        let source_file = enclosing_declaration
            .as_deref()
            .and_then(|enclosing_declaration| get_source_file_of_node(Some(enclosing_declaration)));
        printer.write_node(
            EmitHint::Unspecified,
            &sig.unwrap(),
            source_file,
            Rc::new(RefCell::new(get_trailing_semicolon_deferring_writer(
                writer,
            ))),
        );
        // writer
    }

    pub(super) fn type_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(
            TypeFormatFlags::AllowUniqueESSymbolType
                | TypeFormatFlags::UseAliasDefinedOutsideCurrentScope,
        );
        let writer = writer.unwrap_or_else(|| Rc::new(RefCell::new(create_text_writer(""))));
        let no_truncation = matches!(self.compiler_options.no_error_truncation, Some(true))
            || flags.intersects(TypeFormatFlags::NoTruncation);
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let type_node = self.node_builder.type_to_type_node(
            self,
            type_,
            enclosing_declaration.as_deref(),
            Some(
                self.to_node_builder_flags(Some(flags))
                    | NodeBuilderFlags::IgnoreErrors
                    | if no_truncation {
                        NodeBuilderFlags::NoTruncation
                    } else {
                        NodeBuilderFlags::None
                    },
            ),
            Some((*writer).borrow().as_symbol_tracker()),
        );
        let type_node: Rc<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node.wrap(),
        };
        let options = PrinterOptionsBuilder::default()
            .remove_comments(Some(!ptr::eq(type_, &*self.unresolved_type())))
            .build()
            .unwrap();
        let mut printer = create_printer(options);
        let source_file = enclosing_declaration
            .and_then(|enclosing_declaration| get_source_file_of_node(Some(enclosing_declaration)));
        printer.write_node(
            EmitHint::Unspecified,
            &type_node,
            source_file,
            writer.clone(),
        );
        let result = (*writer).borrow().get_text();

        let max_length = if no_truncation {
            no_truncation_maximum_truncation_length * 2
        } else {
            default_maximum_truncation_length * 2
        };
        if max_length != 0 && !result.is_empty() && result.len() >= max_length {
            return format!("{}...", &result[0..max_length - "...".len()]);
        }
        result
    }

    pub(super) fn get_type_names_for_error_display(
        &self,
        left: &Type,
        right: &Type,
    ) -> (String, String) {
        let mut left_str = if let Some(symbol) = left.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(&symbol) {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(left, enclosing_declaration, None, None)
            } else {
                self.type_to_string_(left, Option::<&Node>::None, None, None)
            }
        } else {
            self.type_to_string_(left, Option::<&Node>::None, None, None)
        };
        let mut right_str = if let Some(symbol) = right.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(&symbol) {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(right, enclosing_declaration, None, None)
            } else {
                self.type_to_string_(right, Option::<&Node>::None, None, None)
            }
        } else {
            self.type_to_string_(right, Option::<&Node>::None, None, None)
        };
        if left_str == right_str {
            left_str = self.get_type_name_for_error_display(left);
            right_str = self.get_type_name_for_error_display(right);
        }
        (left_str, right_str)
    }

    pub(super) fn get_type_name_for_error_display(&self, type_: &Type) -> String {
        self.type_to_string_(
            type_,
            Option::<&Node>::None,
            Some(TypeFormatFlags::UseFullyQualifiedType),
            None,
        )
    }

    pub(super) fn symbol_value_declaration_is_context_sensitive(&self, symbol: &Symbol) -> bool {
        /*symbol &&*/
        matches!(
            symbol.maybe_value_declaration(),
            Some(value_declaration) if is_expression(&value_declaration) && !self.is_context_sensitive(&value_declaration)
        )
    }

    pub(super) fn to_node_builder_flags(&self, flags: Option<TypeFormatFlags>) -> NodeBuilderFlags {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        NodeBuilderFlags::from_bits((flags & TypeFormatFlags::NodeBuilderFlagsMask).bits()).unwrap()
    }

    pub(super) fn is_class_instance_side(&self, type_: &Type) -> bool {
        matches!(
            type_.maybe_symbol(),
            Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::Class) && (
                ptr::eq(type_, &*self.get_declared_type_of_class_or_interface(&type_symbol)) ||
                type_.flags().intersects(TypeFlags::Object) && get_object_flags(type_).intersects(ObjectFlags::IsClassInstanceClone)
            )
        )
    }

    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<&dyn EmitTextWriter>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        unimplemented!()
    }
}

pub(super) fn create_node_builder() -> NodeBuilder {
    NodeBuilder::new()
}

#[derive(Debug)]
pub struct NodeBuilder {}

impl NodeBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn type_to_type_node<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_to_type_node_helper(type_checker, Some(type_), context)
        })
    }

    pub fn index_info_to_index_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        index_info: &IndexInfo,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.index_info_to_index_signature_declaration_helper(
                index_info,
                context,
                Option::<&Node>::None,
            ))
        })
    }

    pub fn signature_to_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        kind: SyntaxKind,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.signature_to_signature_declaration_helper(signature, kind, context, None))
        })
    }

    pub fn symbol_to_entity_name<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*EntityName*/> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.symbol_to_name(type_checker, symbol, context, meaning, false))
        })
    }

    pub fn symbol_to_expression<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.symbol_to_expression_(type_checker, symbol, context, meaning))
        })
    }

    pub fn symbol_to_type_parameter_declarations<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_parameters_to_type_parameter_declarations(symbol, context)
        })
    }

    pub fn symbol_to_parameter_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*ParameterDeclaration*/> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.symbol_to_parameter_declaration_(
                symbol,
                context,
                None,
                Option::<fn(&Symbol)>::None,
                None,
            ))
        })
    }

    pub fn type_parameter_to_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        parameter: &Type, /*TypeParameter*/
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*TypeParameterDeclaration*/> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            Some(self.type_parameter_to_declaration_(type_checker, parameter, context, None))
        })
    }

    pub fn symbol_table_to_declaration_statements<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol_table: &SymbolTable,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
        bundled: Option<bool>,
    ) -> Option<Vec<Rc<Node /*Statement*/>>> {
        self.with_context(enclosing_declaration, flags, tracker, |context| {
            self.symbol_table_to_declaration_statements_(symbol_table, context, bundled)
        })
    }

    pub(super) fn with_context<
        TReturn,
        TEnclosingDeclaration: Borrow<Node>,
        TCallback: FnOnce(&NodeBuilderContext) -> Option<TReturn>,
    >(
        &self,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
        cb: TCallback,
    ) -> Option<TReturn> {
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        Debug_.assert(
            match enclosing_declaration.as_ref() {
                None => true,
                Some(enclosing_declaration) => !enclosing_declaration
                    .flags()
                    .intersects(NodeFlags::Synthesized),
            },
            None,
        );
        // TODO: finish this
        let default_tracker: Option<DefaultNodeBuilderContextSymbolTracker> = match tracker {
            Some(_) => None,
            None => Some(DefaultNodeBuilderContextSymbolTracker::new()),
        };
        let context = NodeBuilderContext::new(
            enclosing_declaration,
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.unwrap_or_else(|| default_tracker.as_ref().unwrap()),
        );
        let resulting_node = cb(&context);
        resulting_node
    }

    pub(super) fn check_truncation_length(&self, context: &NodeBuilderContext) -> bool {
        if matches!(context.truncating.get(), Some(true)) {
            return true;
        }
        context.truncating.set(Some(
            context.approximate_length.get()
                > if context.flags().intersects(NodeBuilderFlags::NoTruncation) {
                    no_truncation_maximum_truncation_length
                } else {
                    default_maximum_truncation_length
                },
        ));
        context.truncating.get().unwrap()
    }

    pub(super) fn type_to_type_node_helper<TType: Borrow<Type>>(
        &self,
        type_checker: &TypeChecker,
        type_: Option<TType>,
        context: &NodeBuilderContext,
    ) -> Option<Node> {
        if let Some(cancellation_token) = type_checker.maybe_cancellation_token()
        /*&& cancellationToken.throwIfCancellationRequested*/
        {
            // cancellationToken.throwIfCancellationRequested();
        }
        let in_type_alias = context.flags().intersects(NodeBuilderFlags::InTypeAlias);
        context.set_flags(context.flags() & !NodeBuilderFlags::InTypeAlias);

        if type_.is_none() {
            if !context
                .flags()
                .intersects(NodeBuilderFlags::AllowEmptyUnionOrIntersection)
            {
                context.encountered_error.set(true);
                return None;
            }
            context.increment_approximate_length_by(3);
            return Some(
                Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::AnyKeyword)
                    })
                }))
                .into(),
            );
        }
        let type_ = type_.unwrap();
        let mut type_ = type_.borrow().type_wrapper();

        if !context
            .flags()
            .intersects(NodeBuilderFlags::NoTypeReduction)
        {
            type_ = type_checker.get_reduced_type(&type_);
        }

        if type_.flags().intersects(TypeFlags::Any) {
            if let Some(type_alias_symbol) = type_.maybe_alias_symbol() {
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_type_reference_node(
                                synthetic_factory_,
                                self.symbol_to_entity_name_node(&type_alias_symbol).wrap(),
                                self.map_to_type_nodes(
                                    type_checker,
                                    type_.maybe_alias_type_arguments().as_deref(),
                                    context,
                                    None,
                                ),
                            )
                            .into()
                    })
                }));
            }
            if Rc::ptr_eq(&type_, &type_checker.unresolved_type()) {
                let ret: Node =
                    synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            Into::<KeywordTypeNode>::into(factory_.create_keyword_type_node(
                                synthetic_factory_,
                                SyntaxKind::AnyKeyword,
                            ))
                            .into()
                        })
                    });
                add_synthetic_leading_comment(
                    &ret,
                    SyntaxKind::MultiLineCommentTrivia,
                    "unresolved",
                    None,
                );
                return Some(ret);
            }
            context.increment_approximate_length_by(3);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(factory_.create_keyword_type_node(
                        synthetic_factory_,
                        if Rc::ptr_eq(&type_, &type_checker.intrinsic_marker_type()) {
                            SyntaxKind::IntrinsicKeyword
                        } else {
                            SyntaxKind::AnyKeyword
                        },
                    ))
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Unknown) {
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_.create_keyword_type_node(
                            synthetic_factory_,
                            SyntaxKind::UnknownKeyword,
                        ),
                    )
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::String) {
            context.increment_approximate_length_by(6);
            return Some(
                Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::StringKeyword)
                    })
                }))
                .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::Number) {
            context.increment_approximate_length_by(6);
            return Some(
                Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::NumberKeyword)
                    })
                }))
                .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::BigInt) {
            context.increment_approximate_length_by(6);
            return Some(
                Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::BigIntKeyword)
                    })
                }))
                .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::Boolean) && type_.maybe_alias_symbol().is_none() {
            context.increment_approximate_length_by(7);
            return Some(
                Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_keyword_type_node(
                            synthetic_factory_,
                            SyntaxKind::BooleanKeyword,
                        )
                    })
                }))
                .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::EnumLiteral)
            && !type_.flags().intersects(TypeFlags::Union)
        {
            let parent_symbol = type_checker.get_parent_of_symbol(&type_.symbol()).unwrap();
            let parent_name = self.symbol_to_type_node(
                type_checker,
                &parent_symbol,
                context,
                SymbolFlags::Type,
                None,
            );
            if Rc::ptr_eq(
                &type_checker.get_declared_type_of_symbol(&parent_symbol),
                &type_,
            ) {
                return Some(parent_name);
            }
            let member_name = symbol_name(&type_.symbol());
            if is_identifier_text(&member_name, Some(ScriptTarget::ES3), None) {
                return Some(self.append_reference_to_type(
                    &parent_name,
                    &synthetic_factory.with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_
                                .create_type_reference_node(
                                    synthetic_factory_,
                                    member_name,
                                    Option::<NodeArray>::None,
                                )
                                .into()
                        })
                    }),
                ));
            }
            if is_import_type_node(&parent_name) {
                parent_name.as_import_type_node().set_is_type_of(true);
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                parent_name.wrap(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                member_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    })
                }));
            } else if is_type_reference_node(&parent_name) {
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_indexed_access_type_node(
                                synthetic_factory_,
                                factory_
                                    .create_type_query_node(
                                        synthetic_factory_,
                                        parent_name.as_type_reference_node().type_name.clone(),
                                    )
                                    .into(),
                                factory_
                                    .create_literal_type_node(
                                        synthetic_factory_,
                                        factory_
                                            .create_string_literal(
                                                synthetic_factory_,
                                                member_name,
                                                None,
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into(),
                            )
                            .into()
                    })
                }));
            } else {
                Debug_.fail(Some(
                    "Unhandled type node kind returned from `symbolToTypeNode`.",
                ));
            }
        }
        if type_.flags().intersects(TypeFlags::EnumLike) {
            return Some(self.symbol_to_type_node(
                type_checker,
                &type_.symbol(),
                context,
                SymbolFlags::Type,
                None,
            ));
        }
        if type_.flags().intersects(TypeFlags::StringLiteral) {
            let value = type_.as_string_literal_type().value.clone();
            context.increment_approximate_length_by(value.len() + 2);
            return Some(
                synthetic_factory
                    .with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_.create_literal_type_node(
                            synthetic_factory_,
                            set_emit_flags(
                                factory_
                                    .create_string_literal(
                                        synthetic_factory_,
                                        value,
                                        Some(context.flags().intersects(
                                            NodeBuilderFlags::UseSingleQuotesForStringLiteralType,
                                        )),
                                        None,
                                    )
                                    .into(),
                                EmitFlags::NoAsciiEscaping,
                            ),
                        )
                        })
                    })
                    .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::NumberLiteral) {
            let value = type_.as_number_literal_type().value.value();
            context.increment_approximate_length_by(value.to_string().len());
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_literal_type_node(
                            synthetic_factory_,
                            if value < 0.0 {
                                factory_
                                    .create_prefix_unary_expression(
                                        synthetic_factory_,
                                        SyntaxKind::MinusToken,
                                        factory_
                                            .create_numeric_literal(
                                                synthetic_factory_,
                                                (-value).to_string(),
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into()
                            } else {
                                factory_
                                    .create_numeric_literal(
                                        synthetic_factory_,
                                        value.to_string(),
                                        None,
                                    )
                                    .into()
                            },
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            let value = &type_.as_big_int_literal_type().value;
            context.increment_approximate_length_by(pseudo_big_int_to_string(value).len() + 1);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_literal_type_node(
                            synthetic_factory_,
                            factory_
                                .create_big_int_literal(synthetic_factory_, value.clone())
                                .into(),
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            let type_intrinsic_name = type_.as_intrinsic_type().intrinsic_name();
            context.increment_approximate_length_by(type_intrinsic_name.len());
            return Some(
                synthetic_factory
                    .with(|synthetic_factory_| {
                        factory.with(|factory_| {
                            factory_.create_literal_type_node(
                                synthetic_factory_,
                                if type_intrinsic_name == "true" {
                                    factory_.create_true(synthetic_factory_)
                                } else {
                                    factory_.create_false(synthetic_factory_)
                                }
                                .into(),
                            )
                        })
                    })
                    .into(),
            );
        }
        if type_.flags().intersects(TypeFlags::UniqueESSymbol) {
            if !context
                .flags()
                .intersects(NodeBuilderFlags::AllowUniqueESSymbolType)
            {
                if type_checker.is_value_symbol_accessible(
                    &type_.symbol(),
                    context.enclosing_declaration.as_deref(),
                ) {
                    context.increment_approximate_length_by(6);
                    return Some(self.symbol_to_type_node(
                        type_checker,
                        &type_.symbol(),
                        context,
                        SymbolFlags::Value,
                        None,
                    ));
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context.tracker.report_inaccessible_unique_symbol_error();
                // }
            }
            context.increment_approximate_length_by(13);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_type_operator_node(
                            synthetic_factory_,
                            SyntaxKind::UniqueKeyword,
                            Into::<KeywordTypeNode>::into(factory_.create_keyword_type_node(
                                synthetic_factory_,
                                SyntaxKind::SymbolKeyword,
                            ))
                            .into(),
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Void) {
            context.increment_approximate_length_by(4);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::VoidKeyword),
                    )
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Undefined) {
            context.increment_approximate_length_by(9);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_.create_keyword_type_node(
                            synthetic_factory_,
                            SyntaxKind::UndefinedKeyword,
                        ),
                    )
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Null) {
            context.increment_approximate_length_by(9);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_literal_type_node(
                            synthetic_factory_,
                            factory_.create_null(synthetic_factory_).into(),
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Never) {
            context.increment_approximate_length_by(5);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_
                            .create_keyword_type_node(synthetic_factory_, SyntaxKind::NeverKeyword),
                    )
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::ESSymbol) {
            context.increment_approximate_length_by(6);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_.create_keyword_type_node(
                            synthetic_factory_,
                            SyntaxKind::SymbolKeyword,
                        ),
                    )
                    .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::NonPrimitive) {
            context.increment_approximate_length_by(6);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Into::<KeywordTypeNode>::into(
                        factory_.create_keyword_type_node(
                            synthetic_factory_,
                            SyntaxKind::ObjectKeyword,
                        ),
                    )
                    .into()
                })
            }));
        }
        if type_checker.is_this_type_parameter(&type_) {
            if context
                .flags()
                .intersects(NodeBuilderFlags::InObjectTypeLiteral)
            {
                if !context.encountered_error.get()
                    && !context
                        .flags()
                        .intersects(NodeBuilderFlags::AllowThisInObjectLiteral)
                {
                    context.encountered_error.set(true);
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context.tracker.report_inaccessible_unique_symbol_error();
                // }
            }
            context.increment_approximate_length_by(4);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| factory_.create_this_type_node(synthetic_factory_).into())
            }));
        }

        if !in_type_alias {
            if let Some(type_alias_symbol) = type_.maybe_alias_symbol() {
                if context
                    .flags()
                    .intersects(NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope)
                    || type_checker.is_type_symbol_accessible(
                        &type_alias_symbol,
                        context.enclosing_declaration.as_deref(),
                    )
                {
                    let type_argument_nodes = self.map_to_type_nodes(
                        type_checker,
                        type_.maybe_alias_type_arguments().as_deref(),
                        context,
                        None,
                    );
                    if type_checker.is_reserved_member_name(type_alias_symbol.escaped_name())
                        && !type_alias_symbol.flags().intersects(SymbolFlags::Class)
                    {
                        return Some(synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_type_reference_node(
                                        synthetic_factory_,
                                        Into::<Rc<Node>>::into(factory_.create_identifier(
                                            synthetic_factory_,
                                            "",
                                            Option::<NodeArray>::None,
                                            None,
                                        )),
                                        type_argument_nodes,
                                    )
                                    .into()
                            })
                        }));
                    }
                    return Some(self.symbol_to_type_node(
                        type_checker,
                        &type_alias_symbol,
                        context,
                        SymbolFlags::Type,
                        type_argument_nodes.as_deref(),
                    ));
                }
            }
        }

        let object_flags = get_object_flags(&type_);

        if object_flags.intersects(ObjectFlags::Reference) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return Some(if type_.as_type_reference().node.borrow().is_some() {
                self.visit_and_transform_type(&type_, |type_| {
                    self.type_reference_to_type_node(type_)
                })
            } else {
                self.type_reference_to_type_node(&type_)
            });
        }
        if type_.flags().intersects(TypeFlags::TypeParameter)
            || object_flags.intersects(ObjectFlags::ClassOrInterface)
        {
            if type_.flags().intersects(TypeFlags::TypeParameter)
                && contains_rc(
                    context.infer_type_parameters.borrow().as_deref(),
                    &type_.type_wrapper(),
                )
            {
                context.increment_approximate_length_by(symbol_name(&type_.symbol()).len() + 6);
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_infer_type_node(
                                synthetic_factory_,
                                self.type_parameter_to_declaration_with_constraint(
                                    &type_, context, None,
                                )
                                .wrap(),
                            )
                            .into()
                    })
                }));
            }
            if context
                .flags()
                .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
                && type_.flags().intersects(TypeFlags::TypeParameter)
                && !type_checker.is_type_symbol_accessible(
                    &type_.symbol(),
                    context.enclosing_declaration.as_deref(),
                )
            {
                let name = self.type_parameter_to_name(&type_, context);
                context.increment_approximate_length_by(id_text(&name).len());
                return Some(synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_type_reference_node(
                                synthetic_factory_,
                                Into::<Rc<Node>>::into(factory_.create_identifier(
                                    synthetic_factory_,
                                    &id_text(&name),
                                    Option::<NodeArray>::None,
                                    None,
                                )),
                                Option::<NodeArray>::None,
                            )
                            .into()
                    })
                }));
            }
            return Some(if let Some(type_symbol) = type_.maybe_symbol() {
                self.symbol_to_type_node(
                    type_checker,
                    &type_symbol,
                    context,
                    SymbolFlags::Type,
                    None,
                )
            } else {
                synthetic_factory.with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_
                            .create_type_reference_node(
                                synthetic_factory_,
                                Into::<Rc<Node>>::into(factory_.create_identifier(
                                    synthetic_factory_,
                                    "?",
                                    Option::<NodeArray>::None,
                                    None,
                                )),
                                Option::<NodeArray>::None,
                            )
                            .into()
                    })
                })
            });
        }
        if type_.flags().intersects(TypeFlags::Union) {
            if let Some(type_origin) = type_.as_union_type().origin.as_ref() {
                type_ = type_origin.clone();
            }
        }
        if type_
            .flags()
            .intersects(TypeFlags::Union | TypeFlags::Intersection)
        {
            let types = {
                let types = type_.as_union_or_intersection_type_interface().types();
                if type_.flags().intersects(TypeFlags::Union) {
                    type_checker.format_union_types(types)
                } else {
                    types.to_vec()
                }
            };
            if types.len() == 1 {
                return self.type_to_type_node_helper(type_checker, Some(&*types[0]), context);
            }
            let type_nodes =
                self.map_to_type_nodes(type_checker, Some(&types), context, Some(true));
            if let Some(type_nodes) = type_nodes {
                if !type_nodes.is_empty() {
                    return Some(if type_.flags().intersects(TypeFlags::Union) {
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_.create_union_type_node(synthetic_factory_, type_nodes)
                            })
                        })
                    } else {
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_intersection_type_node(synthetic_factory_, type_nodes)
                            })
                        })
                    });
                }
            }
            if !context.encountered_error.get()
                && !context
                    .flags()
                    .intersects(NodeBuilderFlags::AllowEmptyUnionOrIntersection)
            {
                context.encountered_error.set(true);
            }
            return None;
        }
        if object_flags.intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return Some(self.create_anonymous_type_node(type_checker, context, &type_));
        }
        if type_.flags().intersects(TypeFlags::Index) {
            let indexed_type = &type_.as_index_type().type_;
            context.increment_approximate_length_by(6);
            let index_type_node = self
                .type_to_type_node_helper(type_checker, Some(&**indexed_type), context)
                .unwrap();
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_type_operator_node(
                            synthetic_factory_,
                            SyntaxKind::KeyOfKeyword,
                            index_type_node.wrap(),
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::TemplateLiteral) {
            let type_as_template_literal_type = type_.as_template_literal_type();
            let texts = &type_as_template_literal_type.texts;
            let types = &type_as_template_literal_type.types;
            let template_head: Rc<Node> = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_template_head(
                            synthetic_factory_,
                            Some(texts[0].clone()),
                            None,
                            None,
                        )
                        .into()
                })
            });
            let template_spans = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_node_array(
                        map(Some(types), |t: &Rc<Type>, i| -> Rc<Node> {
                            factory_
                                .create_template_literal_type_span(
                                    synthetic_factory_,
                                    self.type_to_type_node_helper(
                                        type_checker,
                                        Some(&**t),
                                        context,
                                    )
                                    .unwrap()
                                    .wrap(),
                                    if i < types.len() - 1 {
                                        factory_
                                            .create_template_middle(
                                                synthetic_factory_,
                                                Some(texts[i + 1].clone()),
                                                None,
                                                None,
                                            )
                                            .into()
                                    } else {
                                        factory_
                                            .create_template_tail(
                                                synthetic_factory_,
                                                Some(texts[i + 1].clone()),
                                                None,
                                                None,
                                            )
                                            .into()
                                    },
                                )
                                .into()
                        }),
                        None,
                    )
                })
            });
            context.increment_approximate_length_by(2);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_template_literal_type(
                            synthetic_factory_,
                            template_head,
                            template_spans,
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::StringMapping) {
            let type_as_string_mapping_type = type_.as_string_mapping_type();
            let type_node = self
                .type_to_type_node_helper(
                    type_checker,
                    Some(&*type_as_string_mapping_type.type_),
                    context,
                )
                .unwrap();
            return Some(self.symbol_to_type_node(
                type_checker,
                &type_as_string_mapping_type.symbol,
                context,
                SymbolFlags::Type,
                Some(&vec![type_node.wrap()]),
            ));
        }
        if type_.flags().intersects(TypeFlags::IndexedAccess) {
            let type_as_indexed_access_type = type_.as_indexed_access_type();
            let object_type_node = self
                .type_to_type_node_helper(
                    type_checker,
                    Some(&*type_as_indexed_access_type.object_type),
                    context,
                )
                .unwrap();
            let index_type_node = self
                .type_to_type_node_helper(
                    type_checker,
                    Some(&*type_as_indexed_access_type.index_type),
                    context,
                )
                .unwrap();
            context.increment_approximate_length_by(2);
            return Some(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_indexed_access_type_node(
                            synthetic_factory_,
                            object_type_node.wrap(),
                            index_type_node.wrap(),
                        )
                        .into()
                })
            }));
        }
        if type_.flags().intersects(TypeFlags::Conditional) {
            return Some(self.visit_and_transform_type(&type_, |type_| {
                self.conditional_type_to_type_node(type_checker, context, type_)
            }));
        }
        if type_.flags().intersects(TypeFlags::Substitution) {
            return self.type_to_type_node_helper(
                type_checker,
                Some(&*type_.as_substitution_type().base_type),
                context,
            );
        }

        Debug_.fail(Some("Should be unreachable."));
    }
}

struct DefaultNodeBuilderContextSymbolTracker {}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new() -> Self {
        Self {}
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {}

pub struct NodeBuilderContext<'symbol_tracker> {
    pub enclosing_declaration: Option<Rc<Node>>,
    pub flags: Cell<NodeBuilderFlags>,
    pub tracker: &'symbol_tracker dyn SymbolTracker,

    pub encountered_error: Cell<bool>,
    pub reported_diagnostic: Cell<bool>,
    pub visited_types: RefCell<Option<HashSet<TypeId>>>,
    pub symbol_depth: RefCell<Option<HashMap<String, usize>>>,
    pub infer_type_parameters: RefCell<Option<Vec<Rc<Type /*TypeParameter*/>>>>,
    pub approximate_length: Cell<usize>,
    pub truncating: Cell<Option<bool>>,
    pub type_parameter_symbol_list: RefCell<Option<HashSet<SymbolId>>>,
    pub type_parameter_names: RefCell<Option<HashMap<TypeId, Rc<Node /*Identifier*/>>>>,
    pub type_parameter_names_by_text: RefCell<Option<HashSet<String>>>,
    pub type_parameter_names_by_text_next_name_count: RefCell<Option<HashMap<String, usize>>>,
    pub used_symbol_names: RefCell<Option<HashSet<String>>>,
    pub remapped_symbol_names: RefCell<Option<HashMap<SymbolId, String>>>,
    pub reverse_mapped_stack: RefCell<Option<Vec<Rc<Symbol /*ReverseMappedSymbol*/>>>>,
}

impl<'symbol_tracker> NodeBuilderContext<'symbol_tracker> {
    pub fn new(
        enclosing_declaration: Option<Rc<Node>>,
        flags: NodeBuilderFlags,
        tracker: &'symbol_tracker dyn SymbolTracker,
    ) -> Self {
        Self {
            enclosing_declaration,
            flags: Cell::new(flags),
            tracker,
            encountered_error: Cell::new(false),
            reported_diagnostic: Cell::new(false),
            visited_types: RefCell::new(None),
            symbol_depth: RefCell::new(None),
            infer_type_parameters: RefCell::new(None),
            approximate_length: Cell::new(0),
            truncating: Cell::new(None),
            type_parameter_symbol_list: RefCell::new(None),
            type_parameter_names: RefCell::new(None),
            type_parameter_names_by_text: RefCell::new(None),
            type_parameter_names_by_text_next_name_count: RefCell::new(None),
            used_symbol_names: RefCell::new(None),
            remapped_symbol_names: RefCell::new(None),
            reverse_mapped_stack: RefCell::new(None),
        }
    }

    pub fn flags(&self) -> NodeBuilderFlags {
        self.flags.get()
    }

    pub fn set_flags(&self, flags: NodeBuilderFlags) {
        self.flags.set(flags);
    }

    pub fn increment_approximate_length_by(&self, amount: usize) {
        self.approximate_length
            .set(self.approximate_length.get() + amount);
    }
}
