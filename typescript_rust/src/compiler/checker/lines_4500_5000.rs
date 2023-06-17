use std::{
    borrow::Borrow,
    cell::{Cell, Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    io, mem, ptr,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, Trace};

use super::SignatureToSignatureDeclarationOptions;
use crate::{
    add_synthetic_leading_comment, append_if_unique_gc, contains_gc, create_printer,
    create_text_writer, default_maximum_truncation_length, every, get_factory,
    get_first_identifier, get_object_flags, get_text_of_node,
    get_trailing_semicolon_deferring_writer, has_syntactic_modifier, id_text, is_binding_element,
    is_expression, is_expression_with_type_arguments_in_class_extends_clause,
    is_external_or_common_js_module, is_identifier_text, is_import_type_node, is_in_js_file,
    is_late_visibility_painted_statement, is_module_with_string_literal_name,
    is_type_reference_node, is_variable_declaration, is_variable_statement, maybe_filter,
    maybe_get_source_file_of_node, no_truncation_maximum_truncation_length,
    pseudo_big_int_to_string, ref_mut_unwrapped, ref_unwrapped, set_emit_flags, symbol_name,
    try_map, try_using_single_line_string_writer, using_single_line_string_writer, Debug_,
    EmitFlags, EmitHint, EmitTextWriter, FileIncludeReason, IndexInfo, KeywordTypeNode,
    ModifierFlags, ModuleSpecifierResolutionHost,
    ModuleSpecifierResolutionHostAndGetCommonSourceDirectory, MultiMap, Node, NodeArray,
    NodeBuilderFlags, NodeFlags, NodeInterface, ObjectFlags, Path, PrinterOptionsBuilder,
    RedirectTargetsMap, ScriptTarget, Signature, SignatureKind, Symbol, SymbolAccessibility,
    SymbolFlags, SymbolFormatFlags, SymbolId, SymbolInterface, SymbolTable, SymbolTracker,
    SymbolVisibilityResult, SymlinkCache, SyntaxKind, Type, TypeChecker, TypeCheckerHostDebuggable,
    TypeFlags, TypeFormatFlags, TypeId, TypeInterface,
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
            Option<Vec<Gc<Node /*LateVisibilityPaintedStatement*/>>>,
        > = RefCell::new(None);
        if !every(
            &maybe_filter(symbol.maybe_declarations().as_deref(), |d: &Gc<Node>| {
                d.kind() != SyntaxKind::Identifier
            })
            .unwrap_or_else(|| vec![]),
            |d: &Gc<Node>, _| {
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
        aliases_to_make_visible: &RefCell<Option<Vec<Gc<Node>>>>,
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
                && !has_syntactic_modifier(&declaration.parent().parent(), ModifierFlags::Export)
                && self.is_declaration_visible(&declaration.parent().parent().parent())
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
        aliases_to_make_visible: &RefCell<Option<Vec<Gc<Node>>>>,
        declaration: &Node,        /*Declaration*/
        aliasing_statement: &Node, /*LateVisibilityPaintedStatement*/
    ) -> bool {
        if should_compute_aliases_to_make_visible {
            self.get_node_links(declaration).borrow_mut().is_visible = Some(true);
            let mut aliases_to_make_visible = aliases_to_make_visible.borrow_mut();
            if aliases_to_make_visible.is_none() {
                *aliases_to_make_visible = Some(vec![]);
            }
            append_if_unique_gc(
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
    ) -> io::Result<SymbolVisibilityResult> {
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
            Option::<Gc<Node>>::None,
            false,
            None,
        )?;
        if matches!(symbol.as_ref(), Some(symbol) if symbol.flags().intersects(SymbolFlags::TypeParameter) && meaning.intersects(SymbolFlags::Type))
        {
            return Ok(SymbolVisibilityResult {
                accessibility: SymbolAccessibility::Accessible,
                aliases_to_make_visible: None,
                error_symbol_name: None,
                error_node: None,
            });
        }

        Ok(symbol
            .and_then(|symbol| self.has_visible_declarations(&symbol, true))
            .unwrap_or_else(|| SymbolVisibilityResult {
                accessibility: SymbolAccessibility::NotAccessible,
                aliases_to_make_visible: None,
                error_symbol_name: Some(get_text_of_node(&first_identifier, None).into_owned()),
                error_node: Some(first_identifier),
            }))
    }

    pub(super) fn symbol_to_string_(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
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
        let symbol_to_string_worker = |writer: Gc<Box<dyn EmitTextWriter>>| -> io::Result<_> {
            let entity = builder(
                &self.node_builder(),
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                enclosing_declaration.as_deref(),
                Some(node_flags),
                None,
            )?
            .unwrap();
            let printer = if matches!(
                enclosing_declaration.as_ref(),
                Some(enclosing_declaration) if enclosing_declaration.kind() == SyntaxKind::SourceFile
            ) {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .never_ascii_escape(Some(true))
                        .build()
                        .unwrap(),
                    None,
                )
            } else {
                create_printer(
                    PrinterOptionsBuilder::default()
                        .remove_comments(Some(true))
                        .build()
                        .unwrap(),
                    None,
                )
            };
            let source_file = enclosing_declaration
                .as_deref()
                .and_then(|enclosing_declaration| {
                    maybe_get_source_file_of_node(Some(enclosing_declaration))
                });
            printer.write_node(
                EmitHint::Unspecified,
                &entity,
                source_file.as_deref(),
                writer,
            )?;
            // writer

            Ok(())
        };
        Ok(if let Some(writer) = writer {
            symbol_to_string_worker(writer.clone())?;
            writer.get_text()
        } else {
            using_single_line_string_writer(symbol_to_string_worker)?
        })
    }

    pub(super) fn signature_to_string_(
        &self,
        signature: Gc<Signature>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        Ok(if let Some(writer) = writer {
            self.signature_to_string_worker(
                signature,
                enclosing_declaration,
                flags,
                kind,
                writer.clone(),
            )?;
            writer.get_text()
        } else {
            try_using_single_line_string_writer(|writer: Gc<Box<dyn EmitTextWriter>>| {
                self.signature_to_string_worker(
                    signature,
                    enclosing_declaration,
                    flags,
                    kind,
                    writer,
                )
            })?
        })
    }

    pub(super) fn signature_to_string_worker(
        &self,
        signature: Gc<Signature>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: TypeFormatFlags,
        kind: Option<SignatureKind>,
        writer: Gc<Box<dyn EmitTextWriter>>,
    ) -> io::Result<()> {
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
        let sig = self.node_builder().signature_to_signature_declaration(
            signature,
            sig_output,
            enclosing_declaration.as_deref(),
            Some(
                self.to_node_builder_flags(Some(flags))
                    | NodeBuilderFlags::IgnoreErrors
                    | NodeBuilderFlags::WriteTypeParametersInQualifiedName,
            ),
            None,
        )?;
        let printer = create_printer(
            PrinterOptionsBuilder::default()
                .remove_comments(Some(true))
                .omit_trailing_semicolon(Some(true))
                .build()
                .unwrap(),
            None,
        );
        let source_file = enclosing_declaration
            .as_deref()
            .and_then(|enclosing_declaration| {
                maybe_get_source_file_of_node(Some(enclosing_declaration))
            });
        printer.write_node(
            EmitHint::Unspecified,
            &sig.unwrap(),
            source_file.as_deref(),
            get_trailing_semicolon_deferring_writer(writer),
        )?;
        // writer

        Ok(())
    }

    pub(super) fn type_to_string_(
        &self,
        type_: &Type,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> io::Result<String> {
        let flags = flags.unwrap_or(
            TypeFormatFlags::AllowUniqueESSymbolType
                | TypeFormatFlags::UseAliasDefinedOutsideCurrentScope,
        );
        let writer = writer.unwrap_or_else(|| create_text_writer("").as_dyn_emit_text_writer());
        let no_truncation = matches!(self.compiler_options.no_error_truncation, Some(true))
            || flags.intersects(TypeFormatFlags::NoTruncation);
        let enclosing_declaration = enclosing_declaration
            .map(|enclosing_declaration| enclosing_declaration.borrow().node_wrapper());
        let type_node = self.node_builder().type_to_type_node(
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
            Some(writer.as_symbol_tracker()),
        )?;
        let type_node: Gc<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node,
        };
        let options = PrinterOptionsBuilder::default()
            .remove_comments(Some(!ptr::eq(type_, &*self.unresolved_type())))
            .build()
            .unwrap();
        let printer = create_printer(options, None);
        let source_file = enclosing_declaration.and_then(|enclosing_declaration| {
            maybe_get_source_file_of_node(Some(enclosing_declaration))
        });
        printer.write_node(
            EmitHint::Unspecified,
            &type_node,
            source_file.as_deref(),
            writer.clone(),
        )?;
        let result = writer.get_text();

        let max_length = if no_truncation {
            no_truncation_maximum_truncation_length * 2
        } else {
            default_maximum_truncation_length * 2
        };
        if max_length != 0 && !result.is_empty() && result.len() >= max_length {
            return Ok(format!("{}...", &result[0..max_length - "...".len()]));
        }
        Ok(result)
    }

    pub(super) fn get_type_names_for_error_display(
        &self,
        left: &Type,
        right: &Type,
    ) -> io::Result<(String, String)> {
        let mut left_str = if let Some(symbol) = left.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(Some(&symbol))? {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(left, enclosing_declaration, None, None)?
            } else {
                self.type_to_string_(left, Option::<&Node>::None, None, None)?
            }
        } else {
            self.type_to_string_(left, Option::<&Node>::None, None, None)?
        };
        let mut right_str = if let Some(symbol) = right.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(Some(&symbol))? {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(right, enclosing_declaration, None, None)?
            } else {
                self.type_to_string_(right, Option::<&Node>::None, None, None)?
            }
        } else {
            self.type_to_string_(right, Option::<&Node>::None, None, None)?
        };
        if left_str == right_str {
            left_str = self.get_type_name_for_error_display(left)?;
            right_str = self.get_type_name_for_error_display(right)?;
        }
        Ok((left_str, right_str))
    }

    pub(super) fn get_type_name_for_error_display(&self, type_: &Type) -> io::Result<String> {
        self.type_to_string_(
            type_,
            Option::<&Node>::None,
            Some(TypeFormatFlags::UseFullyQualifiedType),
            None,
        )
    }

    pub(super) fn symbol_value_declaration_is_context_sensitive(
        &self,
        symbol: Option<&Symbol>,
    ) -> io::Result<bool> {
        if symbol.is_none() {
            return Ok(false);
        }
        let symbol = symbol.unwrap();
        Ok(matches!(
            symbol.maybe_value_declaration(),
            Some(value_declaration) if is_expression(&value_declaration) && !self.is_context_sensitive(&value_declaration)?
        ))
    }

    pub(super) fn to_node_builder_flags(&self, flags: Option<TypeFormatFlags>) -> NodeBuilderFlags {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        NodeBuilderFlags::from_bits((flags & TypeFormatFlags::NodeBuilderFlagsMask).bits()).unwrap()
    }

    pub(super) fn is_class_instance_side(&self, type_: &Type) -> io::Result<bool> {
        Ok(matches!(
            type_.maybe_symbol(),
            Some(type_symbol) if type_symbol.flags().intersects(SymbolFlags::Class) && (
                ptr::eq(type_, &*self.get_declared_type_of_class_or_interface(&type_symbol)?) ||
                type_.flags().intersects(TypeFlags::Object) && get_object_flags(type_).intersects(ObjectFlags::IsClassInstanceClone)
            )
        ))
    }

    pub(super) fn create_node_builder(&self) -> Gc<NodeBuilder> {
        NodeBuilder::new(self.rc_wrapper())
    }
}

#[derive(Clone, Debug, Trace, Finalize)]
pub struct NodeBuilder {
    pub(super) _rc_wrapper: GcCell<Option<Gc<NodeBuilder>>>,
    pub type_checker: Gc<TypeChecker>,
}

impl NodeBuilder {
    pub fn new(type_checker: Gc<TypeChecker>) -> Gc<Self> {
        let ret = Gc::new(Self {
            type_checker,
            _rc_wrapper: Default::default(),
        });
        *ret._rc_wrapper.borrow_mut() = Some(ret.clone());
        ret
    }

    pub fn rc_wrapper(&self) -> Gc<Self> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub fn type_to_type_node(
        &self,
        type_: &Type,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_to_type_node_helper(Some(type_), context)
        })
    }

    pub fn index_info_to_index_signature_declaration(
        &self,
        index_info: &IndexInfo,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(
                self.index_info_to_index_signature_declaration_helper(
                    index_info,
                    context,
                    Option::<&Node>::None,
                )?,
            ))
        })
    }

    pub fn signature_to_signature_declaration(
        &self,
        signature: Gc<Signature>,
        kind: SyntaxKind,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/>>>
    {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.signature_to_signature_declaration_helper(
                signature,
                kind,
                context,
                Option::<SignatureToSignatureDeclarationOptions<fn(&Symbol)>>::None,
            )?))
        })
    }

    pub fn symbol_to_entity_name(
        &self,
        symbol: &Symbol,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*EntityName*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_name(symbol, context, meaning, false)?))
        })
    }

    pub fn symbol_to_expression(
        &self,
        symbol: &Symbol,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_expression_(symbol, context, meaning)?))
        })
    }

    pub fn symbol_to_type_parameter_declarations(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<NodeArray> /*<TypeParameterDeclaration>*/>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            self.type_parameters_to_type_parameter_declarations(symbol, context)
        })
    }

    pub fn symbol_to_parameter_declaration(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*ParameterDeclaration*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_to_parameter_declaration_(
                symbol,
                context,
                None,
                Option::<&fn(&Symbol)>::None,
                None,
            )?))
        })
    }

    pub fn type_parameter_to_declaration(
        &self,
        parameter: &Type, /*TypeParameter*/
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
    ) -> io::Result<Option<Gc<Node /*TypeParameterDeclaration*/>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.type_parameter_to_declaration_(
                parameter, context, None,
            )?))
        })
    }

    pub fn symbol_table_to_declaration_statements(
        &self,
        symbol_table: Gc<GcCell<SymbolTable>>,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
        bundled: Option<bool>,
    ) -> io::Result<Option<Vec<Gc<Node /*Statement*/>>>> {
        self.try_with_context(enclosing_declaration, flags, tracker, |context| {
            Ok(Some(self.symbol_table_to_declaration_statements_(
                symbol_table,
                context,
                bundled,
            )?))
        })
    }

    #[allow(dead_code)]
    pub(super) fn with_context<TReturn>(
        &self,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
        cb: impl FnOnce(&NodeBuilderContext) -> Option<TReturn>,
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
        let tracker = tracker
            .filter(|tracker| tracker.is_track_symbol_supported())
            .unwrap_or_else(|| {
                DefaultNodeBuilderContextSymbolTracker::new(self.type_checker.host.clone(), flags)
                    .as_dyn_symbol_tracker()
            });
        let context = NodeBuilderContext::new(
            enclosing_declaration,
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.clone(),
        );
        let context_tracker = wrap_symbol_tracker_to_report_for_context(context.clone(), tracker);
        context.set_tracker(Gc::new(Box::new(context_tracker)));
        let context = (*context).borrow();
        let resulting_node = cb(&context);
        if context.truncating.get() == Some(true)
            && context
                .flags
                .get()
                .intersects(NodeBuilderFlags::NoTruncation)
        {
            context.tracker().report_truncation_error();
        }
        if context.encountered_error() {
            None
        } else {
            resulting_node
        }
    }

    pub(super) fn try_with_context<TReturn>(
        &self,
        enclosing_declaration: Option<impl Borrow<Node>>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<Gc<Box<dyn SymbolTracker>>>,
        cb: impl FnOnce(&NodeBuilderContext) -> io::Result<Option<TReturn>>,
    ) -> io::Result<Option<TReturn>> {
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
        let tracker = tracker
            .filter(|tracker| tracker.is_track_symbol_supported())
            .unwrap_or_else(|| {
                DefaultNodeBuilderContextSymbolTracker::new(self.type_checker.host.clone(), flags)
                    .as_dyn_symbol_tracker()
            });
        let context = NodeBuilderContext::new(
            enclosing_declaration,
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.clone(),
        );
        let context_tracker = wrap_symbol_tracker_to_report_for_context(context.clone(), tracker);
        context.set_tracker(Gc::new(Box::new(context_tracker)));
        let context = (*context).borrow();
        let resulting_node = cb(&context)?;
        if context.truncating.get() == Some(true)
            && context
                .flags
                .get()
                .intersects(NodeBuilderFlags::NoTruncation)
        {
            context.tracker().report_truncation_error();
        }
        Ok(if context.encountered_error() {
            None
        } else {
            resulting_node
        })
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

    pub(super) fn type_to_type_node_helper(
        &self,
        type_: Option<impl Borrow<Type>>,
        context: &NodeBuilderContext,
    ) -> io::Result<Option<Gc<Node>>> {
        if let Some(_cancellation_token) = self.type_checker.maybe_cancellation_token()
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
                context.set_encountered_error(true);
                return Ok(None);
            }
            context.increment_approximate_length_by(3);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::AnyKeyword),
                )
                .wrap(),
            ));
        }
        let type_ = type_.unwrap();
        let mut type_ = type_.borrow().type_wrapper();

        if !context
            .flags()
            .intersects(NodeBuilderFlags::NoTypeReduction)
        {
            type_ = self.type_checker.get_reduced_type(&type_)?;
        }

        if type_.flags().intersects(TypeFlags::Any) {
            if let Some(type_alias_symbol) = type_.maybe_alias_symbol() {
                return Ok(Some(
                    get_factory()
                        .create_type_reference_node(
                            self.symbol_to_entity_name_node(&type_alias_symbol),
                            self.map_to_type_nodes(
                                type_.maybe_alias_type_arguments().as_deref(),
                                context,
                                None,
                            )?,
                        )
                        .wrap(),
                ));
            }
            if Gc::ptr_eq(&type_, &self.type_checker.unresolved_type()) {
                let ret: Node = Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::AnyKeyword),
                )
                .into();
                add_synthetic_leading_comment(
                    &ret,
                    SyntaxKind::MultiLineCommentTrivia,
                    "unresolved",
                    None,
                );
                return Ok(Some(ret.wrap()));
            }
            context.increment_approximate_length_by(3);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(get_factory().create_keyword_type_node(
                    if Gc::ptr_eq(&type_, &self.type_checker.intrinsic_marker_type()) {
                        SyntaxKind::IntrinsicKeyword
                    } else {
                        SyntaxKind::AnyKeyword
                    },
                ))
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Unknown) {
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::UnknownKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::String) {
            context.increment_approximate_length_by(6);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::StringKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Number) {
            context.increment_approximate_length_by(6);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::NumberKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::BigInt) {
            context.increment_approximate_length_by(6);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::BigIntKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Boolean) && type_.maybe_alias_symbol().is_none() {
            context.increment_approximate_length_by(7);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::BooleanKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::EnumLiteral)
            && !type_.flags().intersects(TypeFlags::Union)
        {
            let parent_symbol = self
                .type_checker
                .get_parent_of_symbol(&type_.symbol())?
                .unwrap();
            let parent_name =
                self.symbol_to_type_node(&parent_symbol, context, SymbolFlags::Type, None)?;
            if Gc::ptr_eq(
                &self
                    .type_checker
                    .get_declared_type_of_symbol(&parent_symbol)?,
                &type_,
            ) {
                return Ok(Some(parent_name));
            }
            let type_symbol = type_.symbol();
            let member_name = symbol_name(&type_symbol);
            if is_identifier_text(&member_name, Some(ScriptTarget::ES3), None) {
                return Ok(Some(
                    self.append_reference_to_type(
                        &parent_name,
                        &get_factory()
                            .create_type_reference_node(
                                &*member_name,
                                Option::<Gc<NodeArray>>::None,
                            )
                            .into(),
                    ),
                ));
            }
            if is_import_type_node(&parent_name) {
                parent_name.as_import_type_node().set_is_type_of(true);
                return Ok(Some(
                    get_factory()
                        .create_indexed_access_type_node(
                            parent_name,
                            get_factory()
                                .create_literal_type_node(
                                    get_factory()
                                        .create_string_literal(member_name.into_owned(), None, None)
                                        .wrap(),
                                )
                                .wrap(),
                        )
                        .wrap(),
                ));
            } else if is_type_reference_node(&parent_name) {
                return Ok(Some(
                    get_factory()
                        .create_indexed_access_type_node(
                            get_factory()
                                .create_type_query_node(
                                    parent_name.as_type_reference_node().type_name.clone(),
                                )
                                .wrap(),
                            get_factory()
                                .create_literal_type_node(
                                    get_factory()
                                        .create_string_literal(member_name.into_owned(), None, None)
                                        .wrap(),
                                )
                                .wrap(),
                        )
                        .wrap(),
                ));
            } else {
                Debug_.fail(Some(
                    "Unhandled type node kind returned from `symbolToTypeNode`.",
                ));
            }
        }
        if type_.flags().intersects(TypeFlags::EnumLike) {
            return Ok(Some(self.symbol_to_type_node(
                &type_.symbol(),
                context,
                SymbolFlags::Type,
                None,
            )?));
        }
        if type_.flags().intersects(TypeFlags::StringLiteral) {
            let value = type_.as_string_literal_type().value.clone();
            context.increment_approximate_length_by(value.len() + 2);
            return Ok(Some(
                get_factory()
                    .create_literal_type_node(set_emit_flags(
                        get_factory()
                            .create_string_literal(
                                value,
                                Some(context.flags().intersects(
                                    NodeBuilderFlags::UseSingleQuotesForStringLiteralType,
                                )),
                                None,
                            )
                            .wrap(),
                        EmitFlags::NoAsciiEscaping,
                    ))
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::NumberLiteral) {
            let value = type_.as_number_literal_type().value.value();
            context.increment_approximate_length_by(value.to_string().len());
            return Ok(Some(
                get_factory()
                    .create_literal_type_node(if value < 0.0 {
                        get_factory()
                            .create_prefix_unary_expression(
                                SyntaxKind::MinusToken,
                                get_factory()
                                    .create_numeric_literal((-value).to_string(), None)
                                    .wrap(),
                            )
                            .wrap()
                    } else {
                        get_factory()
                            .create_numeric_literal(value.to_string(), None)
                            .wrap()
                    })
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            let value = &type_.as_big_int_literal_type().value;
            context.increment_approximate_length_by(pseudo_big_int_to_string(value).len() + 1);
            return Ok(Some(
                get_factory()
                    .create_literal_type_node(
                        get_factory().create_big_int_literal(value.clone()).wrap(),
                    )
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            let type_intrinsic_name = type_.as_intrinsic_type().intrinsic_name();
            context.increment_approximate_length_by(type_intrinsic_name.len());
            return Ok(Some(
                get_factory()
                    .create_literal_type_node(
                        if type_intrinsic_name == "true" {
                            get_factory().create_true()
                        } else {
                            get_factory().create_false()
                        }
                        .wrap(),
                    )
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::UniqueESSymbol) {
            if !context
                .flags()
                .intersects(NodeBuilderFlags::AllowUniqueESSymbolType)
            {
                if self.type_checker.is_value_symbol_accessible(
                    &type_.symbol(),
                    context.maybe_enclosing_declaration().as_deref(),
                )? {
                    context.increment_approximate_length_by(6);
                    return Ok(Some(self.symbol_to_type_node(
                        &type_.symbol(),
                        context,
                        SymbolFlags::Value,
                        None,
                    )?));
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context.tracker().report_inaccessible_unique_symbol_error();
                // }
            }
            context.increment_approximate_length_by(13);
            return Ok(Some(
                get_factory()
                    .create_type_operator_node(
                        SyntaxKind::UniqueKeyword,
                        Into::<KeywordTypeNode>::into(
                            get_factory().create_keyword_type_node(SyntaxKind::SymbolKeyword),
                        )
                        .wrap(),
                    )
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Void) {
            context.increment_approximate_length_by(4);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::VoidKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Undefined) {
            context.increment_approximate_length_by(9);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::UndefinedKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Null) {
            context.increment_approximate_length_by(9);
            return Ok(Some(
                get_factory()
                    .create_literal_type_node(get_factory().create_null().wrap())
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Never) {
            context.increment_approximate_length_by(5);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::NeverKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::ESSymbol) {
            context.increment_approximate_length_by(6);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::SymbolKeyword),
                )
                .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::NonPrimitive) {
            context.increment_approximate_length_by(6);
            return Ok(Some(
                Into::<KeywordTypeNode>::into(
                    get_factory().create_keyword_type_node(SyntaxKind::ObjectKeyword),
                )
                .wrap(),
            ));
        }
        if self.type_checker.is_this_type_parameter(&type_) {
            if context
                .flags()
                .intersects(NodeBuilderFlags::InObjectTypeLiteral)
            {
                if !context.encountered_error()
                    && !context
                        .flags()
                        .intersects(NodeBuilderFlags::AllowThisInObjectLiteral)
                {
                    context.set_encountered_error(true);
                }
                // if (context.tracker.reportInaccessibleUniqueSymbolError) {
                context.tracker().report_inaccessible_unique_symbol_error();
                // }
            }
            context.increment_approximate_length_by(4);
            return Ok(Some(get_factory().create_this_type_node().wrap()));
        }

        if !in_type_alias {
            if let Some(type_alias_symbol) = type_.maybe_alias_symbol() {
                if context
                    .flags()
                    .intersects(NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope)
                    || self.type_checker.is_type_symbol_accessible(
                        &type_alias_symbol,
                        context.maybe_enclosing_declaration().as_deref(),
                    )?
                {
                    let type_argument_nodes = self.map_to_type_nodes(
                        type_.maybe_alias_type_arguments().as_deref(),
                        context,
                        None,
                    )?;
                    if self
                        .type_checker
                        .is_reserved_member_name(type_alias_symbol.escaped_name())
                        && !type_alias_symbol.flags().intersects(SymbolFlags::Class)
                    {
                        return Ok(Some(
                            get_factory()
                                .create_type_reference_node(
                                    get_factory().create_identifier(""),
                                    type_argument_nodes,
                                )
                                .wrap(),
                        ));
                    }
                    return Ok(Some(self.symbol_to_type_node(
                        &type_alias_symbol,
                        context,
                        SymbolFlags::Type,
                        type_argument_nodes.as_deref(),
                    )?));
                }
            }
        }

        let object_flags = get_object_flags(&type_);

        if object_flags.intersects(ObjectFlags::Reference) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return Ok(Some(
                if type_.as_type_reference_interface().maybe_node().is_some() {
                    self.try_visit_and_transform_type(context, &type_, |type_| {
                        Ok(self.type_reference_to_type_node(context, type_)?.unwrap())
                    })?
                } else {
                    self.type_reference_to_type_node(context, &type_)?.unwrap()
                },
            ));
        }
        if type_.flags().intersects(TypeFlags::TypeParameter)
            || object_flags.intersects(ObjectFlags::ClassOrInterface)
        {
            if type_.flags().intersects(TypeFlags::TypeParameter)
                && contains_gc(
                    (*context.infer_type_parameters).borrow().as_deref(),
                    &type_.type_wrapper(),
                )
            {
                context.increment_approximate_length_by(symbol_name(&type_.symbol()).len() + 6);
                return Ok(Some(
                    get_factory()
                        .create_infer_type_node(
                            self.type_parameter_to_declaration_with_constraint(
                                &type_, context, None,
                            )?,
                        )
                        .wrap(),
                ));
            }
            if context
                .flags()
                .intersects(NodeBuilderFlags::GenerateNamesForShadowedTypeParams)
                && type_.flags().intersects(TypeFlags::TypeParameter)
                && !self.type_checker.is_type_symbol_accessible(
                    &type_.symbol(),
                    context.maybe_enclosing_declaration().as_deref(),
                )?
            {
                let name = self.type_parameter_to_name(&type_, context)?;
                context.increment_approximate_length_by(id_text(&name).len());
                return Ok(Some(
                    get_factory()
                        .create_type_reference_node(
                            get_factory().create_identifier(&id_text(&name)),
                            Option::<Gc<NodeArray>>::None,
                        )
                        .wrap(),
                ));
            }
            return Ok(Some(if let Some(type_symbol) = type_.maybe_symbol() {
                self.symbol_to_type_node(&type_symbol, context, SymbolFlags::Type, None)?
            } else {
                get_factory()
                    .create_type_reference_node(
                        get_factory().create_identifier("?"),
                        Option::<Gc<NodeArray>>::None,
                    )
                    .wrap()
            }));
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
                    self.type_checker.format_union_types(types)?
                } else {
                    types.to_vec()
                }
            };
            if types.len() == 1 {
                return self.type_to_type_node_helper(Some(&*types[0]), context);
            }
            let type_nodes = self.map_to_type_nodes(Some(&types), context, Some(true))?;
            if let Some(type_nodes) = type_nodes {
                if !type_nodes.is_empty() {
                    return Ok(Some(if type_.flags().intersects(TypeFlags::Union) {
                        get_factory().create_union_type_node(type_nodes).wrap()
                    } else {
                        get_factory()
                            .create_intersection_type_node(type_nodes)
                            .wrap()
                    }));
                }
            }
            if !context.encountered_error()
                && !context
                    .flags()
                    .intersects(NodeBuilderFlags::AllowEmptyUnionOrIntersection)
            {
                context.set_encountered_error(true);
            }
            return Ok(None);
        }
        if object_flags.intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return Ok(Some(self.create_anonymous_type_node(context, &type_)?));
        }
        if type_.flags().intersects(TypeFlags::Index) {
            let indexed_type = &type_.as_index_type().type_;
            context.increment_approximate_length_by(6);
            let index_type_node = self
                .type_to_type_node_helper(Some(&**indexed_type), context)?
                .unwrap();
            return Ok(Some(
                get_factory()
                    .create_type_operator_node(SyntaxKind::KeyOfKeyword, index_type_node)
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::TemplateLiteral) {
            let type_as_template_literal_type = type_.as_template_literal_type();
            let texts = &type_as_template_literal_type.texts;
            let types = &type_as_template_literal_type.types;
            let template_head: Gc<Node> = get_factory()
                .create_template_head(Some(texts[0].clone()), None, None)
                .wrap();
            let template_spans = get_factory().create_node_array(
                Some(try_map(types, |t: &Gc<Type>, i| -> io::Result<Gc<Node>> {
                    Ok(get_factory()
                        .create_template_literal_type_span(
                            self.type_to_type_node_helper(Some(&**t), context)?.unwrap(),
                            if i < types.len() - 1 {
                                get_factory()
                                    .create_template_middle(Some(texts[i + 1].clone()), None, None)
                                    .wrap()
                            } else {
                                get_factory()
                                    .create_template_tail(Some(texts[i + 1].clone()), None, None)
                                    .wrap()
                            },
                        )
                        .wrap())
                })?),
                None,
            );
            context.increment_approximate_length_by(2);
            return Ok(Some(
                get_factory()
                    .create_template_literal_type(template_head, template_spans)
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::StringMapping) {
            let type_node = self
                .type_to_type_node_helper(Some(&*type_.as_string_mapping_type().type_), context)?
                .unwrap();
            return Ok(Some(self.symbol_to_type_node(
                &type_.symbol(),
                context,
                SymbolFlags::Type,
                Some(&vec![type_node]),
            )?));
        }
        if type_.flags().intersects(TypeFlags::IndexedAccess) {
            let type_as_indexed_access_type = type_.as_indexed_access_type();
            let object_type_node = self
                .type_to_type_node_helper(Some(&*type_as_indexed_access_type.object_type), context)?
                .unwrap();
            let index_type_node = self
                .type_to_type_node_helper(Some(&*type_as_indexed_access_type.index_type), context)?
                .unwrap();
            context.increment_approximate_length_by(2);
            return Ok(Some(
                get_factory()
                    .create_indexed_access_type_node(object_type_node, index_type_node)
                    .wrap(),
            ));
        }
        if type_.flags().intersects(TypeFlags::Conditional) {
            return Ok(Some(self.try_visit_and_transform_type(
                context,
                &type_,
                |type_| self.conditional_type_to_type_node(context, type_),
            )?));
        }
        if type_.flags().intersects(TypeFlags::Substitution) {
            return self
                .type_to_type_node_helper(Some(&*type_.as_substitution_type().base_type), context);
        }

        Debug_.fail(Some("Should be unreachable."));
    }
}

#[derive(Trace, Finalize)]
struct DefaultNodeBuilderContextSymbolTracker {
    _dyn_rc_wrapper: GcCell<Option<Gc<Box<dyn SymbolTracker>>>>,
    pub module_resolver_host:
        Option<Gc<Box<dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory>>>,
}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new(
        host: Gc<Box<dyn TypeCheckerHostDebuggable>>,
        flags: Option<NodeBuilderFlags>,
    ) -> Gc<Box<Self>> {
        let dyn_rc_wrapper: Gc<Box<dyn SymbolTracker>> = Gc::new(Box::new(Self {
            _dyn_rc_wrapper: Default::default(),
            module_resolver_host: if matches!(
                flags,
                Some(flags) if flags.intersects(NodeBuilderFlags::DoNotIncludeSymbolChain)
            ) {
                Some(Gc::new(Box::new(
                    DefaultNodeBuilderContextSymbolTrackerModuleResolverHost::new(host),
                )))
            } else {
                None
            },
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(dyn_rc_wrapper.clone()) };
        *downcasted._dyn_rc_wrapper.borrow_mut() = Some(dyn_rc_wrapper);
        downcasted
    }

    pub fn as_dyn_symbol_tracker(&self) -> Gc<Box<dyn SymbolTracker>> {
        self._dyn_rc_wrapper.borrow().clone().unwrap()
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {
    fn track_symbol(
        &self,
        _symbol: &Symbol,
        _enclosing_declaration: Option<Gc<Node>>,
        _meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        Some(Ok(false))
    }

    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn disable_track_symbol(&self) {}

    fn reenable_track_symbol(&self) {}

    fn module_resolver_host(
        &self,
    ) -> Option<&dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.module_resolver_host
            .as_ref()
            .map(|module_resolver_host| &***module_resolver_host)
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.module_resolver_host.as_ref().is_some()
    }

    // TODO: are these correct?
    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        false
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        false
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        false
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        false
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        false
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        false
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        false
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        false
    }
}

#[derive(Trace, Finalize)]
struct DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    pub host: Gc<Box<dyn TypeCheckerHostDebuggable>>,
}

impl DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    pub fn new(host: Gc<Box<dyn TypeCheckerHostDebuggable>>) -> Self {
        Self { host }
    }
}

impl ModuleSpecifierResolutionHostAndGetCommonSourceDirectory
    for DefaultNodeBuilderContextSymbolTrackerModuleResolverHost
{
    fn get_common_source_directory(&self) -> String {
        self.host
            .get_common_source_directory()
            .unwrap_or_else(|| "".to_owned())
    }

    fn as_dyn_module_specifier_resolution_host(&self) -> &dyn ModuleSpecifierResolutionHost {
        self
    }
}

impl ModuleSpecifierResolutionHost for DefaultNodeBuilderContextSymbolTrackerModuleResolverHost {
    fn get_current_directory(&self) -> String {
        self.host.get_current_directory()
    }

    fn get_symlink_cache(&self) -> Option<Gc<SymlinkCache>> {
        self.host.get_symlink_cache()
    }

    fn use_case_sensitive_file_names(&self) -> Option<bool> {
        self.host.use_case_sensitive_file_names()
    }

    fn redirect_targets_map(&self) -> Rc<RefCell<RedirectTargetsMap>> {
        self.host.redirect_targets_map()
    }

    fn get_project_reference_redirect(&self, file_name: &str) -> Option<String> {
        ModuleSpecifierResolutionHost::get_project_reference_redirect(&**self.host, file_name)
    }

    fn is_source_of_project_reference_redirect(&self, file_name: &str) -> bool {
        ModuleSpecifierResolutionHost::is_source_of_project_reference_redirect(
            &**self.host,
            file_name,
        )
    }

    fn file_exists(&self, file_name: &str) -> bool {
        self.host.file_exists(file_name)
    }

    fn get_file_include_reasons(&self) -> Gc<GcCell<MultiMap<Path, Gc<FileIncludeReason>>>> {
        self.host.get_file_include_reasons()
    }

    fn read_file(&self, file_name: &str) -> Option<io::Result<Option<String>>> {
        self.host.read_file(file_name)
    }

    fn is_read_file_supported(&self) -> bool {
        self.host.is_read_file_supported()
    }

    fn is_get_nearest_ancestor_directory_with_package_json_supported(&self) -> bool {
        false
    }
}

pub(super) fn wrap_symbol_tracker_to_report_for_context(
    context: Gc<NodeBuilderContext>,
    tracker: Gc<Box<dyn SymbolTracker>>,
) -> NodeBuilderContextWrappedSymbolTracker {
    NodeBuilderContextWrappedSymbolTracker::new(tracker, context)
}

#[derive(Trace, Finalize)]
pub(super) struct NodeBuilderContextWrappedSymbolTracker {
    #[unsafe_ignore_trace]
    is_track_symbol_disabled: Cell<bool>,
    tracker: Gc<Box<dyn SymbolTracker>>,
    context: Gc<NodeBuilderContext>,
}

impl NodeBuilderContextWrappedSymbolTracker {
    pub(super) fn new(
        tracker: Gc<Box<dyn SymbolTracker>>,
        context: Gc<NodeBuilderContext>,
    ) -> Self {
        Self {
            is_track_symbol_disabled: Default::default(),
            tracker,
            context,
        }
    }

    fn mark_context_reported_diagnostic(&self) {
        (*self.context).borrow().reported_diagnostic.set(true);
    }
}

impl SymbolTracker for NodeBuilderContextWrappedSymbolTracker {
    fn report_cyclic_structure_error(&self) {
        if self.tracker.is_report_cyclic_structure_error_supported() {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.report_cyclic_structure_error()
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        self.tracker.is_report_cyclic_structure_error_supported()
    }

    fn report_inaccessible_this_error(&self) {
        if self.tracker.is_report_inaccessible_this_error_supported() {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.report_inaccessible_this_error()
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self.tracker.is_report_inaccessible_this_error_supported()
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        if self
            .tracker
            .is_report_inaccessible_unique_symbol_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.report_inaccessible_unique_symbol_error()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self.tracker
            .is_report_inaccessible_unique_symbol_error_supported()
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        if self
            .tracker
            .is_report_likely_unsafe_import_required_error_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .report_likely_unsafe_import_required_error(specifier)
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        self.tracker
            .is_report_likely_unsafe_import_required_error_supported()
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: &Node, /*SourceFile*/
        parent_symbol: &Symbol,
        augmenting_symbol: &Symbol,
    ) {
        if self.tracker.is_report_nonlocal_augmentation_supported() {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .report_nonlocal_augmentation(containing_file, parent_symbol, augmenting_symbol)
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        self.tracker.is_report_nonlocal_augmentation_supported()
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        if self
            .tracker
            .is_report_private_in_base_of_class_expression_supported()
        {
            self.mark_context_reported_diagnostic();
        }
        self.tracker
            .report_private_in_base_of_class_expression(property_name)
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self.tracker
            .is_report_private_in_base_of_class_expression_supported()
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        if self.tracker.is_report_non_serializable_property_supported() {
            self.mark_context_reported_diagnostic();
        }
        self.tracker.report_non_serializable_property(property_name)
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        self.tracker.is_report_non_serializable_property_supported()
    }

    fn track_symbol(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<Gc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        if self.is_track_symbol_disabled.get() {
            return Some(Ok(false));
        }
        let result = self
            .tracker
            .track_symbol(symbol, enclosing_declaration, meaning);
        if matches!(
            result.as_ref(),
            Some(result) if matches!(
                result,
                Ok(result) if *result
            )
        ) {
            self.mark_context_reported_diagnostic();
        }
        result
    }

    fn is_track_symbol_supported(&self) -> bool {
        self.tracker.is_track_symbol_supported()
    }

    fn disable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(true);
    }

    fn reenable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(false);
    }

    fn report_truncation_error(&self) {
        self.tracker.report_truncation_error()
    }

    fn module_resolver_host(
        &self,
    ) -> Option<&dyn ModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.tracker.module_resolver_host()
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.tracker.is_module_resolver_host_supported()
    }

    fn track_referenced_ambient_module(
        &self,
        decl: &Node, /*ModuleDeclaration*/
        symbol: &Symbol,
    ) -> io::Result<()> {
        self.tracker.track_referenced_ambient_module(decl, symbol)
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        self.tracker.is_track_referenced_ambient_module_supported()
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: &Symbol) {
        self.tracker
            .track_external_module_symbol_of_import_type_node(symbol)
    }
}

#[derive(Trace, Finalize)]
pub struct NodeBuilderContext {
    pub(super) _rc_wrapper: GcCell<Option<Gc<NodeBuilderContext>>>,
    pub(super) enclosing_declaration: Gc<GcCell<Option<Gc<Node>>>>,
    #[unsafe_ignore_trace]
    pub flags: Cell<NodeBuilderFlags>,
    pub(super) tracker: GcCell<Gc<Box<dyn SymbolTracker>>>,

    #[unsafe_ignore_trace]
    pub encountered_error: Cell<bool>,
    #[unsafe_ignore_trace]
    pub reported_diagnostic: Cell<bool>,
    #[unsafe_ignore_trace]
    pub visited_types: Rc<RefCell<Option<HashSet<TypeId>>>>,
    #[unsafe_ignore_trace]
    pub symbol_depth: Rc<RefCell<Option<HashMap<String, usize>>>>,
    pub infer_type_parameters: Gc<GcCell<Option<Vec<Gc<Type /*TypeParameter*/>>>>>,
    #[unsafe_ignore_trace]
    pub approximate_length: Cell<usize>,
    #[unsafe_ignore_trace]
    pub truncating: Cell<Option<bool>>,
    #[unsafe_ignore_trace]
    pub type_parameter_symbol_list: Rc<RefCell<Option<HashSet<SymbolId>>>>,
    pub type_parameter_names: Gc<GcCell<Option<HashMap<TypeId, Gc<Node /*Identifier*/>>>>>,
    #[unsafe_ignore_trace]
    pub type_parameter_names_by_text: Rc<RefCell<Option<HashSet<String>>>>,
    #[unsafe_ignore_trace]
    pub type_parameter_names_by_text_next_name_count: Rc<RefCell<Option<HashMap<String, usize>>>>,
    #[unsafe_ignore_trace]
    pub used_symbol_names: Rc<RefCell<Option<HashSet<String>>>>,
    #[unsafe_ignore_trace]
    pub remapped_symbol_names: Rc<RefCell<Option<HashMap<SymbolId, String>>>>,
    pub reverse_mapped_stack: Gc<GcCell<Option<Vec<Gc<Symbol /*ReverseMappedSymbol*/>>>>>,
}

impl NodeBuilderContext {
    pub fn new(
        enclosing_declaration: Option<Gc<Node>>,
        flags: NodeBuilderFlags,
        tracker: Gc<Box<dyn SymbolTracker>>,
    ) -> Gc<Self> {
        let ret = Gc::new(Self {
            _rc_wrapper: Default::default(),
            enclosing_declaration: Gc::new(GcCell::new(enclosing_declaration)),
            flags: Cell::new(flags),
            tracker: GcCell::new(tracker),
            encountered_error: Default::default(),
            reported_diagnostic: Default::default(),
            visited_types: Default::default(),
            symbol_depth: Default::default(),
            infer_type_parameters: Default::default(),
            approximate_length: Default::default(),
            truncating: Default::default(),
            type_parameter_symbol_list: Default::default(),
            type_parameter_names: Default::default(),
            type_parameter_names_by_text: Default::default(),
            type_parameter_names_by_text_next_name_count: Default::default(),
            used_symbol_names: Default::default(),
            remapped_symbol_names: Default::default(),
            reverse_mapped_stack: Default::default(),
        });
        *ret._rc_wrapper.borrow_mut() = Some(ret.clone());
        ret
    }

    pub fn rc_wrapper(&self) -> Gc<Self> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub(super) fn set_rc_wrapper(&self, rc_wrapper: Option<Gc<Self>>) {
        *self._rc_wrapper.borrow_mut() = rc_wrapper;
    }

    pub fn maybe_enclosing_declaration(&self) -> Option<Gc<Node>> {
        (*self.enclosing_declaration).borrow().clone()
    }

    pub fn enclosing_declaration(&self) -> Gc<Node> {
        (*self.enclosing_declaration).borrow().clone().unwrap()
    }

    pub fn set_enclosing_declaration(&self, enclosing_declaration: Option<Gc<Node>>) {
        *self.enclosing_declaration.borrow_mut() = enclosing_declaration;
    }

    pub fn flags(&self) -> NodeBuilderFlags {
        self.flags.get()
    }

    pub fn set_flags(&self, flags: NodeBuilderFlags) {
        self.flags.set(flags);
    }

    pub fn tracker(&self) -> Gc<Box<dyn SymbolTracker>> {
        self.tracker.borrow().clone()
    }

    pub fn set_tracker(&self, tracker: Gc<Box<dyn SymbolTracker>>) {
        *self.tracker.borrow_mut() = tracker;
    }

    pub fn encountered_error(&self) -> bool {
        self.encountered_error.get()
    }

    pub fn set_encountered_error(&self, encountered_error: bool) {
        self.encountered_error.set(encountered_error);
    }

    pub fn reported_diagnostic(&self) -> bool {
        self.reported_diagnostic.get()
    }

    pub fn set_reported_diagnostic(&self, reported_diagnostic: bool) {
        self.reported_diagnostic.set(reported_diagnostic);
    }

    pub fn increment_approximate_length_by(&self, amount: usize) {
        self.approximate_length
            .set(self.approximate_length.get() + amount);
    }

    pub fn maybe_used_symbol_names(&self) -> Ref<Option<HashSet<String>>> {
        (*self.used_symbol_names).borrow()
    }

    pub fn maybe_used_symbol_names_mut(&self) -> RefMut<Option<HashSet<String>>> {
        (*self.used_symbol_names).borrow_mut()
    }

    pub fn remapped_symbol_names(&self) -> Ref<HashMap<SymbolId, String>> {
        ref_unwrapped(&*self.remapped_symbol_names)
    }

    pub fn remapped_symbol_names_mut(&self) -> RefMut<HashMap<SymbolId, String>> {
        ref_mut_unwrapped(&*self.remapped_symbol_names)
    }
}

impl Clone for NodeBuilderContext {
    fn clone(&self) -> Self {
        Self {
            _rc_wrapper: Default::default(),
            enclosing_declaration: self.enclosing_declaration.clone(),
            flags: self.flags.clone(),
            tracker: self.tracker.clone(),
            encountered_error: self.encountered_error.clone(),
            reported_diagnostic: self.reported_diagnostic.clone(),
            visited_types: self.visited_types.clone(),
            symbol_depth: self.symbol_depth.clone(),
            infer_type_parameters: self.infer_type_parameters.clone(),
            approximate_length: self.approximate_length.clone(),
            truncating: self.truncating.clone(),
            type_parameter_symbol_list: self.type_parameter_symbol_list.clone(),
            type_parameter_names: self.type_parameter_names.clone(),
            type_parameter_names_by_text: self.type_parameter_names_by_text.clone(),
            type_parameter_names_by_text_next_name_count: self
                .type_parameter_names_by_text_next_name_count
                .clone(),
            used_symbol_names: self.used_symbol_names.clone(),
            remapped_symbol_names: self.remapped_symbol_names.clone(),
            reverse_mapped_stack: self.reverse_mapped_stack.clone(),
        }
    }
}
