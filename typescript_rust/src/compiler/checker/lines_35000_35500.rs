use gc::Gc;
use std::ptr;
use std::rc::Rc;
use std::{borrow::Borrow, io};

use super::{DeclarationSpaces, TypeFacts};
use crate::{
    map, Signature, SignatureKind, UnionReduction, __String, declaration_name_to_string,
    for_each_child_returns, get_declaration_of_kind, get_escaped_text_of_identifier_or_literal,
    get_module_instance_state, get_name_of_declaration, has_syntactic_modifier, is_ambient_module,
    is_computed_property_name, is_entity_name_expression, is_export_assignment,
    is_private_identifier, is_property_name_literal, is_static, maybe_for_each, node_is_missing,
    node_is_present, return_ok_none_if_none, try_map, try_maybe_for_each, Debug_,
    DiagnosticMessage, Diagnostics, ModifierFlags, ModuleInstanceState, Node, NodeArray,
    NodeInterface, OptionTry, ReadonlyTextRange, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface,
};

impl TypeChecker {
    pub(super) fn report_implementation_expected_error(
        &self,
        is_constructor: bool,
        node: &Node, /*SignatureDeclaration*/
    ) {
        let node_as_signature_declaration = node.as_signature_declaration();
        if matches!(
            node_as_signature_declaration.maybe_name().as_ref(),
            Some(node_name) if node_is_missing(Some(&**node_name))
        ) {
            return;
        }

        let mut seen = false;
        let subsequent_node = for_each_child_returns(
            &node.parent(),
            |c: &Node| {
                if seen {
                    Some(c.node_wrapper())
                } else {
                    seen = ptr::eq(c, node);
                    None
                }
            },
            Option::<fn(&NodeArray) -> Option<Gc<Node>>>::None,
        );
        if let Some(subsequent_node) = subsequent_node
            .as_ref()
            .filter(|subsequent_node| subsequent_node.pos() == node.end())
        {
            if subsequent_node.kind() == node.kind() {
                let error_node = subsequent_node
                    .as_named_declaration()
                    .maybe_name()
                    .unwrap_or_else(|| subsequent_node.clone());
                let subsequent_name = subsequent_node.as_named_declaration().maybe_name();
                if let (Some(node_name), Some(subsequent_name)) = (
                    node.as_named_declaration().maybe_name().as_ref(),
                    subsequent_name.as_ref(),
                ) {
                    if is_private_identifier(node_name)
                        && is_private_identifier(subsequent_name)
                        && node_name.as_private_identifier().escaped_text
                            == subsequent_name.as_private_identifier().escaped_text
                        || is_computed_property_name(node_name)
                            && is_computed_property_name(subsequent_name)
                        || is_property_name_literal(node_name)
                            && is_property_name_literal(subsequent_name)
                            && get_escaped_text_of_identifier_or_literal(node_name)
                                == get_escaped_text_of_identifier_or_literal(subsequent_name)
                    {
                        let report_error = matches!(
                            node.kind(),
                            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature
                        ) && is_static(node) != is_static(subsequent_node);
                        if report_error {
                            let diagnostic = if is_static(node) {
                                &*Diagnostics::Function_overload_must_be_static
                            } else {
                                &*Diagnostics::Function_overload_must_not_be_static
                            };
                            self.error(Some(&*error_node), diagnostic, None);
                        }
                        return;
                    }
                }
                if node_is_present(subsequent_node.as_function_like_declaration().maybe_body()) {
                    self.error(
                        Some(&*error_node),
                        &Diagnostics::Function_implementation_name_must_be_0,
                        Some(vec![declaration_name_to_string(
                            node.as_named_declaration().maybe_name(),
                        )
                        .into_owned()]),
                    );
                    return;
                }
            }
        }
        let error_node = node
            .as_signature_declaration()
            .maybe_name()
            .unwrap_or_else(|| node.node_wrapper());
        if is_constructor {
            self.error(
                Some(&*error_node),
                &Diagnostics::Constructor_implementation_is_missing,
                None,
            );
        } else {
            if has_syntactic_modifier(node, ModifierFlags::Abstract) {
                self.error(
                    Some(&*error_node),
                    &Diagnostics::All_declarations_of_an_abstract_method_must_be_consecutive,
                    None,
                );
            } else {
                self.error(
                    Some(&*error_node),
                    &Diagnostics::Function_implementation_is_missing_or_not_immediately_following_the_declaration,
                    None,
                );
            }
        }
    }

    pub(super) fn check_exports_on_merged_declarations(
        &self,
        node: &Node, /*Declaration*/
    ) -> io::Result<()> {
        if !self.produce_diagnostics {
            return Ok(());
        }

        let mut symbol = node.maybe_local_symbol();
        if symbol.is_none() {
            symbol = self.get_symbol_of_node(node)?;
            let symbol = symbol.as_ref().unwrap();
            if symbol.maybe_export_symbol().is_none() {
                return Ok(());
            }
        }
        let symbol = symbol.unwrap();

        if !matches!(
            get_declaration_of_kind(&symbol, node.kind()).as_ref(),
            Some(declaration) if ptr::eq(&**declaration, node)
        ) {
            return Ok(());
        }

        let mut exported_declaration_spaces = DeclarationSpaces::None;
        let mut non_exported_declaration_spaces = DeclarationSpaces::None;
        let mut default_exported_declaration_spaces = DeclarationSpaces::None;
        for d in symbol.maybe_declarations().as_ref().unwrap() {
            let declaration_spaces = self.get_declaration_spaces(d)?;
            let effective_declaration_flags = self
                .get_effective_declaration_flags(d, ModifierFlags::Export | ModifierFlags::Default);

            if effective_declaration_flags.intersects(ModifierFlags::Export) {
                if effective_declaration_flags.intersects(ModifierFlags::Default) {
                    default_exported_declaration_spaces |= declaration_spaces;
                } else {
                    exported_declaration_spaces |= declaration_spaces;
                }
            } else {
                non_exported_declaration_spaces |= declaration_spaces;
            }
        }

        let non_default_exported_declaration_spaces =
            exported_declaration_spaces | non_exported_declaration_spaces;

        let common_declaration_spaces_for_exports_and_locals =
            exported_declaration_spaces & non_exported_declaration_spaces;
        let common_declaration_spaces_for_default_and_non_default =
            default_exported_declaration_spaces & non_default_exported_declaration_spaces;

        if common_declaration_spaces_for_exports_and_locals != DeclarationSpaces::None
            || common_declaration_spaces_for_default_and_non_default != DeclarationSpaces::None
        {
            for d in symbol.maybe_declarations().as_ref().unwrap() {
                let declaration_spaces = self.get_declaration_spaces(d)?;

                let name = get_name_of_declaration(Some(&**d));
                if declaration_spaces
                    .intersects(common_declaration_spaces_for_default_and_non_default)
                {
                    self.error(
                        name.as_deref(),
                        &Diagnostics::Merged_declaration_0_cannot_include_a_default_export_declaration_Consider_adding_a_separate_export_default_0_declaration_instead,
                        Some(vec![
                            declaration_name_to_string(name.as_deref()).into_owned()
                        ])
                    );
                } else if declaration_spaces
                    .intersects(common_declaration_spaces_for_exports_and_locals)
                {
                    self.error(
                        name.as_deref(),
                        &Diagnostics::Individual_declarations_in_merged_declaration_0_must_be_all_exported_or_all_local,
                        Some(vec![
                            declaration_name_to_string(name.as_deref()).into_owned()
                        ])
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_declaration_spaces(
        &self,
        decl: &Node, /*Declaration*/
    ) -> io::Result<DeclarationSpaces> {
        let d = decl;
        Ok(match d.kind() {
            SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => DeclarationSpaces::ExportType,
            SyntaxKind::ModuleDeclaration => {
                if is_ambient_module(d)
                    || get_module_instance_state(d, None) != ModuleInstanceState::NonInstantiated
                {
                    DeclarationSpaces::ExportNamespace | DeclarationSpaces::ExportValue
                } else {
                    DeclarationSpaces::ExportNamespace
                }
            }
            SyntaxKind::ClassDeclaration | SyntaxKind::EnumDeclaration | SyntaxKind::EnumMember => {
                DeclarationSpaces::ExportType | DeclarationSpaces::ExportValue
            }
            SyntaxKind::SourceFile => {
                DeclarationSpaces::ExportType
                    | DeclarationSpaces::ExportValue
                    | DeclarationSpaces::ExportNamespace
            }
            SyntaxKind::ExportAssignment | SyntaxKind::BinaryExpression => {
                let node = d;
                let expression = if is_export_assignment(node) {
                    node.as_export_assignment().expression.clone()
                } else {
                    node.as_binary_expression().right.clone()
                };
                if !is_entity_name_expression(&expression) {
                    return Ok(DeclarationSpaces::ExportValue);
                }

                let d = expression;
                let mut result = DeclarationSpaces::None;
                let target = self.resolve_alias(&self.get_symbol_of_node(&d)?.unwrap())?;
                try_maybe_for_each(
                    target.maybe_declarations().as_deref(),
                    |d: &Gc<Node>, _| -> io::Result<Option<()>> {
                        result |= self.get_declaration_spaces(d)?;
                        Ok(None)
                    },
                )?;
                result
            }
            SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportClause => {
                let mut result = DeclarationSpaces::None;
                let target = self.resolve_alias(&self.get_symbol_of_node(&d)?.unwrap())?;
                try_maybe_for_each(
                    target.maybe_declarations().as_deref(),
                    |d: &Gc<Node>, _| -> io::Result<Option<()>> {
                        result |= self.get_declaration_spaces(d)?;
                        Ok(None)
                    },
                )?;
                result
            }
            SyntaxKind::VariableDeclaration
            | SyntaxKind::BindingElement
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::Identifier => DeclarationSpaces::ExportValue,
            _ => Debug_.fail_bad_syntax_kind(d, None),
        })
    }

    pub(super) fn get_awaited_type_of_promise(
        &self,
        type_: &Type,
        error_node: Option<impl Borrow<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Gc<Type>>> {
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        let promised_type = self.get_promised_type_of_promise(type_, error_node.as_deref())?;
        promised_type.as_ref().try_and_then(|promised_type| {
            self.get_awaited_type_(promised_type, error_node, diagnostic_message, args)
        })
    }

    pub(super) fn get_promised_type_of_promise(
        &self,
        type_: &Type,
        error_node: Option<impl Borrow<Node>>,
    ) -> io::Result<Option<Gc<Type>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(None);
        }

        let type_as_promise = type_;
        if let Some(type_as_promise_promised_type_of_promise) =
            type_as_promise.maybe_promised_type_of_promise().as_ref()
        {
            return Ok(Some(type_as_promise_promised_type_of_promise.clone()));
        }

        if self.is_reference_to_type(type_, &*self.get_global_promise_type(false)?) {
            let ret = self.get_type_arguments(type_)?[0].clone();
            *type_as_promise.maybe_promised_type_of_promise() = Some(ret.clone());
            return Ok(Some(ret));
        }

        if self.all_types_assignable_to_kind(
            type_,
            TypeFlags::Primitive | TypeFlags::Never,
            None,
        )? {
            return Ok(None);
        }

        let then_function = self.get_type_of_property_of_type_(type_, "then")?;
        if self.is_type_any(then_function.as_deref()) {
            return Ok(None);
        }

        let then_signatures = if let Some(then_function) = then_function.as_ref() {
            self.get_signatures_of_type(then_function, SignatureKind::Call)?
        } else {
            vec![]
        };
        if then_signatures.is_empty() {
            if error_node.is_some() {
                self.error(
                    error_node,
                    &Diagnostics::A_promise_must_have_a_then_method,
                    None,
                );
            }
            return Ok(None);
        }

        let onfulfilled_parameter_type = self.get_type_with_facts(
            &*self.get_union_type(
                &try_map(&then_signatures, |then_signature: &Gc<Signature>, _| {
                    self.get_type_of_first_parameter_of_signature(then_signature)
                })?,
                None,
                Option::<&Symbol>::None,
                None,
                Option::<&Type>::None,
            )?,
            TypeFacts::NEUndefinedOrNull,
        )?;
        if self.is_type_any(Some(&*onfulfilled_parameter_type)) {
            return Ok(None);
        }

        let onfulfilled_parameter_signatures =
            self.get_signatures_of_type(&onfulfilled_parameter_type, SignatureKind::Call)?;
        if onfulfilled_parameter_signatures.is_empty() {
            if error_node.is_some() {
                self.error(
                    error_node,
                    &Diagnostics::The_first_parameter_of_the_then_method_of_a_promise_must_be_a_callback,
                    None,
                );
            }
            return Ok(None);
        }

        let ret = self.get_union_type(
            &try_map(
                &onfulfilled_parameter_signatures,
                |signature: &Gc<Signature>, _| {
                    self.get_type_of_first_parameter_of_signature(signature)
                },
            )?,
            Some(UnionReduction::Subtype),
            Option::<&Symbol>::None,
            None,
            Option::<&Type>::None,
        )?;
        *type_as_promise.maybe_promised_type_of_promise() = Some(ret.clone());
        Ok(Some(ret))
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> io::Result<Gc<Type>> {
        let awaited_type = if with_alias {
            self.get_awaited_type_(type_, Some(error_node), Some(diagnostic_message), args)?
        } else {
            self.get_awaited_type_no_alias(type_, Some(error_node), Some(diagnostic_message), args)?
        };
        Ok(awaited_type.unwrap_or_else(|| self.error_type()))
    }

    pub(super) fn is_thenable_type(&self, type_: &Type) -> io::Result<bool> {
        if self.all_types_assignable_to_kind(
            type_,
            TypeFlags::Primitive | TypeFlags::Never,
            None,
        )? {
            return Ok(false);
        }

        let then_function = self.get_type_of_property_of_type_(type_, "then")?;
        Ok(matches!(
            then_function.as_ref(),
            Some(then_function) if !self.get_signatures_of_type(
                &*self.get_type_with_facts(then_function, TypeFacts::NEUndefinedOrNull)?,
                SignatureKind::Call
            )?.is_empty()
        ))
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> io::Result<bool> {
        if type_.flags().intersects(TypeFlags::Conditional) {
            let awaited_symbol = self.get_global_awaited_symbol(false)?;
            return Ok(matches!(
                awaited_symbol.as_ref(),
                Some(awaited_symbol) if matches!(
                    type_.maybe_alias_symbol().as_ref(),
                    Some(type_alias_symbol) if Gc::ptr_eq(
                        type_alias_symbol,
                        awaited_symbol
                    )
                )
            ) && matches!(
                type_.maybe_alias_type_arguments().as_ref(),
                Some(type_alias_type_arguments) if type_alias_type_arguments.len() == 1
            ));
        }
        Ok(false)
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> io::Result<Gc<Type>> {
        Ok(if type_.flags().intersects(TypeFlags::Union) {
            self.try_map_type(
                type_,
                &mut |type_| Ok(Some(self.unwrap_awaited_type(type_)?)),
                None,
            )?
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_)? {
            type_.maybe_alias_type_arguments().as_ref().unwrap()[0].clone()
        } else {
            type_.type_wrapper()
        })
    }

    pub(super) fn create_awaited_type_if_needed(&self, type_: &Type) -> io::Result<Gc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Ok(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_)? {
            return Ok(type_.type_wrapper());
        }

        if self.is_generic_object_type(type_)? {
            let base_constraint = self.get_base_constraint_of_type(type_)?;
            if match base_constraint.as_ref() {
                None => true,
                Some(base_constraint) => {
                    base_constraint.flags().intersects(TypeFlags::AnyOrUnknown)
                        || self.is_empty_object_type(base_constraint)?
                        || self.is_thenable_type(base_constraint)?
                }
            } {
                let awaited_symbol = self.get_global_awaited_symbol(true)?;
                if let Some(awaited_symbol) = awaited_symbol.as_ref() {
                    return self.get_type_alias_instantiation(
                        awaited_symbol,
                        Some(&[self.unwrap_awaited_type(type_)?]),
                        Option::<&Symbol>::None,
                        None,
                    );
                }
            }
        }

        Debug_.assert(
            self.get_promised_type_of_promise(type_, Option::<&Node>::None)?
                .is_none(),
            Some("type provided should not be a non-generic 'promise'-like."),
        );
        Ok(type_.type_wrapper())
    }

    pub(super) fn get_awaited_type_(
        &self,
        type_: &Type,
        error_node: Option<impl Borrow<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Gc<Type>>> {
        let awaited_type =
            self.get_awaited_type_no_alias(type_, error_node, diagnostic_message, args)?;
        awaited_type
            .as_ref()
            .try_map(|awaited_type| self.create_awaited_type_if_needed(awaited_type))
    }

    pub(super) fn get_awaited_type_no_alias(
        &self,
        type_: &Type,
        error_node: Option<impl Borrow<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Gc<Type>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(Some(type_.type_wrapper()));
        }

        if self.is_awaited_type_instantiation(type_)? {
            return Ok(Some(type_.type_wrapper()));
        }

        let type_as_awaitable = type_;
        if let Some(type_as_awaitable_awaited_type_of_type) =
            type_as_awaitable.maybe_awaited_type_of_type().as_ref()
        {
            return Ok(Some(type_as_awaitable_awaited_type_of_type.clone()));
        }

        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        if type_.flags().intersects(TypeFlags::Union) {
            let mut mapper = |constituent_type: &Type| {
                if error_node.is_some() {
                    self.get_awaited_type_no_alias(
                        constituent_type,
                        error_node.as_deref(),
                        diagnostic_message,
                        args.clone(),
                    )
                } else {
                    self.get_awaited_type_no_alias(
                        constituent_type,
                        Option::<&Node>::None,
                        None,
                        None,
                    )
                }
            };
            let ret = self.try_map_type(type_, &mut mapper, None)?;
            *type_as_awaitable.maybe_awaited_type_of_type() = ret.clone();
            return Ok(ret);
        }

        let promised_type = self.get_promised_type_of_promise(type_, Option::<&Node>::None)?;
        if let Some(promised_type) = promised_type.as_ref() {
            if type_.id() == promised_type.id()
                || self
                    .awaited_type_stack()
                    .iter()
                    .rev()
                    .position(|awaited_type_id| *awaited_type_id == promised_type.id())
                    .is_some()
            {
                if error_node.is_some() {
                    self.error(
                        error_node.as_deref(),
                        &Diagnostics::Type_is_referenced_directly_or_indirectly_in_the_fulfillment_callback_of_its_own_then_method,
                        None,
                    );
                }
                return Ok(None);
            }

            self.awaited_type_stack().push(type_.id());
            let awaited_type = self.get_awaited_type_no_alias(
                promised_type,
                error_node.as_deref(),
                diagnostic_message,
                args.clone(),
            )?;
            self.awaited_type_stack().pop();

            let awaited_type = return_ok_none_if_none!(awaited_type);

            *type_as_awaitable.maybe_awaited_type_of_type() = Some(awaited_type.clone());
            return Ok(Some(awaited_type));
        }

        if self.is_thenable_type(type_)? {
            if error_node.is_some() {
                Debug_.assert_is_defined(&diagnostic_message, None);
                self.error(error_node.as_deref(), diagnostic_message.unwrap(), args);
                return Ok(None);
            }
        }

        let ret = type_.type_wrapper();
        *type_as_awaitable.maybe_awaited_type_of_type() = Some(ret.clone());
        Ok(Some(ret))
    }
}
