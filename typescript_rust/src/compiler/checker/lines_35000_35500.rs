use std::{borrow::Borrow, io, ptr};

use gc::Gc;
use id_arena::Id;

use super::{DeclarationSpaces, TypeFacts};
use crate::{
    declaration_name_to_string, for_each_child_returns, get_declaration_of_kind,
    get_escaped_text_of_identifier_or_literal, get_module_instance_state, get_name_of_declaration,
    has_syntactic_modifier, is_ambient_module, is_computed_property_name,
    is_entity_name_expression, is_export_assignment, is_private_identifier,
    is_property_name_literal, is_static, node_is_missing, node_is_present, return_ok_none_if_none,
    try_map, try_maybe_for_each, Debug_, DiagnosticMessage, Diagnostics, HasArena, InArena,
    ModifierFlags, ModuleInstanceState, Node, NodeArray, NodeInterface, OptionTry,
    ReadonlyTextRange, Signature, SignatureKind, Symbol, SymbolInterface, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeInterface, UnionReduction,
    OptionInArena,
};

impl TypeChecker {
    pub(super) fn report_implementation_expected_error(
        &self,
        is_constructor: bool,
        node: Id<Node>, /*SignatureDeclaration*/
    ) {
        let node_ref = node.ref_(self);
        let node_as_signature_declaration = node_ref.as_signature_declaration();
        if matches!(
            node_as_signature_declaration.maybe_name(),
            Some(node_name) if node_is_missing(Some(&node_name.ref_(self)))
        ) {
            return;
        }

        let mut seen = false;
        let subsequent_node = for_each_child_returns(
            node.ref_(self).parent(),
            |c: Id<Node>| {
                if seen {
                    Some(c)
                } else {
                    seen = c == node;
                    None
                }
            },
            Option::<fn(&NodeArray) -> Option<Id<Node>>>::None,
            self,
        );
        if let Some(subsequent_node) = subsequent_node
            .filter(|subsequent_node| subsequent_node.ref_(self).pos() == node.ref_(self).end())
        {
            if subsequent_node.ref_(self).kind() == node.ref_(self).kind() {
                let error_node = subsequent_node
                    .ref_(self).as_named_declaration()
                    .maybe_name()
                    .unwrap_or(subsequent_node);
                let subsequent_name = subsequent_node.ref_(self).as_named_declaration().maybe_name();
                if let (Some(node_name), Some(subsequent_name)) = (
                    node.ref_(self).as_named_declaration().maybe_name(),
                    subsequent_name,
                ) {
                    if is_private_identifier(&node_name.ref_(self))
                        && is_private_identifier(&subsequent_name.ref_(self))
                        && node_name.ref_(self).as_private_identifier().escaped_text
                            == subsequent_name.ref_(self).as_private_identifier().escaped_text
                        || is_computed_property_name(&node_name.ref_(self))
                            && is_computed_property_name(&subsequent_name.ref_(self))
                        || is_property_name_literal(&node_name.ref_(self))
                            && is_property_name_literal(&subsequent_name.ref_(self))
                            && get_escaped_text_of_identifier_or_literal(&node_name.ref_(self))
                                == get_escaped_text_of_identifier_or_literal(&subsequent_name.ref_(self))
                    {
                        let report_error = matches!(
                            node.ref_(self).kind(),
                            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature
                        ) && is_static(node, self) != is_static(subsequent_node, self);
                        if report_error {
                            let diagnostic = if is_static(node, self) {
                                &*Diagnostics::Function_overload_must_be_static
                            } else {
                                &*Diagnostics::Function_overload_must_not_be_static
                            };
                            self.error(Some(error_node), diagnostic, None);
                        }
                        return;
                    }
                }
                if node_is_present(subsequent_node.ref_(self).as_function_like_declaration().maybe_body().refed(self).as_deref()) {
                    self.error(
                        Some(error_node),
                        &Diagnostics::Function_implementation_name_must_be_0,
                        Some(vec![declaration_name_to_string(
                            node.ref_(self).as_named_declaration().maybe_name(),
                            self,
                        )
                        .into_owned()]),
                    );
                    return;
                }
            }
        }
        let error_node = node
            .ref_(self).as_signature_declaration()
            .maybe_name()
            .unwrap_or(node);
        if is_constructor {
            self.error(
                Some(error_node),
                &Diagnostics::Constructor_implementation_is_missing,
                None,
            );
        } else {
            if has_syntactic_modifier(node, ModifierFlags::Abstract, self) {
                self.error(
                    Some(error_node),
                    &Diagnostics::All_declarations_of_an_abstract_method_must_be_consecutive,
                    None,
                );
            } else {
                self.error(
                    Some(error_node),
                    &Diagnostics::Function_implementation_is_missing_or_not_immediately_following_the_declaration,
                    None,
                );
            }
        }
    }

    pub(super) fn check_exports_on_merged_declarations(
        &self,
        node: Id<Node>, /*Declaration*/
    ) -> io::Result<()> {
        if !self.produce_diagnostics {
            return Ok(());
        }

        let mut symbol = node.ref_(self).maybe_local_symbol();
        if symbol.is_none() {
            symbol = self.get_symbol_of_node(node)?;
            let symbol = symbol.unwrap();
            if symbol.ref_(self).maybe_export_symbol().is_none() {
                return Ok(());
            }
        }
        let symbol = symbol.unwrap();

        if !matches!(
            get_declaration_of_kind(symbol, node.ref_(self).kind(), self),
            Some(declaration) if declaration == node
        ) {
            return Ok(());
        }

        let mut exported_declaration_spaces = DeclarationSpaces::None;
        let mut non_exported_declaration_spaces = DeclarationSpaces::None;
        let mut default_exported_declaration_spaces = DeclarationSpaces::None;
        for &d in symbol.ref_(self).maybe_declarations().as_ref().unwrap() {
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
            for &d in symbol.ref_(self).maybe_declarations().as_ref().unwrap() {
                let declaration_spaces = self.get_declaration_spaces(d)?;

                let name = get_name_of_declaration(Some(d), self);
                if declaration_spaces
                    .intersects(common_declaration_spaces_for_default_and_non_default)
                {
                    self.error(
                        name,
                        &Diagnostics::Merged_declaration_0_cannot_include_a_default_export_declaration_Consider_adding_a_separate_export_default_0_declaration_instead,
                        Some(vec![
                            declaration_name_to_string(name, self).into_owned()
                        ])
                    );
                } else if declaration_spaces
                    .intersects(common_declaration_spaces_for_exports_and_locals)
                {
                    self.error(
                        name,
                        &Diagnostics::Individual_declarations_in_merged_declaration_0_must_be_all_exported_or_all_local,
                        Some(vec![
                            declaration_name_to_string(name, self).into_owned()
                        ])
                    );
                }
            }
        }

        Ok(())
    }

    pub(super) fn get_declaration_spaces(
        &self,
        decl: Id<Node>, /*Declaration*/
    ) -> io::Result<DeclarationSpaces> {
        let d = decl;
        Ok(match d.ref_(self).kind() {
            SyntaxKind::InterfaceDeclaration
            | SyntaxKind::TypeAliasDeclaration
            | SyntaxKind::JSDocTypedefTag
            | SyntaxKind::JSDocCallbackTag
            | SyntaxKind::JSDocEnumTag => DeclarationSpaces::ExportType,
            SyntaxKind::ModuleDeclaration => {
                if is_ambient_module(d, self)
                    || get_module_instance_state(d, None, self) != ModuleInstanceState::NonInstantiated
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
                let expression = if is_export_assignment(&node.ref_(self)) {
                    node.ref_(self).as_export_assignment().expression
                } else {
                    node.ref_(self).as_binary_expression().right
                };
                if !is_entity_name_expression(expression, self) {
                    return Ok(DeclarationSpaces::ExportValue);
                }

                let d = expression;
                let mut result = DeclarationSpaces::None;
                let target = self.resolve_alias(self.get_symbol_of_node(d)?.unwrap())?;
                try_maybe_for_each(
                    target.ref_(self).maybe_declarations().as_deref(),
                    |&d: &Id<Node>, _| -> io::Result<Option<()>> {
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
                let target = self.resolve_alias(self.get_symbol_of_node(d)?.unwrap())?;
                try_maybe_for_each(
                    target.ref_(self).maybe_declarations().as_deref(),
                    |&d: &Id<Node>, _| -> io::Result<Option<()>> {
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
            _ => Debug_.fail_bad_syntax_kind(&d.ref_(self), None),
        })
    }

    pub(super) fn get_awaited_type_of_promise(
        &self,
        type_: Id<Type>,
        error_node: Option<Id<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Id<Type>>> {
        let promised_type = self.get_promised_type_of_promise(type_, error_node)?;
        promised_type.try_and_then(|promised_type| {
            self.get_awaited_type_(promised_type, error_node, diagnostic_message, args)
        })
    }

    pub(super) fn get_promised_type_of_promise(
        &self,
        type_: Id<Type>,
        error_node: Option<Id<Node>>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(None);
        }

        let type_as_promise = type_;
        if let Some(type_as_promise_promised_type_of_promise) =
            *type_as_promise.ref_(self).maybe_promised_type_of_promise()
        {
            return Ok(Some(type_as_promise_promised_type_of_promise.clone()));
        }

        if self.is_reference_to_type(type_, self.get_global_promise_type(false)?) {
            let ret = self.get_type_arguments(type_)?[0].clone();
            *type_as_promise.ref_(self).maybe_promised_type_of_promise() = Some(ret.clone());
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
        if self.is_type_any(then_function) {
            return Ok(None);
        }

        let then_signatures = if let Some(then_function) = then_function {
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
            self.get_union_type(
                &try_map(&then_signatures, |then_signature: &Id<Signature>, _| {
                    self.get_type_of_first_parameter_of_signature(then_signature)
                })?,
                None,
                Option::<Id<Symbol>>::None,
                None,
                None,
            )?,
            TypeFacts::NEUndefinedOrNull,
        )?;
        if self.is_type_any(Some(onfulfilled_parameter_type)) {
            return Ok(None);
        }

        let onfulfilled_parameter_signatures =
            self.get_signatures_of_type(onfulfilled_parameter_type, SignatureKind::Call)?;
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
                |signature: &Id<Signature>, _| {
                    self.get_type_of_first_parameter_of_signature(signature)
                },
            )?,
            Some(UnionReduction::Subtype),
            Option::<Id<Symbol>>::None,
            None,
            None,
        )?;
        *type_as_promise.ref_(self).maybe_promised_type_of_promise() = Some(ret.clone());
        Ok(Some(ret))
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: Id<Type>,
        with_alias: bool,
        error_node: Id<Node>,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> io::Result<Id<Type>> {
        let awaited_type = if with_alias {
            self.get_awaited_type_(type_, Some(error_node), Some(diagnostic_message), args)?
        } else {
            self.get_awaited_type_no_alias(type_, Some(error_node), Some(diagnostic_message), args)?
        };
        Ok(awaited_type.unwrap_or_else(|| self.error_type()))
    }

    pub(super) fn is_thenable_type(&self, type_: Id<Type>) -> io::Result<bool> {
        if self.all_types_assignable_to_kind(
            type_,
            TypeFlags::Primitive | TypeFlags::Never,
            None,
        )? {
            return Ok(false);
        }

        let then_function = self.get_type_of_property_of_type_(type_, "then")?;
        Ok(matches!(
            then_function,
            Some(then_function) if !self.get_signatures_of_type(
                self.get_type_with_facts(then_function, TypeFacts::NEUndefinedOrNull)?,
                SignatureKind::Call
            )?.is_empty()
        ))
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: Id<Type>) -> io::Result<bool> {
        if type_.ref_(self).flags().intersects(TypeFlags::Conditional) {
            let awaited_symbol = self.get_global_awaited_symbol(false)?;
            return Ok(matches!(
                awaited_symbol,
                Some(awaited_symbol) if matches!(
                    type_.ref_(self).maybe_alias_symbol(),
                    Some(type_alias_symbol) if type_alias_symbol == awaited_symbol
                )
            ) && matches!(
                type_.ref_(self).maybe_alias_type_arguments().as_ref(),
                Some(type_alias_type_arguments) if type_alias_type_arguments.len() == 1
            ));
        }
        Ok(false)
    }

    pub(super) fn unwrap_awaited_type(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        Ok(if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            self.try_map_type(
                type_,
                &mut |type_| Ok(Some(self.unwrap_awaited_type(type_)?)),
                None,
            )?
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_)? {
            type_
                .ref_(self)
                .maybe_alias_type_arguments()
                .as_ref()
                .unwrap()[0]
                .clone()
        } else {
            type_
        })
    }

    pub(super) fn create_awaited_type_if_needed(&self, type_: Id<Type>) -> io::Result<Id<Type>> {
        if self.is_type_any(Some(type_)) {
            return Ok(type_);
        }

        if self.is_awaited_type_instantiation(type_)? {
            return Ok(type_);
        }

        if self.is_generic_object_type(type_)? {
            let base_constraint = self.get_base_constraint_of_type(type_)?;
            if match base_constraint {
                None => true,
                Some(base_constraint) => {
                    base_constraint
                        .ref_(self)
                        .flags()
                        .intersects(TypeFlags::AnyOrUnknown)
                        || self.is_empty_object_type(base_constraint)?
                        || self.is_thenable_type(base_constraint)?
                }
            } {
                let awaited_symbol = self.get_global_awaited_symbol(true)?;
                if let Some(awaited_symbol) = awaited_symbol {
                    return self.get_type_alias_instantiation(
                        awaited_symbol,
                        Some(&[self.unwrap_awaited_type(type_)?]),
                        Option::<Id<Symbol>>::None,
                        None,
                    );
                }
            }
        }

        Debug_.assert(
            self.get_promised_type_of_promise(type_, Option::<Id<Node>>::None)?
                .is_none(),
            Some("type provided should not be a non-generic 'promise'-like."),
        );
        Ok(type_)
    }

    pub(super) fn get_awaited_type_(
        &self,
        type_: Id<Type>,
        error_node: Option<Id<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Id<Type>>> {
        let awaited_type =
            self.get_awaited_type_no_alias(type_, error_node, diagnostic_message, args)?;
        awaited_type.try_map(|awaited_type| self.create_awaited_type_if_needed(awaited_type))
    }

    pub(super) fn get_awaited_type_no_alias(
        &self,
        type_: Id<Type>,
        error_node: Option<Id<Node>>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> io::Result<Option<Id<Type>>> {
        if self.is_type_any(Some(type_)) {
            return Ok(Some(type_));
        }

        if self.is_awaited_type_instantiation(type_)? {
            return Ok(Some(type_));
        }

        let type_as_awaitable = type_;
        if let Some(type_as_awaitable_awaited_type_of_type) =
            *type_as_awaitable.ref_(self).maybe_awaited_type_of_type()
        {
            return Ok(Some(type_as_awaitable_awaited_type_of_type.clone()));
        }

        if type_.ref_(self).flags().intersects(TypeFlags::Union) {
            let mut mapper = |constituent_type: Id<Type>| {
                if error_node.is_some() {
                    self.get_awaited_type_no_alias(
                        constituent_type,
                        error_node,
                        diagnostic_message,
                        args.clone(),
                    )
                } else {
                    self.get_awaited_type_no_alias(
                        constituent_type,
                        Option::<Id<Node>>::None,
                        None,
                        None,
                    )
                }
            };
            let ret = self.try_map_type(type_, &mut mapper, None)?;
            *type_as_awaitable.ref_(self).maybe_awaited_type_of_type() = ret.clone();
            return Ok(ret);
        }

        let promised_type = self.get_promised_type_of_promise(type_, Option::<Id<Node>>::None)?;
        if let Some(promised_type) = promised_type {
            if type_.ref_(self).id() == promised_type.ref_(self).id()
                || self
                    .awaited_type_stack()
                    .iter()
                    .rev()
                    .position(|awaited_type_id| *awaited_type_id == promised_type.ref_(self).id())
                    .is_some()
            {
                if error_node.is_some() {
                    self.error(
                        error_node,
                        &Diagnostics::Type_is_referenced_directly_or_indirectly_in_the_fulfillment_callback_of_its_own_then_method,
                        None,
                    );
                }
                return Ok(None);
            }

            self.awaited_type_stack().push(type_.ref_(self).id());
            let awaited_type = self.get_awaited_type_no_alias(
                promised_type,
                error_node,
                diagnostic_message,
                args.clone(),
            )?;
            self.awaited_type_stack().pop();

            let awaited_type = return_ok_none_if_none!(awaited_type);

            *type_as_awaitable.ref_(self).maybe_awaited_type_of_type() = Some(awaited_type.clone());
            return Ok(Some(awaited_type));
        }

        if self.is_thenable_type(type_)? {
            if error_node.is_some() {
                Debug_.assert_is_defined(&diagnostic_message, None);
                self.error(error_node, diagnostic_message.unwrap(), args);
                return Ok(None);
            }
        }

        let ret = type_;
        *type_as_awaitable.ref_(self).maybe_awaited_type_of_type() = Some(ret.clone());
        Ok(Some(ret))
    }
}
