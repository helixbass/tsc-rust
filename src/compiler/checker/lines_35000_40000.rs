#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::ptr;
use std::rc::Rc;

use super::{CheckMode, DeclarationSpaces, IterationTypeKind, IterationUse, UnusedKind};
use crate::{
    declaration_name_to_string, for_each, for_each_child_returns,
    get_containing_function_or_class_static_block, get_declaration_of_kind,
    get_effective_initializer, get_escaped_text_of_identifier_or_literal, get_function_flags,
    get_module_instance_state, get_name_of_declaration, has_syntactic_modifier, is_ambient_module,
    is_binding_element, is_computed_property_name, is_entity_name_expression, is_export_assignment,
    is_function_or_module_block, is_private_identifier, is_property_name_literal, is_static,
    maybe_for_each, node_is_missing, node_is_present, Debug_, Diagnostic, DiagnosticMessage,
    Diagnostics, FunctionFlags, HasTypeParametersInterface, IterationTypes, IterationTypesResolver,
    ModifierFlags, ModuleInstanceState, Node, NodeArray, NodeInterface, ReadonlyTextRange, Symbol,
    SymbolInterface, SyntaxKind, Type, TypeChecker, TypeFlags, TypeInterface,
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
            Option::<fn(&NodeArray) -> Option<Rc<Node>>>::None,
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

    pub(super) fn check_exports_on_merged_declarations(&self, node: &Node /*Declaration*/) {
        if !self.produce_diagnostics {
            return;
        }

        let mut symbol = node.maybe_local_symbol();
        if symbol.is_none() {
            symbol = self.get_symbol_of_node(node);
            let symbol = symbol.as_ref().unwrap();
            if symbol.maybe_export_symbol().is_none() {
                return;
            }
        }
        let symbol = symbol.unwrap();

        if !matches!(
            get_declaration_of_kind(&symbol, node.kind()).as_ref(),
            Some(declaration) if ptr::eq(&**declaration, node)
        ) {
            return;
        }

        let mut exported_declaration_spaces = DeclarationSpaces::None;
        let mut non_exported_declaration_spaces = DeclarationSpaces::None;
        let mut default_exported_declaration_spaces = DeclarationSpaces::None;
        for d in symbol.maybe_declarations().as_ref().unwrap() {
            let declaration_spaces = self.get_declaration_spaces(d);
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
                let declaration_spaces = self.get_declaration_spaces(d);

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
    }

    pub(super) fn get_declaration_spaces(
        &self,
        decl: &Node, /*Declaration*/
    ) -> DeclarationSpaces {
        let d = decl;
        match d.kind() {
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
                    return DeclarationSpaces::ExportValue;
                }

                let d = expression;
                let mut result = DeclarationSpaces::None;
                let target = self.resolve_alias(&self.get_symbol_of_node(&d).unwrap());
                maybe_for_each(
                    target.maybe_declarations().as_deref(),
                    |d: &Rc<Node>, _| -> Option<()> {
                        result |= self.get_declaration_spaces(d);
                        None
                    },
                );
                result
            }
            SyntaxKind::ImportEqualsDeclaration
            | SyntaxKind::NamespaceImport
            | SyntaxKind::ImportClause => {
                let mut result = DeclarationSpaces::None;
                let target = self.resolve_alias(&self.get_symbol_of_node(&d).unwrap());
                maybe_for_each(
                    target.maybe_declarations().as_deref(),
                    |d: &Rc<Node>, _| -> Option<()> {
                        result |= self.get_declaration_spaces(d);
                        None
                    },
                );
                result
            }
            SyntaxKind::VariableDeclaration
            | SyntaxKind::BindingElement
            | SyntaxKind::FunctionDeclaration
            | SyntaxKind::ImportSpecifier
            | SyntaxKind::Identifier => DeclarationSpaces::ExportValue,
            _ => Debug_.fail_bad_syntax_kind(d, None),
        }
    }

    pub(super) fn get_awaited_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        let error_node = error_node.map(|error_node| error_node.borrow().node_wrapper());
        let promised_type = self.get_promised_type_of_promise(type_, error_node.as_deref());
        promised_type.as_ref().and_then(|promised_type| {
            self.get_awaited_type_(promised_type, error_node, diagnostic_message, args)
        })
    }

    pub(super) fn get_promised_type_of_promise<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return None;
        }

        // let type_as_promise =
        unimplemented!()
    }

    pub(super) fn check_awaited_type(
        &self,
        type_: &Type,
        with_alias: bool,
        error_node: &Node,
        diagnostic_message: &DiagnosticMessage,
        args: Option<Vec<String>>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_awaited_type_instantiation(&self, type_: &Type) -> bool {
        if type_.flags().intersects(TypeFlags::Conditional) {
            unimplemented!()
        }
        false
    }

    pub(super) fn unwrap_awaited_type(&self, type_: &Type) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Union) {
            self.map_type(
                type_,
                &mut |type_| Some(self.unwrap_awaited_type(type_)),
                None,
            )
            .unwrap()
        } else if self.is_awaited_type_instantiation(type_) {
            unimplemented!()
        } else {
            type_.type_wrapper()
        }
    }

    pub(super) fn get_awaited_type_<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_awaited_type_no_alias<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        error_node: Option<TErrorNode>,
        diagnostic_message: Option<&DiagnosticMessage>,
        args: Option<Vec<String>>,
    ) -> Option<Rc<Type>> {
        if self.is_type_any(Some(type_)) {
            return Some(type_.type_wrapper());
        }

        if self.is_awaited_type_instantiation(type_) {
            return Some(type_.type_wrapper());
        }

        unimplemented!()
    }

    pub(super) fn check_async_function_return_type(
        &self,
        node: &Node,             /*FunctionLikeDeclaration | MethodSignature*/
        return_type_node: &Node, /*TypeNode*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_decorators(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_function_declaration(&self, node: &Node /*FunctionDeclaration*/) {
        if self.produce_diagnostics {
            self.check_function_or_method_declaration(node);
        }
    }

    pub(super) fn check_function_or_method_declaration(
        &self,
        node: &Node, /*FunctionDeclaration | MethodDeclaration | MethodSignature*/
    ) {
        // self.check_decorators(node);
        // self.check_signature_declaration(node);
    }

    pub(super) fn register_for_unused_identifiers_check(
        &self,
        node: &Node, /*PotentiallyUnusedIdentifier*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_unused_identifiers<
        TAddDiagnostic: FnMut(&Node, UnusedKind, Rc<Diagnostic>),
    >(
        &self,
        potentially_unused_identifiers: &[Rc<Node /*PotentiallyUnusedIdentifier*/>],
        add_diagnostic: TAddDiagnostic, /*AddUnusedDiagnostic*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_block(&self, node: &Node /*Block*/) {
        let node_as_block = node.as_block();
        if is_function_or_module_block(node) {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        } else {
            for_each(&node_as_block.statements, |statement, _| {
                self.check_source_element(Some(statement.clone()));
                Option::<()>::None
            });
        }
    }

    pub(super) fn check_collision_with_arguments_in_generated_code(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) {
        unimplemented!()
    }

    pub(super) fn check_collisions_for_declaration_name<TName: Borrow<Node>>(
        &self,
        node: &Node,
        name: Option<TName /*Identifier*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn convert_auto_to_any(&self, type_: &Type) -> Rc<Type> {
        type_.type_wrapper()
    }

    pub(super) fn check_variable_like_declaration(&self, node: &Node) {
        let node_as_variable_like_declaration = node.as_variable_like_declaration();
        if !is_binding_element(node) {
            self.check_source_element(node_as_variable_like_declaration.maybe_type());
        }

        let symbol = self.get_symbol_of_node(node).unwrap();

        let type_ = self.convert_auto_to_any(&self.get_type_of_symbol(&*symbol));
        let value_declaration = symbol.maybe_value_declaration();
        if value_declaration.is_some() && ptr::eq(node, &*value_declaration.unwrap()) {
            let initializer = get_effective_initializer(node);
            if let Some(initializer) = initializer {
                if true {
                    let initializer_type = self.check_expression_cached(&initializer, None);
                    self.check_type_assignable_to_and_optionally_elaborate(
                        &initializer_type,
                        &type_,
                        Some(node),
                        Some(&*initializer),
                        None,
                        None,
                    );
                }
            }
        } else {
            unimplemented!()
        }
    }

    pub(super) fn error_next_variable_or_property_declaration_must_have_same_type<
        TFirstDeclaration: Borrow<Node>,
    >(
        &self,
        first_declaration: Option<TFirstDeclaration /*Declaration*/>,
        first_type: &Type,
        next_declaration: &Node, /*Declaration*/
        next_type: &Type,
    ) {
        unimplemented!()
    }

    pub(super) fn check_variable_declaration(&self, node: &Node /*VariableDeclaration*/) {
        self.check_variable_like_declaration(node);
    }

    pub(super) fn check_variable_statement(&self, node: &Node /*VariableStatement*/) {
        for_each(
            &node
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations,
            |declaration, _| Some(self.check_source_element(Some(&**declaration))),
        );
    }

    pub(super) fn check_expression_statement(&self, node: &Node /*ExpressionStatement*/) {
        let expression = &node.as_expression_statement().expression;
        self.check_expression(expression, None, None);
    }

    pub(super) fn check_if_statement(&self, node: &Node /*IfStatement*/) {
        let node_as_if_statement = node.as_if_statement();
        let type_ = self.check_truthiness_expression(&node_as_if_statement.expression, None);
        self.check_source_element(Some(&*node_as_if_statement.then_statement));

        if node_as_if_statement.then_statement.kind() == SyntaxKind::EmptyStatement {
            self.error(
                Some(&*node_as_if_statement.then_statement),
                &Diagnostics::The_body_of_an_if_statement_cannot_be_the_empty_statement,
                None,
            );
        }

        self.check_source_element(node_as_if_statement.else_statement.clone());
    }

    pub(super) fn check_testing_known_truthy_callable_or_awaitable_type<TBody: Borrow<Node>>(
        &self,
        cond_expr: &Node, /*Expression*/
        type_: &Type,
        body: Option<TBody /*Statement | Expression*/>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_truthiness_of_type(&self, type_: &Type, node: &Node) -> Rc<Type> {
        if type_.flags().intersects(TypeFlags::Void) {
            self.error(
                Some(node),
                &Diagnostics::An_expression_of_type_void_cannot_be_tested_for_truthiness,
                None,
            );
        }

        type_.type_wrapper()
    }

    pub(super) fn check_truthiness_expression(
        &self,
        node: &Node, /*Expression*/
        check_mode: Option<CheckMode>,
    ) -> Rc<Type> {
        self.check_truthiness_of_type(&self.check_expression(node, check_mode, None), node)
    }

    pub(super) fn check_right_hand_side_of_for_of(
        &self,
        statement: &Node, /*ForOfStatement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iterated_type_or_element_type<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        input_type: &Type,
        sent_type: &Type,
        error_node: Option<TErrorNode>,
        check_assignability: bool,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        use_: IterationUse,
        type_kind: IterationTypeKind,
        input_type: &Type,
        error_node: Option<TErrorNode>,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn create_iteration_types(
        &self,
        yield_type: Option<Rc<Type>>,
        return_type: Option<Rc<Type>>,
        next_type: Option<Rc<Type>>,
    ) -> IterationTypes {
        let yield_type = yield_type.unwrap_or_else(|| self.never_type());
        let return_type = return_type.unwrap_or_else(|| self.never_type());
        let next_type = next_type.unwrap_or_else(|| self.unknown_type());
        IterationTypes::new(yield_type, return_type, next_type)
    }

    pub(super) fn get_iteration_types_of_iterable<TErrorNode: Borrow<Node>>(
        &self,
        type_: &Type,
        use_: IterationUse,
        error_node: Option<TErrorNode>,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_global_iterable_type(
        &self,
        global_type: &Type,
        resolver: &IterationTypesResolver,
    ) -> IterationTypes {
        unimplemented!()
    }

    pub(super) fn get_iteration_type_of_generator_function_return_type(
        &self,
        kind: IterationTypeKind,
        return_type: &Type,
        is_async_generator: bool,
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_iteration_types_of_generator_function_return_type(
        &self,
        type_: &Type,
        is_async_generator: bool,
    ) -> Option<IterationTypes> {
        unimplemented!()
    }

    pub(super) fn unwrap_return_type(
        &self,
        return_type: &Type,
        function_flags: FunctionFlags,
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn is_unwrapped_return_type_void_or_any(
        &self,
        func: &Node, /*SignatureDeclaration*/
        return_type: &Type,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_return_statement(&self, node: &Node /*ReturnStatement*/) {
        let container = get_containing_function_or_class_static_block(node);

        if container.is_none() {
            unimplemented!()
        }
        let container = container.unwrap();

        let signature = self.get_signature_from_declaration_(&container);
        let return_type = self.get_return_type_of_signature(signature);
        let function_flags = get_function_flags(Some(&*container));
        let node_as_return_statement = node.as_return_statement();
        if self.strict_null_checks
            || node_as_return_statement.expression.is_some()
            || return_type.flags().intersects(TypeFlags::Never)
        {
            let expr_type = match node_as_return_statement.expression.as_ref() {
                Some(expression) => self.check_expression_cached(&expression, None),
                None => self.undefined_type(),
            };
            if false {
                unimplemented!()
            } else if self.get_return_type_from_annotation(&container).is_some() {
                let unwrapped_return_type = self
                    .unwrap_return_type(&return_type, function_flags)/*.unwrap_or(return_type)*/;
                let unwrapped_expr_type = if function_flags.intersects(FunctionFlags::Async) {
                    self.check_awaited_type(&expr_type, false, node, &Diagnostics::The_return_type_of_an_async_function_must_either_be_a_valid_promise_or_must_not_contain_a_callable_then_member, None)
                } else {
                    expr_type
                };
                // if unwrappedReturnType {
                self.check_type_assignable_to_and_optionally_elaborate(
                    &unwrapped_expr_type,
                    &unwrapped_return_type,
                    Some(node),
                    node_as_return_statement.expression.clone(),
                    None,
                    None,
                );
                // }
            }
        }
    }

    pub(super) fn check_index_constraints(
        &self,
        type_: &Type,
        symbol: &Symbol,
        is_static_index: Option<bool>,
    ) {
        unimplemented!()
    }

    pub(super) fn check_type_name_is_reserved(
        &self,
        name: &Node, /*Identifier*/
        message: &'static DiagnosticMessage,
    ) {
        unimplemented!()
    }

    pub(super) fn check_type_parameters(
        &self,
        type_parameter_declarations: Option<&[Rc<Node /*TypeParameterDeclaration*/>]>,
    ) {
        if let Some(type_parameter_declarations) = type_parameter_declarations {
            for node in type_parameter_declarations {
                self.check_type_parameter(&node);
            }
        }
    }

    pub(super) fn check_class_expression(&self, node: &Node /*ClassExpression*/) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_target_symbol(&self, s: &Symbol) -> Rc<Symbol> {
        unimplemented!()
    }

    pub(super) fn is_property_without_initializer(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_property_initialized_in_static_blocks(
        &self,
        prop_name: &Node, /*Identifier | PrivateIdentifier*/
        prop_type: &Type,
        static_blocks: &[Rc<Node /*ClassStaticBlockDeclaration*/>],
        start_pos: isize,
        end_pos: isize,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_interface_declaration(&self, node: &Node /*InterfaceDeclaration*/) {
        let node_as_interface_declaration = node.as_interface_declaration();
        self.check_type_parameters(
            node_as_interface_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        for_each(&node_as_interface_declaration.members, |member, _| {
            self.check_source_element(Some(&**member));
            Option::<()>::None
        });
    }

    pub(super) fn check_type_alias_declaration(&self, node: &Node /*TypeAliasDeclaration*/) {
        let node_as_type_alias_declaration = node.as_type_alias_declaration();
        self.check_type_parameters(
            node_as_type_alias_declaration
                .maybe_type_parameters()
                .as_deref(),
        );
        if false {
            unimplemented!()
        } else {
            self.check_source_element(Some(&*node_as_type_alias_declaration.type_));
        }
    }
}
