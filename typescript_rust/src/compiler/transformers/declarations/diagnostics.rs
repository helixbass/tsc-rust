use id_arena::Id;

use crate::{
    get_name_of_declaration, has_syntactic_modifier, is_binding_element,
    is_call_signature_declaration, is_class_declaration, is_construct_signature_declaration,
    is_constructor_declaration, is_expression_with_type_arguments, is_function_declaration,
    is_get_accessor, is_heritage_clause, is_import_equals_declaration,
    is_index_signature_declaration, is_jsdoc_type_alias, is_method_declaration,
    is_method_signature, is_parameter, is_parameter_property_declaration,
    is_property_access_expression, is_property_declaration, is_property_signature, is_set_accessor,
    is_static, is_type_alias_declaration, is_type_parameter_declaration, is_variable_declaration,
    Debug_, DiagnosticMessage, Diagnostics, ModifierFlags, NamedDeclarationInterface, Node,
    NodeInterface, NonEmpty, SymbolAccessibility, SymbolAccessibilityResult, SyntaxKind, HasArena, AllArenas,
    InArena,
};

pub trait GetSymbolAccessibilityDiagnosticInterface {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>>;
}

pub type GetSymbolAccessibilityDiagnostic = Id<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>;

pub struct SymbolAccessibilityDiagnostic {
    pub error_node: Id<Node>,
    pub diagnostic_message: &'static DiagnosticMessage,
    pub type_name: Option<Id<Node /*DeclarationName | QualifiedName*/>>,
}

pub fn can_produce_diagnostics(node: &Node) -> bool {
    is_variable_declaration(node)
        || is_property_declaration(node)
        || is_property_signature(node)
        || is_binding_element(node)
        || is_set_accessor(node)
        || is_get_accessor(node)
        || is_construct_signature_declaration(node)
        || is_call_signature_declaration(node)
        || is_method_declaration(node)
        || is_method_signature(node)
        || is_function_declaration(node)
        || is_parameter(node)
        || is_type_parameter_declaration(node)
        || is_expression_with_type_arguments(node)
        || is_import_equals_declaration(node)
        || is_type_alias_declaration(node)
        || is_constructor_declaration(node)
        || is_index_signature_declaration(node)
        || is_property_access_expression(node)
        || is_jsdoc_type_alias(node)
}

pub fn create_get_symbol_accessibility_diagnostic_for_node(
    node: Id<Node> /*DeclarationDiagnosticProducing*/,
    arena: &impl HasArena
) -> GetSymbolAccessibilityDiagnostic {
    if is_variable_declaration(&node.ref_(arena))
        || is_property_declaration(&node.ref_(arena))
        || is_property_signature(&node.ref_(arena))
        || is_property_access_expression(&node.ref_(arena))
        || is_binding_element(&node.ref_(arena))
        || is_constructor_declaration(&node.ref_(arena))
    {
        GetVariableDeclarationTypeVisibilityError::new(node, arena)
    } else if is_set_accessor(&node.ref_(arena)) || is_get_accessor(&node.ref_(arena)) {
        GetAccessorDeclarationTypeVisibilityError::new(node, arena)
    } else if is_construct_signature_declaration(&node.ref_(arena))
        || is_call_signature_declaration(&node.ref_(arena))
        || is_method_declaration(&node.ref_(arena))
        || is_method_signature(&node.ref_(arena))
        || is_function_declaration(&node.ref_(arena))
        || is_index_signature_declaration(&node.ref_(arena))
    {
        GetReturnTypeVisibilityError::new(node, arena)
    } else if is_parameter(&node.ref_(arena)) {
        if is_parameter_property_declaration(node, node.ref_(arena).parent(), arena)
            && has_syntactic_modifier(node.ref_(arena).parent(), ModifierFlags::Private, arena)
        {
            return GetVariableDeclarationTypeVisibilityError::new(node, arena);
        }
        GetParameterDeclarationTypeVisibilityError::new(node, arena)
    } else if is_type_parameter_declaration(&node.ref_(arena)) {
        GetTypeParameterConstraintVisibilityError::new(node, arena)
    } else if is_expression_with_type_arguments(&node.ref_(arena)) {
        GetHeritageClauseVisibilityError::new(node, arena)
    } else if is_import_equals_declaration(&node.ref_(arena)) {
        GetImportEntityNameVisibilityError::new(node, arena)
    } else if is_type_alias_declaration(&node.ref_(arena)) || is_jsdoc_type_alias(&node.ref_(arena)) {
        GetTypeAliasDeclarationVisibilityError::new(node, arena)
    } else {
        Debug_.assert_never(
            node,
            Some(&format!(
                "Attempted to set a declaration diagnostic context for unhandled node kind: {:?}",
                node.ref_(arena).kind()
            )),
        )
    }
}

fn get_variable_declaration_type_visibility_diagnostic_message(
    node: Id<Node>,
    symbol_accessibility_result: &SymbolAccessibilityResult,
    arena: &impl HasArena,
) -> Option<&'static DiagnosticMessage> {
    if matches!(
        node.ref_(arena).kind(),
        SyntaxKind::VariableDeclaration | SyntaxKind::BindingElement
    ) {
        return Some(
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                if symbol_accessibility_result.accessibility == SymbolAccessibility::CannotBeNamed {
                    &*Diagnostics::Exported_variable_0_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                } else {
                    &*Diagnostics::Exported_variable_0_has_or_is_using_name_1_from_private_module_2
                }
            } else {
                &*Diagnostics::Exported_variable_0_has_or_is_using_private_name_1
            },
        );
    } else if matches!(
        node.ref_(arena).kind(),
        SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::PropertySignature
    ) || node.ref_(arena).kind() == SyntaxKind::Parameter
        && has_syntactic_modifier(node.ref_(arena).parent(), ModifierFlags::Private, arena)
    {
        if is_static(node, arena) {
            return Some(
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Public_static_property_0_of_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Public_static_property_0_of_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Public_static_property_0_of_exported_class_has_or_is_using_private_name_1
                },
            );
        } else if node.ref_(arena).parent().ref_(arena).kind() == SyntaxKind::ClassDeclaration
            || node.ref_(arena).kind() == SyntaxKind::Parameter
        {
            return Some(
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Public_property_0_of_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Public_property_0_of_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Public_property_0_of_exported_class_has_or_is_using_private_name_1
                },
            );
        } else {
            return Some(
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Property_0_of_exported_interface_has_or_is_using_name_1_from_private_module_2
                } else {
                    &*Diagnostics::Property_0_of_exported_interface_has_or_is_using_private_name_1
                },
            );
        }
    }
    None
}

struct GetVariableDeclarationTypeVisibilityError {
    node: Id<Node>,
}

impl GetVariableDeclarationTypeVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetVariableDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message = get_variable_declaration_type_visibility_diagnostic_message(
            self.node,
            symbol_accessibility_result,
            self,
        );
        diagnostic_message.map(|diagnostic_message| {
            self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
                diagnostic_message,
                error_node: self.node,
                type_name: self.node.ref_(self).as_named_declaration().maybe_name(),
            })
        })
    }
}

impl HasArena for GetVariableDeclarationTypeVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetAccessorDeclarationTypeVisibilityError {
    node: Id<Node>,
}

impl GetAccessorDeclarationTypeVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetAccessorDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        if self.node.ref_(self).kind() == SyntaxKind::SetAccessor {
            if is_static(self.node, self) {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Parameter_type_of_public_static_setter_0_from_exported_class_has_or_is_using_name_1_from_private_module_2
                } else {
                    &*Diagnostics::Parameter_type_of_public_static_setter_0_from_exported_class_has_or_is_using_private_name_1
                }
            } else {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Parameter_type_of_public_setter_0_from_exported_class_has_or_is_using_name_1_from_private_module_2
                } else {
                    &*Diagnostics::Parameter_type_of_public_setter_0_from_exported_class_has_or_is_using_private_name_1
                }
            }
        } else {
            if is_static(self.node, self) {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Return_type_of_public_static_getter_0_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Return_type_of_public_static_getter_0_from_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Return_type_of_public_static_getter_0_from_exported_class_has_or_is_using_private_name_1
                }
            } else {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Return_type_of_public_getter_0_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Return_type_of_public_getter_0_from_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Return_type_of_public_getter_0_from_exported_class_has_or_is_using_private_name_1
                }
            }
        }
        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node.ref_(self).as_named_declaration().name(),
            type_name: self.node.ref_(self).as_named_declaration().maybe_name(),
        }))
    }
}

impl HasArena for GetAccessorDeclarationTypeVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetReturnTypeVisibilityError {
    node: Id<Node>,
}

impl GetReturnTypeVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetReturnTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        match self.node.ref_(self).kind() {
            SyntaxKind::ConstructSignature => {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Return_type_of_constructor_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
                } else {
                    &*Diagnostics::Return_type_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_0
                }
            }
            SyntaxKind::CallSignature => {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Return_type_of_call_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
                } else {
                    &*Diagnostics::Return_type_of_call_signature_from_exported_interface_has_or_is_using_private_name_0
                }
            }
            SyntaxKind::IndexSignature => {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Return_type_of_index_signature_from_exported_interface_has_or_is_using_name_0_from_private_module_1
                } else {
                    &*Diagnostics::Return_type_of_index_signature_from_exported_interface_has_or_is_using_private_name_0
                }
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                if is_static(self.node, self) {
                    diagnostic_message = if symbol_accessibility_result
                        .error_module_name
                        .as_ref()
                        .non_empty()
                        .is_some()
                    {
                        if symbol_accessibility_result.accessibility
                            == SymbolAccessibility::CannotBeNamed
                        {
                            &*Diagnostics::Return_type_of_public_static_method_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                        } else {
                            &*Diagnostics::Return_type_of_public_static_method_from_exported_class_has_or_is_using_name_0_from_private_module_1
                        }
                    } else {
                        &*Diagnostics::Return_type_of_public_static_method_from_exported_class_has_or_is_using_private_name_0
                    };
                } else if self.node.ref_(self).parent().ref_(self).kind() == SyntaxKind::ClassDeclaration {
                    diagnostic_message = if symbol_accessibility_result
                        .error_module_name
                        .as_ref()
                        .non_empty()
                        .is_some()
                    {
                        if symbol_accessibility_result.accessibility
                            == SymbolAccessibility::CannotBeNamed
                        {
                            &*Diagnostics::Return_type_of_public_method_from_exported_class_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                        } else {
                            &*Diagnostics::Return_type_of_public_method_from_exported_class_has_or_is_using_name_0_from_private_module_1
                        }
                    } else {
                        &*Diagnostics::Return_type_of_public_method_from_exported_class_has_or_is_using_private_name_0
                    };
                } else {
                    diagnostic_message = if symbol_accessibility_result
                        .error_module_name
                        .as_ref()
                        .non_empty()
                        .is_some()
                    {
                        &*Diagnostics::Return_type_of_method_from_exported_interface_has_or_is_using_name_0_from_private_module_1
                    } else {
                        &*Diagnostics::Return_type_of_method_from_exported_interface_has_or_is_using_private_name_0
                    };
                }
            }
            SyntaxKind::FunctionDeclaration => {
                diagnostic_message = if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Return_type_of_exported_function_has_or_is_using_name_0_from_external_module_1_but_cannot_be_named
                    } else {
                        &*Diagnostics::Return_type_of_exported_function_has_or_is_using_name_0_from_private_module_1
                    }
                } else {
                    &*Diagnostics::Return_type_of_exported_function_has_or_is_using_private_name_0
                };
            }
            _ => Debug_.fail(Some(&format!(
                "This is unknown kind for signature: {:?}",
                self.node.ref_(self).kind()
            ))),
        }

        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self
                .node
                .ref_(self).as_named_declaration()
                .maybe_name()
                .unwrap_or(self.node),
            type_name: None,
        }))
    }
}

impl HasArena for GetReturnTypeVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetParameterDeclarationTypeVisibilityError {
    node: Id<Node>,
}

impl GetParameterDeclarationTypeVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetParameterDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message = get_parameter_declaration_type_visibility_diagnostic_message(
            self.node,
            symbol_accessibility_result,
            self,
        );
        /* diagnostisMessage !== undefined ?*/
        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node,
            type_name: self.node.ref_(self).as_named_declaration().maybe_name(),
        }))
        /*: undefined*/
    }
}

impl HasArena for GetParameterDeclarationTypeVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

fn get_parameter_declaration_type_visibility_diagnostic_message(
    node: Id<Node>,
    symbol_accessibility_result: &SymbolAccessibilityResult,
    arena: &impl HasArena,
) -> &'static DiagnosticMessage {
    match node.ref_(arena).parent().ref_(arena).kind() {
        SyntaxKind::Constructor => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                if symbol_accessibility_result.accessibility == SymbolAccessibility::CannotBeNamed {
                    &*Diagnostics::Parameter_0_of_constructor_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                } else {
                    &*Diagnostics::Parameter_0_of_constructor_from_exported_class_has_or_is_using_name_1_from_private_module_2
                }
            } else {
                &*Diagnostics::Parameter_0_of_constructor_from_exported_class_has_or_is_using_private_name_1
            }
        }
        SyntaxKind::ConstructSignature | SyntaxKind::ConstructorType => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                &*Diagnostics::Parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_name_1_from_private_module_2
            } else {
                &*Diagnostics::Parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_1
            }
        }
        SyntaxKind::CallSignature => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                &*Diagnostics::Parameter_0_of_call_signature_from_exported_interface_has_or_is_using_name_1_from_private_module_2
            } else {
                &*Diagnostics::Parameter_0_of_call_signature_from_exported_interface_has_or_is_using_private_name_1
            }
        }
        SyntaxKind::IndexSignature => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                &*Diagnostics::Parameter_0_of_index_signature_from_exported_interface_has_or_is_using_name_1_from_private_module_2
            } else {
                &*Diagnostics::Parameter_0_of_index_signature_from_exported_interface_has_or_is_using_private_name_1
            }
        }
        SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
            if is_static(node.ref_(arena).parent(), arena) {
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Parameter_0_of_public_static_method_from_exported_class_has_or_is_using_private_name_1
                }
            } else if node.ref_(arena).parent().ref_(arena).parent().ref_(arena).kind() == SyntaxKind::ClassDeclaration {
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    if symbol_accessibility_result.accessibility
                        == SymbolAccessibility::CannotBeNamed
                    {
                        &*Diagnostics::Parameter_0_of_public_method_from_exported_class_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                    } else {
                        &*Diagnostics::Parameter_0_of_public_method_from_exported_class_has_or_is_using_name_1_from_private_module_2
                    }
                } else {
                    &*Diagnostics::Parameter_0_of_public_method_from_exported_class_has_or_is_using_private_name_1
                }
            } else {
                if symbol_accessibility_result
                    .error_module_name
                    .as_ref()
                    .non_empty()
                    .is_some()
                {
                    &*Diagnostics::Parameter_0_of_method_from_exported_interface_has_or_is_using_name_1_from_private_module_2
                } else {
                    &*Diagnostics::Parameter_0_of_method_from_exported_interface_has_or_is_using_private_name_1
                }
            }
        }
        SyntaxKind::FunctionDeclaration | SyntaxKind::FunctionType => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                if symbol_accessibility_result.accessibility == SymbolAccessibility::CannotBeNamed {
                    &*Diagnostics::Parameter_0_of_exported_function_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                } else {
                    &*Diagnostics::Parameter_0_of_exported_function_has_or_is_using_name_1_from_private_module_2
                }
            } else {
                &*Diagnostics::Parameter_0_of_exported_function_has_or_is_using_private_name_1
            }
        }
        SyntaxKind::SetAccessor | SyntaxKind::GetAccessor => {
            if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                if symbol_accessibility_result.accessibility == SymbolAccessibility::CannotBeNamed {
                    &*Diagnostics::Parameter_0_of_accessor_has_or_is_using_name_1_from_external_module_2_but_cannot_be_named
                } else {
                    &*Diagnostics::Parameter_0_of_accessor_has_or_is_using_name_1_from_private_module_2
                }
            } else {
                &*Diagnostics::Parameter_0_of_accessor_has_or_is_using_private_name_1
            }
        }
        _ => Debug_.fail(Some(&format!(
            "Unknown parent for parameter: {:?}",
            node.ref_(arena).parent().ref_(arena).kind()
        ))),
    }
}

struct GetTypeParameterConstraintVisibilityError {
    node: Id<Node>,
}

impl GetTypeParameterConstraintVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetTypeParameterConstraintVisibilityError {
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        match self.node.ref_(self).parent().ref_(self).kind() {
            SyntaxKind::ClassDeclaration => {
                diagnostic_message =
                    &Diagnostics::Type_parameter_0_of_exported_class_has_or_is_using_private_name_1;
            }
            SyntaxKind::InterfaceDeclaration => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_exported_interface_has_or_is_using_private_name_1;
            }
            SyntaxKind::MappedType => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_exported_mapped_object_type_is_using_private_name_1;
            }
            SyntaxKind::ConstructorType | SyntaxKind::ConstructSignature => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_constructor_signature_from_exported_interface_has_or_is_using_private_name_1;
            }
            SyntaxKind::CallSignature => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_call_signature_from_exported_interface_has_or_is_using_private_name_1;
            }
            SyntaxKind::MethodDeclaration | SyntaxKind::MethodSignature => {
                if is_static(self.node.ref_(self).parent(), self) {
                    diagnostic_message = &Diagnostics::Type_parameter_0_of_public_static_method_from_exported_class_has_or_is_using_private_name_1;
                } else if self.node.ref_(self).parent().ref_(self).parent().ref_(self).kind() == SyntaxKind::ClassDeclaration {
                    diagnostic_message = &Diagnostics::Type_parameter_0_of_public_method_from_exported_class_has_or_is_using_private_name_1;
                } else {
                    diagnostic_message = &Diagnostics::Type_parameter_0_of_method_from_exported_interface_has_or_is_using_private_name_1;
                }
            }
            SyntaxKind::FunctionType | SyntaxKind::FunctionDeclaration => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_exported_function_has_or_is_using_private_name_1;
            }
            SyntaxKind::TypeAliasDeclaration => {
                diagnostic_message = &Diagnostics::Type_parameter_0_of_exported_type_alias_has_or_is_using_private_name_1;
            }
            _ => Debug_.fail(Some(&format!(
                "This is unknown parent for type parameter: {:?}",
                self.node.ref_(self).parent().ref_(self).kind()
            ))),
        }

        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node,
            type_name: self.node.ref_(self).as_named_declaration().maybe_name(),
        }))
    }
}

impl HasArena for GetTypeParameterConstraintVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetHeritageClauseVisibilityError {
    node: Id<Node>,
}

impl GetHeritageClauseVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetHeritageClauseVisibilityError {
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        if is_class_declaration(&self.node.ref_(self).parent().ref_(self).parent().ref_(self)) {
            diagnostic_message = if is_heritage_clause(&self.node.ref_(self).parent().ref_(self))
                && self.node.ref_(self).parent().ref_(self).as_heritage_clause().token == SyntaxKind::ImplementsKeyword
            {
                &*Diagnostics::Implements_clause_of_exported_class_0_has_or_is_using_private_name_1
            } else if self
                .node
                .ref_(self).parent()
                .ref_(self).parent()
                .ref_(self).as_class_declaration()
                .maybe_name()
                .is_some()
            {
                &*Diagnostics::extends_clause_of_exported_class_0_has_or_is_using_private_name_1
            } else {
                &*Diagnostics::extends_clause_of_exported_class_has_or_is_using_private_name_0
            };
        } else {
            diagnostic_message =
                &Diagnostics::extends_clause_of_exported_interface_0_has_or_is_using_private_name_1;
        }

        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node,
            type_name: get_name_of_declaration(self.node.ref_(self).parent().ref_(self).maybe_parent(), self),
        }))
    }
}

impl HasArena for GetHeritageClauseVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetImportEntityNameVisibilityError {
    node: Id<Node>,
}

impl GetImportEntityNameVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetImportEntityNameVisibilityError {
    fn call(
        &self,
        _symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message: &Diagnostics::Import_declaration_0_is_using_private_name_1,
            error_node: self.node,
            type_name: self.node.ref_(self).as_named_declaration().maybe_name(),
        }))
    }
}

impl HasArena for GetImportEntityNameVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

struct GetTypeAliasDeclarationVisibilityError {
    node: Id<Node>,
}

impl GetTypeAliasDeclarationVisibilityError {
    fn new(node: Id<Node>, arena: &impl HasArena) -> GetSymbolAccessibilityDiagnostic {
        arena.alloc_get_symbol_accessibility_diagnostic_interface(Box::new(Self {
            node,
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetTypeAliasDeclarationVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Id<SymbolAccessibilityDiagnostic>> {
        Some(self.alloc_symbol_accessibility_diagnostic(SymbolAccessibilityDiagnostic {
            diagnostic_message: if symbol_accessibility_result
                .error_module_name
                .as_ref()
                .non_empty()
                .is_some()
            {
                &*Diagnostics::Exported_type_alias_0_has_or_is_using_private_name_1_from_module_2
            } else {
                &*Diagnostics::Exported_type_alias_0_has_or_is_using_private_name_1
            },
            error_node: if is_jsdoc_type_alias(&self.node.ref_(self)) {
                Debug_.check_defined(
                    self.node.ref_(self).as_jsdoc_type_like_tag().maybe_type_expression(),
                    None,
                )
            } else {
                self.node.ref_(self).as_type_alias_declaration().type_
            },
            type_name: if is_jsdoc_type_alias(&self.node.ref_(self)) {
                get_name_of_declaration(Some(self.node), self)
            } else {
                self.node.ref_(self).as_type_alias_declaration().maybe_name()
            },
        }))
    }
}

impl HasArena for GetTypeAliasDeclarationVisibilityError {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
