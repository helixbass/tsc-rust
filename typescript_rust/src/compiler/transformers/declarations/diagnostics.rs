use gc::{Finalize, Gc, Trace};

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
    NodeInterface, NonEmpty, SymbolAccessibility, SymbolAccessibilityResult, SyntaxKind,
};

pub trait GetSymbolAccessibilityDiagnosticInterface: Trace + Finalize {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>>;
}

pub type GetSymbolAccessibilityDiagnostic = Gc<Box<dyn GetSymbolAccessibilityDiagnosticInterface>>;

#[derive(Trace, Finalize)]
pub struct SymbolAccessibilityDiagnostic {
    pub error_node: Gc<Node>,
    pub diagnostic_message: &'static DiagnosticMessage,
    pub type_name: Option<Gc<Node /*DeclarationName | QualifiedName*/>>,
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
    node: &Node, /*DeclarationDiagnosticProducing*/
) -> GetSymbolAccessibilityDiagnostic {
    if is_variable_declaration(node)
        || is_property_declaration(node)
        || is_property_signature(node)
        || is_property_access_expression(node)
        || is_binding_element(node)
        || is_constructor_declaration(node)
    {
        GetVariableDeclarationTypeVisibilityError::new(node)
    } else if is_set_accessor(node) || is_get_accessor(node) {
        GetAccessorDeclarationTypeVisibilityError::new(node)
    } else if is_construct_signature_declaration(node)
        || is_call_signature_declaration(node)
        || is_method_declaration(node)
        || is_method_signature(node)
        || is_function_declaration(node)
        || is_index_signature_declaration(node)
    {
        GetReturnTypeVisibilityError::new(node)
    } else if is_parameter(node) {
        if is_parameter_property_declaration(node, &node.parent())
            && has_syntactic_modifier(&node.parent(), ModifierFlags::Private)
        {
            return GetVariableDeclarationTypeVisibilityError::new(node);
        }
        GetParameterDeclarationTypeVisibilityError::new(node)
    } else if is_type_parameter_declaration(node) {
        GetTypeParameterConstraintVisibilityError::new(node)
    } else if is_expression_with_type_arguments(node) {
        GetHeritageClauseVisibilityError::new(node)
    } else if is_import_equals_declaration(node) {
        GetImportEntityNameVisibilityError::new(node)
    } else if is_type_alias_declaration(node) || is_jsdoc_type_alias(node) {
        GetTypeAliasDeclarationVisibilityError::new(node)
    } else {
        Debug_.assert_never(
            node,
            Some(&format!(
                "Attempted to set a declaration diagnostic context for unhandled node kind: {:?}",
                node.kind()
            )),
        )
    }
}

fn get_variable_declaration_type_visibility_diagnostic_message(
    node: &Node,
    symbol_accessibility_result: &SymbolAccessibilityResult,
) -> Option<&'static DiagnosticMessage> {
    if matches!(
        node.kind(),
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
        node.kind(),
        SyntaxKind::PropertyDeclaration
            | SyntaxKind::PropertyAccessExpression
            | SyntaxKind::PropertySignature
    ) || node.kind() == SyntaxKind::Parameter
        && has_syntactic_modifier(&node.parent(), ModifierFlags::Private)
    {
        if is_static(node) {
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
        } else if node.parent().kind() == SyntaxKind::ClassDeclaration
            || node.kind() == SyntaxKind::Parameter
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

#[derive(Trace, Finalize)]
struct GetVariableDeclarationTypeVisibilityError {
    node: Gc<Node>,
}

impl GetVariableDeclarationTypeVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetVariableDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message = get_variable_declaration_type_visibility_diagnostic_message(
            &self.node,
            symbol_accessibility_result,
        );
        diagnostic_message.map(|diagnostic_message| {
            Gc::new(SymbolAccessibilityDiagnostic {
                diagnostic_message,
                error_node: self.node.clone(),
                type_name: self.node.as_named_declaration().maybe_name(),
            })
        })
    }
}

#[derive(Trace, Finalize)]
struct GetAccessorDeclarationTypeVisibilityError {
    node: Gc<Node>,
}

impl GetAccessorDeclarationTypeVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetAccessorDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        if self.node.kind() == SyntaxKind::SetAccessor {
            if is_static(&self.node) {
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
            if is_static(&self.node) {
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
        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node.as_named_declaration().name(),
            type_name: self.node.as_named_declaration().maybe_name(),
        }))
    }
}

#[derive(Trace, Finalize)]
struct GetReturnTypeVisibilityError {
    node: Gc<Node>,
}

impl GetReturnTypeVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetReturnTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        match self.node.kind() {
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
                if is_static(&self.node) {
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
                } else if self.node.parent().kind() == SyntaxKind::ClassDeclaration {
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
                self.node.kind()
            ))),
        }

        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self
                .node
                .as_named_declaration()
                .maybe_name()
                .unwrap_or_else(|| self.node.clone()),
            type_name: None,
        }))
    }
}

#[derive(Trace, Finalize)]
struct GetParameterDeclarationTypeVisibilityError {
    node: Gc<Node>,
}

impl GetParameterDeclarationTypeVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetParameterDeclarationTypeVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message = get_parameter_declaration_type_visibility_diagnostic_message(
            &self.node,
            symbol_accessibility_result,
        );
        /* diagnostisMessage !== undefined ?*/
        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node.clone(),
            type_name: self.node.as_named_declaration().maybe_name(),
        }))
        /*: undefined*/
    }
}

fn get_parameter_declaration_type_visibility_diagnostic_message(
    node: &Node,
    symbol_accessibility_result: &SymbolAccessibilityResult,
) -> &'static DiagnosticMessage {
    match node.parent().kind() {
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
            if is_static(&node.parent()) {
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
            } else if node.parent().parent().kind() == SyntaxKind::ClassDeclaration {
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
            node.parent().kind()
        ))),
    }
}

#[derive(Trace, Finalize)]
struct GetTypeParameterConstraintVisibilityError {
    node: Gc<Node>,
}

impl GetTypeParameterConstraintVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetTypeParameterConstraintVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        match self.node.parent().kind() {
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
                if is_static(&self.node.parent()) {
                    diagnostic_message = &Diagnostics::Type_parameter_0_of_public_static_method_from_exported_class_has_or_is_using_private_name_1;
                } else if self.node.parent().parent().kind() == SyntaxKind::ClassDeclaration {
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
                self.node.parent().kind()
            ))),
        }

        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node.clone(),
            type_name: self.node.as_named_declaration().maybe_name(),
        }))
    }
}

#[derive(Trace, Finalize)]
struct GetHeritageClauseVisibilityError {
    node: Gc<Node>,
}

impl GetHeritageClauseVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetHeritageClauseVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        let diagnostic_message: &'static DiagnosticMessage;
        if is_class_declaration(&self.node.parent().parent()) {
            diagnostic_message = if is_heritage_clause(&self.node.parent())
                && self.node.parent().as_heritage_clause().token == SyntaxKind::ImplementsKeyword
            {
                &*Diagnostics::Implements_clause_of_exported_class_0_has_or_is_using_private_name_1
            } else if self
                .node
                .parent()
                .parent()
                .as_class_declaration()
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

        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message,
            error_node: self.node.clone(),
            type_name: get_name_of_declaration(self.node.parent().maybe_parent()),
        }))
    }
}

#[derive(Trace, Finalize)]
struct GetImportEntityNameVisibilityError {
    node: Gc<Node>,
}

impl GetImportEntityNameVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetImportEntityNameVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        Some(Gc::new(SymbolAccessibilityDiagnostic {
            diagnostic_message: &Diagnostics::Import_declaration_0_is_using_private_name_1,
            error_node: self.node.clone(),
            type_name: self.node.as_named_declaration().maybe_name(),
        }))
    }
}

#[derive(Trace, Finalize)]
struct GetTypeAliasDeclarationVisibilityError {
    node: Gc<Node>,
}

impl GetTypeAliasDeclarationVisibilityError {
    fn new(node: &Node) -> GetSymbolAccessibilityDiagnostic {
        Gc::new(Box::new(Self {
            node: node.node_wrapper(),
        }))
    }
}

impl GetSymbolAccessibilityDiagnosticInterface for GetTypeAliasDeclarationVisibilityError {
    fn call(
        &self,
        symbol_accessibility_result: &SymbolAccessibilityResult,
    ) -> Option<Gc<SymbolAccessibilityDiagnostic>> {
        Some(Gc::new(SymbolAccessibilityDiagnostic {
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
            error_node: if is_jsdoc_type_alias(&self.node) {
                Debug_.check_defined(
                    self.node.as_jsdoc_type_like_tag().maybe_type_expression(),
                    None,
                )
            } else {
                self.node.as_type_alias_declaration().type_.clone()
            },
            type_name: if is_jsdoc_type_alias(&self.node) {
                get_name_of_declaration(Some(&*self.node))
            } else {
                self.node.as_type_alias_declaration().maybe_name()
            },
        }))
    }
}
