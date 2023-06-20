use gc::Gc;
use once_cell::unsync::Lazy;

use super::TransformModule;
use crate::{
    has_syntactic_modifier, id_text, is_binding_pattern, is_generated_identifier,
    is_omitted_expression, EmitFlags, EmitHelper, ModifierFlags, Node, NodeArray, NodeExt,
    NodeInterface, ReadonlyTextRange, ScopedEmitHelperBuilder, ScriptTarget, VisitResult, _d,
};

impl TransformModule {
    pub(super) fn append_exports_of_import_equals_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*ImportEqualsDeclaration*/
    ) /*: Statement[] | undefined */
    {
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        self.append_exports_of_declaration(statements, decl, None);
    }

    pub(super) fn append_exports_of_variable_statement(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        node: &Node, /*VariableStatement*/
    ) /*: Statement[] | undefined */
    {
        let node_as_variable_statement = node.as_variable_statement();
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        for decl in &node_as_variable_statement
            .declaration_list
            .as_variable_declaration_list()
            .declarations
        {
            self.append_exports_of_binding_element(statements, decl);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_binding_element(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*VariableDeclaration | BindingElement*/
    ) /*: Statement[] | undefined */
    {
        let decl_as_named_declaration = decl.as_named_declaration();
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        if is_binding_pattern(decl_as_named_declaration.maybe_name()) {
            for element in &decl_as_named_declaration
                .name()
                .as_has_elements()
                .elements()
            {
                if !is_omitted_expression(element) {
                    self.append_exports_of_binding_element(statements, element);
                }
            }
        } else if !is_generated_identifier(&decl_as_named_declaration.name()) {
            self.append_exports_of_declaration(statements, decl, None);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_hoisted_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined */
    {
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        if has_syntactic_modifier(decl, ModifierFlags::Export) {
            let ref export_name = if has_syntactic_modifier(decl, ModifierFlags::Default) {
                self.factory.create_identifier("default")
            } else {
                self.factory.get_declaration_name(Some(decl), None, None)
            };
            self.append_export_statement(
                statements,
                export_name,
                &self.factory.get_local_name(decl, None, None),
                Some(decl),
                None,
                None,
            );
        }

        if decl.as_named_declaration().maybe_name().is_some() {
            self.append_exports_of_declaration(statements, decl, None);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*Declaration*/
        live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        let ref name = self.factory.get_declaration_name(Some(decl), None, None);
        let current_module_info = self.current_module_info();
        let export_specifiers = current_module_info.export_specifiers.get(id_text(name));
        if let Some(export_specifiers) = export_specifiers {
            for export_specifier in export_specifiers {
                let export_specifier_as_export_specifier = export_specifier.as_export_specifier();
                self.append_export_statement(
                    statements,
                    &export_specifier_as_export_specifier.name,
                    name,
                    Some(&*export_specifier_as_export_specifier.name),
                    None,
                    live_binding,
                );
            }
        }
        // return statements;
    }

    pub(super) fn append_export_statement(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        export_name: &Node, /*Identifier*/
        expression: &Node,  /*Expression*/
        location: Option<&impl ReadonlyTextRange>,
        allow_comments: Option<bool>,
        live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        statements
            .get_or_insert_with(|| _d())
            .push(self.create_export_statement(
                export_name,
                expression,
                location,
                allow_comments,
                live_binding,
            ));
        // return statements;
    }

    pub(super) fn create_underscore_underscore_es_module(&self) -> Gc<Node> {
        if self.language_version == ScriptTarget::ES3 {
            self.factory
                .create_expression_statement(self.create_export_expression(
                    &self.factory.create_identifier("__esModule"),
                    &self.factory.create_true().wrap(),
                    Option::<&Node>::None,
                    None,
                ))
                .wrap()
        } else {
            self.factory
                .create_expression_statement(
                    self.factory
                        .create_call_expression(
                            self.factory
                                .create_property_access_expression(
                                    self.factory.create_identifier("Object"),
                                    "defineProperty",
                                )
                                .wrap(),
                            Option::<Gc<NodeArray>>::None,
                            Some(vec![
                                self.factory.create_identifier("exports"),
                                self.factory
                                    .create_string_literal("__esModule".to_owned(), None, None)
                                    .wrap(),
                                self.factory
                                    .create_object_literal_expression(
                                        Some(vec![self
                                            .factory
                                            .create_property_assignment(
                                                "value",
                                                self.factory.create_true().wrap(),
                                            )
                                            .wrap()]),
                                        None,
                                    )
                                    .wrap(),
                            ]),
                        )
                        .wrap(),
                )
                .wrap()
        }
        .set_emit_flags(EmitFlags::CustomPrologue)
    }

    pub(super) fn create_export_statement(
        &self,
        _name: &Node,  /*Identifier*/
        _value: &Node, /*Expression*/
        _location: Option<&impl ReadonlyTextRange>,
        _allow_comments: Option<bool>,
        _live_binding: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn create_export_expression(
        &self,
        _name: &Node,  /*Identifier*/
        _value: &Node, /*Expression*/
        _location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        _live_binding: Option<bool>,
    ) -> Gc<Node> {
        unimplemented!()
    }

    pub(super) fn modifier_visitor(&self, _node: &Node) -> VisitResult /*<Node>*/ {
        unimplemented!()
    }

    pub(super) fn get_exports(
        &self,
        _name: &Node, /*Identifier*/
    ) -> Option<Vec<Gc<Node /*Identifier*/>>> {
        unimplemented!()
    }
}

thread_local! {
    static _dynamic_import_umd_helper: Lazy<Gc<EmitHelper>> = Lazy::new(||
        ScopedEmitHelperBuilder::default()
            .name("typescript:dynamicimport-sync-require")
            .text("\n            var __syncRequire = typeof module === \"object\" && typeof module.exports === \"object\";".to_owned())
            .build().unwrap().into()
    )
}
pub(super) fn dynamic_import_umd_helper() -> Gc<EmitHelper> {
    _dynamic_import_umd_helper
        .with(|dynamic_import_umd_helper| (**dynamic_import_umd_helper).clone())
}
