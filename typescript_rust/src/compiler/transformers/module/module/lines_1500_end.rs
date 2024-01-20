use std::io;

use gc::Gc;
use id_arena::Id;
use once_cell::unsync::Lazy;

use super::TransformModule;
use crate::{
    get_original_node_id, has_syntactic_modifier, id_text, is_binding_pattern,
    is_generated_identifier, is_omitted_expression, set_emit_flags, EmitFlags, EmitHelper,
    GetOrInsertDefault, ModifierFlags, Node, NodeArray, NodeExt, NodeInterface, OptionTry,
    ReadonlyTextRange, ScopedEmitHelperBuilder, ScriptTarget, SyntaxKind, VisitResult,
    InArena, OptionInArena,
};

impl TransformModule {
    pub(super) fn append_exports_of_import_equals_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ImportEqualsDeclaration*/
    ) /*: Statement[] | undefined */
    {
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        self.append_exports_of_declaration(statements, decl, None);
    }

    pub(super) fn append_exports_of_variable_statement(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        node: Id<Node>, /*VariableStatement*/
    ) /*: Statement[] | undefined */
    {
        let node_ref = node.ref_(self);
        let node_as_variable_statement = node_ref.as_variable_statement();
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
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*VariableDeclaration | BindingElement*/
    ) /*: Statement[] | undefined */
    {
        let decl_ref = decl.ref_(self);
        let decl_as_named_declaration = decl_ref.as_named_declaration();
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        if is_binding_pattern(decl_as_named_declaration.maybe_name().refed(self)) {
            for &element in &decl_as_named_declaration
                .name()
                .ref_(self).as_has_elements()
                .elements()
            {
                if !is_omitted_expression(element) {
                    self.append_exports_of_binding_element(statements, element);
                }
            }
        } else if !is_generated_identifier(&decl_as_named_declaration.name().ref_(self)) {
            self.append_exports_of_declaration(statements, decl, None);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_hoisted_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined */
    {
        if self.current_module_info().export_equals.is_some() {
            return /*statements*/;
        }

        if has_syntactic_modifier(decl, ModifierFlags::Export, self) {
            let export_name = &if has_syntactic_modifier(decl, ModifierFlags::Default, self) {
                self.factory.create_identifier("default")
            } else {
                self.factory.get_declaration_name(Some(decl), None, None)
            };
            self.append_export_statement(
                statements,
                export_name,
                &self.factory.get_local_name(decl, None, None),
                Some(&*decl.ref_(self)),
                None,
                None,
            );
        }

        if decl.ref_(self).as_named_declaration().maybe_name().is_some() {
            self.append_exports_of_declaration(statements, decl, None);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_declaration(
        &self,
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        decl: Id<Node>, /*Declaration*/
        live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        let name = self.factory.get_declaration_name(Some(decl), None, None);
        let current_module_info = self.current_module_info();
        let export_specifiers = current_module_info.export_specifiers.get(id_text(&*name.ref_(self)));
        if let Some(export_specifiers) = export_specifiers {
            for export_specifier in export_specifiers {
                let export_specifier_ref = export_specifier.ref_(self);
                let export_specifier_as_export_specifier = export_specifier_ref.as_export_specifier();
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
        statements: &mut Option<Vec<Id<Node /*Statement*/>>>,
        export_name: Id<Node>, /*Identifier*/
        expression: Id<Node>,  /*Expression*/
        location: Option<&impl ReadonlyTextRange>,
        allow_comments: Option<bool>,
        live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        statements
            .get_or_insert_default_()
            .push(self.create_export_statement(
                export_name,
                expression,
                location,
                allow_comments,
                live_binding,
            ));
        // return statements;
    }

    pub(super) fn create_underscore_underscore_es_module(&self) -> Id<Node> {
        if self.language_version == ScriptTarget::ES3 {
            self.factory
                .create_expression_statement(self.create_export_expression(
                    self.factory.create_identifier("__esModule"),
                    self.factory.create_true(),
                    Option::<Id<Node>>::None,
                    None,
                ))
        } else {
            self.factory
                .create_expression_statement(self.factory.create_call_expression(
                    self.factory.create_property_access_expression(
                        self.factory.create_identifier("Object"),
                        "defineProperty",
                    ),
                    Option::<Gc<NodeArray>>::None,
                    Some(vec![
                                self.factory.create_identifier("exports"),
                                self.factory
                                    .create_string_literal("__esModule".to_owned(), None, None)
                                    ,
                                self.factory
                                    .create_object_literal_expression(
                                        Some(vec![self
                                            .factory
                                            .create_property_assignment(
                                                "value",
                                                self.factory.create_true(),
                                            )
                                            ]),
                                        None,
                                    )
                                    ,
                            ]),
                ))
        }
        .set_emit_flags(EmitFlags::CustomPrologue, self)
    }

    pub(super) fn create_export_statement(
        &self,
        name: Id<Node>,  /*Identifier*/
        value: Id<Node>, /*Expression*/
        location: Option<&impl ReadonlyTextRange>,
        allow_comments: Option<bool>,
        live_binding: Option<bool>,
    ) -> Id<Node> {
        let statement = self
            .factory
            .create_expression_statement(self.create_export_expression(
                name,
                value,
                Option::<&Node>::None,
                live_binding,
            ))
            .set_text_range(location, self)
            .start_on_new_line(self);
        if allow_comments != Some(true) {
            set_emit_flags(statement, EmitFlags::NoComments, self);
        }

        statement
    }

    pub(super) fn create_export_expression(
        &self,
        name: Id<Node>,  /*Identifier*/
        value: Id<Node>, /*Expression*/
        location: Option<&(impl ReadonlyTextRange + ?Sized)>,
        live_binding: Option<bool>,
    ) -> Id<Node> {
        if live_binding == Some(true) && self.language_version != ScriptTarget::ES3 {
            self.factory.create_call_expression(
                self.factory.create_property_access_expression(
                    self.factory.create_identifier("Object"),
                    "defineProperty",
                ),
                Option::<Gc<NodeArray>>::None,
                Some(vec![
                    self.factory.create_identifier("exports"),
                    self.factory.create_string_literal_from_node(name),
                    self.factory.create_object_literal_expression(
                        Some(vec![
                            self.factory.create_property_assignment(
                                "enumerable",
                                self.factory.create_true(),
                            ),
                            self.factory.create_property_assignment(
                                "get",
                                self.factory.create_function_expression(
                                    Option::<Gc<NodeArray>>::None,
                                    None,
                                    Option::<Id<Node>>::None,
                                    Option::<Gc<NodeArray>>::None,
                                    Some(vec![]),
                                    None,
                                    self.factory.create_block(
                                        vec![self
                                            .factory
                                            .create_return_statement(Some(value))],
                                        None,
                                    ),
                                ),
                            ),
                        ]),
                        None,
                    ),
                ]),
            )
        } else {
            self.factory.create_assignment(
                self.factory.create_property_access_expression(
                    self.factory.create_identifier("exports"),
                    self.factory.clone_node(name),
                ),
                value,
            )
        }
        .set_text_range(location, self)
    }

    pub(super) fn modifier_visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        match node.ref_(self).kind() {
            SyntaxKind::ExportKeyword | SyntaxKind::DefaultKeyword => None,
            _ => Some(node.into()),
        }
    }

    pub(super) fn get_exports(
        &self,
        name: Id<Node>, /*Identifier*/
    ) -> io::Result<Option<Vec<Id<Node /*Identifier*/>>>> {
        if !is_generated_identifier(&name.ref_(self)) {
            let value_declaration = self
                .resolver
                .get_referenced_import_declaration(name)?
                .try_or_else(|| self.resolver.get_referenced_value_declaration(name))?;
            if let Some(ref value_declaration) = value_declaration {
                return Ok(self
                    .maybe_current_module_info()
                    .and_then(|current_module_info| {
                        current_module_info
                            .exported_bindings
                            .get(&get_original_node_id(value_declaration, self))
                            .cloned()
                    }));
            }
        }
        Ok(None)
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
