use gc::Gc;
use once_cell::unsync::Lazy;

use super::TransformModule;
use crate::{
    is_binding_pattern, is_generated_identifier, is_omitted_expression, EmitHelper, Node,
    ReadonlyTextRange, ScopedEmitHelperBuilder, VisitResult,
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
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined */
    {
        unimplemented!()
    }

    pub(super) fn append_exports_of_declaration(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _decl: &Node, /*Declaration*/
        _live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        unimplemented!()
    }

    pub(super) fn append_export_statement(
        &self,
        _statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        _export_name: &Node, /*Identifier*/
        _expression: &Node,  /*Expression*/
        _location: Option<&impl ReadonlyTextRange>,
        _allow_comments: Option<bool>,
        _live_binding: Option<bool>,
    ) /*: Statement[] | undefined */
    {
        unimplemented!()
    }

    pub(super) fn create_underscore_underscore_es_module(&self) -> Gc<Node> {
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
