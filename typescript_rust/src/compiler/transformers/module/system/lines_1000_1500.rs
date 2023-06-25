use gc::Gc;

use super::TransformSystemModule;
use crate::{
    get_emit_flags, get_text_of_identifier_or_literal, has_syntactic_modifier, id_text,
    is_binding_pattern, is_generated_identifier, is_identifier, is_omitted_expression,
    set_emit_flags, EmitFlags, GetOrInsertDefault, ModifierFlags, Node, NodeArray, NodeExt,
    NodeInterface, SyntaxKind, VisitResult,
};

impl TransformSystemModule {
    pub(super) fn append_exports_of_binding_element(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*VariableDeclaration | BindingElement*/
        export_self: bool,
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        let decl_name = &decl.as_named_declaration().name();
        if is_binding_pattern(Some(&**decl_name)) {
            for element in &decl_name.as_has_elements().elements() {
                if !is_omitted_expression(element) {
                    self.append_exports_of_binding_element(statements, element, export_self);
                }
            }
        } else if !is_generated_identifier(decl_name) {
            let mut exclude_name = None;
            if export_self {
                self.append_export_statement(
                    statements,
                    decl_name,
                    &self.factory.get_local_name(decl, None, None),
                    None,
                );
                exclude_name = Some(id_text(decl_name));
            }

            self.append_exports_of_declaration(statements, decl, exclude_name);
        }

        // return statements;
    }

    pub(super) fn append_exports_of_hoisted_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*ClassDeclaration | FunctionDeclaration*/
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        let mut exclude_name = None;
        if has_syntactic_modifier(decl, ModifierFlags::Export) {
            let export_name = &if has_syntactic_modifier(decl, ModifierFlags::Default) {
                self.factory
                    .create_string_literal("default".to_owned(), None, None)
            } else {
                decl.as_named_declaration().name()
            };
            self.append_export_statement(
                statements,
                export_name,
                &self.factory.get_local_name(decl, None, None),
                None,
            );
            exclude_name = Some(get_text_of_identifier_or_literal(export_name).into_owned());
        }

        if decl.as_named_declaration().maybe_name().is_some() {
            self.append_exports_of_declaration(statements, decl, exclude_name.as_deref());
        }

        // return statements;
    }

    pub(super) fn append_exports_of_declaration(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        decl: &Node, /*Declaration*/
        exclude_name: Option<&str>,
    ) /*: Statement[] | undefined*/
    {
        if self.module_info().export_equals.is_some() {
            return /*statements*/;
        }

        let name = &self.factory.get_declaration_name(Some(decl), None, None);
        let module_info = self.module_info();
        let export_specifiers = module_info.export_specifiers.get(id_text(name));
        if let Some(export_specifiers) = export_specifiers {
            for export_specifier in export_specifiers {
                let export_specifier_as_export_specifier = export_specifier.as_export_specifier();
                if Some(
                    &*export_specifier_as_export_specifier
                        .name
                        .as_identifier()
                        .escaped_text,
                ) != exclude_name
                {
                    self.append_export_statement(
                        statements,
                        &export_specifier_as_export_specifier.name,
                        name,
                        None,
                    );
                }
            }
        }
        // return statements;
    }

    pub(super) fn append_export_statement(
        &self,
        statements: &mut Option<Vec<Gc<Node /*Statement*/>>>,
        export_name: &Node, /*Identifier | StringLiteral*/
        expression: &Node,  /*Expression*/
        allow_comments: Option<bool>,
    ) /*: Statement[] | undefined*/
    {
        statements
            .get_or_insert_default_()
            .push(self.create_export_statement(export_name, expression, allow_comments));
        // return statements;
    }

    pub(super) fn create_export_statement(
        &self,
        name: &Node,  /*Identifier | StringLiteral*/
        value: &Node, /*Expression*/
        allow_comments: Option<bool>,
    ) -> Gc<Node> {
        let statement = self
            .factory
            .create_expression_statement(self.create_export_expression(name, value))
            .start_on_new_line();
        if allow_comments != Some(true) {
            set_emit_flags(&*statement, EmitFlags::NoComments);
        }

        statement
    }

    pub(super) fn create_export_expression(
        &self,
        name: &Node,  /*Identifier | StringLiteral*/
        value: &Node, /*Expression*/
    ) -> Gc<Node> {
        let export_name = if is_identifier(name) {
            self.factory.create_string_literal_from_node(name)
        } else {
            name.node_wrapper()
        };
        set_emit_flags(value, get_emit_flags(value) | EmitFlags::NoComments);
        self.factory
            .create_call_expression(
                self.export_function(),
                Option::<Gc<NodeArray>>::None,
                Some(vec![export_name, value.node_wrapper()]),
            )
            .set_comment_range(value)
    }

    pub(super) fn top_level_nested_visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        match node.kind() {
            SyntaxKind::VariableStatement => self.visit_variable_statement(node),
            SyntaxKind::FunctionDeclaration => self.visit_function_declaration(node),
            SyntaxKind::ClassDeclaration => self.visit_class_declaration(node),
            SyntaxKind::ForStatement => self.visit_for_statement(node, true),
            SyntaxKind::ForInStatement => self.visit_for_in_statement(node),
            SyntaxKind::ForOfStatement => self.visit_for_of_statement(node),
            SyntaxKind::DoStatement => self.visit_do_statement(node),
            SyntaxKind::WhileStatement => self.visit_while_statement(node),
            SyntaxKind::LabeledStatement => self.visit_labeled_statement(node),
            SyntaxKind::WithStatement => self.visit_with_statement(node),
            SyntaxKind::SwitchStatement => self.visit_switch_statement(node),
            SyntaxKind::CaseBlock => Some(self.visit_case_block(node).into()),
            SyntaxKind::CaseClause => self.visit_case_clause(node),
            SyntaxKind::DefaultClause => self.visit_default_clause(node),
            SyntaxKind::TryStatement => self.visit_try_statement(node),
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node).into()),
            SyntaxKind::Block => Some(self.visit_block(node).into()),
            SyntaxKind::MergeDeclarationMarker => self.visit_merge_declaration_marker(node),
            SyntaxKind::EndOfDeclarationMarker => self.visit_end_of_declaration_marker(node),
            _ => self.visitor(node),
        }
    }

    pub(super) fn visit_for_statement(
        &self,
        _node: &Node, /*ForStatement*/
        _is_top_level: bool,
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_for_in_statement(
        &self,
        _node: &Node, /*ForInStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_for_of_statement(
        &self,
        _node: &Node, /*ForOfStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_do_statement(&self, _node: &Node /*DoStatement*/) -> VisitResult /*<Statement>*/
    {
        unimplemented!()
    }

    pub(super) fn visit_while_statement(
        &self,
        _node: &Node, /*WhileStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_labeled_statement(
        &self,
        _node: &Node, /*LabeledStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_with_statement(&self, _node: &Node /*WithStatement*/) -> VisitResult /*<Statement>*/
    {
        unimplemented!()
    }

    pub(super) fn visit_switch_statement(
        &self,
        _node: &Node, /*SwitchStatement*/
    ) -> VisitResult /*<Statement>*/ {
        unimplemented!()
    }

    pub(super) fn visit_case_block(
        &self,
        _node: &Node, /*CaseBlock*/
    ) -> Gc<Node /*CaseBlock*/> {
        unimplemented!()
    }

    pub(super) fn visit_case_clause(&self, _node: &Node /*CaseClause*/) -> VisitResult /*<CaseOrDefaultClause>*/
    {
        unimplemented!()
    }

    pub(super) fn visit_default_clause(&self, _node: &Node /*DefaultClause*/) -> VisitResult /*<CaseOrDefaultClause>*/
    {
        unimplemented!()
    }

    pub(super) fn visit_try_statement(&self, _node: &Node /*TryStatement*/) -> VisitResult /*<Statement>*/
    {
        unimplemented!()
    }

    pub(super) fn visit_catch_clause(
        &self,
        _node: &Node, /*CatchClause*/
    ) -> Gc<Node /*CatchClause*/> {
        unimplemented!()
    }

    pub(super) fn visit_block(&self, _node: &Node /*Block*/) -> Gc<Node /*Block*/> {
        unimplemented!()
    }
}
