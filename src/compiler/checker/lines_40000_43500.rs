#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::convert::TryInto;
use std::rc::Rc;

use super::{ambient_module_symbol_regex, EmitResolverCreateResolver, UnusedKind};
use crate::{
    are_option_rcs_equal, create_diagnostic_for_node, filter, find, first_or_undefined,
    get_effective_return_type_node, get_jsdoc_type_parameter_declarations, get_object_flags,
    has_abstract_modifier, has_syntactic_modifier, id_text, is_binary_expression,
    is_child_of_node_with_kind, is_class_like, is_computed_property_name, is_declaration,
    is_function_like, is_in_js_file, is_let, is_omitted_expression, is_private_identifier,
    is_property_declaration, is_spread_element, is_static, is_string_literal, is_type_literal_node,
    is_var_const, length, skip_trivia, text_span_end, token_to_string, DiagnosticMessage,
    Diagnostics, ExternalEmitHelpers, HasInitializerInterface, HasTypeInterface,
    HasTypeParametersInterface, LiteralLikeNodeInterface, ModifierFlags, ModuleKind,
    NamedDeclarationInterface, NodeArray, NodeFlags, ObjectFlags, ReadonlyTextRange, SignatureKind,
    SourceFileLike, SymbolInterface, Ternary, TokenFlags, TypeFlags, TypeInterface, __String,
    bind_source_file, create_file_diagnostic, for_each, for_each_bool, get_source_file_of_node,
    get_span_of_token_at_position, is_accessor, is_external_or_common_js_module,
    is_literal_type_node, is_prefix_unary_expression, AllAccessorDeclarations,
    CancellationTokenDebuggable, Diagnostic, EmitResolver, EmitResolverDebuggable, IndexInfo, Node,
    NodeBuilderFlags, NodeCheckFlags, NodeInterface, ScriptTarget, Signature, SignatureFlags,
    StringOrNumber, Symbol, SymbolAccessibilityResult, SymbolFlags, SymbolTracker,
    SymbolVisibilityResult, SyntaxKind, Type, TypeChecker, TypeReferenceSerializationKind,
};

impl TypeChecker {
    pub(super) fn is_duplicated_common_js_export(&self, declarations: Option<&[Rc<Node>]>) -> bool {
        unimplemented!()
    }

    pub(super) fn check_source_element<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if let Some(node) = node {
            let node = node.borrow();
            self.check_source_element_worker(node);
        }
    }

    pub(super) fn check_source_element_worker(&self, node: &Node) {
        match node {
            Node::PropertySignature(_) => self.check_property_signature(node),
            Node::TypeReferenceNode(_) => self.check_type_reference_node(node),
            Node::KeywordTypeNode(_) | Node::LiteralTypeNode(_) => (),
            Node::ArrayTypeNode(_) => self.check_array_type(node),
            Node::UnionTypeNode(_) => self.check_union_or_intersection_type(node),
            Node::FunctionDeclaration(_) => self.check_function_declaration(node),
            Node::Block(_) => self.check_block(node),
            Node::VariableStatement(_) => self.check_variable_statement(node),
            Node::ExpressionStatement(_) => self.check_expression_statement(node),
            Node::IfStatement(_) => self.check_if_statement(node),
            Node::ReturnStatement(_) => self.check_return_statement(node),
            Node::VariableDeclaration(_) => self.check_variable_declaration(node),
            Node::InterfaceDeclaration(_) => self.check_interface_declaration(node),
            Node::TypeAliasDeclaration(_) => self.check_type_alias_declaration(node),
            _ => unimplemented!("{:?}", node.kind()),
        };
    }

    pub(super) fn get_type_from_jsdoc_variadic_type(
        &self,
        node: &Node, /*JSDocVariadicType*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn check_node_deferred(&self, node: &Node) {
        unimplemented!()
    }

    pub(super) fn check_source_file(&self, source_file: &Node /*SourceFile*/) {
        self.check_source_file_worker(source_file)
    }

    pub(super) fn unused_is_error(&self, kind: UnusedKind, is_ambient: bool) -> bool {
        unimplemented!()
    }

    pub(super) fn get_potentially_unused_identifiers(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Node /*PotentiallyUnusedIdentifier*/>> {
        unimplemented!()
    }

    pub(super) fn check_source_file_worker(&self, node: &Node /*SourceFile*/) {
        if true {
            for_each(&node.as_source_file().statements, |statement, _index| {
                self.check_source_element(Some(&**statement));
                Option::<()>::None
            });
        }
    }

    pub fn get_diagnostics(
        &self,
        source_file: &Node, /*SourceFile*/
        ct: Option<Rc<dyn CancellationTokenDebuggable>>,
    ) -> Vec<Rc<Diagnostic>> {
        self.get_diagnostics_worker(source_file)
    }

    pub(super) fn get_diagnostics_worker(
        &self,
        source_file: &Node, /*SourceFile*/
    ) -> Vec<Rc<Diagnostic>> {
        self.check_source_file(source_file);

        let semantic_diagnostics = self
            .diagnostics()
            .get_diagnostics(Some(&source_file.as_source_file().file_name()));

        semantic_diagnostics
    }

    pub(super) fn get_symbols_in_scope_(
        &self,
        location_in: &Node,
        meaning: SymbolFlags,
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn is_type_reference_identifier(&self, node: &Node /*EntityName*/) -> bool {
        unimplemented!()
    }

    pub(super) fn for_each_enclosing_class<TReturn, TCallback: FnMut(&Node) -> Option<TReturn>>(
        &self,
        node: &Node,
        callback: TCallback,
    ) -> Option<TReturn> {
        unimplemented!()
    }

    pub(super) fn is_node_used_during_class_initialization(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn is_node_within_class(
        &self,
        node: &Node,
        class_declaration: &Node, /*ClassLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn is_in_right_side_of_import_or_export_assignment(
        &self,
        node: &Node, /*EntityName*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_symbol_at_location_(
        &self,
        node: &Node,
        ignore_errors: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_index_infos_at_location_(&self, node: &Node) -> Option<Vec<Rc<IndexInfo>>> {
        unimplemented!()
    }

    pub(super) fn get_shorthand_assignment_value_symbol_<TNode: Borrow<Node>>(
        &self,
        location: Option<TNode>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_export_specifier_local_target_symbol_(
        &self,
        node: &Node, /*Identifier | ExportSpecifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_type_of_node(&self, node: &Node) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_type_of_assignment_pattern_(
        &self,
        expr: &Node, /*AssignmentPattern*/
    ) -> Option<Rc<Type>> {
        unimplemented!()
    }

    pub(super) fn get_property_symbol_of_destructuring_assignment_(
        &self,
        location: &Node, /*Identifier*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_parent_type_of_class_element(
        &self,
        node: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_class_element_property_key_type(
        &self,
        element: &Node, /*ClassElement*/
    ) -> Rc<Type> {
        unimplemented!()
    }

    pub(super) fn get_augmented_properties_of_type(&self, type_: &Type) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn type_has_call_or_construct_signatures(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn is_const_enum_or_const_enum_only_module(&self, s: &Symbol) -> bool {
        unimplemented!()
    }

    pub(super) fn is_implementation_of_overload_(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn get_enum_member_value(
        &self,
        node: &Node, /*EnumMember*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn can_have_constant_value(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn get_constant_value_(
        &self,
        node: &Node, /*EnumMember | AccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    pub(super) fn is_function_type(&self, type_: &Type) -> bool {
        unimplemented!()
    }

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn get_jsx_fragment_factory_entity(
        &self,
        location: &Node,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    pub(super) fn create_resolver(&self) -> Rc<dyn EmitResolverDebuggable> {
        Rc::new(EmitResolverCreateResolver::new())
    }

    pub(super) fn initialize_type_checker(&mut self) {
        for file in self.host.get_source_files() {
            bind_source_file(&*file, self.compiler_options.clone());
            println!("post-binding: {:#?}", file);
        }

        for file in self.host.get_source_files() {
            if !is_external_or_common_js_module(&file) {
                self.merge_symbol_table(
                    &mut *self.globals_mut(),
                    &RefCell::borrow(&file.locals()),
                    None,
                );
            }
        }

        self.global_object_type =
            self.get_global_type(&__String::new("Object".to_owned()), 0, true);
        self.global_boolean_type =
            self.get_global_type(&__String::new("Boolean".to_owned()), 0, true);
    }

    pub(super) fn check_external_emit_helpers(
        &self,
        location: &Node,
        helpers: ExternalEmitHelpers,
    ) {
        unimplemented!()
    }

    pub(super) fn check_grammar_decorators_and_modifiers(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_disallowed_trailing_comma(
        &self,
        list: Option<&NodeArray>,
        diag: Option<&'static DiagnosticMessage>,
    ) -> bool {
        let diag = diag.unwrap_or(&Diagnostics::Trailing_comma_not_allowed);
        unimplemented!()
    }

    pub(super) fn check_grammar_function_like_declaration(
        &self,
        node: &Node, /*FunctionLikeDeclaration | MethodSignature*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_index_signature(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_type_arguments(
        &self,
        node: &Node,
        type_arguments: Option<&NodeArray /*<TypeNode>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_tagged_template_chain(
        &self,
        node: &Node, /*TaggedTemplateExpression*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_arguments(
        &self,
        args: Option<&NodeArray /*<Expression>*/>,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_computed_property_name(&self, node: &Node) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_generator(
        &self,
        node: &Node, /*FunctionLikeDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_object_literal_expression(
        &self,
        node: &Node, /*ObjectLiteralExpression*/
        in_destructuring: bool,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_jsx_element(
        &self,
        node: &Node, /*JsxOpeningLikeElement*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_jsx_expression(&self, node: &Node /*JsxExpression*/) -> bool {
        unimplemented!()
    }

    pub(super) fn get_accessor_this_parameter(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<Rc<Node /*ParameterDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn check_grammar_for_invalid_dynamic_name(
        &self,
        node: &Node, /*DeclarationName*/
        message: &'static DiagnosticMessage,
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_grammar_method(
        &self,
        node: &Node, /*MethodDeclaration | MethodSignature*/
    ) -> bool {
        unimplemented!()
    }

    pub(super) fn check_ambient_initializer(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature*/
    ) -> bool {
        unimplemented!()
    }
}
