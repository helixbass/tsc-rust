#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::UnusedKind;
use crate::{
    are_option_rcs_equal, filter, find, get_object_flags, length, ObjectFlags, SignatureKind,
    SymbolInterface, Ternary, TypeFlags, TypeInterface, __String, bind_source_file, for_each,
    is_accessor, is_external_or_common_js_module, AllAccessorDeclarations,
    CancellationTokenDebuggable, Diagnostic, EmitResolver, EmitResolverDebuggable, IndexInfo, Node,
    NodeBuilderFlags, NodeCheckFlags, NodeInterface, Signature, SignatureFlags, StringOrNumber,
    Symbol, SymbolAccessibilityResult, SymbolFlags, SymbolTracker, SymbolVisibilityResult,
    SyntaxKind, Type, TypeChecker, TypeReferenceSerializationKind,
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

    pub(super) fn get_referenced_value_symbol(
        &self,
        reference: &Node, /*Identifier*/
        start_in_declaration_container: Option<bool>,
    ) -> Option<Rc<Symbol>> {
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
                    &mut *self.globals(),
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

    pub(super) fn get_accessor_this_parameter(
        &self,
        accessor: &Node, /*AccessorDeclaration*/
    ) -> Option<Rc<Node /*ParameterDeclaration*/>> {
        unimplemented!()
    }

    pub(super) fn check_grammar_numeric_literal(
        &self,
        node: &Node, /*NumericLiteral*/
    ) -> bool {
        false
    }

    pub(super) fn find_matching_type_reference_or_type_alias_reference(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> Option<Rc<Type>> {
        let source_object_flags = get_object_flags(source);
        if source_object_flags.intersects(ObjectFlags::Reference | ObjectFlags::Anonymous)
            && union_target.flags().intersects(TypeFlags::Union)
        {
            return find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |target: &Rc<Type>, _| {
                    if target.flags().intersects(TypeFlags::Object) {
                        let overlap_obj_flags = source_object_flags & get_object_flags(target);
                        if overlap_obj_flags.intersects(ObjectFlags::Reference) {
                            return Rc::ptr_eq(
                                &source.as_type_reference().target,
                                &target.as_type_reference().target,
                            );
                        }
                        if overlap_obj_flags.intersects(ObjectFlags::Anonymous) {
                            return source.maybe_alias_symbol().is_some()
                                && are_option_rcs_equal(
                                    source.maybe_alias_symbol().as_ref(),
                                    target.maybe_alias_symbol().as_ref(),
                                );
                        }
                    }
                    false
                },
            )
            .map(Clone::clone);
        }
        None
    }

    pub(super) fn find_best_type_for_object_literal(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> Option<Rc<Type>> {
        if get_object_flags(source).intersects(ObjectFlags::ObjectLiteral)
            && self.some_type(union_target, |type_: &Type| self.is_array_like_type(type_))
        {
            return find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Rc<Type>, _| !self.is_array_like_type(t),
            )
            .map(Clone::clone);
        }
        None
    }

    pub(super) fn find_best_type_for_invokable(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> Option<Rc<Type>> {
        let mut signature_kind = SignatureKind::Call;
        let has_signatures = !self
            .get_signatures_of_type(source, signature_kind)
            .is_empty()
            || {
                signature_kind = SignatureKind::Construct;
                !self
                    .get_signatures_of_type(source, signature_kind)
                    .is_empty()
            };
        if has_signatures {
            return find(
                union_target
                    .as_union_or_intersection_type_interface()
                    .types(),
                |t: &Rc<Type>, _| !self.get_signatures_of_type(t, signature_kind).is_empty(),
            )
            .map(Clone::clone);
        }
        None
    }

    pub(super) fn find_most_overlappy_type(
        &self,
        source: &Type,
        union_target: &Type, /*UnionOrIntersectionType*/
    ) -> Option<Rc<Type>> {
        let mut best_match: Option<Rc<Type>> = None;
        let mut matching_count = 0;
        for target in union_target
            .as_union_or_intersection_type_interface()
            .types()
        {
            let overlap = self.get_intersection_type(
                &vec![
                    self.get_index_type(source, None, None),
                    self.get_index_type(target, None, None),
                ],
                Option::<&Symbol>::None,
                None,
            );
            if overlap.flags().intersects(TypeFlags::Index) {
                best_match = Some(target.clone());
                matching_count = usize::MAX;
            } else if overlap.flags().intersects(TypeFlags::Union) {
                let len = length(
                    filter(
                        Some(overlap.as_union_or_intersection_type_interface().types()),
                        |type_: &Rc<Type>| self.is_unit_type(type_),
                    )
                    .as_deref(),
                );
                if len >= matching_count {
                    best_match = Some(target.clone());
                    matching_count = len;
                }
            } else if self.is_unit_type(&overlap) && 1 >= matching_count {
                best_match = Some(target.clone());
                matching_count = 1;
            }
        }
        best_match
    }

    pub(super) fn filter_primitives_if_contains_non_primitive(
        &self,
        type_: &Type, /*UnionType*/
    ) -> Rc<Type> {
        if self.maybe_type_of_kind(type_, TypeFlags::NonPrimitive) {
            let result = self.filter_type(type_, |t: &Type| {
                !t.flags().intersects(TypeFlags::Primitive)
            });
            if !result.flags().intersects(TypeFlags::Never) {
                return result;
            }
        }
        type_.type_wrapper()
    }

    pub(super) fn find_matching_discriminant_type<TIsRelatedTo: FnMut(&Type, &Type) -> Ternary>(
        &self,
        source: &Type,
        target: &Type,
        mut is_related_to: TIsRelatedTo,
        skip_partial: Option<bool>,
    ) -> Option<Rc<Type>> {
        if target.flags().intersects(TypeFlags::Union)
            && source
                .flags()
                .intersects(TypeFlags::Intersection | TypeFlags::Object)
        {
            let match_ = self.get_matching_union_constituent_for_type(target, source);
            if match_.is_some() {
                return match_;
            }
            let source_properties = self.get_properties_of_type(source);
            // if (sourceProperties) {
            let source_properties_filtered =
                self.find_discriminant_properties(&source_properties, target);
            if let Some(source_properties_filtered) = source_properties_filtered.as_ref() {
                return self.discriminate_type_by_discriminable_items(
                    target,
                    &*source_properties_filtered
                        .into_iter()
                        .map(|p: &Rc<Symbol>| {
                            let p_clone = p.clone();
                            (
                                move || self.get_type_of_symbol(&p_clone),
                                p.escaped_name().clone(),
                            )
                        })
                        .collect::<Vec<_>>(),
                    |source: &Type, target: &Type| is_related_to(source, target) != Ternary::False,
                    Option::<&Type>::None,
                    skip_partial,
                );
            }
            // }
        }
        None
    }
}

#[derive(Debug)]
pub(super) struct EmitResolverCreateResolver {}

impl EmitResolverCreateResolver {
    pub fn new() -> Self {
        Self {}
    }
}

impl EmitResolver for EmitResolverCreateResolver {
    fn has_global_name(&self, name: &str) -> bool {
        unimplemented!()
    }

    fn get_referenced_export_container(
        &self,
        node: &Node, /*Identifier*/
        prefix_locals: Option<bool>,
    ) -> Option<Rc<Node /*SourceFile | ModuleDeclaration | EnumDeclaration*/>> {
        unimplemented!()
    }

    fn get_referenced_import_declaration(
        &self,
        node: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>> {
        unimplemented!()
    }

    fn is_declaration_with_colliding_name(&self, node: &Node /*Declaration*/) -> bool {
        unimplemented!()
    }

    fn is_value_alias_declaration(&self, node: &Node) -> bool {
        unimplemented!()
    }

    fn is_referenced_alias_declaration(&self, node: &Node, check_children: Option<bool>) -> bool {
        unimplemented!()
    }

    fn is_top_level_value_import_equals_with_entity_name(
        &self,
        node: &Node, /*ImportEqualsDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    fn get_node_check_flags(&self, node: &Node) -> NodeCheckFlags {
        unimplemented!()
    }

    fn is_declaration_visible(&self, node: &Node /*Declaration | AnyImportSyntax*/) -> bool {
        unimplemented!()
    }

    fn is_late_bound(&self, node: &Node /*Declaration*/) -> bool {
        unimplemented!()
    }

    fn collect_linked_aliases(
        &self,
        node: &Node, /*Identifier*/
        set_visibility: Option<bool>,
    ) -> Option<Vec<Rc<Node>>> {
        unimplemented!()
    }

    fn is_implementation_of_overload(
        &self,
        node: &Node, /*SignatureDeclaration*/
    ) -> Option<bool> {
        unimplemented!()
    }

    fn is_required_initialized_parameter(&self, node: &Node /*ParameterDeclaration*/) -> bool {
        unimplemented!()
    }

    fn is_optional_uninitialized_parameter_property(
        &self,
        node: &Node, /*ParameterDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    fn is_expando_function_declaration(&self, node: &Node /*FunctionDeclaration*/) -> bool {
        unimplemented!()
    }

    fn get_properties_of_container_function(
        &self,
        node: &Node, /*Declaration*/
    ) -> Vec<Rc<Symbol>> {
        unimplemented!()
    }

    fn create_type_of_declaration(
        &self,
        declaration: &Node, /*AccessorDeclaration | VariableLikeDeclaration | PropertyAccessExpression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
        add_undefined: Option<bool>,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        unimplemented!()
    }

    fn create_return_type_of_signature_declaration(
        &self,
        signature_declaration: &Node, /*SignatureDeclaration*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        unimplemented!()
    }

    fn create_type_of_expression(
        &self,
        expr: &Node, /*Expression*/
        enclosing_declaration: &Node,
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
    ) -> Option<Rc<Node /*TypeNode*/>> {
        unimplemented!()
    }

    fn create_literal_const_value(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
        tracker: &dyn SymbolTracker,
    ) -> Rc<Node /*Expression*/> {
        unimplemented!()
    }

    fn is_symbol_accessible(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<&Node>,
        meaning: Option<SymbolFlags>,
        should_compute_alias_to_mark_visible: bool,
    ) -> SymbolAccessibilityResult {
        unimplemented!()
    }

    fn is_entity_name_visible(
        &self,
        entity_name: &Node, /*EntityNameOrEntityNameExpression*/
        enclosing_declaration: &Node,
    ) -> SymbolVisibilityResult {
        unimplemented!()
    }

    fn get_constant_value(
        &self,
        node: &Node, /*EnumMember | PropertyAccessExpression | ElementAccessExpression*/
    ) -> Option<StringOrNumber> {
        unimplemented!()
    }

    fn get_referenced_value_declaration(
        &self,
        reference: &Node, /*Identifier*/
    ) -> Option<Rc<Node /*Declaration*/>> {
        unimplemented!()
    }

    fn get_type_reference_serialization_kind(
        &self,
        type_name: &Node, /*EntityName*/
        location: Option<&Node>,
    ) -> TypeReferenceSerializationKind {
        unimplemented!()
    }

    fn is_optional_parameter(&self, node: &Node /*ParameterDeclaration*/) -> bool {
        unimplemented!()
    }

    fn module_exports_some_value(
        &self,
        module_reference_expression: &Node, /*Expression*/
    ) -> bool {
        unimplemented!()
    }

    fn is_arguments_local_binding(&self, node: &Node /*Identifier*/) -> bool {
        unimplemented!()
    }

    fn get_external_module_file_from_declaration(
        &self,
        declaration: &Node, /*ImportEqualsDeclaration | ImportDeclaration | ExportDeclaration | ModuleDeclaration | ImportTypeNode | ImportCall*/
    ) -> Option<Rc<Node /*SourceFile*/>> {
        unimplemented!()
    }

    fn get_type_reference_directives_for_entity_name(
        &self,
        name: &Node, /*EntityNameOrEntityNameExpression*/
    ) -> Option<Vec<String>> {
        unimplemented!()
    }

    fn get_type_reference_directives_for_symbol(
        &self,
        symbol: &Symbol,
        meaning: Option<SymbolFlags>,
    ) -> Option<Vec<String>> {
        unimplemented!()
    }

    fn is_literal_const_declaration(
        &self,
        node: &Node, /*VariableDeclaration | PropertyDeclaration | PropertySignature | ParameterDeclaration*/
    ) -> bool {
        unimplemented!()
    }

    fn get_jsx_factory_entity(&self, location: Option<&Node>) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    fn get_jsx_fragment_factory_entity(
        &self,
        location: Option<&Node>,
    ) -> Option<Rc<Node /*EntityName*/>> {
        unimplemented!()
    }

    fn get_all_accessor_declarations(
        &self,
        declaration: &Node, /*AccessorDeclaration*/
    ) -> AllAccessorDeclarations {
        unimplemented!()
    }

    fn get_symbol_of_external_module_specifier(
        &self,
        node: &Node, /*StringLiteralLike*/
    ) -> Option<Rc<Symbol>> {
        unimplemented!()
    }

    fn is_binding_captured_by_node(
        &self,
        node: &Node,
        decl: &Node, /*VariableDeclaration | BindingElement*/
    ) -> bool {
        unimplemented!()
    }

    fn get_declaration_statements_for_source_file(
        &self,
        node: &Node, /*SourceFile*/
        flags: NodeBuilderFlags,
        tracker: &dyn SymbolTracker,
        bundled: Option<bool>,
    ) -> Option<Vec<Rc<Node /*Statement*/>>> {
        unimplemented!()
    }

    fn is_import_required_by_augmentation(&self, decl: &Node /*ImportDeclaration*/) -> bool {
        unimplemented!()
    }
}

impl EmitResolverDebuggable for EmitResolverCreateResolver {}

pub(super) fn is_not_accessor(declaration: &Node /*Declaration*/) -> bool {
    !is_accessor(declaration)
}

pub(super) fn is_not_overload(declaration: &Node /*Declaration*/) -> bool {
    !matches!(
        declaration.kind(),
        SyntaxKind::FunctionDeclaration | SyntaxKind::MethodDeclaration
    ) || declaration
        .as_function_like_declaration()
        .maybe_body()
        .is_some()
}

pub(super) mod JsxNames {
    use crate::__String;

    lazy_static! {
        pub static ref JSX: __String = __String::new("JSX".to_owned());
    }

    lazy_static! {
        pub static ref IntrinsicElements: __String = __String::new("IntrinsicElements".to_owned());
    }

    lazy_static! {
        pub static ref ElementClass: __String = __String::new("ElementClass".to_owned());
    }

    lazy_static! {
        pub static ref ElementAttributesPropertyNameContainer: __String =
            __String::new("ElementAttributesProperty".to_owned());
    }

    lazy_static! {
        pub static ref ElementChildrenAttributeNameContainer: __String =
            __String::new("ElementChildrenAttribute".to_owned());
    }

    lazy_static! {
        pub static ref Element: __String = __String::new("Element".to_owned());
    }

    lazy_static! {
        pub static ref IntrinsicAttributes: __String =
            __String::new("IntrinsicAttributes".to_owned());
    }

    lazy_static! {
        pub static ref IntrinsicClassAttributes: __String =
            __String::new("IntrinsicClassAttributes".to_owned());
    }

    lazy_static! {
        pub static ref LibraryManagedAttributes: __String =
            __String::new("LibraryManagedAttributes".to_owned());
    }
}

pub(super) fn signature_has_rest_parameter(s: &Signature) -> bool {
    s.flags.intersects(SignatureFlags::HasRestParameter)
}
