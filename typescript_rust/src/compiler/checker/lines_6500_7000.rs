#![allow(non_upper_case_globals)]

use gc::{Finalize, Gc, GcCell, GcCellRef, Trace};
use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::{wrap_symbol_tracker_to_report_for_context, NodeBuilderContext};
use crate::{
    create_symbol_table, filter, flat_map, for_each_entry, get_effective_modifier_flags,
    get_name_of_declaration, has_syntactic_modifier, id_text, is_export_assignment,
    is_export_declaration, is_identifier, is_module_block, is_module_declaration,
    is_variable_statement, map, node_has_name, unescape_leading_underscores,
    using_single_line_string_writer, with_synthetic_factory_and_factory, EmitTextWriter,
    InternalSymbolName, ModifierFlags, Node, NodeArray, NodeBuilder, NodeInterface, Symbol,
    SymbolAccessibility, SymbolFlags, SymbolId, SymbolInterface, SymbolTable, SymbolTracker,
    SyntaxKind, TypeChecker, TypeFormatFlags, TypePredicate,
};

impl NodeBuilder {
    pub(super) fn get_effective_dot_dot_dot_for_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn get_name_for_jsdoc_function_parameter(
        &self,
        p: &Node, /*ParameterDeclaration*/
        index: usize,
    ) -> Option<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn symbol_table_to_declaration_statements_(
        &self,
        symbol_table: Gc<GcCell<SymbolTable>>,
        context: &NodeBuilderContext,
        bundled: Option<bool>,
    ) -> Vec<Gc<Node /*Statement*/>> {
        SymbolTableToDeclarationStatements::new(
            context,
            self.type_checker.clone(),
            self,
            symbol_table,
            bundled,
        )
        .call()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct SymbolTableToDeclarationStatements {
    pub(super) enclosing_declaration: Gc<Node>,
    pub(super) results: GcCell<Vec<Gc<Node>>>,
    pub(super) visited_symbols: GcCell<HashSet<SymbolId>>,
    pub(super) deferred_privates_stack: GcCell<Vec<HashMap<SymbolId, Gc<Symbol>>>>,
    pub(super) oldcontext: Gc<NodeBuilderContext>,
    pub(super) symbol_table: GcCell<Gc<GcCell<SymbolTable>>>,
    #[unsafe_ignore_trace]
    pub(super) adding_declare: Cell<bool>,
}

impl SymbolTableToDeclarationStatements {
    pub fn new(
        context: &NodeBuilderContext,
        type_checker: Gc<TypeChecker>,
        node_builder: &NodeBuilder,
        symbol_table: Gc<GcCell<SymbolTable>>,
        bundled: Option<bool>,
    ) -> Gc<Self> {
        let oldcontext = context.rc_wrapper();
        let mut context = context.clone();
        context.used_symbol_names = Rc::new(RefCell::new(Some(
            match oldcontext.maybe_used_symbol_names().as_ref() {
                None => Default::default(),
                Some(oldcontext_used_symbol_names) => oldcontext_used_symbol_names.clone(),
            },
        )));
        context.remapped_symbol_names = Rc::new(RefCell::new(Some(Default::default())));
        let context = Gc::new(context);
        let ret = Gc::new(Self {
            enclosing_declaration: context.enclosing_declaration(),
            results: Default::default(),
            visited_symbols: Default::default(),
            deferred_privates_stack: Default::default(),
            oldcontext: oldcontext.clone(),
            symbol_table: GcCell::new(symbol_table),
            adding_declare: Cell::new(bundled != Some(true)),
        });
        context.set_tracker(SymbolTableToDeclarationStatementsSymbolTracker::new(
            oldcontext.tracker(),
            type_checker,
            node_builder.clone(),
            context.clone(),
            ret.clone(),
        ));
        context.set_tracker(Gc::new(Box::new(
            wrap_symbol_tracker_to_report_for_context(context.clone(), context.tracker()),
        )));
        ret
    }

    pub fn results(&self) -> GcCellRef<Vec<Gc<Node>>> {
        self.results.borrow()
    }

    pub fn set_results(&self, results: Vec<Gc<Node>>) {
        *self.results.borrow_mut() = results;
    }

    pub fn symbol_table(&self) -> Gc<GcCell<SymbolTable>> {
        self.symbol_table.borrow().clone()
    }

    pub fn set_symbol_table(&self, symbol_table: Gc<GcCell<SymbolTable>>) {
        *self.symbol_table.borrow_mut() = symbol_table;
    }

    pub fn call(&self) -> Vec<Gc<Node>> {
        for_each_entry(
            &*(*self.symbol_table()).borrow(),
            |symbol: &Gc<Symbol>, name: &String| -> Option<()> {
                let base_name = unescape_leading_underscores(name);
                self.get_internal_symbol_name(symbol, base_name);
                None
            },
        );
        let export_equals = (*self.symbol_table())
            .borrow()
            .get(InternalSymbolName::ExportEquals)
            .cloned();
        if let Some(export_equals) = export_equals.filter(|export_equals| {
            (*self.symbol_table()).borrow().len() > 1
                && export_equals.flags().intersects(SymbolFlags::Alias)
        }) {
            self.set_symbol_table(Gc::new(GcCell::new(create_symbol_table(None))));
            self.symbol_table()
                .borrow_mut()
                .insert(InternalSymbolName::ExportEquals.to_owned(), export_equals);
        }

        self.visit_symbol_table(self.symbol_table(), None, None);
        self.merge_redundant_statements(&self.results())
    }

    pub(super) fn is_identifier_and_not_undefined(&self, node: Option<impl Borrow<Node>>) -> bool {
        matches!(
            node,
            Some(node) if node.borrow().kind() == SyntaxKind::Identifier
        )
    }

    pub(super) fn get_names_of_declaration(
        &self,
        statement: &Node, /*Statement*/
    ) -> Vec<Gc<Node /*Identifier*/>> {
        if is_variable_statement(statement) {
            return statement
                .as_variable_statement()
                .declaration_list
                .as_variable_declaration_list()
                .declarations
                .iter()
                .map(|declaration| get_name_of_declaration(Some(&**declaration)))
                .filter(|declaration| self.is_identifier_and_not_undefined(declaration.as_deref()))
                .map(Option::unwrap)
                .collect();
        }
        [get_name_of_declaration(Some(statement))]
            .into_iter()
            .filter(|declaration| self.is_identifier_and_not_undefined(declaration.as_deref()))
            .map(Option::unwrap)
            .collect()
    }

    pub(super) fn flatten_export_assigned_namespace(
        &self,
        statements: &[Gc<Node /*Statement*/>],
    ) -> Vec<Gc<Node>> {
        let export_assignment = statements
            .iter()
            .find(|statement| is_export_assignment(statement));
        let ns_index = statements
            .iter()
            .position(|statement| is_module_declaration(statement));
        let ns = ns_index.map(|ns_index| statements[ns_index].clone());
        let mut statements = statements.to_owned();
        if let (Some(mut ns), Some(export_assignment)) = (ns, export_assignment) {
            let export_assignment_as_export_assignment = export_assignment.as_export_assignment();
            if is_identifier(&export_assignment_as_export_assignment.expression)
                && is_identifier(&ns.as_module_declaration().name)
                && id_text(&ns.as_module_declaration().name)
                    == id_text(&export_assignment_as_export_assignment.expression)
            {
                if let Some(ns_body) = ns
                    .as_module_declaration()
                    .body
                    .as_ref()
                    .filter(|ns_body| is_module_block(ns_body))
                {
                    let excess_exports = filter(&statements, |s: &Gc<Node>| {
                        get_effective_modifier_flags(s).intersects(ModifierFlags::Export)
                    });
                    let ref name = ns.as_module_declaration().name.clone();
                    let mut body = ns_body.clone();
                    if !excess_exports.is_empty() {
                        ns = with_synthetic_factory_and_factory(|synthetic_factory_, factory_| {
                            factory_.update_module_declaration(
                                synthetic_factory_,
                                &ns,
                                ns.maybe_decorators(),
                                ns.maybe_modifiers(),
                                ns.as_module_declaration().name.clone(),
                                Some({
                                    body = factory_.update_module_block(
                                        synthetic_factory_,
                                        &body,
                                        factory_.create_node_array(
                                            Some({
                                                let mut arg = ns_body.as_module_block().statements.to_vec();
                                                arg.push(
                                                    factory_.create_export_declaration(
                                                        synthetic_factory_,
                                                        Option::<Gc<NodeArray>>::None,
                                                        Option::<Gc<NodeArray>>::None,
                                                        false,
                                                        Some(factory_.create_named_exports(
                                                            synthetic_factory_,
                                                            map(
                                                                flat_map(
                                                                    Some(&excess_exports),
                                                                    |e: &Gc<Node>, _| self.get_names_of_declaration(e)
                                                                ),
                                                                |id: Gc<Node>, _| {
                                                                    Gc::<Node>::from(factory_.create_export_specifier(
                                                                        synthetic_factory_,
                                                                        false,
                                                                        Option::<Gc<Node>>::None,
                                                                        id,
                                                                    ))
                                                                }
                                                            )
                                                        ).into()),
                                                        None,
                                                        None,
                                                    ).into()
                                                );
                                                arg
                                            }),
                                            None,
                                        )
                                    );
                                    body.clone()
                                })
                            )
                        });
                        let mut statements_ = statements[0..ns_index.unwrap()].to_owned();
                        statements_.push(ns.clone());
                        statements_.extend(statements.iter().skip(ns_index.unwrap() + 1).cloned());
                        statements = statements_;
                    }

                    if !statements
                        .iter()
                        .any(|s| !Gc::ptr_eq(s, &ns) && node_has_name(s, name))
                    {
                        self.set_results(vec![]);
                        let body_as_module_block = body.as_module_block();
                        let mixin_export_flag = !body_as_module_block.statements.iter().any(|s| {
                            has_syntactic_modifier(s, ModifierFlags::Export)
                                || is_export_assignment(s)
                                || is_export_declaration(s)
                        });
                        body_as_module_block.statements.iter().for_each(|s| {
                            self.add_result(
                                s,
                                if mixin_export_flag {
                                    ModifierFlags::Export
                                } else {
                                    ModifierFlags::None
                                },
                            );
                        });
                        statements = {
                            let mut statements = filter(&statements, |s: &Gc<Node>| {
                                !Gc::ptr_eq(s, &ns) && !Gc::ptr_eq(s, export_assignment)
                            });
                            statements.extend(self.results().iter().cloned());
                            statements
                        };
                    }
                }
            }
        }
        statements
    }

    pub(super) fn merge_redundant_statements(
        &self,
        statements: &[Gc<Node /*Statement*/>],
    ) -> Vec<Gc<Node>> {
        unimplemented!()
    }

    pub(super) fn visit_symbol_table(
        &self,
        symbol_table: Gc<GcCell<SymbolTable>>,
        suppress_new_private_context: Option<bool>,
        property_as_alias: Option<bool>,
    ) {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct SymbolTableToDeclarationStatementsSymbolTracker {
    oldcontext_tracker: Gc<Box<dyn SymbolTracker>>,
    type_checker: Gc<TypeChecker>,
    node_builder: NodeBuilder,
    context: Gc<NodeBuilderContext>,
    symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
}

impl SymbolTableToDeclarationStatementsSymbolTracker {
    fn new(
        oldcontext_tracker: Gc<Box<dyn SymbolTracker>>,
        type_checker: Gc<TypeChecker>,
        node_builder: NodeBuilder,
        context: Gc<NodeBuilderContext>,
        symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
    ) -> Gc<Box<dyn SymbolTracker>> {
        Gc::new(Box::new(Self {
            oldcontext_tracker,
            type_checker,
            node_builder,
            context,
            symbol_table_to_declaration_statements,
        }))
    }
}

impl SymbolTracker for SymbolTableToDeclarationStatementsSymbolTracker {
    fn is_track_symbol_supported(&self) -> bool {
        true
    }

    fn track_symbol(
        &self,
        sym: &Symbol,
        decl: Option<Gc<Node>>,
        meaning: SymbolFlags,
    ) -> Option<bool> {
        let accessible_result =
            self.type_checker
                .is_symbol_accessible(Some(sym), decl.as_deref(), meaning, false);
        if accessible_result.accessibility == SymbolAccessibility::Accessible {
            let chain = self.node_builder.lookup_symbol_chain_worker(
                sym,
                &self.context,
                Some(meaning),
                None,
            );
            if !sym.flags().intersects(SymbolFlags::Property) {
                self.symbol_table_to_declaration_statements
                    .include_private_symbol(&chain[0]);
            }
        } else if
        /*oldcontext.tracker && */
        self.oldcontext_tracker.is_track_symbol_supported() {
            return self.oldcontext_tracker.track_symbol(sym, decl, meaning);
        }
        Some(false)
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_inaccessible_this_error_supported()
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_private_in_base_of_class_expression_supported()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_inaccessible_unique_symbol_error_supported()
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_cyclic_structure_error_supported()
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_likely_unsafe_import_required_error_supported()
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.oldcontext_tracker.is_module_resolver_host_supported()
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_track_referenced_ambient_module_supported()
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_nonlocal_augmentation_supported()
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        self.oldcontext_tracker
            .is_report_non_serializable_property_supported()
    }

    fn report_inaccessible_this_error(&self) {
        self.oldcontext_tracker.report_inaccessible_this_error()
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        self.oldcontext_tracker
            .report_private_in_base_of_class_expression(property_name)
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        self.oldcontext_tracker
            .report_inaccessible_unique_symbol_error()
    }

    fn report_cyclic_structure_error(&self) {
        self.oldcontext_tracker.report_cyclic_structure_error()
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        self.oldcontext_tracker
            .report_likely_unsafe_import_required_error(specifier)
    }

    fn report_truncation_error(&self) {
        self.oldcontext_tracker.report_truncation_error()
    }

    fn module_resolver_host(
        &self,
    ) -> Option<&dyn crate::ModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.oldcontext_tracker.module_resolver_host()
    }

    fn track_referenced_ambient_module(
        &self,
        decl: &Node, /*ModuleDeclaration*/
        symbol: &Symbol,
    ) {
        self.oldcontext_tracker
            .track_referenced_ambient_module(decl, symbol)
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: &Symbol) {
        self.oldcontext_tracker
            .track_external_module_symbol_of_import_type_node(symbol)
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: &Node, /*SourceFile*/
        parent_symbol: &Symbol,
        augmenting_symbol: &Symbol,
    ) {
        self.oldcontext_tracker.report_nonlocal_augmentation(
            containing_file,
            parent_symbol,
            augmenting_symbol,
        )
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        self.oldcontext_tracker
            .report_non_serializable_property(property_name)
    }
}

impl TypeChecker {
    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Gc<Box<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        if let Some(writer) = writer {
            self.type_predicate_to_string_worker(
                type_predicate,
                enclosing_declaration,
                flags,
                writer.clone(),
            );
            writer.get_text()
        } else {
            using_single_line_string_writer(|writer| {
                self.type_predicate_to_string_worker(
                    type_predicate,
                    enclosing_declaration,
                    flags,
                    writer,
                )
            })
        }
    }
}
