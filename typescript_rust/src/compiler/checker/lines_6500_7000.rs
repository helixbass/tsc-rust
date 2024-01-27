use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    io, ptr,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

use super::{
    wrap_symbol_tracker_to_report_for_context, MakeSerializePropertySymbol,
    MakeSerializePropertySymbolCreateProperty, NodeBuilderContext,
};
use crate::{
    cast, continue_if_none, create_empty_exports, create_symbol_table, every, filter,
    find_ancestor, find_index, flat_map, for_each_entry, gc_cell_ref_unwrapped,
    get_effective_modifier_flags, get_factory, get_name_of_declaration, get_symbol_id, group,
    has_scope_marker, has_syntactic_modifier, id_text, indices_of, is_binary_expression,
    is_class_declaration, is_class_expression, is_enum_declaration, is_export_assignment,
    is_export_declaration, is_external_module_augmentation, is_external_module_indicator,
    is_external_or_common_js_module, is_function_declaration, is_global_scope_augmentation,
    is_identifier, is_interface_declaration, is_module_block, is_module_declaration,
    is_named_exports, is_property_access_expression, is_source_file,
    is_string_a_non_contextual_keyword, is_string_literal, is_variable_declaration,
    is_variable_declaration_list, is_variable_statement, length, map, map_defined,
    needs_scope_marker, node_has_name, ordered_remove_item_at, set_text_range_id_node,
    unescape_leading_underscores, AllArenas, HasArena, InArena, InternalSymbolName,
    LiteralLikeNodeInterface, ModifierFlags, Node, NodeArray, NodeArrayOrVec, NodeBuilder,
    NodeBuilderFlags, NodeFlags, NodeInterface, StrOrRcNode, Symbol, SymbolAccessibility,
    SymbolFlags, SymbolId, SymbolInterface, SymbolTable, SymbolTracker, SyntaxKind, TypeChecker,
    TypeInterface,
    OptionInArena, IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory,
};

impl NodeBuilder {
    pub(super) fn get_effective_dot_dot_dot_for_parameter(
        &self,
        _p: Id<Node>, /*ParameterDeclaration*/
    ) -> Option<Id<Node>> {
        unimplemented!()
    }

    pub(super) fn get_name_for_jsdoc_function_parameter(
        &self,
        _p: Id<Node>, /*ParameterDeclaration*/
        _index: usize,
    ) -> Option<Id<Node>> {
        unimplemented!()
    }

    pub(super) fn symbol_table_to_declaration_statements_(
        &self,
        symbol_table: Gc<GcCell<SymbolTable>>,
        context: &NodeBuilderContext,
        bundled: Option<bool>,
    ) -> io::Result<Vec<Id<Node /*Statement*/>>> {
        SymbolTableToDeclarationStatements::new(
            context,
            self.type_checker.clone(),
            self,
            symbol_table,
            bundled,
            self,
        )
        .call()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct SymbolTableToDeclarationStatements {
    pub(super) _rc_wrapper: GcCell<Option<Gc<Self>>>,
    pub(super) bundled: Option<bool>,
    pub(super) type_checker: Gc<TypeChecker>,
    pub(super) context: GcCell<Gc<NodeBuilderContext>>,
    pub(super) node_builder: Gc<NodeBuilder>,
    pub(super) serialize_property_symbol_for_class: GcCell<Option<MakeSerializePropertySymbol>>,
    pub(super) serialize_property_symbol_for_interface_worker:
        GcCell<Option<MakeSerializePropertySymbol>>,
    pub(super) enclosing_declaration: Id<Node>,
    pub(super) results: GcCell<Vec<Id<Node>>>,
    pub(super) visited_symbols: GcCell<HashSet<SymbolId>>,
    pub(super) deferred_privates_stack: GcCell<Vec<HashMap<SymbolId, Id<Symbol>>>>,
    pub(super) oldcontext: Gc<NodeBuilderContext>,
    pub(super) symbol_table: GcCell<Gc<GcCell<SymbolTable>>>,
    #[unsafe_ignore_trace]
    pub(super) adding_declare: Cell<bool>,
}

impl HasArena for SymbolTableToDeclarationStatements {
    fn arena(&self) -> &AllArenas {
        self.type_checker.arena()
    }
}

impl SymbolTableToDeclarationStatements {
    pub fn new(
        context: &NodeBuilderContext,
        type_checker: Gc<TypeChecker>,
        node_builder: &NodeBuilder,
        symbol_table: Gc<GcCell<SymbolTable>>,
        bundled: Option<bool>,
        arena: &impl HasArena,
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
            _rc_wrapper: Default::default(),
            bundled,
            type_checker: type_checker.clone(),
            context: GcCell::new(context.clone()),
            node_builder: node_builder.rc_wrapper(),
            serialize_property_symbol_for_class: Default::default(),
            serialize_property_symbol_for_interface_worker: Default::default(),
            enclosing_declaration: context.enclosing_declaration(),
            results: Default::default(),
            visited_symbols: Default::default(),
            deferred_privates_stack: Default::default(),
            oldcontext: oldcontext.clone(),
            symbol_table: GcCell::new(symbol_table),
            adding_declare: Cell::new(bundled != Some(true)),
        });
        *ret._rc_wrapper.borrow_mut() = Some(ret.clone());
        *ret.serialize_property_symbol_for_class.borrow_mut() =
            Some(ret.make_serialize_property_symbol(
                MakeSerializePropertySymbolCreatePropertyDeclaration::new(),
                SyntaxKind::MethodDeclaration,
                true,
            ));
        *ret.serialize_property_symbol_for_interface_worker
            .borrow_mut() = Some(ret.make_serialize_property_symbol(
            MakeSerializePropertySymbolCreatePropertySignature::new(),
            SyntaxKind::MethodSignature,
            false,
        ));
        context.set_tracker(SymbolTableToDeclarationStatementsSymbolTracker::new(
            oldcontext.tracker(),
            type_checker,
            node_builder.clone(),
            context.clone(),
            ret.clone(),
            arena,
        ));
        context.set_tracker(arena.alloc_symbol_tracker(Box::new(
            wrap_symbol_tracker_to_report_for_context(context.clone(), context.tracker()),
        )));
        ret
    }

    pub fn rc_wrapper(&self) -> Gc<Self> {
        self._rc_wrapper.borrow().clone().unwrap()
    }

    pub fn context(&self) -> Gc<NodeBuilderContext> {
        self.context.borrow().clone()
    }

    pub fn set_context(&self, context: Gc<NodeBuilderContext>) {
        *self.context.borrow_mut() = context;
    }

    pub fn serialize_property_symbol_for_class(&self) -> GcCellRef<MakeSerializePropertySymbol> {
        gc_cell_ref_unwrapped(&self.serialize_property_symbol_for_class)
    }

    pub fn serialize_property_symbol_for_interface_worker(
        &self,
    ) -> GcCellRef<MakeSerializePropertySymbol> {
        gc_cell_ref_unwrapped(&self.serialize_property_symbol_for_interface_worker)
    }

    pub fn results(&self) -> GcCellRef<Vec<Id<Node>>> {
        self.results.borrow()
    }

    pub fn results_mut(&self) -> GcCellRefMut<Vec<Id<Node>>> {
        self.results.borrow_mut()
    }

    pub fn set_results(&self, results: Vec<Id<Node>>) {
        *self.results.borrow_mut() = results;
    }

    pub fn visited_symbols(&self) -> GcCellRef<HashSet<SymbolId>> {
        self.visited_symbols.borrow()
    }

    pub fn visited_symbols_mut(&self) -> GcCellRefMut<HashSet<SymbolId>> {
        self.visited_symbols.borrow_mut()
    }

    pub fn deferred_privates_stack(&self) -> GcCellRef<Vec<HashMap<SymbolId, Id<Symbol>>>> {
        self.deferred_privates_stack.borrow()
    }

    pub fn deferred_privates_stack_mut(&self) -> GcCellRefMut<Vec<HashMap<SymbolId, Id<Symbol>>>> {
        self.deferred_privates_stack.borrow_mut()
    }

    pub fn symbol_table(&self) -> Gc<GcCell<SymbolTable>> {
        self.symbol_table.borrow().clone()
    }

    pub fn set_symbol_table(&self, symbol_table: Gc<GcCell<SymbolTable>>) {
        *self.symbol_table.borrow_mut() = symbol_table;
    }

    pub fn adding_declare(&self) -> bool {
        self.adding_declare.get()
    }

    pub fn set_adding_declare(&self, adding_declare: bool) {
        self.adding_declare.set(adding_declare);
    }

    pub fn call(&self) -> io::Result<Vec<Id<Node>>> {
        for_each_entry(
            &*(*self.symbol_table()).borrow(),
            |&symbol: &Id<Symbol>, name: &String| -> Option<()> {
                let base_name = unescape_leading_underscores(name);
                self.get_internal_symbol_name(symbol, base_name);
                None
            },
        );
        let export_equals = (*self.symbol_table())
            .borrow()
            .get(InternalSymbolName::ExportEquals)
            .cloned();
        if let Some(export_equals) = export_equals.filter(|&export_equals| {
            (*self.symbol_table()).borrow().len() > 1
                && export_equals
                    .ref_(self)
                    .flags()
                    .intersects(SymbolFlags::Alias)
        }) {
            self.set_symbol_table(Gc::new(GcCell::new(create_symbol_table(
                self.type_checker.arena(),
                Option::<&[Id<Symbol>]>::None,
            ))));
            self.symbol_table()
                .borrow_mut()
                .insert(InternalSymbolName::ExportEquals.to_owned(), export_equals);
        }

        self.visit_symbol_table(self.symbol_table(), None, None)?;
        Ok(self.merge_redundant_statements(&{
            let value = self.results().clone();
            value
        }))
    }

    pub(super) fn is_identifier_and_not_undefined(&self, node: Option<Id<Node>>) -> bool {
        matches!(
            node,
            Some(node) if node.ref_(self).kind() == SyntaxKind::Identifier
        )
    }

    pub(super) fn get_names_of_declaration(
        &self,
        statement: Id<Node>, /*Statement*/
    ) -> Vec<Id<Node /*Identifier*/>> {
        if is_variable_statement(&statement.ref_(self)) {
            return statement
                .ref_(self).as_variable_statement()
                .declaration_list
                .ref_(self).as_variable_declaration_list()
                .declarations
                .ref_(self).iter()
                .map(|&declaration| get_name_of_declaration(Some(declaration), self))
                .filter(|&declaration| self.is_identifier_and_not_undefined(declaration))
                .map(Option::unwrap)
                .collect();
        }
        [get_name_of_declaration(Some(statement), self)]
            .into_iter()
            .filter(|&declaration| self.is_identifier_and_not_undefined(declaration))
            .map(Option::unwrap)
            .collect()
    }

    pub(super) fn flatten_export_assigned_namespace(
        &self,
        statements: &[Id<Node /*Statement*/>],
    ) -> Vec<Id<Node>> {
        let export_assignment = statements
            .iter()
            .find(|statement| is_export_assignment(&statement.ref_(self)))
            .copied();
        let ns_index = statements
            .iter()
            .position(|statement| is_module_declaration(&statement.ref_(self)));
        let ns = ns_index.map(|ns_index| statements[ns_index].clone());
        let mut statements = statements.to_owned();
        if let (Some(mut ns), Some(export_assignment)) = (ns, export_assignment) {
            let export_assignment_ref = export_assignment.ref_(self);
            let export_assignment_as_export_assignment = export_assignment_ref.as_export_assignment();
            if is_identifier(&export_assignment_as_export_assignment.expression.ref_(self))
                && is_identifier(&ns.ref_(self).as_module_declaration().name.ref_(self))
                && id_text(&ns.ref_(self).as_module_declaration().name.ref_(self))
                    == id_text(&export_assignment_as_export_assignment.expression.ref_(self))
            {
                if let Some(ns_body) = ns
                    .ref_(self).as_module_declaration()
                    .body
                    .filter(|ns_body| is_module_block(&ns_body.ref_(self)))
                {
                    let excess_exports = filter(&statements, |&s: &Id<Node>| {
                        get_effective_modifier_flags(s, self).intersects(ModifierFlags::Export)
                    });
                    let name = ns.ref_(self).as_module_declaration().name;
                    let mut body = ns_body;
                    if !excess_exports.is_empty() {
                        ns = get_factory(self).update_module_declaration(
                            ns,
                            ns.ref_(self).maybe_decorators(),
                            ns.ref_(self).maybe_modifiers(),
                            ns.ref_(self).as_module_declaration().name,
                            Some({
                                body = get_factory(self).update_module_block(
                                    body,
                                    get_factory(self).create_node_array(
                                        Some({
                                            let mut arg =
                                                ns_body.ref_(self).as_module_block().statements.ref_(self).to_vec();
                                            arg.push(get_factory(self).create_export_declaration(
                                                Option::<Id<NodeArray>>::None,
                                                Option::<Id<NodeArray>>::None,
                                                false,
                                                Some(get_factory(self).create_named_exports(map(
                                                    flat_map(
                                                        Some(&excess_exports),
                                                        |&e: &Id<Node>, _| {
                                                            self.get_names_of_declaration(e)
                                                        },
                                                    ),
                                                    |id: Id<Node>, _| {
                                                        get_factory(self).create_export_specifier(
                                                            false,
                                                            Option::<Id<Node>>::None,
                                                            id,
                                                        )
                                                    },
                                                ))),
                                                None,
                                                None,
                                            ));
                                            arg
                                        }),
                                        None,
                                    ),
                                );
                                body.clone()
                            }),
                        );
                        let mut statements_ = statements[0..ns_index.unwrap()].to_owned();
                        statements_.push(ns.clone());
                        statements_.extend(statements.iter().skip(ns_index.unwrap() + 1).cloned());
                        statements = statements_;
                    }

                    if !statements
                        .iter()
                        .any(|&s| s != ns && node_has_name(s, name, self))
                    {
                        self.set_results(vec![]);
                        let body_ref = body.ref_(self);
                        let body_as_module_block = body_ref.as_module_block();
                        let mixin_export_flag = !body_as_module_block.statements.ref_(self).iter().any(|&s| {
                            has_syntactic_modifier(s, ModifierFlags::Export, self)
                                || is_export_assignment(&s.ref_(self))
                                || is_export_declaration(&s.ref_(self))
                        });
                        body_as_module_block.statements.ref_(self).iter().for_each(|&s| {
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
                            let mut statements = filter(&statements, |&s: &Id<Node>| {
                                s != ns && s != export_assignment
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

    pub(super) fn merge_export_declarations(
        &self,
        statements: &[Id<Node /*Statement*/>],
    ) -> Vec<Id<Node>> {
        let exports = filter(statements, |d: &Id<Node>| {
            is_export_declaration(&d.ref_(self)) && {
                let d_ref = d.ref_(self);
                let d_as_export_declaration = d_ref.as_export_declaration();
                d_as_export_declaration.module_specifier.is_none()
                    && matches!(
                        d_as_export_declaration.export_clause,
                        Some(d_export_clause) if is_named_exports(&d_export_clause.ref_(self))
                    )
            }
        });
        let mut statements = statements.to_owned();
        if length(Some(&exports)) > 1 {
            let non_exports = filter(&statements, |d: &Id<Node>| {
                !is_export_declaration(&d.ref_(self)) || {
                    let d_ref = d.ref_(self);
                    let d_as_export_declaration = d_ref.as_export_declaration();
                    d_as_export_declaration.module_specifier.is_some()
                        || d_as_export_declaration.export_clause.is_none()
                }
            });
            statements = {
                let mut statements = non_exports;
                statements.push(get_factory(self).create_export_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    false,
                    Some(get_factory(self).create_named_exports(flat_map(
                        Some(&exports),
                        |e: &Id<Node>, _| {
                            cast(
                                e.ref_(self).as_export_declaration().export_clause,
                                |export_clause: &Id<Node>| is_named_exports(&export_clause.ref_(self)),
                            )
                            .ref_(self).as_named_exports()
                            .elements
                            .ref_(self).to_vec()
                        },
                    ))),
                    None,
                    None,
                ));
                statements
            };
        }

        let reexports = filter(&statements, |d: &Id<Node>| {
            is_export_declaration(&d.ref_(self)) && {
                let d_ref = d.ref_(self);
                let d_as_export_declaration = d_ref.as_export_declaration();
                d_as_export_declaration.module_specifier.is_some()
                    && matches!(
                        d_as_export_declaration.export_clause,
                        Some(d_export_clause) if is_named_exports(&d_export_clause.ref_(self))
                    )
            }
        });
        if length(Some(&reexports)) > 1 {
            let groups: Vec<Vec<Id<Node>>> = group(
                &reexports,
                |decl: &Id<Node>| {
                    let decl_ref = decl.ref_(self);
                    let decl_as_export_declaration = decl_ref.as_export_declaration();
                    if is_string_literal(
                        &decl_as_export_declaration
                            .module_specifier
                            .unwrap().ref_(self),
                    ) {
                        format!(
                            ">{:?}",
                            &*decl_as_export_declaration
                                .module_specifier
                                .unwrap()
                                .ref_(self).as_string_literal()
                                .text()
                        )
                    } else {
                        ">".to_owned()
                    }
                },
                |values: Vec<Id<Node>>| values,
            );
            if groups.len() != reexports.len() {
                for group in groups {
                    if group.len() > 1 {
                        statements = {
                            let mut statements = filter(&statements, |&s: &Id<Node>| {
                                group
                                    .iter()
                                    .position(|&item: &Id<Node>| item == s)
                                    .is_none()
                            });
                            statements.push(get_factory(self).create_export_declaration(
                                Option::<Id<NodeArray>>::None,
                                Option::<Id<NodeArray>>::None,
                                false,
                                Some(get_factory(self).create_named_exports(flat_map(
                                    Some(&group),
                                    |e: &Id<Node>, _| {
                                        cast(
                                            e.ref_(self).as_export_declaration().export_clause,
                                            |export_clause: &Id<Node>| {
                                                is_named_exports(&export_clause.ref_(self))
                                            },
                                        )
                                        .ref_(self).as_named_exports()
                                        .elements
                                        .ref_(self).to_vec()
                                    },
                                ))),
                                group[0].ref_(self).as_export_declaration().module_specifier,
                                None,
                            ));
                            statements
                        }
                    }
                }
            }
        }
        statements
    }

    pub(super) fn inline_export_modifiers(
        &self,
        mut statements: Vec<Id<Node /*Statement*/>>,
    ) -> Vec<Id<Node>> {
        let index = find_index(
            &statements,
            |d: &Id<Node>, _| {
                is_export_declaration(&d.ref_(self)) && {
                    let d_ref = d.ref_(self);
                    let d_as_export_declaration = d_ref.as_export_declaration();
                    d_as_export_declaration.module_specifier.is_none()
                        && d_as_export_declaration.assert_clause.is_none()
                        && matches!(
                            d_as_export_declaration.export_clause,
                            Some(d_export_clause) if is_named_exports(&d_export_clause.ref_(self))
                        )
                }
            },
            None,
        );
        if let Some(index) = index {
            let export_decl = statements[index];
            let export_decl_ref = export_decl.ref_(self);
            let export_decl_as_export_declaration = export_decl_ref.as_export_declaration();
            let replacements = map_defined(
                Some(
                    &*export_decl_as_export_declaration
                        .export_clause
                        .as_ref()
                        .unwrap()
                        .ref_(self).as_named_exports()
                        .elements.ref_(self),
                ),
                |e: &Id<Node>, _| {
                    let e_ref = e.ref_(self);
                    let e_as_export_specifier = e_ref.as_export_specifier();
                    if e_as_export_specifier.property_name.is_none() {
                        let indices = indices_of(&statements);
                        let associated_indices = filter(&indices, |&i: &usize| {
                            node_has_name(statements[i], e_as_export_specifier.name, self)
                        });
                        if length(Some(&associated_indices)) > 0
                            && every(&associated_indices, |&i: &usize, _| {
                                self.can_have_export_modifier(statements[i])
                            })
                        {
                            for index in associated_indices {
                                statements[index] = self.add_export_modifier(statements[index]);
                            }
                            return None;
                        }
                    }
                    Some(e.clone())
                },
            );
            if replacements.is_empty() {
                ordered_remove_item_at(&mut statements, index);
            } else {
                statements[index] = get_factory(self).update_export_declaration(
                    export_decl,
                    export_decl.ref_(self).maybe_decorators(),
                    export_decl.ref_(self).maybe_modifiers(),
                    export_decl_as_export_declaration.is_type_only,
                    Some(
                        get_factory(self).update_named_exports(
                            export_decl_as_export_declaration
                                .export_clause
                                .unwrap(),
                            replacements,
                        ),
                    ),
                    export_decl_as_export_declaration.module_specifier.clone(),
                    export_decl_as_export_declaration.assert_clause.clone(),
                );
            }
        }
        statements
    }

    pub(super) fn merge_redundant_statements(
        &self,
        statements: &[Id<Node /*Statement*/>],
    ) -> Vec<Id<Node>> {
        let statements = self.flatten_export_assigned_namespace(statements);
        let statements = self.merge_export_declarations(&statements);
        let mut statements = self.inline_export_modifiers(statements);
        if
        /*enclosingDeclaration &&*/
        (is_source_file(&self.enclosing_declaration.ref_(self))
            && is_external_or_common_js_module(&self.enclosing_declaration.ref_(self))
            || is_module_declaration(&self.enclosing_declaration.ref_(self)))
            && (!statements
                .iter()
                .any(|&statement| is_external_module_indicator(statement, self))
                || !has_scope_marker(&statements, self)
                    && statements
                        .iter()
                        .any(|&statement| needs_scope_marker(statement, self)))
        {
            statements.push(create_empty_exports(&get_factory(self)));
        }
        statements
    }

    pub(super) fn can_have_export_modifier(&self, node: Id<Node> /*Statement*/) -> bool {
        is_enum_declaration(&node.ref_(self))
            || is_variable_statement(&node.ref_(self))
            || is_function_declaration(&node.ref_(self))
            || is_class_declaration(&node.ref_(self))
            || is_module_declaration(&node.ref_(self))
                && !is_external_module_augmentation(node, self)
                && !is_global_scope_augmentation(&node.ref_(self))
            || is_interface_declaration(&node.ref_(self))
            || self.type_checker.is_type_declaration(node)
    }

    pub(super) fn add_export_modifier(
        &self,
        node: Id<Node>, /*Extract<HasModifiers, Statement>*/
    ) -> Id<Node> {
        let flags =
            (get_effective_modifier_flags(node, self) | ModifierFlags::Export) & !ModifierFlags::Ambient;
        get_factory(self).update_modifiers(node, flags)
    }

    pub(super) fn remove_export_modifier(
        &self,
        node: Id<Node>, /*Extract<HasModifiers, Statement>*/
    ) -> Id<Node> {
        let flags = get_effective_modifier_flags(node, self) & !ModifierFlags::Export;
        get_factory(self).update_modifiers(node, flags)
    }

    pub(super) fn visit_symbol_table(
        &self,
        symbol_table: Gc<GcCell<SymbolTable>>,
        suppress_new_private_context: Option<bool>,
        property_as_alias: Option<bool>,
    ) -> io::Result<()> {
        if suppress_new_private_context != Some(true) {
            self.deferred_privates_stack_mut().push(Default::default());
        }
        (*symbol_table)
            .borrow()
            .values()
            .try_for_each(|&symbol| -> io::Result<_> {
                self.serialize_symbol(symbol, false, property_as_alias == Some(true))?;

                Ok(())
            })?;
        if suppress_new_private_context != Some(true) {
            let deferred_privates_stack_last_values = {
                let deferred_privates_stack = self.deferred_privates_stack();
                deferred_privates_stack[deferred_privates_stack.len() - 1]
                    .values()
                    .cloned()
                    .collect::<Vec<_>>()
            };
            deferred_privates_stack_last_values.iter().try_for_each(
                |&symbol| -> io::Result<_> {
                    self.serialize_symbol(symbol, true, property_as_alias == Some(true))?;

                    Ok(())
                },
            )?;
            self.deferred_privates_stack_mut().pop();
        }

        Ok(())
    }

    pub(super) fn serialize_symbol(
        &self,
        symbol: Id<Symbol>,
        is_private: bool,
        property_as_alias: bool,
    ) -> io::Result<()> {
        let visited_sym = self.type_checker.get_merged_symbol(Some(symbol)).unwrap();
        if self
            .visited_symbols()
            .contains(&get_symbol_id(&visited_sym.ref_(self)))
        {
            return Ok(());
        }
        self.visited_symbols_mut()
            .insert(get_symbol_id(&visited_sym.ref_(self)));
        let skip_membership_check = !is_private;
        if skip_membership_check
            || length(symbol.ref_(self).maybe_declarations().as_deref()) > 0
                && symbol
                    .ref_(self)
                    .maybe_declarations()
                    .as_ref()
                    .unwrap()
                    .iter()
                    .any(|&d| {
                        find_ancestor(Some(d), |n: Id<Node>| {
                            n == self.enclosing_declaration
                        }, self)
                        .is_some()
                    })
        {
            let old_context = self.context();
            self.set_context(self.node_builder.clone_node_builder_context(self.context()));
            /*const result =*/
            self.serialize_symbol_worker(symbol, is_private, property_as_alias)?;
            if self.context().reported_diagnostic() {
                self.oldcontext
                    .set_reported_diagnostic(self.context().reported_diagnostic());
            }
            self.set_context(old_context);
            return Ok(()) /*result*/;
        }

        Ok(())
    }

    pub(super) fn serialize_symbol_worker(
        &self,
        symbol: Id<Symbol>,
        mut is_private: bool,
        property_as_alias: bool,
    ) -> io::Result<()> {
        let symbol_ref = symbol.ref_(self);
        let symbol_name = unescape_leading_underscores(symbol_ref.escaped_name());
        let is_default = symbol.ref_(self).escaped_name() == InternalSymbolName::Default;
        if is_private
            && self
                .context()
                .flags()
                .intersects(NodeBuilderFlags::AllowAnonymousIdentifier)
            && is_string_a_non_contextual_keyword(symbol_name)
            && !is_default
        {
            self.context().set_encountered_error(true);
            return Ok(());
        }
        let mut needs_post_export_default = is_default
            && (symbol
                .ref_(self)
                .flags()
                .intersects(SymbolFlags::ExportDoesNotSupportDefaultModifier)
                || symbol.ref_(self).flags().intersects(SymbolFlags::Function)
                    && self
                        .type_checker
                        .get_properties_of_type(self.type_checker.get_type_of_symbol(symbol)?)?
                        .len()
                        > 0)
            && !symbol.ref_(self).flags().intersects(SymbolFlags::Alias);
        let mut needs_export_declaration = !needs_post_export_default
            && !is_private
            && is_string_a_non_contextual_keyword(symbol_name)
            && !is_default;
        if needs_post_export_default || needs_export_declaration {
            is_private = true;
        }
        let modifier_flags = (if !is_private {
            ModifierFlags::Export
        } else {
            ModifierFlags::None
        }) | (if is_default && !needs_post_export_default {
            ModifierFlags::Default
        } else {
            ModifierFlags::None
        });
        let is_const_merged_with_ns = symbol.ref_(self).flags().intersects(SymbolFlags::Module)
            && symbol.ref_(self).flags().intersects(
                SymbolFlags::BlockScopedVariable
                    | SymbolFlags::FunctionScopedVariable
                    | SymbolFlags::Property,
            )
            && symbol.ref_(self).escaped_name() != InternalSymbolName::ExportEquals;
        let is_const_merged_with_ns_printable_as_signature_merge = is_const_merged_with_ns
            && self.is_type_representable_as_function_namespace_merge(
                self.type_checker.get_type_of_symbol(symbol)?,
                symbol,
            )?;
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::Function | SymbolFlags::Method)
            || is_const_merged_with_ns_printable_as_signature_merge
        {
            self.serialize_as_function_namespace_merge(
                self.type_checker.get_type_of_symbol(symbol)?,
                symbol,
                &self.get_internal_symbol_name(symbol, symbol_name),
                modifier_flags,
            )?;
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::TypeAlias) {
            self.serialize_type_alias(symbol, symbol_name, modifier_flags)?;
        }
        if symbol.ref_(self).flags().intersects(
            SymbolFlags::BlockScopedVariable
                | SymbolFlags::FunctionScopedVariable
                | SymbolFlags::Property,
        ) && symbol.ref_(self).escaped_name() != InternalSymbolName::ExportEquals
            && !symbol.ref_(self).flags().intersects(SymbolFlags::Prototype)
            && !symbol.ref_(self).flags().intersects(SymbolFlags::Class)
            && !is_const_merged_with_ns_printable_as_signature_merge
        {
            if property_as_alias {
                let created_export = self.serialize_maybe_alias_assignment(symbol)?;
                if created_export {
                    needs_export_declaration = false;
                    needs_post_export_default = false;
                }
            } else {
                let type_ = self.type_checker.get_type_of_symbol(symbol)?;
                let local_name = self.get_internal_symbol_name(symbol, symbol_name);
                if !symbol.ref_(self).flags().intersects(SymbolFlags::Function)
                    && self.is_type_representable_as_function_namespace_merge(type_, symbol)?
                {
                    self.serialize_as_function_namespace_merge(
                        type_,
                        symbol,
                        &local_name,
                        modifier_flags,
                    )?;
                } else {
                    let flags = if !symbol
                        .ref_(self)
                        .flags()
                        .intersects(SymbolFlags::BlockScopedVariable)
                    {
                        None
                    } else if self.type_checker.is_const_variable(symbol) {
                        Some(NodeFlags::Const)
                    } else {
                        Some(NodeFlags::Let)
                    };
                    let name = if needs_post_export_default
                        || !symbol.ref_(self).flags().intersects(SymbolFlags::Property)
                    {
                        local_name.clone()
                    } else {
                        self.get_unused_name(&local_name, Some(symbol))
                    };
                    let mut text_range = symbol.ref_(self).maybe_declarations().as_ref().and_then(
                        |symbol_declarations| {
                            symbol_declarations
                                .iter()
                                .find(|d| is_variable_declaration(&d.ref_(self)))
                                .copied()
                        },
                    );
                    if let Some(text_range_present) = text_range.as_ref().filter(|text_range| {
                        is_variable_declaration_list(&text_range.ref_(self).parent().ref_(self))
                            && text_range
                                .ref_(self).parent()
                                .ref_(self).as_variable_declaration_list()
                                .declarations
                                .ref_(self).len()
                                == 1
                    }) {
                        text_range = text_range_present.ref_(self).parent().ref_(self).maybe_parent();
                    }
                    let property_access_require =
                        symbol.ref_(self).maybe_declarations().as_ref().and_then(
                            |symbol_declarations| {
                                symbol_declarations
                                    .iter()
                                    .find(|declaration| is_property_access_expression(&declaration.ref_(self)))
                                    .copied()
                            },
                        );
                    if let Some(property_access_require) = property_access_require.as_ref().filter(|property_access_require| {
                        let property_access_require_parent = property_access_require.ref_(self).parent();
                        is_binary_expression(&property_access_require_parent.ref_(self))
                            && is_identifier(&property_access_require_parent.ref_(self).as_binary_expression().right.ref_(self)) &&
                            matches!(
                                type_.ref_(self).maybe_symbol().and_then(|type_symbol| type_symbol.ref_(self).maybe_value_declaration()),
                                Some(type_symbol_value_declaration) if is_source_file(&type_symbol_value_declaration.ref_(self))
                            )
                    }) {
                        let property_access_require_parent_right = property_access_require.ref_(self).parent().ref_(self).as_binary_expression().right;
                        let alias = if local_name == property_access_require_parent_right.ref_(self).as_identifier().escaped_text {
                            None
                        } else {
                            Some(property_access_require_parent_right)
                        };
                        self.add_result(
                            get_factory(self).create_export_declaration(
                                Option::<Id<NodeArray>>::None,
                                Option::<Id<NodeArray>>::None,
                                false,
                                Some(get_factory(self).create_named_exports(
                                    vec![
                                        get_factory(self).create_export_specifier(
                                            false,
                                            alias,
                                            &*local_name,
                                        )
                                    ]
                                )),
                                None, None,
                            ),
                            ModifierFlags::None
                        );
                        self.context().tracker_ref().track_symbol(
                            type_.ref_(self).symbol(),
                            self.context().maybe_enclosing_declaration(),
                            SymbolFlags::Value,
                        ).transpose()?;
                    } else {
                        let statement = set_text_range_id_node(
                            get_factory(self).create_variable_statement(
                                Option::<Id<NodeArray>>::None,
                                    get_factory(self).create_variable_declaration_list(
                                        vec![
                                            get_factory(self).create_variable_declaration(
                                                Some(&*name),
                                                None,
                                                Some(self.node_builder.serialize_type_for_declaration(
                                                    &self.context(),
                                                    type_,
                                                    symbol,
                                                    Some(self.enclosing_declaration),
                                                    Some(&|symbol: Id<Symbol>| self.include_private_symbol(symbol)),
                                                    self.bundled,
                                                )?),
                                                None,
                                            )
                                        ],
                                        flags,
                                    )
                            ),
                            text_range.refed(self).as_deref(),
                            self,
                        );
                        self.add_result(
                            statement,
                            if name != local_name {
                                modifier_flags & !ModifierFlags::Export
                            } else {
                                modifier_flags
                            }
                        );
                        if name != local_name && !is_private {
                            self.add_result(
                                get_factory(self).create_export_declaration(
                                        Option::<Id<NodeArray>>::None,
                                        Option::<Id<NodeArray>>::None,
                                        false,
                                        Some(get_factory(self).create_named_exports(
                                            vec![
                                                get_factory(self).create_export_specifier(
                                                    false,
                                                    Some(&*name),
                                                    &*local_name,
                                                )
                                            ]
                                        )),
                                        None, None,
                                    ),
                                ModifierFlags::None,
                            );
                            needs_export_declaration = false;
                            needs_post_export_default = false;
                        }
                    }
                }
            }
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Enum) {
            self.serialize_enum(symbol, symbol_name, modifier_flags)?;
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Class) {
            if symbol.ref_(self).flags().intersects(SymbolFlags::Property)
                && matches!(
                    symbol.ref_(self).maybe_value_declaration(),
                    Some(symbol_value_declaration) if is_binary_expression(&symbol_value_declaration.ref_(self).parent().ref_(self)) &&
                        is_class_expression(&symbol_value_declaration.ref_(self).parent().ref_(self).as_binary_expression().right.ref_(self))
                )
            {
                self.serialize_as_alias(
                    symbol,
                    &self.get_internal_symbol_name(symbol, symbol_name),
                    modifier_flags,
                )?;
            } else {
                self.serialize_as_class(
                    symbol,
                    &self.get_internal_symbol_name(symbol, symbol_name),
                    modifier_flags,
                )?;
            }
        }
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::ValueModule | SymbolFlags::NamespaceModule)
            && (!is_const_merged_with_ns || self.is_type_only_namespace(symbol)?)
            || is_const_merged_with_ns_printable_as_signature_merge
        {
            self.serialize_module(symbol, symbol_name, modifier_flags)?;
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Interface)
            && !symbol.ref_(self).flags().intersects(SymbolFlags::Class)
        {
            self.serialize_interface(symbol, symbol_name, modifier_flags)?;
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Alias) {
            self.serialize_as_alias(
                symbol,
                &self.get_internal_symbol_name(symbol, symbol_name),
                modifier_flags,
            )?;
        }
        if symbol.ref_(self).flags().intersects(SymbolFlags::Property)
            && symbol.ref_(self).escaped_name() == InternalSymbolName::ExportEquals
        {
            self.serialize_maybe_alias_assignment(symbol)?;
        }
        if symbol
            .ref_(self)
            .flags()
            .intersects(SymbolFlags::ExportStar)
        {
            if let Some(symbol_declarations) = symbol.ref_(self).maybe_declarations().as_ref() {
                for &node in symbol_declarations {
                    let resolved_module =
                        continue_if_none!(self.type_checker.resolve_external_module_name_(
                            node,
                            node.ref_(self).as_export_declaration()
                                .module_specifier
                                .unwrap(),
                            None,
                        )?);
                    self.add_result(
                        get_factory(self).create_export_declaration(
                            Option::<Id<NodeArray>>::None,
                            Option::<Id<NodeArray>>::None,
                            false,
                            None,
                            Some(get_factory(self).create_string_literal(
                                self.node_builder.get_specifier_for_module_symbol(
                                    resolved_module,
                                    &self.context(),
                                )?,
                                None,
                                None,
                            )),
                            None,
                        ),
                        ModifierFlags::None,
                    );
                }
            }
        }
        if needs_post_export_default {
            self.add_result(
                get_factory(self).create_export_assignment(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    Some(false),
                    get_factory(self)
                        .create_identifier(&self.get_internal_symbol_name(symbol, symbol_name)),
                ),
                ModifierFlags::None,
            );
        } else if needs_export_declaration {
            self.add_result(
                get_factory(self).create_export_declaration(
                    Option::<Id<NodeArray>>::None,
                    Option::<Id<NodeArray>>::None,
                    false,
                    Some(get_factory(self).create_named_exports(vec![
                        get_factory(self).create_export_specifier(
                            false,
                            Some(&*self.get_internal_symbol_name(symbol, symbol_name)),
                            symbol_name,
                        ),
                    ])),
                    None,
                    None,
                ),
                ModifierFlags::None,
            );
        }

        Ok(())
    }
}

#[derive(Trace, Finalize)]
struct SymbolTableToDeclarationStatementsSymbolTracker {
    #[unsafe_ignore_trace]
    is_track_symbol_disabled: Cell<bool>,
    oldcontext_tracker: Id<Box<dyn SymbolTracker>>,
    type_checker: Gc<TypeChecker>,
    node_builder: NodeBuilder,
    context: Gc<NodeBuilderContext>,
    symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
}

impl SymbolTableToDeclarationStatementsSymbolTracker {
    fn new(
        oldcontext_tracker: Id<Box<dyn SymbolTracker>>,
        type_checker: Gc<TypeChecker>,
        node_builder: NodeBuilder,
        context: Gc<NodeBuilderContext>,
        symbol_table_to_declaration_statements: Gc<SymbolTableToDeclarationStatements>,
        arena: &impl HasArena,
    ) -> Id<Box<dyn SymbolTracker>> {
        arena.alloc_symbol_tracker(Box::new(Self {
            is_track_symbol_disabled: Default::default(),
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
        sym: Id<Symbol>,
        decl: Option<Id<Node>>,
        meaning: SymbolFlags,
    ) -> Option<io::Result<bool>> {
        if self.is_track_symbol_disabled.get() {
            return Some(Ok(false));
        }
        let accessible_result =
            self.type_checker
                .is_symbol_accessible(Some(sym), decl, meaning, false);
        let accessible_result = match accessible_result {
            Err(accessible_result) => return Some(Err(accessible_result)),
            Ok(accessible_result) => accessible_result,
        };
        if accessible_result.accessibility == SymbolAccessibility::Accessible {
            let chain = match self.node_builder.lookup_symbol_chain_worker(
                sym,
                &self.context,
                Some(meaning),
                None,
            ) {
                Err(err) => return Some(Err(err)),
                Ok(chain) => chain,
            };
            if !sym.ref_(self).flags().intersects(SymbolFlags::Property) {
                self.symbol_table_to_declaration_statements
                    .include_private_symbol(chain[0]);
            }
        } else if
        /*oldcontext.tracker && */
        self.oldcontext_tracker.ref_(self).is_track_symbol_supported() {
            return self.oldcontext_tracker.ref_(self).track_symbol(sym, decl, meaning);
        }
        Some(Ok(false))
    }

    fn disable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(true);
    }

    fn reenable_track_symbol(&self) {
        self.is_track_symbol_disabled.set(false);
    }

    fn is_report_inaccessible_this_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_inaccessible_this_error_supported()
    }

    fn is_report_private_in_base_of_class_expression_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_private_in_base_of_class_expression_supported()
    }

    fn is_report_inaccessible_unique_symbol_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_inaccessible_unique_symbol_error_supported()
    }

    fn is_report_cyclic_structure_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_cyclic_structure_error_supported()
    }

    fn is_report_likely_unsafe_import_required_error_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_likely_unsafe_import_required_error_supported()
    }

    fn is_module_resolver_host_supported(&self) -> bool {
        self.oldcontext_tracker.ref_(self).is_module_resolver_host_supported()
    }

    fn is_track_referenced_ambient_module_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_track_referenced_ambient_module_supported()
    }

    fn is_report_nonlocal_augmentation_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_nonlocal_augmentation_supported()
    }

    fn is_report_non_serializable_property_supported(&self) -> bool {
        self.oldcontext_tracker
            .ref_(self).is_report_non_serializable_property_supported()
    }

    fn report_inaccessible_this_error(&self) {
        self.oldcontext_tracker.ref_(self).report_inaccessible_this_error()
    }

    fn report_private_in_base_of_class_expression(&self, property_name: &str) {
        self.oldcontext_tracker
            .ref_(self).report_private_in_base_of_class_expression(property_name)
    }

    fn report_inaccessible_unique_symbol_error(&self) {
        self.oldcontext_tracker
            .ref_(self).report_inaccessible_unique_symbol_error()
    }

    fn report_cyclic_structure_error(&self) {
        self.oldcontext_tracker.ref_(self).report_cyclic_structure_error()
    }

    fn report_likely_unsafe_import_required_error(&self, specifier: &str) {
        self.oldcontext_tracker
            .ref_(self).report_likely_unsafe_import_required_error(specifier)
    }

    fn report_truncation_error(&self) {
        self.oldcontext_tracker.ref_(self).report_truncation_error()
    }

    fn module_resolver_host(
        &self,
    ) -> Option<IdForModuleSpecifierResolutionHostAndGetCommonSourceDirectory> {
        self.oldcontext_tracker.ref_(self).module_resolver_host()
    }

    fn track_referenced_ambient_module(
        &self,
        decl: Id<Node>, /*ModuleDeclaration*/
        symbol: Id<Symbol>,
    ) -> io::Result<()> {
        self.oldcontext_tracker
            .ref_(self).track_referenced_ambient_module(decl, symbol)
    }

    fn track_external_module_symbol_of_import_type_node(&self, symbol: Id<Symbol>) {
        self.oldcontext_tracker
            .ref_(self).track_external_module_symbol_of_import_type_node(symbol)
    }

    fn report_nonlocal_augmentation(
        &self,
        containing_file: Id<Node>, /*SourceFile*/
        parent_symbol: Id<Symbol>,
        augmenting_symbol: Id<Symbol>,
    ) {
        self.oldcontext_tracker.ref_(self).report_nonlocal_augmentation(
            containing_file,
            parent_symbol,
            augmenting_symbol,
        )
    }

    fn report_non_serializable_property(&self, property_name: &str) {
        self.oldcontext_tracker
            .ref_(self).report_non_serializable_property(property_name)
    }
}

impl HasArena for SymbolTableToDeclarationStatementsSymbolTracker {
    fn arena(&self) -> &AllArenas {
        self.type_checker.arena()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbolCreatePropertyDeclaration;

impl MakeSerializePropertySymbolCreatePropertyDeclaration {
    pub fn new() -> Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        Gc::new(Box::new(Self))
    }
}

impl MakeSerializePropertySymbolCreateProperty
    for MakeSerializePropertySymbolCreatePropertyDeclaration
{
    fn call(
        &self,
        decorators: Option<NodeArrayOrVec /*Decorator*/>,
        modifiers: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question_or_exclamation_token: Option<Id<Node /*QuestionToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        get_factory(self).create_property_declaration(
            decorators,
            modifiers,
            name,
            question_or_exclamation_token,
            type_,
            initializer,
        )
    }
}

impl HasArena for MakeSerializePropertySymbolCreatePropertyDeclaration {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
pub(super) struct MakeSerializePropertySymbolCreatePropertySignature;

impl MakeSerializePropertySymbolCreatePropertySignature {
    pub fn new() -> Gc<Box<dyn MakeSerializePropertySymbolCreateProperty>> {
        Gc::new(Box::new(Self))
    }
}

impl MakeSerializePropertySymbolCreateProperty
    for MakeSerializePropertySymbolCreatePropertySignature
{
    fn call(
        &self,
        _decorators: Option<NodeArrayOrVec /*Decorator*/>,
        mods: Option<NodeArrayOrVec /*Modifier*/>,
        name: StrOrRcNode<'_>, /*PropertyName*/
        question: Option<Id<Node /*QuestionToken*/>>,
        type_: Option<Id<Node /*TypeNode*/>>,
        _initializer: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node> {
        get_factory(self).create_property_signature(mods, name, question, type_)
    }
}

impl HasArena for MakeSerializePropertySymbolCreatePropertySignature {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}
