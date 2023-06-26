use std::{borrow::Borrow, collections::HashMap, io};

use gc::{Finalize, Gc, Trace};

use crate::{
    get_node_id, get_original_node, maybe_get_original_node, BaseNodeFactory, CompilerOptions,
    EmitResolver, GetOrInsertDefault, HasStatementsInterface, NamedDeclarationInterface, Node,
    NodeFactory, NodeId, NodeInterface, OptionTry, SyntaxKind, TransformationContext, Transformer,
    VisitResult, WrapCustomTransformerFactoryHandleDefault, _d, cast,
    create_external_helpers_import_declaration_if_needed, create_multi_map, has_syntactic_modifier,
    id_text, is_binding_pattern, is_generated_identifier, is_named_exports, is_omitted_expression,
    ModifierFlags, MultiMap,
};

pub fn get_original_node_id(node: &Node) -> NodeId {
    let node = get_original_node(node);
    get_node_id(&node)
}

pub fn maybe_get_original_node_id(node: Option<impl Borrow<Node>>) -> NodeId {
    let node = maybe_get_original_node(node);
    if let Some(node) = node {
        get_node_id(&node)
    } else {
        0
    }
}

#[derive(Trace, Finalize)]
pub struct ExternalModuleInfo {
    pub external_imports:
        Vec<Gc<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>>,
    pub external_helpers_import_declaration: Option<Gc<Node /*ImportDeclaration*/>>,
    pub export_specifiers: HashMap<String, Vec<Gc<Node /*ExportSpecifier*/>>>,
    pub exported_bindings: HashMap<NodeId, Vec<Gc<Node /*Identifier*/>>>,
    pub exported_names: Option<Vec<Gc<Node /*Identifier*/>>>,
    pub export_equals: Option<Gc<Node /*ExportAssignment*/>>,
    pub has_export_stars_to_export_values: bool,
}

// TODO: does chain_bundle() need to accept any CoreTnansformationContext's that aren't TransformationContext's?
// pub fn chain_bundle<
//     TBaseNodeFactory: BaseNodeFactory,
//     TContext: CoreTransformationContext<TBaseNodeFactory>,
// >(
//     context: Rc<TContext>,
//     transform_source_file: Transformer,
// ) -> Transformer {

#[derive(Trace, Finalize)]
struct ChainBundle;

impl WrapCustomTransformerFactoryHandleDefault for ChainBundle {
    fn call(
        &self,
        _context: Gc<Box<dyn TransformationContext>>,
        transform_source_file: Transformer,
    ) -> Transformer {
        transform_source_file
    }
}

pub fn chain_bundle() -> Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    thread_local! {
        static CHAIN_BUNDLE: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> = Gc::new(Box::new(ChainBundle));
    }
    CHAIN_BUNDLE.with(|chain_bundle| chain_bundle.clone())
}

pub fn get_export_needs_import_star_helper(_node: &Node /*ExportDeclaration*/) -> bool {
    unimplemented!()
}

pub fn get_import_needs_import_star_helper(_node: &Node /*ImportDeclaration*/) -> bool {
    unimplemented!()
}

pub fn get_import_needs_import_default_helper(_node: &Node /*ImportDeclaration*/) -> bool {
    unimplemented!()
}

pub fn collect_external_module_info(
    context: &dyn TransformationContext,
    source_file: &Node, /*SourceFile*/
    resolver: &dyn EmitResolver,
    compiler_options: &CompilerOptions,
) -> io::Result<ExternalModuleInfo> {
    let source_file_as_source_file = source_file.as_source_file();
    let mut external_imports: Vec<
        Gc<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>,
    > = _d();
    let mut export_specifiers: MultiMap<String, Gc<Node /*ExportSpecifier*/>> = create_multi_map();
    let mut exported_bindings: HashMap<NodeId, Vec<Gc<Node /*Identifier*/>>> = _d();
    let mut unique_exports: HashMap<String, bool> = _d();
    let mut exported_names: Option<Vec<Gc<Node /*Identifier*/>>> = _d();
    let mut has_export_default = false;
    let mut export_equals: Option<Gc<Node /*ExportAssignment*/>> = _d();
    let mut has_export_stars_to_export_values = false;
    let mut has_import_star = false;
    let mut has_import_default = false;

    for node in &source_file_as_source_file.statements() {
        match node.kind() {
            SyntaxKind::ImportDeclaration => {
                external_imports.push(node.clone());
                if !has_import_star && get_import_needs_import_star_helper(node) {
                    has_import_star = true;
                }
                if !has_import_default && get_import_needs_import_default_helper(node) {
                    has_import_default = true;
                }
            }
            SyntaxKind::ImportEqualsDeclaration => {
                let node_as_import_equals_declaration = node.as_import_equals_declaration();
                if node_as_import_equals_declaration.module_reference.kind()
                    == SyntaxKind::ExternalModuleReference
                {
                    external_imports.push(node.clone());
                }
            }
            SyntaxKind::ExportDeclaration => {
                let node_as_export_declaration = node.as_export_declaration();
                if node_as_export_declaration.module_specifier.is_some() {
                    match node_as_export_declaration.export_clause.as_ref() {
                        None => {
                            external_imports.push(node.clone());
                            has_export_stars_to_export_values = true;
                        }
                        Some(node_export_clause) => {
                            external_imports.push(node.clone());
                            if is_named_exports(node_export_clause) {
                                add_exported_names_for_export_declaration(
                                    &mut unique_exports,
                                    &mut export_specifiers,
                                    resolver,
                                    &mut exported_bindings,
                                    &mut exported_names,
                                    node,
                                )?;
                            } else {
                                let name = &node_export_clause.as_namespace_export().name;
                                if unique_exports.get(id_text(name)).copied() != Some(true) {
                                    exported_bindings
                                        .entry(get_original_node_id(node))
                                        .or_default()
                                        .push(name.clone());
                                    unique_exports.insert(id_text(name).to_owned(), true);
                                    exported_names.get_or_insert_default_().push(name.clone());
                                }
                                has_import_star = true;
                            }
                        }
                    }
                } else {
                    add_exported_names_for_export_declaration(
                        &mut unique_exports,
                        &mut export_specifiers,
                        resolver,
                        &mut exported_bindings,
                        &mut exported_names,
                        node,
                    )?;
                }
            }
            SyntaxKind::ExportAssignment => {
                let node_as_export_assignment = node.as_export_assignment();
                if node_as_export_assignment.is_export_equals == Some(true)
                    && export_equals.is_none()
                {
                    export_equals = Some(node.clone());
                }
            }
            SyntaxKind::VariableStatement => {
                if has_syntactic_modifier(node, ModifierFlags::Export) {
                    for decl in &node
                        .as_variable_statement()
                        .declaration_list
                        .as_variable_declaration_list()
                        .declarations
                    {
                        collect_exported_variable_info(
                            decl,
                            &mut unique_exports,
                            &mut exported_names,
                        );
                    }
                }
            }
            SyntaxKind::FunctionDeclaration => {
                if has_syntactic_modifier(node, ModifierFlags::Export) {
                    if has_syntactic_modifier(node, ModifierFlags::Default) {
                        if !has_export_default {
                            exported_bindings
                                .entry(get_original_node_id(node))
                                .or_default()
                                .push(context.factory().get_declaration_name(
                                    Some(&**node),
                                    None,
                                    None,
                                ));
                            has_export_default = true;
                        }
                    } else {
                        let name = node.as_function_declaration().name();
                        if unique_exports.get(id_text(&name)).copied() != Some(true) {
                            exported_bindings
                                .entry(get_original_node_id(node))
                                .or_default()
                                .push(name.clone());
                            unique_exports.insert(id_text(&name).to_owned(), true);
                            exported_names.get_or_insert_default_().push(name);
                        }
                    }
                }
            }
            SyntaxKind::ClassDeclaration => {
                if has_syntactic_modifier(node, ModifierFlags::Export) {
                    if has_syntactic_modifier(node, ModifierFlags::Default) {
                        if !has_export_default {
                            exported_bindings
                                .entry(get_original_node_id(node))
                                .or_default()
                                .push(context.factory().get_declaration_name(
                                    Some(&**node),
                                    None,
                                    None,
                                ));
                            has_export_default = true;
                        }
                    } else {
                        let name = node.as_class_declaration().maybe_name();
                        if let Some(name) = name
                            .filter(|name| unique_exports.get(id_text(name)).copied() != Some(true))
                        {
                            exported_bindings
                                .entry(get_original_node_id(node))
                                .or_default()
                                .push(name.clone());
                            unique_exports.insert(id_text(&name).to_owned(), true);
                            exported_names.get_or_insert_default_().push(name);
                        }
                    }
                }
            }
            _ => (),
        }
    }

    let external_helpers_import_declaration = create_external_helpers_import_declaration_if_needed(
        &context.factory(),
        &context.get_emit_helper_factory(),
        source_file,
        compiler_options,
        Some(has_export_stars_to_export_values),
        Some(has_import_star),
        Some(has_import_default),
    );
    if let Some(external_helpers_import_declaration) = external_helpers_import_declaration.as_ref()
    {
        external_imports.insert(0, external_helpers_import_declaration.clone());
    }

    Ok(ExternalModuleInfo {
        external_imports,
        export_specifiers: export_specifiers.0,
        export_equals,
        has_export_stars_to_export_values,
        exported_bindings,
        exported_names,
        external_helpers_import_declaration,
    })
}

fn add_exported_names_for_export_declaration(
    unique_exports: &mut HashMap<String, bool>,
    export_specifiers: &mut MultiMap<String, Gc<Node /*ExportSpecifier*/>>,
    resolver: &dyn EmitResolver,
    exported_bindings: &mut HashMap<NodeId, Vec<Gc<Node /*Identifier*/>>>,
    exported_names: &mut Option<Vec<Gc<Node /*Identifier*/>>>,
    node: &Node, /*ExportDeclaration*/
) -> io::Result<()> {
    let node_as_export_declaration = node.as_export_declaration();
    for specifier in &cast(
        node_as_export_declaration.export_clause.as_ref(),
        |node: &&Gc<Node>| is_named_exports(node),
    )
    .as_named_exports()
    .elements
    {
        let specifier_as_export_specifier = specifier.as_export_specifier();
        if unique_exports
            .get(id_text(&specifier_as_export_specifier.name))
            .copied()
            != Some(true)
        {
            let name = specifier_as_export_specifier
                .property_name
                .as_ref()
                .unwrap_or(&specifier_as_export_specifier.name);
            if node_as_export_declaration.module_specifier.is_none() {
                export_specifiers.add(id_text(name).to_owned(), specifier.clone());
            }

            let decl = resolver
                .get_referenced_import_declaration(name)?
                .try_or_else(|| resolver.get_referenced_value_declaration(name))?;

            if let Some(decl) = decl {
                exported_bindings
                    .entry(get_original_node_id(&decl))
                    .or_default()
                    .push(specifier_as_export_specifier.name.clone());
            }

            unique_exports.insert(
                id_text(&specifier_as_export_specifier.name).to_owned(),
                true,
            );
            exported_names
                .get_or_insert_default_()
                .push(specifier_as_export_specifier.name.clone());
        }
    }

    Ok(())
}

fn collect_exported_variable_info(
    decl: &Node, /*VariableDeclaration | BindingElement*/
    unique_exports: &mut HashMap<String, bool>,
    exported_names: &mut Option<Vec<Gc<Node /*Identifier*/>>>,
) {
    let decl_name = decl.as_named_declaration().name();
    if is_binding_pattern(Some(&*decl_name)) {
        for element in &decl_name.as_has_elements().elements() {
            if !is_omitted_expression(element) {
                collect_exported_variable_info(element, unique_exports, exported_names);
            }
        }
    } else if !is_generated_identifier(&decl_name) {
        let text = id_text(&decl_name);
        if unique_exports.get(text).copied() != Some(true) {
            unique_exports.insert(text.to_owned(), true);
            exported_names.get_or_insert_default_().push(decl_name);
        }
    }
    // return exportedNames;
}

pub fn is_simple_copiable_expression(_expression: &Node /*Expression*/) -> bool {
    unimplemented!()
}

pub fn is_simple_inlineable_expression(_expression: &Node /*Expression*/) -> bool {
    unimplemented!()
}

pub fn is_compound_assignment(_kind: SyntaxKind) -> bool {
    unimplemented!()
}

pub fn get_non_assignment_operator_for_compound_assignment(
    _kind: SyntaxKind, /*CompoundAssignmentOperator*/
) -> SyntaxKind /*LogicalOperatorOrHigher | SyntaxKind.QuestionQuestionToken*/ {
    unimplemented!()
}

#[allow(clippy::ptr_arg)]
pub fn add_prologue_directives_and_initial_super_call(
    _factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    _ctor: &Node, /*ConstructorDeclaration*/
    _result: &mut Vec<Gc<Node /*Statement*/>>,
    _visitor: impl FnMut(&Node) -> VisitResult,
) -> usize {
    unimplemented!()
}

#[allow(clippy::ptr_arg)]
pub fn try_add_prologue_directives_and_initial_super_call(
    _factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    _ctor: &Node, /*ConstructorDeclaration*/
    _result: &mut Vec<Gc<Node /*Statement*/>>,
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
) -> io::Result<usize> {
    unimplemented!()
}

pub fn get_properties(
    _node: &Node, /*ClassExpression | ClassDeclaration*/
    _require_initializer: bool,
    _is_static: bool,
) -> Vec<Gc<Node /*PropertyDeclaration*/>> {
    unimplemented!()
}

pub fn get_static_properties_and_class_static_block(
    _node: &Node, /*ClassExpression | ClassDeclaration*/
) -> Vec<Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>> {
    unimplemented!()
}

pub fn is_initialized_property(_member: &Node /*ClassElement*/) -> bool {
    unimplemented!()
}

pub fn is_non_static_method_or_accessor_with_private_name(
    _member: &Node, /*ClassElement*/
) -> bool {
    unimplemented!()
}
