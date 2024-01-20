use std::{borrow::Borrow, collections::HashMap, io};

use gc::{Finalize, Gc, Trace};
use id_arena::Id;

use crate::{
    get_node_id, get_original_node, maybe_get_original_node, BaseNodeFactory, CompilerOptions,
    EmitResolver, GetOrInsertDefault, HasStatementsInterface, NamedDeclarationInterface, Node,
    NodeFactory, NodeId, NodeInterface, OptionTry, SyntaxKind, TransformationContext, Transformer,
    VisitResult, WrapCustomTransformerFactoryHandleDefault, _d, cast,
    create_external_helpers_import_declaration_if_needed, create_multi_map,
    get_namespace_declaration_node, has_static_modifier, has_syntactic_modifier, id_text,
    is_binding_pattern, is_class_static_block_declaration, is_default_import,
    is_expression_statement, is_generated_identifier, is_identifier, is_keyword,
    is_method_or_accessor, is_named_exports, is_named_imports, is_omitted_expression,
    is_private_identifier, is_property_declaration, is_statement, is_static,
    is_string_literal_like, is_super_call, return_default_if_none, try_visit_node,
    FunctionLikeDeclarationInterface, HasInitializerInterface, InternalSymbolName, Matches,
    ModifierFlags, MultiMap, HasArena,
    InArena,
};

pub fn get_original_node_id(node: Id<Node>, arena: &impl HasArena) -> NodeId {
    let node = get_original_node(node, arena);
    get_node_id(&node.ref_(arena))
}

pub fn maybe_get_original_node_id(node: Option<Id<Node>>, arena: &impl HasArena) -> NodeId {
    let node = maybe_get_original_node(node, arena);
    if let Some(node) = node {
        get_node_id(&node.ref_(arena))
    } else {
        0
    }
}

#[derive(Trace, Finalize)]
pub struct ExternalModuleInfo {
    pub external_imports:
        Vec<Id<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>>,
    pub external_helpers_import_declaration: Option<Id<Node /*ImportDeclaration*/>>,
    pub export_specifiers: HashMap<String, Vec<Id<Node /*ExportSpecifier*/>>>,
    pub exported_bindings: HashMap<NodeId, Vec<Id<Node /*Identifier*/>>>,
    pub exported_names: Option<Vec<Id<Node /*Identifier*/>>>,
    pub export_equals: Option<Id<Node /*ExportAssignment*/>>,
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

fn contains_default_reference(node: Option<Id<Node /*NamedImportBindings*/>>, arena: &impl HasArena) -> bool {
    let node = return_default_if_none!(node);
    if !is_named_imports(&node.ref_(arena)) {
        return false;
    }
    node.ref_(arena).as_named_imports()
        .elements
        .iter()
        .any(|&element| is_named_default_reference(element, arena))
}

fn is_named_default_reference(e: Id<Node> /*ImportSpecifier*/, arena: &impl HasArena) -> bool {
    let e_ref = e.ref_(arena);
    let e_as_import_specifier = e_ref.as_import_specifier();
    e_as_import_specifier
        .property_name
        .matches(|e_property_name| {
            e_property_name.ref_(arena).as_identifier().escaped_text == InternalSymbolName::Default
        })
}

pub fn chain_bundle() -> Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> {
    thread_local! {
        static CHAIN_BUNDLE: Gc<Box<dyn WrapCustomTransformerFactoryHandleDefault>> = Gc::new(Box::new(ChainBundle));
    }
    CHAIN_BUNDLE.with(|chain_bundle| chain_bundle.clone())
}

pub fn get_export_needs_import_star_helper(node: Id<Node> /*ExportDeclaration*/, arena: &impl HasArena) -> bool {
    get_namespace_declaration_node(node, arena).is_some()
}

pub fn get_import_needs_import_star_helper(node: Id<Node> /*ImportDeclaration*/, arena: &impl HasArena) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_import_declaration = node_ref.as_import_declaration();
    if get_namespace_declaration_node(node, arena).is_some() {
        return true;
    }
    let bindings =
        return_default_if_none!(node_as_import_declaration.import_clause.and_then(
            |node_import_clause| node_import_clause.ref_(arena).as_import_clause().named_bindings
        ));
    if !is_named_imports(&bindings.ref_(arena)) {
        return false;
    }
    let bindings_as_named_imports = bindings.ref_(arena).as_named_imports();
    let mut default_ref_count = 0;
    for &binding in &bindings_as_named_imports.elements {
        if is_named_default_reference(binding, arena) {
            default_ref_count += 1;
        }
    }

    default_ref_count > 0 && default_ref_count != bindings_as_named_imports.elements.len()
        || (bindings_as_named_imports.elements.len() - default_ref_count) != 0
            && is_default_import(node, arena)
}

pub fn get_import_needs_import_default_helper(node: Id<Node> /*ImportDeclaration*/, arena: &impl HasArena) -> bool {
    let node_ref = node.ref_(arena);
    let node_as_import_declaration = node_ref.as_import_declaration();
    !get_import_needs_import_star_helper(node, arena)
        && (is_default_import(node, arena)
            || node_as_import_declaration
                .import_clause
                .as_ref()
                .matches(|node_import_clause| {
                    let node_import_clause_named_bindings = node_import_clause
                        .ref_(arena).as_import_clause()
                        .named_bindings
                        .unwrap();
                    is_named_imports(&node_import_clause_named_bindings.ref_(arena))
                        && contains_default_reference(Some(node_import_clause_named_bindings), arena)
                }))
}

pub fn collect_external_module_info(
    context: &dyn TransformationContext,
    source_file: Id<Node>, /*SourceFile*/
    resolver: &dyn EmitResolver,
    compiler_options: &CompilerOptions,
    arena: &impl HasArena,
) -> io::Result<ExternalModuleInfo> {
    let source_file_ref = source_file.ref_(arena);
    let source_file_as_source_file = source_file_ref.as_source_file();
    let mut external_imports: Vec<
        Id<Node /*ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration*/>,
    > = _d();
    let mut export_specifiers: MultiMap<String, Id<Node /*ExportSpecifier*/>> = create_multi_map();
    let mut exported_bindings: HashMap<NodeId, Vec<Id<Node /*Identifier*/>>> = _d();
    let mut unique_exports: HashMap<String, bool> = _d();
    let mut exported_names: Option<Vec<Id<Node /*Identifier*/>>> = _d();
    let mut has_export_default = false;
    let mut export_equals: Option<Id<Node /*ExportAssignment*/>> = _d();
    let mut has_export_stars_to_export_values = false;
    let mut has_import_star = false;
    let mut has_import_default = false;

    for &node in &source_file_as_source_file.statements() {
        match node.ref_(arena).kind() {
            SyntaxKind::ImportDeclaration => {
                external_imports.push(node.clone());
                if !has_import_star && get_import_needs_import_star_helper(node, arena) {
                    has_import_star = true;
                }
                if !has_import_default && get_import_needs_import_default_helper(node, arena) {
                    has_import_default = true;
                }
            }
            SyntaxKind::ImportEqualsDeclaration => {
                let node_ref = node.ref_(arena);
                let node_as_import_equals_declaration = node_ref.as_import_equals_declaration();
                if node_as_import_equals_declaration.module_reference.ref_(arena).kind()
                    == SyntaxKind::ExternalModuleReference
                {
                    external_imports.push(node);
                }
            }
            SyntaxKind::ExportDeclaration => {
                let node_ref = node.ref_(arena);
                let node_as_export_declaration = node_ref.as_export_declaration();
                if node_as_export_declaration.module_specifier.is_some() {
                    match node_as_export_declaration.export_clause {
                        None => {
                            external_imports.push(node.clone());
                            has_export_stars_to_export_values = true;
                        }
                        Some(node_export_clause) => {
                            external_imports.push(node);
                            if is_named_exports(&node_export_clause.ref_(arena)) {
                                add_exported_names_for_export_declaration(
                                    &mut unique_exports,
                                    &mut export_specifiers,
                                    resolver,
                                    &mut exported_bindings,
                                    &mut exported_names,
                                    node,
                                    arena,
                                )?;
                            } else {
                                let name = node_export_clause.ref_(arena).as_namespace_export().name;
                                if unique_exports.get(id_text(&name.ref_(arena))).copied() != Some(true) {
                                    exported_bindings
                                        .entry(get_original_node_id(node, arena))
                                        .or_default()
                                        .push(name);
                                    unique_exports.insert(id_text(&name.ref_(arena)).to_owned(), true);
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
                        arena,
                    )?;
                }
            }
            SyntaxKind::ExportAssignment => {
                let node_ref = node.ref_(arena);
                let node_as_export_assignment = node_ref.as_export_assignment();
                if node_as_export_assignment.is_export_equals == Some(true)
                    && export_equals.is_none()
                {
                    export_equals = Some(node);
                }
            }
            SyntaxKind::VariableStatement => {
                if has_syntactic_modifier(node, ModifierFlags::Export, arena) {
                    for decl in &node
                        .ref_(arena).as_variable_statement()
                        .declaration_list
                        .as_variable_declaration_list()
                        .declarations
                    {
                        collect_exported_variable_info(
                            decl,
                            &mut unique_exports,
                            &mut exported_names,
                            arena,
                        );
                    }
                }
            }
            SyntaxKind::FunctionDeclaration => {
                if has_syntactic_modifier(node, ModifierFlags::Export, arena) {
                    if has_syntactic_modifier(node, ModifierFlags::Default, arena) {
                        if !has_export_default {
                            exported_bindings
                                .entry(get_original_node_id(node, arena))
                                .or_default()
                                .push(context.factory().get_declaration_name(
                                    Some(node),
                                    None,
                                    None,
                                ));
                            has_export_default = true;
                        }
                    } else {
                        let name = node.ref_(arena).as_function_declaration().name();
                        if unique_exports.get(id_text(&name)).copied() != Some(true) {
                            exported_bindings
                                .entry(get_original_node_id(node, arena))
                                .or_default()
                                .push(name.clone());
                            unique_exports.insert(id_text(&name).to_owned(), true);
                            exported_names.get_or_insert_default_().push(name);
                        }
                    }
                }
            }
            SyntaxKind::ClassDeclaration => {
                if has_syntactic_modifier(node, ModifierFlags::Export, arena) {
                    if has_syntactic_modifier(node, ModifierFlags::Default, arena) {
                        if !has_export_default {
                            exported_bindings
                                .entry(get_original_node_id(node, arena))
                                .or_default()
                                .push(context.factory().get_declaration_name(
                                    Some(node),
                                    None,
                                    None,
                                ));
                            has_export_default = true;
                        }
                    } else {
                        let name = node.ref_(arena).as_class_declaration().maybe_name();
                        if let Some(name) = name
                            .filter(|name| unique_exports.get(id_text(name)).copied() != Some(true))
                        {
                            exported_bindings
                                .entry(get_original_node_id(node, arena))
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
    export_specifiers: &mut MultiMap<String, Id<Node /*ExportSpecifier*/>>,
    resolver: &dyn EmitResolver,
    exported_bindings: &mut HashMap<NodeId, Vec<Id<Node /*Identifier*/>>>,
    exported_names: &mut Option<Vec<Id<Node /*Identifier*/>>>,
    node: Id<Node>, /*ExportDeclaration*/
    arena: &impl HasArena,
) -> io::Result<()> {
    let node_as_export_declaration = node.ref_(arena).as_export_declaration();
    for specifier in &cast(
        node_as_export_declaration.export_clause,
        |node: &Id<Node>| is_named_exports(&node.ref_(arena)),
    )
    .ref_(arena).as_named_exports()
    .elements
    {
        let specifier_as_export_specifier = specifier.ref_(arena).as_export_specifier();
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
                    .entry(get_original_node_id(decl, arena))
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
    decl: Id<Node>, /*VariableDeclaration | BindingElement*/
    unique_exports: &mut HashMap<String, bool>,
    exported_names: &mut Option<Vec<Id<Node /*Identifier*/>>>,
    arena: &impl HasArena,
) {
    let decl_name = decl.ref_(arena).as_named_declaration().name();
    if is_binding_pattern(Some(&*decl_name.ref_(arena))) {
        for &element in &decl_name.ref_(arena).as_has_elements().elements() {
            if !is_omitted_expression(&element.ref_(arena)) {
                collect_exported_variable_info(element, unique_exports, exported_names, arena);
            }
        }
    } else if !is_generated_identifier(&decl_name.ref_(arena)) {
        let text = id_text(&decl_name.ref_(arena));
        if unique_exports.get(text).copied() != Some(true) {
            unique_exports.insert(text.to_owned(), true);
            exported_names.get_or_insert_default_().push(decl_name);
        }
    }
    // return exportedNames;
}

pub fn is_simple_copiable_expression(expression: &Node /*Expression*/) -> bool {
    is_string_literal_like(expression)
        || expression.kind() == SyntaxKind::NumericLiteral
        || is_keyword(expression.kind())
        || is_identifier(expression)
}

pub fn is_simple_inlineable_expression(expression: &Node /*Expression*/) -> bool {
    !is_identifier(expression) && is_simple_copiable_expression(expression)
}

pub fn is_compound_assignment(kind: SyntaxKind) -> bool {
    kind >= SyntaxKind::FirstCompoundAssignment && kind <= SyntaxKind::LastCompoundAssignment
}

pub fn get_non_assignment_operator_for_compound_assignment(
    kind: SyntaxKind, /*CompoundAssignmentOperator*/
) -> SyntaxKind /*LogicalOperatorOrHigher | SyntaxKind.QuestionQuestionToken*/ {
    match kind {
        SyntaxKind::PlusEqualsToken => SyntaxKind::PlusToken,
        SyntaxKind::MinusEqualsToken => SyntaxKind::MinusToken,
        SyntaxKind::AsteriskEqualsToken => SyntaxKind::AsteriskToken,
        SyntaxKind::AsteriskAsteriskEqualsToken => SyntaxKind::AsteriskAsteriskToken,
        SyntaxKind::SlashEqualsToken => SyntaxKind::SlashToken,
        SyntaxKind::PercentEqualsToken => SyntaxKind::PercentToken,
        SyntaxKind::LessThanLessThanEqualsToken => SyntaxKind::LessThanLessThanToken,
        SyntaxKind::GreaterThanGreaterThanEqualsToken => SyntaxKind::GreaterThanGreaterThanToken,
        SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken => {
            SyntaxKind::GreaterThanGreaterThanGreaterThanToken
        }
        SyntaxKind::AmpersandEqualsToken => SyntaxKind::AmpersandToken,
        SyntaxKind::BarEqualsToken => SyntaxKind::BarToken,
        SyntaxKind::CaretEqualsToken => SyntaxKind::CaretToken,
        SyntaxKind::BarBarEqualsToken => SyntaxKind::BarBarToken,
        SyntaxKind::AmpersandAmpersandEqualsToken => SyntaxKind::AmpersandAmpersandToken,
        SyntaxKind::QuestionQuestionEqualsToken => SyntaxKind::QuestionQuestionToken,
        _ => unreachable!(),
    }
}

pub fn add_prologue_directives_and_initial_super_call(
    factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    ctor: Id<Node>, /*ConstructorDeclaration*/
    result: &mut Vec<Id<Node /*Statement*/>>,
    mut visitor: impl FnMut(Id<Node>) -> VisitResult, arena: &impl HasArena,
) -> usize {
    try_add_prologue_directives_and_initial_super_call(factory, ctor, result, |node: Id<Node>| {
        Ok(visitor(node))
    }, arena)
    .unwrap()
}

pub fn try_add_prologue_directives_and_initial_super_call(
    factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    ctor: Id<Node>, /*ConstructorDeclaration*/
    result: &mut Vec<Id<Node /*Statement*/>>,
    mut visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult>,
    arena: &impl HasArena,
) -> io::Result<usize> {
    let ctor_as_constructor_declaration = ctor.ref_(arena).as_constructor_declaration();
    if let Some(ctor_body) = ctor_as_constructor_declaration.maybe_body() {
        let statements = &ctor_body.ref_(arena).as_block().statements;
        let index = factory.try_copy_prologue(
            statements,
            result,
            Some(false),
            Some(|node: Id<Node>| visitor(node)),
        )?;
        if index == statements.len() {
            return Ok(index);
        }

        let super_index = statements
            .iter()
            .skip(index)
            .position(|s| {
                is_expression_statement(s) && is_super_call(s.as_expression_statement().expression, arena)
            })
            .map(|found| found + index);
        if let Some(super_index) = super_index {
            for statement in statements.iter().skip(index).take(super_index - index + 1) {
                result.push(try_visit_node(
                    statement,
                    Some(|node: Id<Node>| visitor(node)),
                    Some(|node: Id<Node>| is_statement(node, arena)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?);
            }
            return Ok(super_index + 1);
        }

        return Ok(index);
    }

    Ok(0)
}

pub fn get_properties(
    node: Id<Node>, /*ClassExpression | ClassDeclaration*/
    require_initializer: bool,
    is_static: bool,
    arena: &impl HasArena,
) -> Vec<Id<Node /*PropertyDeclaration*/>> {
    node.ref_(arena).as_class_like_declaration()
        .members()
        .iter()
        .filter(|&&m| is_initialized_or_static_property(m, require_initializer, is_static, arena))
        .cloned()
        .collect()
}

pub fn is_static_property_declaration_or_class_static_block_declaration(
    element: Id<Node> /*ClassElement*/, arena: &impl HasArena
) -> bool {
    is_static_property_declaration(element, arena) || is_class_static_block_declaration(&element.ref_(arena))
}

pub fn get_static_properties_and_class_static_block(
    node: Id<Node> /*ClassExpression | ClassDeclaration*/, arena: &impl HasArena
) -> Vec<Id<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>> {
    node.ref_(arena).as_class_like_declaration()
        .members()
        .iter()
        .filter(|&&member| is_static_property_declaration_or_class_static_block_declaration(member, arena))
        .cloned()
        .collect()
}

fn is_initialized_or_static_property(
    member: Id<Node>, /*ClassElement*/
    require_initializer: bool,
    is_static: bool,
    arena: &impl HasArena,
) -> bool {
    is_property_declaration(&member.ref_(arena))
        && (member
            .ref_(arena).as_property_declaration()
            .maybe_initializer()
            .is_some()
            || !require_initializer)
        && has_static_modifier(member, arena) == is_static
}

pub fn is_static_property_declaration(member: Id<Node> /*ClassElement*/, arena: &impl HasArena) -> bool {
    is_property_declaration(&member.ref_(arena)) && has_static_modifier(member, arena)
}

pub fn is_initialized_property(member: &Node /*ClassElement*/) -> bool {
    member.kind() == SyntaxKind::PropertyDeclaration
        && member
            .as_property_declaration()
            .maybe_initializer()
            .is_some()
}

pub fn is_non_static_method_or_accessor_with_private_name(
    member: Id<Node>, /*ClassElement*/ arena: &impl HasArena
) -> bool {
    !is_static(member, arena)
        && is_method_or_accessor(&member.ref_(arena))
        && is_private_identifier(&member.ref_(arena).as_named_declaration().name().ref_(arena))
}
