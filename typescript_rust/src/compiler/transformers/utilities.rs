use std::{borrow::Borrow, collections::HashMap, io};

use gc::{Finalize, Gc, Trace};

use crate::{
    get_node_id, get_original_node, maybe_get_original_node, BaseNodeFactory, CompilerOptions,
    EmitResolver, Node, NodeFactory, NodeId, SyntaxKind, TransformationContext, Transformer,
    VisitResult, WrapCustomTransformerFactoryHandleDefault,
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
    pub exported_bindings: Vec<Vec<Gc<Node /*Identifier*/>>>,
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

pub fn collect_external_module_info(
    _context: &dyn TransformationContext,
    _source_file: &Node, /*SourceFile*/
    resolver: &dyn EmitResolver,
    compiler_options: &CompilerOptions,
) -> ExternalModuleInfo {
    unimplemented!()
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

pub fn add_prologue_directives_and_initial_super_call(
    _factory: &NodeFactory<impl 'static + BaseNodeFactory + Trace + Finalize>,
    _ctor: &Node, /*ConstructorDeclaration*/
    _result: &mut Vec<Gc<Node /*Statement*/>>,
    _visitor: impl FnMut(&Node) -> VisitResult,
) -> usize {
    unimplemented!()
}

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
