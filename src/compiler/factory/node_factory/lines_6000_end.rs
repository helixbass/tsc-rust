#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::RefMut;
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{create_node_factory, NodeFactoryFlags};
use crate::{
    add_range, append_if_unique, create_base_node_factory, is_named_declaration, is_property_name,
    set_text_range, BaseNode, BaseNodeFactory, BaseNodeFactoryConcrete, Debug_, EmitFlags,
    EmitNode, Node, NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, PseudoBigInt,
    SourceMapRange, SyntaxKind, TransformFlags,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub(super) fn as_node_array<TArray: Into<NodeArrayOrVec>>(
        &self,
        array: Option<TArray>,
    ) -> Option<NodeArray> {
        array.map(|array| self.create_node_array(Some(array), None))
    }

    pub(super) fn as_name(&self, name: Rc<Node>) -> Rc<Node> {
        name
    }

    pub(super) fn as_token(
        &self,
        base_factory: &TBaseNodeFactory,
        value: SyntaxKindOrRcNode,
    ) -> Rc<Node> {
        match value {
            SyntaxKindOrRcNode::SyntaxKind(value) => self.create_token(base_factory, value).into(),
            SyntaxKindOrRcNode::RcNode(value) => value,
        }
    }

    pub(super) fn as_embedded_statement(&self, statement: Option<Rc<Node>>) -> Option<Rc<Node>> {
        if false {
            unimplemented!()
        } else {
            statement
        }
    }
}

pub enum SyntaxKindOrRcNode {
    SyntaxKind(SyntaxKind),
    RcNode(Rc<Node>),
}

impl From<SyntaxKind> for SyntaxKindOrRcNode {
    fn from(value: SyntaxKind) -> Self {
        Self::SyntaxKind(value)
    }
}

impl From<Rc<Node>> for SyntaxKindOrRcNode {
    fn from(value: Rc<Node>) -> Self {
        Self::RcNode(value)
    }
}

pub(super) fn update_without_original(updated: Rc<Node>, original: &Node) -> Rc<Node> {
    if !ptr::eq(&*updated, original) {
        set_text_range(&*updated, Some(original));
    }
    updated
}

pub(super) fn update_with_original(updated: Rc<Node>, original: &Node) -> Rc<Node> {
    if !ptr::eq(&*updated, original) {
        set_original_node(updated.clone(), Some(original.node_wrapper()));
        set_text_range(&*updated, Some(original));
    }
    updated
}

pub(super) fn get_default_tag_name_for_kind(kind: SyntaxKind) -> &'static str {
    match kind {
        SyntaxKind::JSDocTypeTag => "type",
        SyntaxKind::JSDocReturnTag => "returns",
        SyntaxKind::JSDocThisTag => "this",
        SyntaxKind::JSDocEnumTag => "enum",
        SyntaxKind::JSDocAuthorTag => "author",
        SyntaxKind::JSDocClassTag => "class",
        SyntaxKind::JSDocPublicTag => "public",
        SyntaxKind::JSDocPrivateTag => "private",
        SyntaxKind::JSDocProtectedTag => "protected",
        SyntaxKind::JSDocReadonlyTag => "readonly",
        SyntaxKind::JSDocOverrideTag => "override",
        SyntaxKind::JSDocTemplateTag => "template",
        SyntaxKind::JSDocTypedefTag => "typedef",
        SyntaxKind::JSDocParameterTag => "parameter",
        SyntaxKind::JSDocPropertyTag => "prop",
        SyntaxKind::JSDocCallbackTag => "callback",
        SyntaxKind::JSDocAugmentsTag => "augments",
        SyntaxKind::JSDocImplementsTag => "implements",
        _ => Debug_.fail(Some(&format!(
            "Unsupported kind: {}",
            Debug_.format_syntax_kind(Some(kind))
        ))),
    }
}

pub(super) fn propagate_property_name_flags_of_child(
    node: &Node, /*PropertyName*/
    transform_flags: TransformFlags,
) -> TransformFlags {
    transform_flags | (node.transform_flags() & TransformFlags::PropertyNamePropagatingFlags)
}

pub(super) fn propagate_child_flags<TNode: Borrow<Node>>(child: Option<TNode>) -> TransformFlags {
    if child.is_none() {
        return TransformFlags::None;
    }
    let child = child.unwrap();
    let child = child.borrow();
    let child_flags =
        child.transform_flags() & !get_transform_flags_subtree_exclusions(child.kind());
    if is_named_declaration(child) && is_property_name(&*child.as_named_declaration().name()) {
        propagate_property_name_flags_of_child(&child.as_named_declaration().name(), child_flags)
    } else {
        child_flags
    }
}

pub(super) fn propagate_children_flags(children: Option<&NodeArray>) -> TransformFlags {
    children.map_or(TransformFlags::None, |children| {
        children.transform_flags.unwrap()
    })
}

pub(super) fn aggregate_children_flags(children: &mut NodeArray) {
    let mut subtree_flags = TransformFlags::None;
    for child in children.iter() {
        subtree_flags |= propagate_child_flags(Some(&**child));
    }
    children.transform_flags = Some(subtree_flags);
}

pub(crate) fn get_transform_flags_subtree_exclusions(kind: SyntaxKind) -> TransformFlags {
    if kind >= SyntaxKind::FirstTypeNode && kind <= SyntaxKind::LastTypeNode {
        return TransformFlags::TypeExcludes;
    }

    match kind {
        SyntaxKind::CallExpression
        | SyntaxKind::NewExpression
        | SyntaxKind::ArrayLiteralExpression => TransformFlags::ArrayLiteralOrCallOrNewExcludes,
        SyntaxKind::ModuleDeclaration => TransformFlags::ModuleExcludes,
        SyntaxKind::Parameter => TransformFlags::ParameterExcludes,
        SyntaxKind::ArrowFunction => TransformFlags::ArrowFunctionExcludes,
        SyntaxKind::FunctionExpression | SyntaxKind::FunctionDeclaration => {
            TransformFlags::FunctionExcludes
        }
        SyntaxKind::VariableDeclarationList => TransformFlags::VariableDeclarationListExcludes,
        SyntaxKind::ClassDeclaration | SyntaxKind::ClassExpression => TransformFlags::ClassExcludes,
        SyntaxKind::Constructor => TransformFlags::ConstructorExcludes,
        SyntaxKind::PropertyDeclaration => TransformFlags::PropertyExcludes,
        SyntaxKind::MethodDeclaration | SyntaxKind::GetAccessor | SyntaxKind::SetAccessor => {
            TransformFlags::MethodOrAccessorExcludes
        }
        SyntaxKind::AnyKeyword
        | SyntaxKind::NumberKeyword
        | SyntaxKind::BigIntKeyword
        | SyntaxKind::NeverKeyword
        | SyntaxKind::StringKeyword
        | SyntaxKind::ObjectKeyword
        | SyntaxKind::BooleanKeyword
        | SyntaxKind::SymbolKeyword
        | SyntaxKind::VoidKeyword
        | SyntaxKind::TypeParameter
        | SyntaxKind::PropertySignature
        | SyntaxKind::MethodSignature
        | SyntaxKind::CallSignature
        | SyntaxKind::ConstructSignature
        | SyntaxKind::IndexSignature
        | SyntaxKind::InterfaceDeclaration
        | SyntaxKind::TypeAliasDeclaration => TransformFlags::TypeExcludes,
        SyntaxKind::ObjectLiteralExpression => TransformFlags::ObjectLiteralExcludes,
        SyntaxKind::CatchClause => TransformFlags::CatchClauseExcludes,
        SyntaxKind::ObjectBindingPattern | SyntaxKind::ArrayBindingPattern => {
            TransformFlags::BindingPatternExcludes
        }
        SyntaxKind::TypeAssertionExpression
        | SyntaxKind::AsExpression
        | SyntaxKind::PartiallyEmittedExpression
        | SyntaxKind::ParenthesizedExpression
        | SyntaxKind::SuperKeyword => TransformFlags::OuterExpressionExcludes,
        SyntaxKind::PropertyAccessExpression | SyntaxKind::ElementAccessExpression => {
            TransformFlags::PropertyAccessExcludes
        }
        _ => TransformFlags::NodeExcludes,
    }
}

thread_local! {
    pub(super) static base_factory_static: BaseNodeFactoryConcrete = create_base_node_factory();
}

pub(super) fn make_synthetic(node: BaseNode) -> BaseNode {
    node.set_flags(node.flags() | NodeFlags::Synthesized);
    node
}

thread_local! {
    pub static synthetic_factory: BaseNodeFactorySynthetic = BaseNodeFactorySynthetic::new();
}

// pub fn get_synthetic_factory() -> BaseNodeFactorySynthetic {
//     BaseNodeFactorySynthetic::new()
// }

#[derive(Debug)]
pub struct BaseNodeFactorySynthetic {}

impl BaseNodeFactorySynthetic {
    pub fn new() -> Self {
        Self {}
    }
}

impl BaseNodeFactory for BaseNodeFactorySynthetic {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static
                .with(|base_factory| base_factory.create_base_source_file_node(kind)),
        )
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static.with(|base_factory| base_factory.create_base_identifier_node(kind)),
        )
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static
                .with(|base_factory| base_factory.create_base_private_identifier_node(kind)),
        )
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(
            base_factory_static.with(|base_factory| base_factory.create_base_token_node(kind)),
        )
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        make_synthetic(base_factory_static.with(|base_factory| base_factory.create_base_node(kind)))
    }
}

thread_local! {
    pub static factory: Rc<NodeFactory<BaseNodeFactorySynthetic>> =
        create_node_factory::<BaseNodeFactorySynthetic>(NodeFactoryFlags::NoIndentationOnFreshPropertyAccess);
}

pub enum PseudoBigIntOrString {
    PseudoBigInt(PseudoBigInt),
    String(String),
}

impl From<PseudoBigInt> for PseudoBigIntOrString {
    fn from(pseudo_big_int: PseudoBigInt) -> Self {
        PseudoBigIntOrString::PseudoBigInt(pseudo_big_int)
    }
}

impl From<String> for PseudoBigIntOrString {
    fn from(string: String) -> Self {
        PseudoBigIntOrString::String(string)
    }
}

pub fn set_original_node(node: Rc<Node>, original: Option<Rc<Node>>) -> Rc<Node> {
    node.set_original(original.clone());
    if let Some(original) = original {
        let emit_node = original.maybe_emit_node();
        if let Some(emit_node) = emit_node.as_ref() {
            let mut node_emit_node = node.maybe_emit_node();
            if node_emit_node.is_none() {
                *node_emit_node = Some(Default::default());
            }
            merge_emit_node(
                emit_node,
                &mut *RefMut::map(node_emit_node, |option| option.as_mut().unwrap()),
            );
            // node.set_emit_node(node_emit_node);
        }
    }
    node
}

pub(super) fn merge_emit_node(
    source_emit_node: &EmitNode,
    dest_emit_node: /*Option<*/ &mut EmitNode, /*>*/
) /*-> EmitNode*/
{
    let flags = source_emit_node.flags.as_ref();
    let leading_comments = source_emit_node.leading_comments.as_ref();
    let trailing_comments = source_emit_node.trailing_comments.as_ref();
    let comment_range = source_emit_node.comment_range.as_ref();
    let source_map_range = source_emit_node.source_map_range.as_ref();
    let token_source_map_ranges = source_emit_node.token_source_map_ranges.as_ref();
    let constant_value = source_emit_node.constant_value.as_ref();
    let helpers = source_emit_node.helpers.as_ref();
    let starts_on_new_line = source_emit_node.starts_on_new_line.as_ref();
    if let Some(leading_comments) = leading_comments {
        let mut new_leading_comments = leading_comments.to_vec();
        add_range(
            &mut new_leading_comments,
            dest_emit_node.leading_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.leading_comments = Some(new_leading_comments);
    }
    if let Some(trailing_comments) = trailing_comments {
        let mut new_trailing_comments = trailing_comments.to_vec();
        add_range(
            &mut new_trailing_comments,
            dest_emit_node.trailing_comments.as_deref(),
            None,
            None,
        );
        dest_emit_node.trailing_comments = Some(new_trailing_comments);
    }
    // TODO: should this technically also check "truthiness" of flags?
    if let Some(flags) = flags {
        dest_emit_node.flags = Some(*flags & !EmitFlags::Immutable);
    }
    if comment_range.is_some() {
        dest_emit_node.comment_range = comment_range.map(Clone::clone);
    }
    if source_map_range.is_some() {
        dest_emit_node.source_map_range = source_map_range.map(Clone::clone);
    }
    if let Some(token_source_map_ranges) = token_source_map_ranges {
        dest_emit_node.token_source_map_ranges = Some(merge_token_source_map_ranges(
            token_source_map_ranges,
            dest_emit_node.token_source_map_ranges.as_ref(),
        ));
    }
    if constant_value.is_some() {
        dest_emit_node.constant_value = constant_value.map(Clone::clone);
    }
    if let Some(helpers) = helpers {
        let mut dest_emit_node_helpers = dest_emit_node.helpers.clone();
        for helper in helpers {
            dest_emit_node_helpers = Some(append_if_unique(dest_emit_node_helpers, helper.clone()));
        }
        dest_emit_node.helpers = dest_emit_node_helpers;
    }
    if starts_on_new_line.is_some() {
        dest_emit_node.starts_on_new_line = starts_on_new_line.map(Clone::clone);
    }
    // return destEmitNode
}

pub(super) fn merge_token_source_map_ranges(
    source_ranges: &HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>,
    dest_ranges: Option<&HashMap<SyntaxKind, Option<Rc<SourceMapRange>>>>,
) -> HashMap<SyntaxKind, Option<Rc<SourceMapRange>>> {
    let mut dest_ranges =
        dest_ranges.map_or_else(|| HashMap::new(), |dest_ranges| dest_ranges.clone());
    for (key, value) in source_ranges {
        dest_ranges.insert(*key, value.clone());
    }
    dest_ranges
}
