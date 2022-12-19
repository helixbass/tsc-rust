#![allow(non_upper_case_globals)]

use gc::{Gc, GcCell};
use std::borrow::Borrow;
use std::cell::{RefCell, RefMut};
use std::collections::HashMap;
use std::ptr;
use std::rc::Rc;

use super::{create_node_factory, NodeFactoryFlags};
use crate::{
    add_range, create_base_node_factory, create_scanner, is_named_declaration, is_property_name,
    maybe_append_if_unique_gc, maybe_append_if_unique_rc, set_text_range, BaseNode,
    BaseNodeFactory, BaseNodeFactoryConcrete, Debug_, EmitFlags, EmitNode, LanguageVariant, Node,
    NodeArray, NodeArrayOrVec, NodeFactory, NodeFlags, NodeInterface, PseudoBigInt, Scanner,
    ScriptTarget, SourceMapRange, StrOrRcNode, StringOrNumberOrBoolOrRcNode, StringOrRcNode,
    SyntaxKind, TransformFlags,
};

impl<TBaseNodeFactory: 'static + BaseNodeFactory> NodeFactory<TBaseNodeFactory> {
    pub(super) fn as_node_array<TArray: Into<NodeArrayOrVec>>(
        &self,
        array: Option<TArray>,
    ) -> Option<NodeArray> {
        array.map(|array| self.create_node_array(Some(array), None))
    }

    pub(super) fn as_name<'name, TName: Into<StrOrRcNode<'name>>>(
        &self,
        base_factory: &TBaseNodeFactory,
        name: Option<TName>,
    ) -> Option<Gc<Node>> {
        name.map(|name| match name.into() {
            StrOrRcNode::Str(name) => self
                .create_identifier(base_factory, name, Option::<NodeArray>::None, None)
                .into(),
            StrOrRcNode::RcNode(name) => name,
        })
    }

    pub(super) fn as_expression<TValue: Into<StringOrNumberOrBoolOrRcNode>>(
        &self,
        base_factory: &TBaseNodeFactory,
        value: Option<TValue>,
    ) -> Option<Gc<Node>> {
        value.map(|value| match value.into() {
            StringOrNumberOrBoolOrRcNode::String(value) => self
                .create_string_literal(base_factory, value, None, None)
                .into(),
            StringOrNumberOrBoolOrRcNode::Number(value) => self
                .create_numeric_literal(base_factory, value, None)
                .into(),
            StringOrNumberOrBoolOrRcNode::Bool(value) => {
                if value {
                    self.create_true(base_factory).into()
                } else {
                    self.create_false(base_factory).into()
                }
            }
            StringOrNumberOrBoolOrRcNode::RcNode(value) => value,
        })
    }

    pub(super) fn as_token(
        &self,
        base_factory: &TBaseNodeFactory,
        value: SyntaxKindOrRcNode,
    ) -> Gc<Node> {
        match value {
            SyntaxKindOrRcNode::SyntaxKind(value) => self.create_token(base_factory, value).into(),
            SyntaxKindOrRcNode::RcNode(value) => value,
        }
    }

    pub(super) fn as_embedded_statement(&self, statement: Option<Gc<Node>>) -> Option<Gc<Node>> {
        if false {
            unimplemented!()
        } else {
            statement
        }
    }
}

pub enum SyntaxKindOrRcNode {
    SyntaxKind(SyntaxKind),
    RcNode(Gc<Node>),
}

impl From<SyntaxKind> for SyntaxKindOrRcNode {
    fn from(value: SyntaxKind) -> Self {
        Self::SyntaxKind(value)
    }
}

impl From<Gc<Node>> for SyntaxKindOrRcNode {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

pub(super) fn update_without_original(updated: Gc<Node>, original: &Node) -> Gc<Node> {
    if !ptr::eq(&*updated, original) {
        set_text_range(&*updated, Some(original));
    }
    updated
}

pub(super) fn update_with_original(updated: Gc<Node>, original: &Node) -> Gc<Node> {
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

thread_local! {
    pub(super) static raw_text_scanner: RefCell<Option<Scanner>> = RefCell::new(None);
}

pub(super) enum CookedText {
    InvalidValue,
    String(String),
}

pub(super) fn get_cooked_text(
    kind: SyntaxKind, /*TemplateLiteralToken["kind"]*/
    raw_text: &str,
) -> CookedText {
    raw_text_scanner.with(|raw_text_scanner_| {
        let mut raw_text_scanner_ref = raw_text_scanner_.borrow_mut();
        if raw_text_scanner_ref.is_none() {
            *raw_text_scanner_ref = Some(create_scanner(
                ScriptTarget::Latest,
                false,
                Some(LanguageVariant::Standard),
                None,
                None,
                None,
                None,
            ));
        }
        let mut raw_text_scanner_ref = raw_text_scanner_ref.as_mut().unwrap();
        match kind {
            SyntaxKind::NoSubstitutionTemplateLiteral => {
                let text = format!("`{}`", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateHead => {
                let text = format!("`{}${{", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateMiddle => {
                let text = format!("}}{}${{", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            SyntaxKind::TemplateTail => {
                let text = format!("}}{}`", raw_text);
                raw_text_scanner_ref.set_text(Some(text.chars().collect()), Some(text), None, None);
            }
            _ => panic!("Unexpected kind"),
        }

        let mut token = raw_text_scanner_ref.scan(None);
        if token == SyntaxKind::CloseBraceToken {
            token = raw_text_scanner_ref.re_scan_template_token(None, false);
        }

        if raw_text_scanner_ref.is_unterminated() {
            raw_text_scanner_ref.set_text(None, None, None, None);
            return CookedText::InvalidValue;
        }

        let mut token_value: Option<String> = None;
        match token {
            SyntaxKind::NoSubstitutionTemplateLiteral
            | SyntaxKind::TemplateHead
            | SyntaxKind::TemplateMiddle
            | SyntaxKind::TemplateTail => {
                token_value = Some(raw_text_scanner_ref.get_token_value().clone());
            }
            _ => (),
        }

        if token_value.is_none() || raw_text_scanner_ref.scan(None) != SyntaxKind::EndOfFileToken {
            raw_text_scanner_ref.set_text(None, None, None, None);
            return CookedText::InvalidValue;
        }
        let token_value = token_value.unwrap();

        raw_text_scanner_ref.set_text(None, None, None, None);
        CookedText::String(token_value)
    })
}

pub(super) fn propagate_identifier_name_flags(node: &Node /*Identifier*/) -> TransformFlags {
    propagate_child_flags(Some(node)) & !TransformFlags::ContainsPossibleTopLevelAwait
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

pub fn with_synthetic_factory_and_factory<
    TReturn,
    TCallback: FnOnce(&BaseNodeFactorySynthetic, &Gc<NodeFactory<BaseNodeFactorySynthetic>>) -> TReturn,
>(
    callback: TCallback,
) -> TReturn {
    synthetic_factory
        .with(|synthetic_factory_| factory.with(|factory_| callback(synthetic_factory_, factory_)))
}

pub fn with_factory<
    TReturn,
    TCallback: FnOnce(&Gc<NodeFactory<BaseNodeFactorySynthetic>>) -> TReturn,
>(
    callback: TCallback,
) -> TReturn {
    factory.with(|factory_| callback(factory_))
}

pub fn with_synthetic_factory<TReturn, TCallback: FnOnce(&BaseNodeFactorySynthetic) -> TReturn>(
    callback: TCallback,
) -> TReturn {
    synthetic_factory.with(|synthetic_factory_| callback(synthetic_factory_))
}

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
    pub static factory: Gc<NodeFactory<BaseNodeFactorySynthetic>> =
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

pub fn set_original_node(node: Gc<Node>, original: Option<Gc<Node>>) -> Gc<Node> {
    node.set_original(original.clone());
    if let Some(original) = original {
        let emit_node = original.maybe_emit_node();
        if let Some(emit_node) = emit_node.as_ref() {
            node.maybe_emit_node_mut()
                .get_or_insert_with(|| Gc::new(GcCell::new(Default::default())));
            merge_emit_node(
                &(**emit_node).borrow(),
                &mut (*node.maybe_emit_node().unwrap()).borrow_mut(),
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
            dest_emit_node_helpers =
                Some(maybe_append_if_unique_gc(dest_emit_node_helpers, helper));
        }
        dest_emit_node.helpers = dest_emit_node_helpers;
    }
    if starts_on_new_line.is_some() {
        dest_emit_node.starts_on_new_line = starts_on_new_line.map(Clone::clone);
    }
    // return destEmitNode
}

pub(super) fn merge_token_source_map_ranges(
    source_ranges: &HashMap<SyntaxKind, Option<Gc<SourceMapRange>>>,
    dest_ranges: Option<&HashMap<SyntaxKind, Option<Gc<SourceMapRange>>>>,
) -> HashMap<SyntaxKind, Option<Gc<SourceMapRange>>> {
    let mut dest_ranges =
        dest_ranges.map_or_else(|| HashMap::new(), |dest_ranges| dest_ranges.clone());
    for (key, value) in source_ranges {
        dest_ranges.insert(*key, value.clone());
    }
    dest_ranges
}
