use std::{borrow::Borrow, cell::RefCell};

use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    create_node_factory, maybe_text_char_at_index, object_allocator, BaseNode, BaseNodeFactory,
    CharacterCodes, Node, NodeArray, NodeFactory, NodeFactoryFlags, OptionTry, SourceTextAsChars,
    SyntaxKind,
};

bitflags! {
    pub struct SignatureFlags: u32 {
        const None = 0;
        const Yield = 1 << 0;
        const Await = 1 << 1;
        const Type = 1 << 2;
        const IgnoreMissingOpenBrace = 1 << 4;
        const JSDoc = 1 << 5;
    }
}

#[derive(Eq, PartialEq)]
pub(super) enum SpeculationKind {
    TryParse,
    Lookahead,
    Reparse,
}

#[derive(Debug, Trace, Finalize)]
#[allow(non_snake_case)]
pub struct ParseBaseNodeFactory {
    #[unsafe_ignore_trace]
    NodeConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    TokenConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    IdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    PrivateIdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    #[unsafe_ignore_trace]
    SourceFileConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
}

impl ParseBaseNodeFactory {
    pub fn new() -> Self {
        Self {
            NodeConstructor: RefCell::new(None),
            TokenConstructor: RefCell::new(None),
            IdentifierConstructor: RefCell::new(None),
            PrivateIdentifierConstructor: RefCell::new(None),
            SourceFileConstructor: RefCell::new(None),
        }
    }
}

#[allow(non_snake_case)]
impl BaseNodeFactory for ParseBaseNodeFactory {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut SourceFileConstructor = self.SourceFileConstructor.borrow_mut();
        if SourceFileConstructor.is_none() {
            *SourceFileConstructor = Some(object_allocator.get_source_file_constructor());
        }
        (SourceFileConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut IdentifierConstructor = self.IdentifierConstructor.borrow_mut();
        if IdentifierConstructor.is_none() {
            *IdentifierConstructor = Some(object_allocator.get_identifier_constructor());
        }
        (IdentifierConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut PrivateIdentifierConstructor = self.PrivateIdentifierConstructor.borrow_mut();
        if PrivateIdentifierConstructor.is_none() {
            *PrivateIdentifierConstructor =
                Some(object_allocator.get_private_identifier_constructor());
        }
        (PrivateIdentifierConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut TokenConstructor = self.TokenConstructor.borrow_mut();
        if TokenConstructor.is_none() {
            *TokenConstructor = Some(object_allocator.get_token_constructor());
        }
        (TokenConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut NodeConstructor = self.NodeConstructor.borrow_mut();
        if NodeConstructor.is_none() {
            *NodeConstructor = Some(object_allocator.get_node_constructor());
        }
        (NodeConstructor.unwrap())(kind, -1, -1)
    }
}

thread_local! {
    pub static parse_base_node_factory: Gc<ParseBaseNodeFactory> = Gc::new(ParseBaseNodeFactory::new());
}

pub fn get_parse_base_node_factory() -> Gc<ParseBaseNodeFactory> {
    parse_base_node_factory.with(|parse_base_node_factory_| parse_base_node_factory_.clone())
}

thread_local! {
    pub static parse_node_factory: Gc<NodeFactory<ParseBaseNodeFactory>> = create_node_factory::<ParseBaseNodeFactory>(
        NodeFactoryFlags::NoParenthesizerRules,
        get_parse_base_node_factory(),
    );
}

pub fn get_parse_node_factory() -> Gc<NodeFactory<ParseBaseNodeFactory>> {
    parse_node_factory.with(|parse_node_factory_| parse_node_factory_.clone())
}

pub fn with_parse_base_node_factory_and_factory<TReturn>(
    callback: impl FnOnce(&ParseBaseNodeFactory, &Gc<NodeFactory<ParseBaseNodeFactory>>) -> TReturn,
) -> TReturn {
    parse_base_node_factory.with(|parse_base_node_factory_| {
        parse_node_factory
            .with(|parse_node_factory_| callback(&**parse_base_node_factory_, parse_node_factory_))
    })
}

pub(super) fn visit_node(cb_node: &mut impl FnMut(&Node), node: Option<impl Borrow<Node>>) {
    if let Some(node) = node {
        cb_node(node.borrow());
    }
}

pub(super) fn try_visit_node<TError>(
    cb_node: &mut impl FnMut(&Node) -> Result<(), TError>,
    node: Option<impl Borrow<Node>>,
) -> Result<(), TError> {
    if let Some(node) = node {
        cb_node(node.borrow())?;
    }

    Ok(())
}

pub(super) fn visit_node_returns<
    TNodeRef: Borrow<Node>,
    TReturn,
    TNodeCallback: FnMut(&Node) -> Option<TReturn>,
>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) -> Option<TReturn> {
    node.and_then(|node| cb_node(node.borrow()))
}

pub(super) fn try_visit_node_returns<
    TNodeRef: Borrow<Node>,
    TReturn,
    TError,
    TNodeCallback: FnMut(&Node) -> Result<Option<TReturn>, TError>,
>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) -> Result<Option<TReturn>, TError> {
    node.try_and_then(|node| cb_node(node.borrow()))
}

pub(super) fn visit_nodes(
    cb_node: &mut impl FnMut(&Node),
    cb_nodes: Option<&mut impl FnMut(&NodeArray)>,
    nodes: Option<&NodeArray>,
) {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                cb_nodes(nodes);
            }
            None => {
                for node in nodes.iter() {
                    cb_node(node);
                }
            }
        }
    }
}

pub(super) fn try_visit_nodes<TError>(
    cb_node: &mut impl FnMut(&Node) -> Result<(), TError>,
    cb_nodes: Option<&mut impl FnMut(&NodeArray) -> Result<(), TError>>,
    nodes: Option<&NodeArray>,
) -> Result<(), TError> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                cb_nodes(nodes)?;
            }
            None => {
                for node in nodes.iter() {
                    cb_node(node)?;
                }
            }
        }
    }

    Ok(())
}

pub(super) fn visit_nodes_returns<
    TReturn,
    TNodeCallback: FnMut(&Node) -> Option<TReturn>,
    TNodesCallback: FnMut(&NodeArray) -> Option<TReturn>,
>(
    cb_node: &mut TNodeCallback,
    cb_nodes: Option<&mut TNodesCallback>,
    nodes: Option<&NodeArray>,
) -> Option<TReturn> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                return cb_nodes(nodes);
            }
            None => {
                for node in nodes.iter() {
                    let result = cb_node(node);
                    if result.is_some() {
                        return result;
                    }
                }
            }
        }
    }
    None
}

pub(super) fn try_visit_nodes_returns<
    TReturn,
    TError,
    TNodeCallback: FnMut(&Node) -> Result<Option<TReturn>, TError>,
    TNodesCallback: FnMut(&NodeArray) -> Result<Option<TReturn>, TError>,
>(
    cb_node: &mut TNodeCallback,
    cb_nodes: Option<&mut TNodesCallback>,
    nodes: Option<&NodeArray>,
) -> Result<Option<TReturn>, TError> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                return cb_nodes(nodes);
            }
            None => {
                for node in nodes.iter() {
                    let result = cb_node(node)?;
                    if result.is_some() {
                        return Ok(result);
                    }
                }
            }
        }
    }
    Ok(None)
}

pub(crate) fn is_jsdoc_like_text(text: &SourceTextAsChars, start: usize) -> bool {
    matches!(
        maybe_text_char_at_index(text, start + 1),
        Some(CharacterCodes::asterisk)
    ) && matches!(
        maybe_text_char_at_index(text, start + 2),
        Some(CharacterCodes::asterisk)
    ) && !matches!(
        maybe_text_char_at_index(text, start + 3),
        Some(CharacterCodes::slash)
    )
}
