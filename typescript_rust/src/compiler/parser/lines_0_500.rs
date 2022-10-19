#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use super::ParserType;
use crate::{
    create_node_factory, object_allocator, BaseNode, BaseNodeFactory, CharacterCodes, Node,
    NodeArray, NodeFactory, NodeFactoryFlags, SourceText, SyntaxKind,
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

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct ParseBaseNodeFactory {
    NodeConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    TokenConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    IdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    PrivateIdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
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
    pub static parse_base_node_factory: ParseBaseNodeFactory = ParseBaseNodeFactory::new();
}

thread_local! {
    pub static parse_node_factory: Rc<NodeFactory<ParseBaseNodeFactory>> = create_node_factory::<ParseBaseNodeFactory>(
        NodeFactoryFlags::NoParenthesizerRules,
        /*parse_base_node_factory.with(|_parse_base_node_factory| _parse_base_node_factory)*/
    );
}

pub(super) fn visit_node<TNodeRef: Borrow<Node>, TNodeCallback: FnMut(&Node)>(
    cb_node: &mut TNodeCallback,
    node: Option<TNodeRef>,
) {
    if let Some(node) = node {
        cb_node(node.borrow());
    }
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

pub(super) fn visit_nodes<TNodeCallback: FnMut(&Node), TNodesCallback: FnMut(&NodeArray)>(
    cb_node: &mut TNodeCallback,
    cb_nodes: Option<&mut TNodesCallback>,
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

pub(crate) fn is_jsdoc_like_text(text: &SourceText, start: usize) -> bool {
    text.len() >= start + 3 && {
        let text_slice = text.slice(1, None);
        text_slice.starts_with("**") && !text_slice.starts_with("**/")
    }
}
