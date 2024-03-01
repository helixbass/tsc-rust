use std::cell::RefCell;

use bitflags::bitflags;
use id_arena::Id;

use crate::{
    create_node_factory, impl_has_arena, maybe_text_char_at_index, object_allocator, per_arena,
    released, AllArenas, BaseNode, BaseNodeFactory, CharacterCodes, HasArena, InArena, Node,
    NodeArray, NodeFactory, NodeFactoryFlags, OptionTry, SourceTextAsChars, SyntaxKind,
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
    arena: *const AllArenas,
    NodeConstructor: RefCell<Option<fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode>>,
    TokenConstructor: RefCell<Option<fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode>>,
    IdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode>>,
    PrivateIdentifierConstructor:
        RefCell<Option<fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode>>,
    SourceFileConstructor: RefCell<Option<fn(SyntaxKind, isize, isize, &AllArenas) -> BaseNode>>,
}

impl ParseBaseNodeFactory {
    pub fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
            NodeConstructor: Default::default(),
            TokenConstructor: Default::default(),
            IdentifierConstructor: Default::default(),
            PrivateIdentifierConstructor: Default::default(),
            SourceFileConstructor: Default::default(),
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
        (SourceFileConstructor.unwrap())(kind, -1, -1, self.arena())
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut IdentifierConstructor = self.IdentifierConstructor.borrow_mut();
        if IdentifierConstructor.is_none() {
            *IdentifierConstructor = Some(object_allocator.get_identifier_constructor());
        }
        (IdentifierConstructor.unwrap())(kind, -1, -1, self.arena())
    }

    fn create_base_private_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut PrivateIdentifierConstructor = self.PrivateIdentifierConstructor.borrow_mut();
        if PrivateIdentifierConstructor.is_none() {
            *PrivateIdentifierConstructor =
                Some(object_allocator.get_private_identifier_constructor());
        }
        (PrivateIdentifierConstructor.unwrap())(kind, -1, -1, self.arena())
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut TokenConstructor = self.TokenConstructor.borrow_mut();
        if TokenConstructor.is_none() {
            *TokenConstructor = Some(object_allocator.get_token_constructor());
        }
        (TokenConstructor.unwrap())(kind, -1, -1, self.arena())
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        let mut NodeConstructor = self.NodeConstructor.borrow_mut();
        if NodeConstructor.is_none() {
            *NodeConstructor = Some(object_allocator.get_node_constructor());
        }
        (NodeConstructor.unwrap())(kind, -1, -1, self.arena())
    }
}

impl_has_arena!(ParseBaseNodeFactory);

pub fn get_parse_base_node_factory(arena: &impl HasArena) -> Id<Box<dyn BaseNodeFactory>> {
    per_arena!(
        Box<dyn BaseNodeFactory>,
        arena,
        arena.alloc_base_node_factory(Box::new(ParseBaseNodeFactory::new(arena)))
    )
}

pub fn get_parse_node_factory(arena: &impl HasArena) -> debug_cell::Ref<'_, NodeFactory> {
    per_arena!(
        NodeFactory,
        arena,
        create_node_factory(
            NodeFactoryFlags::NoParenthesizerRules,
            get_parse_base_node_factory(arena),
            arena,
        )
    )
    .ref_(arena)
}

pub(super) fn visit_node(cb_node: &mut impl FnMut(Id<Node>), node: Option<Id<Node>>) {
    if let Some(node) = node {
        cb_node(node);
    }
}

pub(super) fn try_visit_node<TError>(
    cb_node: &mut impl FnMut(Id<Node>) -> Result<(), TError>,
    node: Option<Id<Node>>,
) -> Result<(), TError> {
    if let Some(node) = node {
        cb_node(node)?;
    }

    Ok(())
}

pub(super) fn visit_node_returns<TReturn, TNodeCallback: FnMut(Id<Node>) -> Option<TReturn>>(
    cb_node: &mut TNodeCallback,
    node: Option<Id<Node>>,
) -> Option<TReturn> {
    node.and_then(|node| cb_node(node))
}

pub(super) fn try_visit_node_returns<
    TReturn,
    TError,
    TNodeCallback: FnMut(Id<Node>) -> Result<Option<TReturn>, TError>,
>(
    cb_node: &mut TNodeCallback,
    node: Option<Id<Node>>,
) -> Result<Option<TReturn>, TError> {
    node.try_and_then(|node| cb_node(node))
}

pub(super) fn visit_nodes(
    cb_node: &mut impl FnMut(Id<Node>),
    cb_nodes: Option<&mut impl FnMut(Id<NodeArray>)>,
    nodes: Option<Id<NodeArray>>,
    arena: &impl HasArena,
) {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                cb_nodes(nodes);
            }
            None => {
                for &node in nodes.ref_(arena).iter() {
                    cb_node(node);
                }
            }
        }
    }
}

pub(super) fn try_visit_nodes<TError>(
    cb_node: &mut impl FnMut(Id<Node>) -> Result<(), TError>,
    cb_nodes: Option<&mut impl FnMut(Id<NodeArray>) -> Result<(), TError>>,
    nodes: Option<Id<NodeArray>>,
    arena: &impl HasArena,
) -> Result<(), TError> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                cb_nodes(nodes)?;
            }
            None => {
                for &node in released!(nodes.ref_(arena).clone()).iter() {
                    cb_node(node)?;
                }
            }
        }
    }

    Ok(())
}

pub(super) fn visit_nodes_returns<TReturn>(
    cb_node: &mut impl FnMut(Id<Node>) -> Option<TReturn>,
    cb_nodes: Option<&mut impl FnMut(Id<NodeArray>) -> Option<TReturn>>,
    nodes: Option<Id<NodeArray>>,
    arena: &impl HasArena,
) -> Option<TReturn> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                return cb_nodes(nodes);
            }
            None => {
                for &node in nodes.ref_(arena).iter() {
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

pub(super) fn try_visit_nodes_returns<TReturn, TError>(
    cb_node: &mut impl FnMut(Id<Node>) -> Result<Option<TReturn>, TError>,
    cb_nodes: Option<&mut impl FnMut(Id<NodeArray>) -> Result<Option<TReturn>, TError>>,
    nodes: Option<Id<NodeArray>>,
    arena: &impl HasArena,
) -> Result<Option<TReturn>, TError> {
    if let Some(nodes) = nodes {
        match cb_nodes {
            Some(cb_nodes) => {
                return cb_nodes(nodes);
            }
            None => {
                for &node in nodes.ref_(arena).iter() {
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
