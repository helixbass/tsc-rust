use std::cell::RefCell;

use crate::{object_allocator, BaseNode, SyntaxKind};

pub trait BaseNodeFactory {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode;
    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode;
}

pub fn create_base_node_factory() -> BaseNodeFactoryConcrete {
    BaseNodeFactoryConcrete::new()
}

#[derive(Debug)]
#[allow(non_snake_case)]
pub struct BaseNodeFactoryConcrete {
    SourceFileConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    IdentifierConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    TokenConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
    NodeConstructor: RefCell<Option<fn(SyntaxKind, isize, isize) -> BaseNode>>,
}

impl BaseNodeFactoryConcrete {
    pub fn new() -> Self {
        Self {
            SourceFileConstructor: RefCell::new(None),
            IdentifierConstructor: RefCell::new(None),
            TokenConstructor: RefCell::new(None),
            NodeConstructor: RefCell::new(None),
        }
    }
}

#[allow(non_snake_case)]
impl BaseNodeFactory for BaseNodeFactoryConcrete {
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
