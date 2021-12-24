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

#[allow(non_snake_case)]
pub struct BaseNodeFactoryConcrete {
    SourceFileConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    IdentifierConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    TokenConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
    NodeConstructor: Option<fn(SyntaxKind, isize, isize) -> BaseNode>,
}

impl BaseNodeFactoryConcrete {
    pub fn new() -> Self {
        Self {
            SourceFileConstructor: None,
            IdentifierConstructor: None,
            TokenConstructor: None,
            NodeConstructor: None,
        }
    }
}

impl BaseNodeFactory for BaseNodeFactoryConcrete {
    fn create_base_source_file_node(&self, kind: SyntaxKind) -> BaseNode {
        if self.SourceFileConstructor.is_none() {
            self.SourceFileConstructor = Some(object_allocator.get_source_file_constructor());
        }
        (self.SourceFileConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_identifier_node(&self, kind: SyntaxKind) -> BaseNode {
        if self.IdentifierConstructor.is_none() {
            self.IdentifierConstructor = Some(object_allocator.get_identifier_constructor());
        }
        (self.IdentifierConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_token_node(&self, kind: SyntaxKind) -> BaseNode {
        if self.TokenConstructor.is_none() {
            self.TokenConstructor = Some(object_allocator.get_token_constructor());
        }
        (self.TokenConstructor.unwrap())(kind, -1, -1)
    }

    fn create_base_node(&self, kind: SyntaxKind) -> BaseNode {
        if self.NodeConstructor.is_none() {
            self.NodeConstructor = Some(object_allocator.get_node_constructor());
        }
        (self.NodeConstructor.unwrap())(kind, -1, -1)
    }
}
