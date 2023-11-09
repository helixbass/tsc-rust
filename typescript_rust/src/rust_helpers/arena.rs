use std::cell::RefCell;

use id_arena::{Arena, Id};

use crate::{EmitNode, Node, NodeArray, Symbol, SymbolTable};

pub struct AllArenas {
    pub nodes: RefCell<Arena<Node>>,
    pub node_arrays: RefCell<Arena<NodeArray>>,
    pub emit_nodes: RefCell<Arena<EmitNode>>,
    pub symbols: RefCell<Arena<Symbol>>,
    pub symbol_tables: RefCell<Arena<SymbolTable>>,
}

impl AllArenas {
    pub fn node(&self, node: Id<Node>) -> &Node {
        &self.nodes.borrow()[node]
    }

    pub fn node_mut(&self, node: Id<Node>) -> &mut Node {
        &mut self.nodes.borrow_mut()[node]
    }
}
