use std::rc::Rc;

use debug_cell::{Ref, RefCell, RefMut};
use id_arena::{Arena, Id};
use once_cell::unsync::Lazy;

use crate::{Type, TypeInterface};

#[derive(Default)]
pub struct AllArenas {
    // pub nodes: RefCell<Arena<Node>>,
    // pub node_arrays: RefCell<Arena<NodeArray>>,
    // pub emit_nodes: RefCell<Arena<EmitNode>>,
    // pub symbols: RefCell<Arena<Symbol>>,
    // pub symbol_tables: RefCell<Arena<SymbolTable>>,
    pub types: RefCell<Arena<Type>>,
}

impl AllArenas {
    // pub fn node(&self, node: Id<Node>) -> &Node {
    //     &self.nodes.borrow()[node]
    // }

    // pub fn node_mut(&self, node: Id<Node>) -> &mut Node {
    //     &mut self.nodes.borrow_mut()[node]
    // }

    #[track_caller]
    pub fn type_(&self, type_: Id<Type>) -> Ref<Type> {
        Ref::map(self.types.borrow(), |types| &types[type_])
    }

    // #[track_caller]
    // pub fn type_mut(&self, type_: Id<Type>) -> RefMut<Type> {
    //     RefMut::map(self.types.borrow_mut(), |types| &mut types[type_])
    // }

    pub fn create_type(&self, type_: Type) -> Id<Type> {
        let id = self.types.borrow_mut().alloc(type_);
        self.type_(id).set_arena_id(id);
        id
    }
}

thread_local! {
    static ARENA: Lazy<Rc<AllArenas>> = Lazy::new(Default::default);
}

pub fn static_arena() -> Rc<AllArenas> {
    ARENA.with(|arena| (**arena).clone())
}
