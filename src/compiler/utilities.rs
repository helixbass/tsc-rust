use crate::{BaseNode, SyntaxKind};

fn Node(kind: SyntaxKind) -> BaseNode {
    BaseNode { kind }
}

struct ObjectAllocator {}

impl ObjectAllocator {
    pub fn get_node_constructor(&self) -> fn(SyntaxKind) -> BaseNode {
        Node
    }
}

lazy_static! {
    pub static ref object_allocator: ObjectAllocator = ObjectAllocator {};
}
