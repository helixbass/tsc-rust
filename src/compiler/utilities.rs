use crate::{BaseNode, DiagnosticMessage, DiagnosticWithDetachedLocation, SyntaxKind};

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

pub fn create_detached_diagnostic(
    file_name: &str,
    start: usize,
    length: usize,
    message: &DiagnosticMessage,
) -> DiagnosticWithDetachedLocation {
    DiagnosticWithDetachedLocation {
        start,
        length,

        file_name: file_name.to_string(),
    }
}
