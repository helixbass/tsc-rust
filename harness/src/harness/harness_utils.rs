use std::cell::{Cell, RefCell};

use gc::{Finalize, GcCell, Trace};
use once_cell::sync::Lazy;
use typescript_rust::{
    create_get_canonical_file_name, for_each_child, id_arena::Id, HasArena, InArena, Node,
    NodeArray, NodeInterface, ReadonlyTextRange,
};

pub fn encode_string(s: &str) -> String {
    s.to_owned()
    // return ts.sys.bufferFrom!(s).toString("utf8");
}

pub fn split_content_by_newlines(content: &str) -> Vec<&str> {
    let mut lines = content.split("\r\n").collect::<Vec<_>>();
    if lines.len() == 1 {
        lines = content.split("\n").collect::<Vec<_>>();

        if lines.len() == 1 {
            lines = content.split("\r").collect::<Vec<_>>();
        }
    }
    lines
}

pub fn canonicalize_for_harness(file_name: &str) -> String {
    static INSTANCE: Lazy<fn(&str) -> String> = Lazy::new(|| create_get_canonical_file_name(false));
    (*Lazy::force(&INSTANCE))(file_name)
}

pub fn assert_invariants(node: Option<Id<Node>>, parent: Option<Id<Node>>, arena: &impl HasArena) {
    let mut queue: Vec<(Option<Id<Node>>, Option<Id<Node>>)> = vec![(node, parent)];
    let mut i = 0;
    while i < queue.len() {
        let (node, parent) = queue[i].clone();
        assert_invariants_worker(&mut queue, node, parent, arena);
        i += 1;
    }
}

fn assert_invariants_worker(
    queue: &mut Vec<(Option<Id<Node>>, Option<Id<Node>>)>,
    node: Option<Id<Node>>,
    parent: Option<Id<Node>>,
    arena: &impl HasArena,
) {
    if let Some(node) = node {
        assert!(!(node.ref_(arena).pos() < 0), "node.pos < 0");
        assert!(!(node.ref_(arena).end() < 0), "node.end < 0");
        assert!(
            !(node.ref_(arena).end() < node.ref_(arena).pos()),
            "node.end < node.pos"
        );
        assert!(
            node.ref_(arena).maybe_parent() == parent,
            "node,parent !== parent"
        );

        if let Some(parent) = parent {
            assert!(
                !(node.ref_(arena).pos() < parent.ref_(arena).pos()),
                "node.pos < parent.pos"
            );
            assert!(
                !(node.ref_(arena).end() > parent.ref_(arena).end()),
                "node.end > parent.end"
            );
        }

        for_each_child(
            node,
            |child: Id<Node>| {
                queue.push((Some(child), Some(node)));
            },
            Option::<fn(Id<NodeArray>)>::None,
            arena,
        );

        let current_pos = Cell::new(0);
        for_each_child(
            node,
            |child: Id<Node>| {
                assert!(
                    !(child.ref_(arena).pos() < current_pos.get()),
                    "child.pos < currentPos"
                );
                current_pos.set(child.ref_(arena).end());
            },
            Some(|array: Id<NodeArray>| {
                assert!(
                    !(array.ref_(arena).pos() < node.ref_(arena).pos()),
                    "array.pos < node.pos"
                );
                assert!(
                    !(array.ref_(arena).end() > node.ref_(arena).end()),
                    "array.end > node.end"
                );
                assert!(
                    !(array.ref_(arena).pos() < current_pos.get()),
                    "array.pos < currentPos"
                );

                for item in &*array.ref_(arena) {
                    assert!(
                        !(item.ref_(arena).pos() < current_pos.get()),
                        "array[i].pos < currentPos"
                    );
                    current_pos.set(item.ref_(arena).end());
                }

                current_pos.set(array.ref_(arena).end());
            }),
            arena,
        );

        let child_nodes_and_arrays: RefCell<Vec<RcNodeOrNodeArray>> = Default::default();
        for_each_child(
            node,
            |child: Id<Node>| {
                child_nodes_and_arrays.borrow_mut().push(child.into());
            },
            Some(|array: Id<NodeArray>| {
                child_nodes_and_arrays.borrow_mut().push(array.into());
            }),
            arena,
        );

        // for (const childName in node) {
        //     if (childName === "parent" || childName === "nextContainer" || childName === "modifiers" || childName === "externalModuleIndicator" ||
        //         // for now ignore jsdoc comments
        //         childName === "jsDocComment" || childName === "checkJsDirective" || childName === "commonJsModuleIndicator") {
        //         continue;
        //     }
        //     const child = (node as any)[childName];
        //     if (isNodeOrArray(child)) {
        //         assert.isFalse(childNodesAndArrays.indexOf(child) < 0,
        //             "Missing child when forEach'ing over node: " + (ts as any).SyntaxKind[node.kind] + "-" + childName);
        //     }
        // }
    }
}

#[derive(Trace, Finalize)]
enum RcNodeOrNodeArray {
    RcNode(Id<Node>),
    NodeArray(Id<NodeArray>),
}

impl From<Id<Node>> for RcNodeOrNodeArray {
    fn from(value: Id<Node>) -> Self {
        Self::RcNode(value)
    }
}

impl From<Id<NodeArray>> for RcNodeOrNodeArray {
    fn from(value: Id<NodeArray>) -> Self {
        Self::NodeArray(value)
    }
}
