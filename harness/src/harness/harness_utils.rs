use gc::{Finalize, Gc, GcCell, Trace};
use once_cell::sync::Lazy;
use std::cell::Cell;
use typescript_rust::{
    are_option_gcs_equal, create_get_canonical_file_name, for_each_child, Node, NodeArray,
    NodeInterface, ReadonlyTextRange,
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

pub fn assert_invariants(node: Option<&Node>, parent: Option<&Node>) {
    let mut queue: Vec<(Option<Gc<Node>>, Option<Gc<Node>>)> = vec![(
        node.map(|node| node.node_wrapper()),
        parent.map(|parent| parent.node_wrapper()),
    )];
    let mut i = 0;
    while i < queue.len() {
        let (node, parent) = queue[i].clone();
        assert_invariants_worker(&mut queue, node.as_deref(), parent.as_deref());
        i += 1;
    }
}

fn assert_invariants_worker(
    queue: &mut Vec<(Option<Gc<Node>>, Option<Gc<Node>>)>,
    node: Option<&Node>,
    parent: Option<&Node>,
) {
    if let Some(node) = node {
        assert!(!(node.pos() < 0), "node.pos < 0");
        assert!(!(node.end() < 0), "node.end < 0");
        assert!(!(node.end() < node.pos()), "node.end < node.pos");
        assert!(
            are_option_gcs_equal(
                node.maybe_parent().as_ref(),
                parent.map(|parent| parent.node_wrapper()).as_ref()
            ),
            "node,parent !== parent"
        );

        if let Some(parent) = parent {
            assert!(!(node.pos() < parent.pos()), "node.pos < parent.pos");
            assert!(!(node.end() > parent.end()), "node.end > parent.end");
        }

        for_each_child(
            node,
            |child: &Node| {
                queue.push((Some(child.node_wrapper()), Some(node.node_wrapper())));
            },
            Option::<fn(&NodeArray)>::None,
        );

        let current_pos = Cell::new(0);
        for_each_child(
            node,
            |child: &Node| {
                assert!(!(child.pos() < current_pos.get()), "child.pos < currentPos");
                current_pos.set(child.end());
            },
            Some(|array: &NodeArray| {
                assert!(!(array.pos() < node.pos()), "array.pos < node.pos");
                assert!(!(array.end() > node.end()), "array.end > node.end");
                assert!(!(array.pos() < current_pos.get()), "array.pos < currentPos");

                for item in array {
                    assert!(
                        !(item.pos() < current_pos.get()),
                        "array[i].pos < currentPos"
                    );
                    current_pos.set(item.end());
                }

                current_pos.set(array.end());
            }),
        );

        let child_nodes_and_arrays: GcCell<Vec<RcNodeOrNodeArray>> = Default::default();
        for_each_child(
            node,
            |child: &Node| {
                child_nodes_and_arrays
                    .borrow_mut()
                    .push(child.node_wrapper().into());
            },
            Some(|array: &NodeArray| {
                child_nodes_and_arrays
                    .borrow_mut()
                    .push(array.rc_wrapper().into());
            }),
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
    RcNode(Gc<Node>),
    NodeArray(Gc<NodeArray>),
}

impl From<Gc<Node>> for RcNodeOrNodeArray {
    fn from(value: Gc<Node>) -> Self {
        Self::RcNode(value)
    }
}

impl From<Gc<NodeArray>> for RcNodeOrNodeArray {
    fn from(value: Gc<NodeArray>) -> Self {
        Self::NodeArray(value)
    }
}
