#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::rc::Rc;
use std::sync::RwLock;

use crate::{
    for_each, for_each_child, set_parent, Expression, ExpressionStatement, Node, NodeArray,
    NodeInterface, SourceFile, Statement, SyntaxKind,
};

bitflags! {
    struct ContainerFlags: u32 {
        const None = 0;

        const IsContainer = 1 << 0;

        const IsControlFlowContainer = 1 << 2;

        const HasLocals = 1 << 5;
    }
}

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: Rc<SourceFile>) {
    // binder.call(file);
    create_binder().call(file);
}

struct BinderType {
    file: RwLock<Option<Rc<SourceFile>>>,
    parent: RwLock<Option<Rc<Node>>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: RwLock::new(None),
        parent: RwLock::new(None),
    }
}

impl BinderType {
    fn call(&self, f: Rc<SourceFile>) {
        self.bind_source_file(f);
    }

    fn file(&self) -> Rc<SourceFile> {
        self.file.read().unwrap().as_ref().unwrap().clone()
    }

    fn set_file(&self, file: Option<Rc<SourceFile>>) {
        *self.file.write().unwrap() = file;
    }

    fn parent(&self) -> Rc<Node> {
        self.parent.read().unwrap().as_ref().unwrap().clone()
    }

    fn set_parent(&self, parent: Option<Rc<Node>>) {
        *self.parent.write().unwrap() = parent;
    }

    fn bind_source_file(&self, f: Rc<SourceFile>) {
        self.set_file(Some(f.clone()));

        if true {
            self.bind(Some(Rc::new(self.file().into())));
        }

        self.set_file(None);
        self.set_parent(None);
    }

    fn bind_container(&self, node: Rc<Node>, container_flags: ContainerFlags) {
        self.bind_children(node);
    }

    fn bind_each_functions_first(&self, nodes: &NodeArray) {
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    fn bind_each(&self, nodes: &NodeArray) {
        for_each(nodes, |node, _| {
            self.bind(Some(node.clone()));
            Option::<()>::None
        });
    }

    fn bind_each_callback<TNodeCallback: FnMut(Rc<Node>)>(
        nodes: &NodeArray,
        mut bind_function: TNodeCallback,
    ) {
        for_each(nodes, |node, _| {
            bind_function(node.clone());
            Option::<()>::None
        });
    }

    fn bind_each_child(&self, node: Rc<Node>) {
        for_each_child(
            node,
            |node| self.bind(Some(node)),
            |nodes| self.bind_each(nodes),
        );
    }

    fn bind_children(&self, node: Rc<Node>) {
        match &*node {
            Node::Statement(statement) => match statement {
                Statement::ExpressionStatement(expression_statement) => {
                    self.bind_expression_statement(expression_statement);
                }
                _ => unimplemented!(),
            },
            Node::Expression(expression) => match expression {
                Expression::PrefixUnaryExpression(_) => {
                    self.bind_prefix_unary_expression_flow(node);
                }
                _ => unimplemented!(),
            },
            Node::SourceFile(source_file) => {
                self.bind_each_functions_first(&source_file.statements);
            }
            Node::BaseNode(_) => panic!("Didn't expect to bind BaseNode?"),
        };
    }

    fn bind_expression_statement(&self, node: &ExpressionStatement) {
        self.bind(Some(node.expression.clone()));
    }

    fn bind_prefix_unary_expression_flow(&self, node: Rc<Node>) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    fn get_container_flags(&self, node: Rc<Node>) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::SourceFile => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals;
            }
            _ => (),
        }

        ContainerFlags::None
    }

    fn bind(&self, node: Option<Rc<Node>>) {
        let node = match node.as_ref() {
            None => {
                return;
            }
            Some(node) => node.clone(),
        };
        set_parent(
            &*node,
            match self.parent.read().unwrap().as_ref() {
                None => None,
                Some(parent) => Some(parent.clone()),
            },
        );

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = match *self.parent.read().unwrap() {
                None => None,
                Some(ref rc_node) => Some(rc_node.clone()),
            };
            self.set_parent(Some(node.clone()));
            let container_flags = self.get_container_flags(node.clone());
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }
}
