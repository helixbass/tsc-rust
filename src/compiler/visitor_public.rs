use std::borrow::Borrow;
use std::rc::Rc;

use crate::{Node, VisitResult};

pub fn visit_node<
    TNode: Borrow<Node>,
    TVisitor: FnMut(&Node) -> VisitResult,
    TTest: Fn(&Node) -> bool,
    TLift: Fn(&[Rc<Node>]) -> Rc<Node>,
>(
    node: Option<TNode>,
    visitor: Option<TVisitor>,
    test: Option<TTest>,
    lift: Option<TLift>,
) -> Option<Rc<Node>> {
    unimplemented!()
}
