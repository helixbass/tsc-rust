use std::{io, mem};

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, is_block, maybe_visit_each_child, visit_each_child, visit_node,
    BaseNodeFactorySynthetic, Node, NodeFactory, NodeInterface, SyntaxKind, TransformFlags,
    TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, VisitResult,
    HasArena, AllArenas, InArena,
};

#[derive(Trace, Finalize)]
struct TransformES2019 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Gc<Box<dyn TransformationContext>>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
}

impl TransformES2019 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            context,
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context)
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .ref_(self).transform_flags()
            .intersects(TransformFlags::ContainsES2019)
        {
            return Some(node.into());
        }
        match node.ref_(self).kind() {
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node).into()),
            _ => maybe_visit_each_child(
                Some(&node.ref_(self)),
                |node: Id<Node>| self.visitor(node),
                &**self.context,
            )
            .map(Into::into),
        }
    }

    fn visit_catch_clause(&self, node: Id<Node> /*CatchClause*/) -> Id<Node /*CatchClause*/> {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if node_as_catch_clause.variable_declaration.is_none() {
            return self.factory.update_catch_clause(
                node,
                Some(
                    self.factory.create_variable_declaration(
                        Some(
                            self.factory
                                .create_temp_variable(Option::<fn(Id<Node>)>::None, None),
                        ),
                        None,
                        None,
                        None,
                    ),
                ),
                visit_node(
                    node_as_catch_clause.block,
                    Some(|node: Id<Node>| self.visitor(node)),
                    Some(|node: Id<Node>| is_block(&node.ref_(self))),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                ),
            );
        }
        visit_each_child(&node.ref_(self), |node: Id<Node>| self.visitor(node), &**self.context)
    }
}

impl TransformerInterface for TransformES2019 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }
}

impl HasArena for TransformES2019 {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2019Factory {}

impl TransformES2019Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2019Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformES2019::new(context).as_transformer(),
        )
    }
}

pub fn transform_es2019() -> TransformerFactory {
    Gc::new(Box::new(TransformES2019Factory::new()))
}
