use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    chain_bundle, is_block, visit_each_child, visit_node, BaseNodeFactorySynthetic, Node,
    NodeArray, NodeFactory, NodeInterface, SyntaxKind, TransformFlags, TransformationContext,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
    VisitResult,
};
use std::{io, mem};

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

    fn transform_source_file(&self, node: &Node /*SourceFile*/) -> Gc<Node> {
        if node.as_source_file().is_declaration_file() {
            return node.node_wrapper();
        }

        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }

    fn visitor(&self, node: &Node) -> VisitResult /*<Node>*/ {
        if !node
            .transform_flags()
            .intersects(TransformFlags::ContainsES2019)
        {
            return Some(node.node_wrapper().into());
        }
        match node.kind() {
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node).into()),
            _ => visit_each_child(
                Some(node),
                |node: &Node| self.visitor(node),
                &**self.context,
                Option::<
                    fn(
                        Option<&NodeArray>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<usize>,
                        Option<usize>,
                    ) -> Option<Gc<NodeArray>>,
                >::None,
                Option::<fn(&Node) -> VisitResult>::None,
                Option::<
                    fn(
                        Option<&Node>,
                        Option<&mut dyn FnMut(&Node) -> VisitResult>,
                        Option<&dyn Fn(&Node) -> bool>,
                        Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                    ) -> Option<Gc<Node>>,
                >::None,
            )
            .map(Into::into),
        }
    }

    fn visit_catch_clause(&self, node: &Node /*CatchClause*/) -> Gc<Node /*CatchClause*/> {
        let node_as_catch_clause = node.as_catch_clause();
        if node_as_catch_clause.variable_declaration.is_none() {
            return self.factory.update_catch_clause(
                node,
                Some(
                    self.factory
                        .create_variable_declaration(
                            Some(
                                self.factory
                                    .create_temp_variable(Option::<fn(&Node)>::None, None),
                            ),
                            None,
                            None,
                            None,
                        )
                        .wrap(),
                ),
                visit_node(
                    Some(&*node_as_catch_clause.block),
                    Some(|node: &Node| self.visitor(node)),
                    Some(is_block),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )
                .unwrap(),
            );
        }
        visit_each_child(
            Some(node),
            |node: &Node| self.visitor(node),
            &**self.context,
            Option::<
                fn(
                    Option<&NodeArray>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<usize>,
                    Option<usize>,
                ) -> Option<Gc<NodeArray>>,
            >::None,
            Option::<fn(&Node) -> VisitResult>::None,
            Option::<
                fn(
                    Option<&Node>,
                    Option<&mut dyn FnMut(&Node) -> VisitResult>,
                    Option<&dyn Fn(&Node) -> bool>,
                    Option<&dyn Fn(&[Gc<Node>]) -> Gc<Node>>,
                ) -> Option<Gc<Node>>,
            >::None,
        )
        .unwrap()
    }
}

impl TransformerInterface for TransformES2019 {
    fn call(&self, node: &Node) -> io::Result<Gc<Node>> {
        Ok(self.transform_source_file(node))
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
