use std::{any::Any, io};

use id_arena::Id;

use crate::{
    chain_bundle, impl_has_arena, is_block, maybe_visit_each_child, visit_each_child, visit_node,
    AllArenas, CoreTransformationContext, HasArena, InArena, Node, NodeFactory, NodeInterface,
    SyntaxKind, TransformFlags, TransformNodesTransformationResult, Transformer,
    TransformerFactory, TransformerFactoryInterface, TransformerInterface, VisitResult,
};

struct TransformES2019 {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    factory: Id<NodeFactory>,
}

impl TransformES2019 {
    fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        arena_ref.alloc_transformer(Box::new(Self {
            arena,
            factory: context_ref.factory(),
            context,
        }))
    }

    fn transform_source_file(&self, node: Id<Node> /*SourceFile*/) -> Id<Node> {
        if node.ref_(self).as_source_file().is_declaration_file() {
            return node;
        }

        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }

    fn visitor(&self, node: Id<Node>) -> VisitResult /*<Node>*/ {
        if !node
            .ref_(self)
            .transform_flags()
            .intersects(TransformFlags::ContainsES2019)
        {
            return Some(node.into());
        }
        match node.ref_(self).kind() {
            SyntaxKind::CatchClause => Some(self.visit_catch_clause(node).into()),
            _ => maybe_visit_each_child(
                Some(node),
                |node: Id<Node>| self.visitor(node),
                &*self.context.ref_(self),
                self,
            )
            .map(Into::into),
        }
    }

    fn visit_catch_clause(&self, node: Id<Node> /*CatchClause*/) -> Id<Node /*CatchClause*/> {
        let node_ref = node.ref_(self);
        let node_as_catch_clause = node_ref.as_catch_clause();
        if node_as_catch_clause.variable_declaration.is_none() {
            return self.factory.ref_(self).update_catch_clause(
                node,
                Some(
                    self.factory.ref_(self).create_variable_declaration(
                        Some(
                            self.factory
                                .ref_(self)
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
        visit_each_child(
            node,
            |node: Id<Node>| self.visitor(node),
            &*self.context.ref_(self),
            self,
        )
    }
}

impl TransformerInterface for TransformES2019 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        unimplemented!()
    }
}

impl_has_arena!(TransformES2019);

struct TransformES2019Factory {
    arena: *const AllArenas,
}

impl TransformES2019Factory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformES2019Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self)
            .ref_(self)
            .call(context.clone(), TransformES2019::new(context, self.arena))
    }
}

impl_has_arena!(TransformES2019Factory);

pub fn transform_es2019(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES2019Factory::new(arena)))
}
