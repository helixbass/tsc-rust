use std::{
    any::Any,
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    io,
};

use id_arena::Id;

use crate::{
    chain_bundle, downcast_transformer_ref, get_original_node_id, id_text, impl_has_arena,
    is_identifier, is_private_identifier, is_property_access_expression, is_property_assignment,
    node_is_synthesized, ref_mut_unwrapped, released, string_to_token, AllArenas, BoolExt,
    CoreTransformationContext, EmitHint, HasArena, InArena, JsxEmit, Matches,
    NamedDeclarationInterface, Node, NodeExt, NodeFactory, NodeId, NodeInterface, SyntaxKind,
    TransformNodesTransformationResult, TransformationContext,
    TransformationContextOnEmitNodeOverrider, TransformationContextOnSubstituteNodeOverrider,
    Transformer, TransformerFactory, TransformerFactoryInterface, TransformerInterface,
};

struct TransformES5 {
    arena: *const AllArenas,
    factory: Id<NodeFactory>,
    no_substitution: RefCell<Option<HashMap<NodeId, bool>>>,
}

impl TransformES5 {
    fn new(
        context: Id<TransformNodesTransformationResult>,
        arena: *const AllArenas,
    ) -> Transformer {
        let arena_ref = unsafe { &*arena };
        let context_ref = context.ref_(arena_ref);
        let compiler_options = context_ref.get_compiler_options();
        let ret = arena_ref.alloc_transformer(Box::new(Self {
            arena,
            factory: context_ref.factory(),
            no_substitution: Default::default(),
        }));
        if matches!(
            compiler_options.ref_(arena_ref).jsx,
            Some(JsxEmit::Preserve) | Some(JsxEmit::ReactNative)
        ) {
            context_ref.override_on_emit_node(&mut |previous_on_emit_node| {
                arena_ref.alloc_transformation_context_on_emit_node_overrider(Box::new(
                    TransformES5OnEmitNodeOverrider::new(ret, previous_on_emit_node, arena_ref),
                ))
            });
            context_ref.enable_emit_notification(SyntaxKind::JsxOpeningElement);
            context_ref.enable_emit_notification(SyntaxKind::JsxClosingElement);
            context_ref.enable_emit_notification(SyntaxKind::JsxSelfClosingElement);
            *downcast_transformer_ref::<TransformES5>(ret, arena_ref)
                .no_substitution
                .borrow_mut() = Some(Default::default());
        }
        context_ref.override_on_substitute_node(&mut |previous_on_substitute_node| {
            arena_ref.alloc_transformation_context_on_substitute_node_overrider(Box::new(
                TransformES5OnSubstituteNodeOverrider::new(
                    ret,
                    previous_on_substitute_node,
                    arena_ref,
                ),
            ))
        });
        context_ref.enable_emit_notification(SyntaxKind::PropertyAccessExpression);
        context_ref.enable_emit_notification(SyntaxKind::PropertyAssignment);

        ret
    }

    fn maybe_no_substitution(&self) -> Ref<Option<HashMap<usize, bool>>> {
        self.no_substitution.borrow()
    }

    fn no_substitution_mut(&self) -> RefMut<HashMap<NodeId, bool>> {
        ref_mut_unwrapped(&self.no_substitution)
    }

    fn transform_source_file(&self, node: Id<Node>) -> Id<Node> {
        node
    }
}

impl TransformerInterface for TransformES5 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }

    fn as_dyn_any(&self) -> &dyn Any {
        self
    }
}

impl_has_arena!(TransformES5);

struct TransformES5OnEmitNodeOverrider {
    arena: *const AllArenas,
    transform_es5: Transformer,
    previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES5OnEmitNodeOverrider {
    fn new(
        transform_es5: Transformer,
        previous_on_emit_node: Id<Box<dyn TransformationContextOnEmitNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es5,
            previous_on_emit_node,
        }
    }

    fn transform_es5(&self) -> debug_cell::Ref<'_, TransformES5> {
        downcast_transformer_ref(self.transform_es5, self)
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformES5OnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        hint: EmitHint,
        node: Id<Node>,
        emit_callback: &dyn Fn(EmitHint, Id<Node>) -> io::Result<()>,
    ) -> io::Result<()> {
        match node.ref_(self).kind() {
            SyntaxKind::JsxOpeningElement
            | SyntaxKind::JsxClosingElement
            | SyntaxKind::JsxSelfClosingElement => {
                let tag_name = node.ref_(self).as_has_tag_name().tag_name();
                self.transform_es5()
                    .no_substitution_mut()
                    .insert(get_original_node_id(tag_name, self), true);
            }
            _ => (),
        }

        self.previous_on_emit_node
            .ref_(self)
            .on_emit_node(hint, node, emit_callback)
    }
}

impl_has_arena!(TransformES5OnEmitNodeOverrider);

struct TransformES5OnSubstituteNodeOverrider {
    arena: *const AllArenas,
    transform_es5: Transformer,
    previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES5OnSubstituteNodeOverrider {
    fn new(
        transform_es5: Transformer,
        previous_on_substitute_node: Id<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
            transform_es5,
            previous_on_substitute_node,
        }
    }

    fn transform_es5(&self) -> debug_cell::Ref<'_, TransformES5> {
        downcast_transformer_ref(self.transform_es5, self)
    }

    fn substitute_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> Id<Node /*Expression*/> {
        if is_private_identifier(
            &node
                .ref_(self)
                .as_property_access_expression()
                .name
                .ref_(self),
        ) {
            return node;
        }
        let literal_name = self.try_substitute_reserved_name(released!(
            node.ref_(self).as_property_access_expression().name
        ));
        if let Some(literal_name) = literal_name {
            return self
                .transform_es5()
                .factory
                .ref_(self)
                .create_element_access_expression(
                    node.ref_(self)
                        .as_property_access_expression()
                        .expression
                        .clone(),
                    literal_name,
                )
                .set_text_range(Some(&*node.ref_(self)), self);
        }
        node
    }

    fn substitute_property_assignment(
        &self,
        node: Id<Node>, /*PropertyAssignment*/
    ) -> Id<Node /*PropertyAssignment*/> {
        let node_ref = node.ref_(self);
        let node_as_property_assignment = node_ref.as_property_assignment();
        let literal_name = is_identifier(&node_as_property_assignment.name().ref_(self))
            .then_and(|| self.try_substitute_reserved_name(node_as_property_assignment.name()));
        if let Some(literal_name) = literal_name {
            return self
                .transform_es5()
                .factory
                .ref_(self)
                .update_property_assignment(
                    node,
                    literal_name,
                    node_as_property_assignment.initializer.clone(),
                );
        }
        node
    }

    fn try_substitute_reserved_name(&self, name: Id<Node> /*Identifier*/) -> Option<Id<Node>> {
        let token = name
            .ref_(self)
            .as_identifier()
            .original_keyword_kind
            .or_else(|| {
                node_is_synthesized(&*name.ref_(self))
                    .then_and(|| string_to_token(id_text(&name.ref_(self))))
            });
        if token.matches(|token| {
            token >= SyntaxKind::FirstReservedWord && token <= SyntaxKind::LastReservedWord
        }) {
            return Some(
                self.transform_es5()
                    .factory
                    .ref_(self)
                    .create_string_literal_from_node(name)
                    .set_text_range(Some(&*name.ref_(self)), self),
            );
        }
        None
    }
}

impl TransformationContextOnSubstituteNodeOverrider for TransformES5OnSubstituteNodeOverrider {
    fn on_substitute_node(&self, hint: EmitHint, node: Id<Node>) -> io::Result<Id<Node>> {
        if matches!(
            (node.ref_(self).maybe_id(), self.transform_es5().maybe_no_substitution().as_ref()),
            (Some(node_id), Some(no_substitution)) if no_substitution.get(&node_id).copied() == Some(true)
        ) {
            return self
                .previous_on_substitute_node
                .ref_(self)
                .on_substitute_node(hint, node);
        }

        let node = self
            .previous_on_substitute_node
            .ref_(self)
            .on_substitute_node(hint, node)?;
        if is_property_access_expression(&node.ref_(self)) {
            return Ok(self.substitute_property_access_expression(node));
        } else if is_property_assignment(&node.ref_(self)) {
            return Ok(self.substitute_property_assignment(node));
        }
        Ok(node)
    }
}

impl_has_arena!(TransformES5OnSubstituteNodeOverrider);

struct TransformES5Factory {
    arena: *const AllArenas,
}

impl TransformES5Factory {
    fn new(arena: &impl HasArena) -> Self {
        Self {
            arena: arena.arena(),
        }
    }
}

impl TransformerFactoryInterface for TransformES5Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle(self)
            .ref_(self)
            .call(context.clone(), TransformES5::new(context, self.arena))
    }
}

impl_has_arena!(TransformES5Factory);

pub fn transform_es5(arena: &impl HasArena) -> TransformerFactory {
    arena.alloc_transformer_factory(Box::new(TransformES5Factory::new(arena)))
}
