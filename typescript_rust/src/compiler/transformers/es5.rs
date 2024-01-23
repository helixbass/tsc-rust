use std::{collections::HashMap, io, mem};

use gc::{Finalize, Gc, GcCell, GcCellRef, GcCellRefMut, Trace};
use id_arena::Id;

use crate::{
    chain_bundle, gc_cell_ref_mut_unwrapped, get_original_node_id, id_text, is_identifier,
    is_private_identifier, is_property_access_expression, is_property_assignment,
    node_is_synthesized, string_to_token, BaseNodeFactorySynthetic, BoolExt, CompilerOptions,
    EmitHint, JsxEmit, Matches, NamedDeclarationInterface, Node, NodeExt, NodeFactory, NodeId,
    NodeInterface, SyntaxKind, TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface,
    HasArena, AllArenas, InArena,
    TransformNodesTransformationResult,
};

#[derive(Trace, Finalize)]
struct TransformES5 {
    _transformer_wrapper: GcCell<Option<Transformer>>,
    context: Id<TransformNodesTransformationResult>,
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    compiler_options: Gc<CompilerOptions>,
    no_substitution: GcCell<Option<HashMap<NodeId, bool>>>,
}

impl TransformES5 {
    fn new(context: Id<TransformNodesTransformationResult>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            compiler_options: compiler_options.clone(),
            context: context.clone(),
            no_substitution: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        if matches!(
            compiler_options.jsx,
            Some(JsxEmit::Preserve) | Some(JsxEmit::ReactNative)
        ) {
            context.override_on_emit_node(&mut |previous_on_emit_node| {
                Gc::new(Box::new(TransformES5OnEmitNodeOverrider::new(
                    downcasted.clone(),
                    previous_on_emit_node,
                )))
            });
            context.enable_emit_notification(SyntaxKind::JsxOpeningElement);
            context.enable_emit_notification(SyntaxKind::JsxClosingElement);
            context.enable_emit_notification(SyntaxKind::JsxSelfClosingElement);
            *downcasted.no_substitution.borrow_mut() = Some(Default::default());
        }
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(TransformES5OnSubstituteNodeOverrider::new(
                downcasted.clone(),
                previous_on_substitute_node,
            )))
        });
        context.enable_emit_notification(SyntaxKind::PropertyAccessExpression);
        context.enable_emit_notification(SyntaxKind::PropertyAssignment);

        downcasted
    }

    fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }

    fn maybe_no_substitution(&self) -> GcCellRef<Option<HashMap<usize, bool>>> {
        self.no_substitution.borrow()
    }

    fn no_substitution_mut(
        &self,
    ) -> GcCellRefMut<Option<HashMap<NodeId, bool>>, HashMap<NodeId, bool>> {
        gc_cell_ref_mut_unwrapped(&self.no_substitution)
    }

    fn transform_source_file(&self, node: Id<Node>) -> Id<Node> {
        node
    }
}

impl TransformerInterface for TransformES5 {
    fn call(&self, node: Id<Node>) -> io::Result<Id<Node>> {
        Ok(self.transform_source_file(node))
    }
}

impl HasArena for TransformES5 {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES5OnEmitNodeOverrider {
    transform_es5: Gc<Box<TransformES5>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformES5OnEmitNodeOverrider {
    fn new(
        transform_es5: Gc<Box<TransformES5>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es5,
            previous_on_emit_node,
        }
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
                self.transform_es5
                    .no_substitution_mut()
                    .insert(get_original_node_id(tag_name, self), true);
            }
            _ => (),
        }

        self.previous_on_emit_node
            .on_emit_node(hint, node, emit_callback)
    }
}

impl HasArena for TransformES5OnEmitNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES5OnSubstituteNodeOverrider {
    transform_es5: Gc<Box<TransformES5>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformES5OnSubstituteNodeOverrider {
    fn new(
        transform_es5: Gc<Box<TransformES5>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_es5,
            previous_on_substitute_node,
        }
    }

    fn substitute_property_access_expression(
        &self,
        node: Id<Node>, /*PropertyAccessExpression*/
    ) -> Id<Node /*Expression*/> {
        let node_ref = node.ref_(self);
        let node_as_property_access_expression = node_ref.as_property_access_expression();
        if is_private_identifier(&node_as_property_access_expression.name.ref_(self)) {
            return node;
        }
        let literal_name =
            self.try_substitute_reserved_name(node_as_property_access_expression.name);
        if let Some(literal_name) = literal_name {
            return self
                .transform_es5
                .factory
                .create_element_access_expression(
                    node_as_property_access_expression.expression.clone(),
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
            return self.transform_es5.factory.update_property_assignment(
                node,
                literal_name,
                node_as_property_assignment.initializer.clone(),
            );
        }
        node
    }

    fn try_substitute_reserved_name(&self, name: Id<Node> /*Identifier*/) -> Option<Id<Node>> {
        let name_ref = name.ref_(self);
        let name_as_identifier = name_ref.as_identifier();
        let token = name_as_identifier
            .original_keyword_kind
            .or_else(|| node_is_synthesized(&*name.ref_(self)).then_and(|| string_to_token(id_text(&name.ref_(self)))));
        if token.matches(|token| {
            token >= SyntaxKind::FirstReservedWord && token <= SyntaxKind::LastReservedWord
        }) {
            return Some(
                self.transform_es5
                    .factory
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
            (node.ref_(self).maybe_id(), self.transform_es5.maybe_no_substitution().as_ref()),
            (Some(node_id), Some(no_substitution)) if no_substitution.get(&node_id).copied() == Some(true)
        ) {
            return self
                .previous_on_substitute_node
                .on_substitute_node(hint, node);
        }

        let node = self
            .previous_on_substitute_node
            .on_substitute_node(hint, node)?;
        if is_property_access_expression(&node.ref_(self)) {
            return Ok(self.substitute_property_access_expression(node));
        } else if is_property_assignment(&node.ref_(self)) {
            return Ok(self.substitute_property_assignment(node));
        }
        Ok(node)
    }
}

impl HasArena for TransformES5OnSubstituteNodeOverrider {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES5Factory {}

impl TransformES5Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES5Factory {
    fn call(&self, context: Id<TransformNodesTransformationResult>) -> Transformer {
        chain_bundle().call(context.clone(), TransformES5::new(context).as_transformer())
    }
}

pub fn transform_es5() -> TransformerFactory {
    Gc::new(Box::new(TransformES5Factory::new()))
}
