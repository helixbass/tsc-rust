use std::{borrow::Borrow, cell::RefCell, io, rc::Rc};

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    GetOrInsertDefault, Node, NodeExt, NodeInterface, NodeWrappered, NonEmpty, ReadonlyTextRange,
    TransformationContext, VisitResult, _d, is_destructuring_assignment, is_empty_array_literal,
    is_empty_object_literal, is_expression, is_identifier, node_is_synthesized, try_visit_node,
    BaseNodeFactory, Debug_, NodeFactory, OptionTry, ReadonlyTextRangeConcrete,
};

trait FlattenContext {
    fn context(&self) -> Gc<Box<dyn TransformationContext>>;
    fn level(&self) -> FlattenLevel;
    fn downlevel_iteration(&self) -> bool;
    fn hoist_temp_variables(&self) -> bool;
    fn has_transformed_prior_element(&self) -> Option<bool> {
        None
    }
    fn emit_expression(&self, value: &Node /*Expression*/);
    fn emit_binding_or_assignment(
        &self,
        target: &Node, /*BindingOrAssignmentElementTarget*/
        value: &Node,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<impl Borrow<Node>>,
    ) -> io::Result<()>;
    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ArrayBindingOrAssignmentPattern*/>;
    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ObjectBindingOrAssignmentPattern*/>;
    fn create_array_binding_or_assignment_element(
        &self,
        node: &Node, /*Identifier*/
    ) -> Gc<Node /*BindingOrAssignmentElement*/>;
    fn visitor(&self, _node: &Node) -> Option<io::Result<VisitResult /*<Node>*/>> {
        None
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FlattenLevel {
    All,
    ObjectRest,
}

pub trait CreateAssignmentCallback: Trace + Finalize {
    fn call(
        &self,
        name: &Node,  /*Identifier*/
        value: &Node, /*Expression*/
        location: Option<&dyn ReadonlyTextRange>,
    ) -> Gc<Node /*Expression*/>;
}

pub trait TryCreateAssignmentCallback: Trace + Finalize {
    fn call(
        &self,
        name: &Node,  /*Identifier*/
        value: &Node, /*Expression*/
        location: Option<&dyn ReadonlyTextRange>,
    ) -> io::Result<Gc<Node /*Expression*/>>;
}

// pub trait Visitor: Trace + Finalize {
//     fn call(
//         &self,
//         node: &Node
//     ) -> VisitResult;
// }

pub fn flatten_destructuring_assignment<'visitor, 'create_assignment_callback>(
    node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    visitor: Option<impl FnMut(&Node) -> VisitResult + 'visitor>,
    context: Gc<Box<dyn TransformationContext>>,
    level: FlattenLevel,
    needs_value: Option<bool>,
    create_assignment_callback: Option<
        impl FnMut(
                &Node, /*Identifier*/
                &Node, /*Expression*/
                Option<&dyn ReadonlyTextRange>,
            ) -> Gc<Node /*Expression*/>
            + 'create_assignment_callback,
    >,
) -> Gc<Node /*Expression*/> {
    try_flatten_destructuring_assignment(
        node,
        visitor.map(|mut visitor| move |node: &Node| Ok(visitor(node))),
        context,
        level,
        needs_value,
        create_assignment_callback.map(|mut create_assignment_callback| {
            move |a: &Node, b: &Node, c: Option<&dyn ReadonlyTextRange>| {
                Ok(create_assignment_callback(a, b, c))
            }
        }),
    )
    .unwrap()
}

fn emit_expression(
    expressions: &mut Option<Vec<Gc<Node /*Expression*/>>>,
    expression: Gc<Node /*Expression*/>,
) {
    expressions.get_or_insert_default_().push(expression);
}

struct FlattenDestructuringAssignmentFlattenContext<'visitor, 'create_assignment_callback> {
    context: Gc<Box<dyn TransformationContext>>,
    level: FlattenLevel,
    downlevel_iteration: bool,
    expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>>,
    create_assignment_callback: Option<
        Rc<
            RefCell<
                dyn FnMut(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>
                    + 'create_assignment_callback,
            >,
        >,
    >,
    visitor: Option<Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>>,
}

impl<'visitor, 'create_assignment_callback>
    FlattenDestructuringAssignmentFlattenContext<'visitor, 'create_assignment_callback>
{
    pub fn new(
        context: Gc<Box<dyn TransformationContext>>,
        level: FlattenLevel,
        downlevel_iteration: bool,
        expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>>,
        create_assignment_callback: Option<
            Rc<
                RefCell<
                    dyn FnMut(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>
                        + 'create_assignment_callback,
                >,
            >,
        >,
        visitor: Option<Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>>,
    ) -> Self {
        Self {
            context,
            level,
            downlevel_iteration,
            expressions,
            create_assignment_callback,
            visitor,
        }
    }
}

impl FlattenContext for FlattenDestructuringAssignmentFlattenContext<'_, '_> {
    fn context(&self) -> Gc<Box<dyn TransformationContext>> {
        self.context.clone()
    }

    fn level(&self) -> FlattenLevel {
        self.level
    }

    fn downlevel_iteration(&self) -> bool {
        self.downlevel_iteration
    }

    fn hoist_temp_variables(&self) -> bool {
        true
    }

    fn emit_expression(&self, value: &Node /*Expression*/) {
        emit_expression(&mut self.expressions.borrow_mut(), value.node_wrapper());
    }

    fn emit_binding_or_assignment(
        &self,
        target: &Node, /*BindingOrAssignmentElementTarget*/
        value: &Node,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<impl Borrow<Node>>,
    ) -> io::Result<()> {
        Debug_.assert_node(
            Some(target),
            Some(if self.create_assignment_callback.is_some() {
                is_identifier
            } else {
                is_expression
            }),
            None,
        );
        let expression = self
            .create_assignment_callback
            .as_ref()
            .try_map_or_else(
                || -> io::Result<_> {
                    Ok(self
                        .context
                        .factory()
                        .create_assignment(
                            try_visit_node(
                                target,
                                self.visitor
                                    .as_ref()
                                    .map(|visitor| |node: &Node| (visitor.borrow_mut())(node)),
                                Some(is_expression),
                                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                            )?,
                            value.node_wrapper(),
                        )
                        .set_text_range(Some(location)))
                },
                |create_assignment_callback| {
                    (create_assignment_callback.borrow_mut())(
                        target,
                        value,
                        Some(&ReadonlyTextRangeConcrete::from(location)),
                    )
                },
            )?
            .and_set_original(original.node_wrappered());
        self.emit_expression(&expression);

        Ok(())
    }

    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ArrayBindingOrAssignmentPattern*/> {
        make_array_assignment_pattern(&self.context.factory(), elements)
    }

    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ObjectBindingOrAssignmentPattern*/> {
        make_object_assignment_pattern(&self.context.factory(), elements)
    }

    fn create_array_binding_or_assignment_element(
        &self,
        node: &Node, /*Identifier*/
    ) -> Gc<Node /*BindingOrAssignmentElement*/> {
        make_assignment_element(node)
    }

    fn visitor(&self, node: &Node) -> Option<io::Result<VisitResult /*<Node>*/>> {
        self.visitor
            .as_ref()
            .map(|visitor| (visitor.borrow_mut())(node))
    }
}

pub fn try_flatten_destructuring_assignment<'visitor, 'create_assignment_callback>(
    node: &Node, /*VariableDeclaration | DestructuringAssignment*/
    visitor: Option<impl FnMut(&Node) -> io::Result<VisitResult> + 'visitor>,
    context: Gc<Box<dyn TransformationContext>>,
    level: FlattenLevel,
    needs_value: Option<bool>,
    // create_assignment_callback: Option<Gc<Box<dyn TryCreateAssignmentCallback>>>,
    create_assignment_callback: Option<
        impl FnMut(
                &Node, /*Identifier*/
                &Node, /*Expression*/
                Option<&dyn ReadonlyTextRange>,
            ) -> io::Result<Gc<Node /*Expression*/>>
            + 'create_assignment_callback,
    >,
) -> io::Result<Gc<Node /*Expression*/>> {
    let mut location/*: TextRange*/ = node.node_wrapper();
    let mut value: Option<Gc<Node /*Expression*/>> = _d();
    let mut node = node.node_wrapper();
    let visitor: Option<Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>> =
        visitor.map(|visitor| {
            Rc::new(RefCell::new(visitor))
                as Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>
        });
    let create_assignment_callback: Option<
        Rc<
            RefCell<
                dyn FnMut(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>
                    + 'create_assignment_callback,
            >,
        >,
    > = create_assignment_callback.map(|create_assignment_callback| {
        Rc::new(RefCell::new(create_assignment_callback))
            as Rc<
                RefCell<
                    dyn FnMut(&Node, &Node, Option<&dyn ReadonlyTextRange>) -> io::Result<Gc<Node>>
                        + 'create_assignment_callback,
                >,
            >
    });
    if is_destructuring_assignment(&node) {
        value = Some(node.as_binary_expression().right.clone());
        while is_empty_array_literal(&node.as_binary_expression().left)
            || is_empty_object_literal(&node.as_binary_expression().left)
        {
            if is_destructuring_assignment(value.as_ref().unwrap()) {
                node = value.clone().unwrap();
                location = value.clone().unwrap();
                value = Some(node.as_binary_expression().right.clone());
            } else {
                return try_visit_node(
                    value.as_ref().unwrap(),
                    visitor
                        .as_ref()
                        .map(|visitor| |node: &Node| (visitor.borrow_mut())(node)),
                    Some(is_expression),
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                );
            }
        }
    }

    let expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>> = _d();
    let flatten_context = FlattenDestructuringAssignmentFlattenContext::new(
        context.clone(),
        level,
        context.get_compiler_options().downlevel_iteration == Some(true),
        expressions.clone(),
        create_assignment_callback.clone(),
        visitor.clone(),
    );

    if value.is_some() {
        value = Some(try_visit_node(
            value.as_ref().unwrap(),
            visitor
                .as_ref()
                .map(|visitor| |node: &Node| (visitor.borrow_mut())(node)),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?);

        if is_identifier(value.as_ref().unwrap())
            && binding_or_assignment_element_assigns_to_name(
                &node,
                &value.as_ref().unwrap().as_identifier().escaped_text,
            )
            || binding_or_assignment_element_contains_non_literal_computed_name(&node)
        {
            value = Some(ensure_identifier(
                &flatten_context,
                value.as_ref().unwrap(),
                false,
                &*location,
            ));
        } else if needs_value == Some(true) {
            value = Some(ensure_identifier(
                &flatten_context,
                value.as_ref().unwrap(),
                true,
                &*location,
            ));
        } else if node_is_synthesized(&*node) {
            location = value.clone().unwrap();
        }
    }

    flatten_binding_or_assignment_element(
        &flatten_context,
        &node,
        value.as_deref(),
        &*location,
        Some(is_destructuring_assignment(&node)),
    );

    if let Some(value) = value.filter(|_| needs_value == Some(true)) {
        if !(*expressions).borrow().as_ref().is_non_empty() {
            return Ok(value);
        }

        expressions.borrow_mut().as_mut().unwrap().push(value);
    }

    let ret = context
        .factory()
        .inline_expressions((*expressions).borrow().as_ref().unwrap()) /* || context.factory.createOmittedExpression()*/;
    Ok(ret)
}

fn binding_or_assignment_element_assigns_to_name(
    _element: &Node,     /*BindingOrAssignmentElement*/
    _escaped_name: &str, /*__String*/
) -> bool {
    unimplemented!()
}

fn binding_or_assignment_element_contains_non_literal_computed_name(
    _element: &Node, /*BindingOrAssignmentElement*/
) -> bool {
    unimplemented!()
}

pub fn flatten_destructuring_binding(
    node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: &(impl TransformationContext + ?Sized),
    level: FlattenLevel,
    rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    skip_initializer: Option<bool>,
) -> Vec<Gc<Node /*VariableDeclaration*/>> {
    try_flatten_destructuring_binding(
        node,
        |node: &Node| Ok(visitor(node)),
        context,
        level,
        rval,
        hoist_temp_variables,
        skip_initializer,
    )
    .unwrap()
}

pub fn try_flatten_destructuring_binding(
    _node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    _visitor: impl FnMut(&Node) -> io::Result<VisitResult>,
    _context: &(impl TransformationContext + ?Sized),
    _level: FlattenLevel,
    _rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    _skip_initializer: Option<bool>,
) -> io::Result<Vec<Gc<Node /*VariableDeclaration*/>>> {
    let _hoist_temp_variables = hoist_temp_variables.unwrap_or(false);
    unimplemented!()
}

fn flatten_binding_or_assignment_element(
    _flatten_context: &impl FlattenContext,
    _element: &Node, /*BindingOrAssignmentElement*/
    _value: Option<impl Borrow<Node /*Expression*/>>,
    _location: &impl ReadonlyTextRange,
    _skip_initializer: Option<bool>,
) {
    unimplemented!()
}

fn ensure_identifier(
    _flatten_context: &impl FlattenContext,
    _value: &Node, /*Expression*/
    _reuse_identifier_expressions: bool,
    _location: &impl ReadonlyTextRange,
) -> Gc<Node> {
    unimplemented!()
}

fn make_array_assignment_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    unimplemented!()
}

fn make_object_assignment_pattern<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    unimplemented!()
}

fn make_assignment_element(name: &Node /*Identifier*/) -> Gc<Node> {
    name.node_wrapper()
}
