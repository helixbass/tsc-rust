use std::{borrow::Borrow, cell::RefCell, io, rc::Rc};

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    GetOrInsertDefault, Node, NodeExt, NodeInterface, NodeWrappered, NonEmpty, ReadonlyTextRange,
    TransformationContext, VisitResult, _d, get_elements_of_binding_or_assignment_pattern,
    get_initializer_of_binding_or_assignment_element, get_target_of_binding_or_assignment_element,
    is_binding_name, is_binding_or_assigment_pattern, is_computed_property_name,
    is_destructuring_assignment, is_empty_array_literal, is_empty_object_literal, is_expression,
    is_identifier, is_literal_expression, is_variable_declaration, node_is_synthesized,
    set_text_range, try_get_property_name_of_binding_or_assignment_element, try_visit_node,
    BaseNodeFactory, Debug_, Matches, NamedDeclarationInterface, NodeFactory, OptionTry,
    ReadonlyTextRangeConcrete, VecExt,
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
    element: &Node,     /*BindingOrAssignmentElement*/
    escaped_name: &str, /*__String*/
) -> bool {
    let target = &get_target_of_binding_or_assignment_element(element).unwrap();
    if is_binding_or_assigment_pattern(target) {
        return binding_or_assignment_pattern_assigns_to_name(target, escaped_name);
    } else if is_identifier(target) {
        return target.as_identifier().escaped_text == escaped_name;
    }
    false
}

fn binding_or_assignment_pattern_assigns_to_name(
    pattern: &Node,     /*BindingOrAssignmentPattern*/
    escaped_name: &str, /*__String*/
) -> bool {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern);
    for ref element in elements {
        if binding_or_assignment_element_assigns_to_name(element, escaped_name) {
            return true;
        }
    }
    false
}

fn binding_or_assignment_element_contains_non_literal_computed_name(
    element: &Node, /*BindingOrAssignmentElement*/
) -> bool {
    let property_name = try_get_property_name_of_binding_or_assignment_element(element);
    if property_name.matches(|ref property_name| {
        is_computed_property_name(property_name)
            && !is_literal_expression(&property_name.as_computed_property_name().expression)
    }) {
        return true;
    }
    let target = get_target_of_binding_or_assignment_element(element);
    target.as_ref().matches(|target| {
        is_binding_or_assigment_pattern(target)
            && binding_or_assignment_pattern_contains_non_literal_computed_name(target)
    })
}

fn binding_or_assignment_pattern_contains_non_literal_computed_name(
    pattern: &Node, /*BindingOrAssignmentPattern*/
) -> bool {
    get_elements_of_binding_or_assignment_pattern(pattern).any(|ref element| {
        binding_or_assignment_element_contains_non_literal_computed_name(element)
    })
}

pub fn flatten_destructuring_binding(
    node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    mut visitor: impl FnMut(&Node) -> VisitResult,
    context: Gc<Box<dyn TransformationContext>>,
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

#[derive(Trace, Finalize)]
struct PendingDeclaration {
    pending_expressions: Option<Vec<Gc<Node /*Expression*/>>>,
    name: Gc<Node /*BindingName*/>,
    value: Gc<Node /*Expression*/>,
    #[unsafe_ignore_trace]
    location: Option<ReadonlyTextRangeConcrete>,
    original: Option<Gc<Node>>,
}

pub fn try_flatten_destructuring_binding<'visitor>(
    node: &Node, /*VariableDeclaration | ParameterDeclaration*/
    visitor: impl FnMut(&Node) -> io::Result<VisitResult> + 'visitor,
    context: Gc<Box<dyn TransformationContext>>,
    level: FlattenLevel,
    rval: Option<impl Borrow<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    skip_initializer: Option<bool>,
) -> io::Result<Vec<Gc<Node /*VariableDeclaration*/>>> {
    let hoist_temp_variables = hoist_temp_variables.unwrap_or(false);
    let pending_expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>> = _d();
    let pending_declarations: Gc<GcCell<Vec<PendingDeclaration>>> = _d();
    let mut declarations: Vec<Gc<Node /*VariableDeclaration*/>> = _d();
    let visitor: Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>> =
        Rc::new(RefCell::new(visitor));
    // as Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>
    let flatten_context = FlattenDestructuringBindingFlattenContext::new(
        context.clone(),
        level,
        context.get_compiler_options().downlevel_iteration == Some(true),
        hoist_temp_variables,
        pending_expressions.clone(),
        pending_declarations.clone(),
        visitor.clone(),
    );

    let mut node = node.node_wrapper();
    if is_variable_declaration(&node) {
        let initializer = get_initializer_of_binding_or_assignment_element(&node);
        if let Some(mut initializer) = initializer.filter(|initializer| {
            is_identifier(initializer)
                && binding_or_assignment_element_assigns_to_name(
                    &node,
                    &initializer.as_identifier().escaped_text,
                )
                || binding_or_assignment_element_contains_non_literal_computed_name(&node)
        }) {
            initializer = ensure_identifier(
                &flatten_context,
                &*try_visit_node(
                    &initializer,
                    Some(|node: &Node| (flatten_context.visitor.borrow_mut())(node)),
                    Option::<fn(&Node) -> bool>::None,
                    Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                )?,
                false,
                &*initializer,
            );
            node = context.factory().update_variable_declaration(
                &node,
                node.as_variable_declaration().maybe_name(),
                None,
                None,
                Some(initializer),
            );
        }
    }

    flatten_binding_or_assignment_element(&flatten_context, &node, rval, &*node, skip_initializer);
    if (*pending_expressions).borrow().is_some() {
        let temp = context
            .factory()
            .create_temp_variable(Option::<fn(&Node)>::None, None);
        if hoist_temp_variables {
            let value = context
                .factory()
                .inline_expressions((*pending_expressions).borrow().as_ref().unwrap());
            *pending_expressions.borrow_mut() = None;
            emit_binding_or_assignment(
                &mut pending_expressions.borrow_mut(),
                &**context,
                &mut pending_declarations.borrow_mut(),
                &temp,
                &value,
                Option::<&Node>::None,
                Option::<&Node>::None,
            );
        } else {
            context.hoist_variable_declaration(&temp);
            let mut pending_declarations = pending_declarations.borrow_mut();
            let pending_declarations_len = pending_declarations.len();
            let pending_declaration = &mut pending_declarations[pending_declarations_len - 1];
            pending_declaration
                .pending_expressions
                .get_or_insert_default_()
                .push(
                    context
                        .factory()
                        .create_assignment(temp.clone(), pending_declaration.value.clone()),
                );
            pending_declaration
                .pending_expressions
                .as_mut()
                .unwrap()
                .extend(
                    (*pending_expressions)
                        .borrow()
                        .as_ref()
                        .unwrap()
                        .iter()
                        .cloned(),
                );
            pending_declaration.value = temp;
        }
    }
    for PendingDeclaration {
        pending_expressions,
        name,
        value,
        location,
        original,
    } in (*pending_declarations).borrow().iter()
    {
        let variable = context.factory().create_variable_declaration(
            Some(name.clone()),
            None,
            None,
            Some(pending_expressions.as_ref().map_or_else(
                || value.clone(),
                |pending_expressions| {
                    context
                        .factory()
                        .inline_expressions(&pending_expressions.clone().and_push(value.clone()))
                },
            )),
        );
        variable.set_original(original.clone());
        set_text_range(&*variable, location.as_ref());
        declarations.push(variable);
    }
    Ok(declarations)
}

fn emit_binding_or_assignment(
    pending_expressions: &mut Option<Vec<Gc<Node /*Expression*/>>>,
    context: &dyn TransformationContext,
    pending_declarations: &mut Vec<PendingDeclaration>,
    target: &Node, /*BindingOrAssignmentElementTarget*/
    value: &Node,  /*Expression*/
    location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    original: Option<impl Borrow<Node>>,
) {
    Debug_.assert_node(Some(target), Some(is_binding_name), None);
    let mut value = value.node_wrapper();
    if pending_expressions.is_some() {
        pending_expressions.as_mut().unwrap().push(value.clone());
        value = context
            .factory()
            .inline_expressions(pending_expressions.as_ref().unwrap());
        *pending_expressions = None;
    }
    pending_declarations.push(PendingDeclaration {
        pending_expressions: pending_expressions.clone(),
        name: target.node_wrapper(),
        value,
        location: location.map(Into::into),
        original: original.node_wrappered(),
    });
}

struct FlattenDestructuringBindingFlattenContext<'visitor> {
    context: Gc<Box<dyn TransformationContext>>,
    level: FlattenLevel,
    downlevel_iteration: bool,
    hoist_temp_variables: bool,
    pending_expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>>,
    pending_declarations: Gc<GcCell<Vec<PendingDeclaration>>>,
    visitor: Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>,
}

impl<'visitor> FlattenDestructuringBindingFlattenContext<'visitor> {
    pub fn new(
        context: Gc<Box<dyn TransformationContext>>,
        level: FlattenLevel,
        downlevel_iteration: bool,
        hoist_temp_variables: bool,
        pending_expressions: Gc<GcCell<Option<Vec<Gc<Node /*Expression*/>>>>>,
        pending_declarations: Gc<GcCell<Vec<PendingDeclaration>>>,
        visitor: Rc<RefCell<dyn FnMut(&Node) -> io::Result<VisitResult> + 'visitor>>,
    ) -> Self {
        Self {
            context,
            level,
            downlevel_iteration,
            hoist_temp_variables,
            pending_expressions,
            pending_declarations,
            visitor,
        }
    }
}

impl FlattenContext for FlattenDestructuringBindingFlattenContext<'_> {
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
        self.hoist_temp_variables
    }

    fn emit_expression(&self, value: &Node /*Expression*/) {
        self.pending_expressions
            .borrow_mut()
            .get_or_insert_default_()
            .push(value.node_wrapper());
    }

    fn emit_binding_or_assignment(
        &self,
        target: &Node, /*BindingOrAssignmentElementTarget*/
        value: &Node,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<impl Borrow<Node>>,
    ) -> io::Result<()> {
        emit_binding_or_assignment(
            &mut self.pending_expressions.borrow_mut(),
            &**self.context,
            &mut self.pending_declarations.borrow_mut(),
            target,
            value,
            Some(location),
            original,
        );

        Ok(())
    }

    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ArrayBindingOrAssignmentPattern*/> {
        make_array_binding_pattern(&self.context.factory(), elements)
    }

    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
    ) -> Gc<Node /*ObjectBindingOrAssignmentPattern*/> {
        make_object_binding_pattern(&self.context.factory(), elements)
    }

    fn create_array_binding_or_assignment_element(
        &self,
        name: &Node, /*Identifier*/
    ) -> Gc<Node /*BindingOrAssignmentElement*/> {
        make_binding_element(&self.context.factory(), name)
    }

    fn visitor(&self, node: &Node) -> Option<io::Result<VisitResult /*<Node>*/>> {
        Some((self.visitor.borrow_mut())(node))
    }
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

fn make_array_binding_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    unimplemented!()
}

fn make_array_assignment_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    unimplemented!()
}

fn make_object_binding_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
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

fn make_binding_element<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    _factory: &NodeFactory<TBaseNodeFactory>,
    _name: &Node, /*Identifier*/
) -> Gc<Node> {
    unimplemented!()
}

fn make_assignment_element(name: &Node /*Identifier*/) -> Gc<Node> {
    name.node_wrapper()
}
