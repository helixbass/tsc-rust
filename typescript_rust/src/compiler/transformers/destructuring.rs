use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    io,
    rc::Rc,
};

use gc::{Finalize, Gc, GcCell, Trace};
use itertools::Itertools;

use crate::{
    GetOrInsertDefault, Node, NodeExt, NodeInterface, NodeWrappered, NonEmpty, ReadonlyTextRange,
    TransformationContext, VisitResult, _d, get_elements_of_binding_or_assignment_pattern,
    get_factory, get_initializer_of_binding_or_assignment_element,
    get_property_name_of_binding_or_assignment_element,
    get_rest_indicator_of_binding_or_assignment_element,
    get_target_of_binding_or_assignment_element, id_text, is_array_binding_element,
    is_array_binding_or_assigment_pattern, is_binding_element, is_binding_name,
    is_binding_or_assigment_pattern, is_computed_property_name, is_declaration_binding_element,
    is_destructuring_assignment, is_empty_array_literal, is_empty_object_literal, is_expression,
    is_identifier, is_literal_expression, is_object_binding_or_assigment_pattern,
    is_omitted_expression, is_property_name_literal, is_simple_inlineable_expression,
    is_string_or_numeric_literal_like, is_variable_declaration, node_is_synthesized,
    set_text_range, try_get_property_name_of_binding_or_assignment_element, try_maybe_visit_node,
    try_visit_node, BaseNodeFactory, Debug_, Matches, NamedDeclarationInterface, NodeFactory,
    Number, OptionTry, ReadonlyTextRangeConcrete, TransformFlags, VecExt,
};

trait FlattenContext {
    fn context(&self) -> Gc<Box<dyn TransformationContext>>;
    fn level(&self) -> FlattenLevel;
    fn downlevel_iteration(&self) -> bool;
    fn hoist_temp_variables(&self) -> bool;
    fn has_transformed_prior_element(&self) -> Option<bool>;
    fn set_has_transformed_prior_element(&self, has_transformed_prior_element: Option<bool>);
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
    fn is_visitor_supported(&self) -> bool;
    fn visitor(&self, _node: &Node) -> Option<io::Result<VisitResult /*<Node>*/>> {
        None
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
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
    has_transformed_prior_element: Cell<Option<bool>>,
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
            has_transformed_prior_element: _d(),
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

    fn is_visitor_supported(&self) -> bool {
        self.visitor.is_some()
    }

    fn has_transformed_prior_element(&self) -> Option<bool> {
        self.has_transformed_prior_element.get()
    }

    fn set_has_transformed_prior_element(&self, has_transformed_prior_element: Option<bool>) {
        self.has_transformed_prior_element
            .set(has_transformed_prior_element);
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
            )?);
        } else if needs_value == Some(true) {
            value = Some(ensure_identifier(
                &flatten_context,
                value.as_ref().unwrap(),
                true,
                &*location,
            )?);
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
    )?;

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
            )?;
            node = context.factory().update_variable_declaration(
                &node,
                node.as_variable_declaration().maybe_name(),
                None,
                None,
                Some(initializer),
            );
        }
    }

    flatten_binding_or_assignment_element(&flatten_context, &node, rval, &*node, skip_initializer)?;
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
    has_transformed_prior_element: Cell<Option<bool>>,
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
            has_transformed_prior_element: _d(),
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

    fn is_visitor_supported(&self) -> bool {
        true
    }

    fn has_transformed_prior_element(&self) -> Option<bool> {
        self.has_transformed_prior_element.get()
    }

    fn set_has_transformed_prior_element(&self, has_transformed_prior_element: Option<bool>) {
        self.has_transformed_prior_element
            .set(has_transformed_prior_element);
    }
}

fn flatten_binding_or_assignment_element(
    flatten_context: &impl FlattenContext,
    element: &Node, /*BindingOrAssignmentElement*/
    value: Option<impl Borrow<Node /*Expression*/>>,
    location: &impl ReadonlyTextRange,
    skip_initializer: Option<bool>,
) -> io::Result<()> {
    let binding_target = get_target_of_binding_or_assignment_element(element).unwrap();
    let mut value = value.node_wrappered();
    if skip_initializer != Some(true) {
        let initializer = try_maybe_visit_node(
            get_initializer_of_binding_or_assignment_element(element),
            flatten_context
                .is_visitor_supported()
                .then(|| |node: &Node| flatten_context.visitor(node).unwrap()),
            Some(is_expression),
            Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
        )?;
        if let Some(initializer) = initializer {
            if value.is_some() {
                value = Some(create_default_value_check(
                    flatten_context,
                    value.as_ref().unwrap(),
                    &initializer,
                    location,
                )?);
                if !is_simple_inlineable_expression(&initializer)
                    && is_binding_or_assigment_pattern(&binding_target)
                {
                    value = Some(ensure_identifier(
                        flatten_context,
                        value.as_ref().unwrap(),
                        true,
                        location,
                    )?);
                }
            } else {
                value = Some(initializer);
            }
        } else if value.is_none() {
            value = Some(flatten_context.context().factory().create_void_zero());
        }
    }
    if is_object_binding_or_assigment_pattern(&binding_target) {
        flatten_object_binding_or_assignment_pattern(
            flatten_context,
            element,
            &binding_target,
            value.as_ref().unwrap(),
            location,
        )?;
    } else if is_array_binding_or_assigment_pattern(&binding_target) {
        flatten_array_binding_or_assignment_pattern(
            flatten_context,
            element,
            &binding_target,
            value.as_ref().unwrap(),
            location,
        )?;
    } else {
        flatten_context.emit_binding_or_assignment(
            &binding_target,
            value.as_ref().unwrap(),
            location,
            Some(element),
        )?;
    }

    Ok(())
}

fn flatten_object_binding_or_assignment_pattern(
    flatten_context: &impl FlattenContext,
    parent: &Node,  /*BindingOrAssignmentElement*/
    pattern: &Node, /*ObjectBindingOrAssignmentPattern*/
    value: &Node,   /*Expression*/
    location: &impl ReadonlyTextRange,
) -> io::Result<()> {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern).collect_vec();
    let num_elements = elements.len();
    let mut value = value.node_wrapper();
    if num_elements != 1 {
        let reuse_identifier_expressions =
            !is_declaration_binding_element(parent) || num_elements != 0;
        value = ensure_identifier(
            flatten_context,
            &value,
            reuse_identifier_expressions,
            location,
        )?;
    }
    let mut binding_elements: Option<Vec<Gc<Node /*BindingOrAssignmentElement*/>>> = _d();
    let mut computed_temp_variables: Option<Vec<Gc<Node /*Expression*/>>> = _d();
    for (i, element) in elements.iter().enumerate() {
        if get_rest_indicator_of_binding_or_assignment_element(element).is_none() {
            let property_name =
                &get_property_name_of_binding_or_assignment_element(element).unwrap();
            if flatten_context.level() >= FlattenLevel::ObjectRest
                && !element.transform_flags().intersects(
                    TransformFlags::ContainsRestOrSpread
                        | TransformFlags::ContainsObjectRestOrSpread,
                )
                && !get_target_of_binding_or_assignment_element(element)
                    .unwrap()
                    .transform_flags()
                    .intersects(
                        TransformFlags::ContainsRestOrSpread
                            | TransformFlags::ContainsObjectRestOrSpread,
                    )
                && !is_computed_property_name(property_name)
            {
                binding_elements
                    .get_or_insert_default_()
                    .push(try_visit_node(
                        element,
                        flatten_context
                            .is_visitor_supported()
                            .then(|| |node: &Node| flatten_context.visitor(node).unwrap()),
                        Option::<fn(&Node) -> bool>::None,
                        Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
                    )?);
            } else {
                if binding_elements.is_some() {
                    flatten_context.emit_binding_or_assignment(
                        &flatten_context.create_object_binding_or_assignment_pattern(
                            binding_elements.as_ref().unwrap(),
                        ),
                        &value,
                        location,
                        Some(pattern),
                    )?;
                    binding_elements = None;
                }
                let rhs_value =
                    create_destructuring_property_access(flatten_context, &value, property_name)?;
                if is_computed_property_name(property_name) {
                    computed_temp_variables.get_or_insert_default_().push(
                        rhs_value
                            .as_element_access_expression()
                            .argument_expression
                            .clone(),
                    );
                }
                flatten_binding_or_assignment_element(
                    flatten_context,
                    element,
                    Some(&*rhs_value),
                    &**element,
                    None,
                )?;
            }
        } else if i == num_elements - 1 {
            if binding_elements.is_some() {
                flatten_context.emit_binding_or_assignment(
                    &flatten_context.create_object_binding_or_assignment_pattern(
                        binding_elements.as_ref().unwrap(),
                    ),
                    &value,
                    location,
                    Some(pattern),
                )?;
                binding_elements = None;
            }
            let rhs_value = flatten_context
                .context()
                .get_emit_helper_factory()
                .create_rest_helper(
                    value.clone(),
                    &elements,
                    computed_temp_variables.as_deref(),
                    pattern,
                );
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &**element,
                None,
            )?;
        }
    }
    if let Some(binding_elements) = binding_elements {
        flatten_context.emit_binding_or_assignment(
            &flatten_context.create_object_binding_or_assignment_pattern(&binding_elements),
            &value,
            location,
            Some(pattern),
        )?;
    }

    Ok(())
}

fn flatten_array_binding_or_assignment_pattern(
    flatten_context: &impl FlattenContext,
    parent: &Node,  /*BindingOrAssignmentElement*/
    pattern: &Node, /*ArrayBindingOrAssignmentPattern*/
    value: &Node,   /*Expression*/
    location: &impl ReadonlyTextRange,
) -> io::Result<()> {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern).collect_vec();
    let num_elements = elements.len();
    let mut value = value.node_wrapper();
    if flatten_context.level() < FlattenLevel::ObjectRest && flatten_context.downlevel_iteration() {
        value = ensure_identifier(
            flatten_context,
            &flatten_context
                .context()
                .get_emit_helper_factory()
                .create_read_helper(
                    value,
                    (!(num_elements > 0
                        && get_rest_indicator_of_binding_or_assignment_element(
                            &elements[num_elements - 1],
                        )
                        .is_some()))
                    .then_some(num_elements),
                )
                .set_text_range(Some(location)),
            false,
            location,
        )?;
    } else if num_elements != 1
        && (flatten_context.level() < FlattenLevel::ObjectRest || num_elements == 0)
        || elements
            .iter()
            .all(|element| is_omitted_expression(element))
    {
        let reuse_identifier_expressions =
            !is_declaration_binding_element(parent) || num_elements != 0;
        value = ensure_identifier(
            flatten_context,
            &value,
            reuse_identifier_expressions,
            location,
        )?;
    }
    let mut binding_elements: Option<Vec<Gc<Node /*BindingOrAssignmentElement*/>>> = _d();
    let mut rest_containing_elements: Option<
        Vec<(
            Gc<Node /*Identifier*/>,
            Gc<Node /*BindingOrAssignmentElement*/>,
        )>,
    > = _d();
    for (i, element) in elements.iter().enumerate() {
        if flatten_context.level() >= FlattenLevel::ObjectRest {
            if element
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
                || flatten_context.has_transformed_prior_element() == Some(true)
                    && !is_simple_binding_or_assignment_element(element)
            {
                flatten_context.set_has_transformed_prior_element(Some(true));
                let temp = flatten_context
                    .context()
                    .factory()
                    .create_temp_variable(Option::<fn(&Node)>::None, None);
                if flatten_context.hoist_temp_variables() {
                    flatten_context.context().hoist_variable_declaration(&temp);
                }

                rest_containing_elements
                    .get_or_insert_default_()
                    .push((temp.clone(), element.clone()));
                binding_elements
                    .get_or_insert_default_()
                    .push(flatten_context.create_array_binding_or_assignment_element(&temp));
            } else {
                binding_elements
                    .get_or_insert_default_()
                    .push(element.clone());
            }
        } else if is_omitted_expression(element) {
            continue;
        } else if get_rest_indicator_of_binding_or_assignment_element(element).is_none() {
            let rhs_value = flatten_context
                .context()
                .factory()
                .create_element_access_expression(value.clone(), Number::new(i as f64));
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &**element,
                None,
            )?;
        } else if i == num_elements - 1 {
            let rhs_value = flatten_context
                .context()
                .factory()
                .create_array_slice_call(value.clone(), Some(Number::new(i as f64)));
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &**element,
                None,
            )?;
        }
    }
    if let Some(binding_elements) = binding_elements {
        flatten_context.emit_binding_or_assignment(
            &flatten_context.create_array_binding_or_assignment_pattern(&binding_elements),
            &value,
            location,
            Some(pattern),
        )?;
    }
    if let Some(rest_containing_elements) = rest_containing_elements {
        for (id, element) in rest_containing_elements {
            flatten_binding_or_assignment_element(
                flatten_context,
                &element,
                Some(id),
                &*element,
                None,
            )?;
        }
    }

    Ok(())
}

fn is_simple_binding_or_assignment_element(element: &Node, /*BindingOrAssignmentElement*/) -> bool {
    let target = get_target_of_binding_or_assignment_element(element);
    if target.is_none() {
        return true;
    }
    let target = &target.unwrap();
    if is_omitted_expression(target) {
        return true;
    }
    let property_name = try_get_property_name_of_binding_or_assignment_element(element);
    if property_name.matches(|ref property_name| !is_property_name_literal(property_name)) {
        return false;
    }
    let initializer = get_initializer_of_binding_or_assignment_element(element);
    if initializer.matches(|ref initializer| !is_simple_inlineable_expression(initializer)) {
        return false;
    }
    if is_binding_or_assigment_pattern(target) {
        return get_elements_of_binding_or_assignment_pattern(target)
            .all(|ref element| is_simple_binding_or_assignment_element(element));
    }
    is_identifier(target)
}

fn create_default_value_check(
    flatten_context: &impl FlattenContext,
    value: &Node,         /*Expression*/
    default_value: &Node, /*Expression*/
    location: &impl ReadonlyTextRange,
) -> io::Result<Gc<Node /*Expression*/>> {
    let value = ensure_identifier(flatten_context, value, true, location)?;
    Ok(flatten_context
        .context()
        .factory()
        .create_conditional_expression(
            flatten_context
                .context()
                .factory()
                .create_type_check(value.clone(), "undefined"),
            None,
            default_value.node_wrapper(),
            None,
            value,
        ))
}

fn create_destructuring_property_access(
    flatten_context: &impl FlattenContext,
    value: &Node,         /*Expression*/
    property_name: &Node, /*PropertyName*/
) -> io::Result<Gc<Node /*LeftHandSideExpression*/>> {
    Ok(if is_computed_property_name(property_name) {
        let argument_expression = ensure_identifier(
            flatten_context,
            &*try_visit_node(
                &property_name.as_computed_property_name().expression,
                flatten_context
                    .is_visitor_supported()
                    .then(|| |node: &Node| flatten_context.visitor(node).unwrap()),
                Option::<fn(&Node) -> bool>::None,
                Option::<fn(&[Gc<Node>]) -> Gc<Node>>::None,
            )?,
            false,
            property_name,
        )?;
        flatten_context
            .context()
            .factory()
            .create_element_access_expression(value.node_wrapper(), argument_expression)
    } else if is_string_or_numeric_literal_like(property_name) {
        let argument_expression = get_factory().clone_node(property_name);
        flatten_context
            .context()
            .factory()
            .create_element_access_expression(value.node_wrapper(), argument_expression)
    } else {
        let name = flatten_context
            .context()
            .factory()
            .create_identifier(id_text(property_name));
        flatten_context
            .context()
            .factory()
            .create_property_access_expression(value.node_wrapper(), name)
    })
}

fn ensure_identifier(
    flatten_context: &impl FlattenContext,
    value: &Node, /*Expression*/
    reuse_identifier_expressions: bool,
    location: &impl ReadonlyTextRange,
) -> io::Result<Gc<Node>> {
    Ok(if is_identifier(value) && reuse_identifier_expressions {
        value.node_wrapper()
    } else {
        let temp = flatten_context
            .context()
            .factory()
            .create_temp_variable(Option::<fn(&Node)>::None, None);
        if flatten_context.hoist_temp_variables() {
            flatten_context.context().hoist_variable_declaration(&temp);
            flatten_context.emit_expression(
                &flatten_context
                    .context()
                    .factory()
                    .create_assignment(temp.clone(), value.node_wrapper())
                    .set_text_range(Some(location)),
            );
        } else {
            flatten_context.emit_binding_or_assignment(
                &temp,
                value,
                location,
                Option::<&Node>::None,
            )?;
        }
        temp
    })
}

fn make_array_binding_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: &NodeFactory<TBaseNodeFactory>,
    elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    Debug_.assert_each_node(elements, is_array_binding_element, None);
    factory.create_array_binding_pattern(elements.to_owned())
}

fn make_array_assignment_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: &NodeFactory<TBaseNodeFactory>,
    elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    factory.create_array_literal_expression(
        Some(
            elements
                .into_iter()
                .map(|element| {
                    factory
                        .converters()
                        .convert_to_array_assignment_element(element)
                })
                .collect_vec(),
        ),
        None,
    )
}

fn make_object_binding_pattern<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: &NodeFactory<TBaseNodeFactory>,
    elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    Debug_.assert_each_node(elements, is_binding_element, None);
    factory.create_object_binding_pattern(elements.to_owned())
}

fn make_object_assignment_pattern<
    TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize,
>(
    factory: &NodeFactory<TBaseNodeFactory>,
    elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
) -> Gc<Node> {
    factory.create_object_literal_expression(
        Some(
            elements
                .into_iter()
                .map(|element| {
                    factory
                        .converters()
                        .convert_to_object_assignment_element(element)
                })
                .collect_vec(),
        ),
        None,
    )
}

fn make_binding_element<TBaseNodeFactory: 'static + BaseNodeFactory + Trace + Finalize>(
    factory: &NodeFactory<TBaseNodeFactory>,
    name: &Node, /*Identifier*/
) -> Gc<Node> {
    factory.create_binding_element(None, Option::<Gc<Node>>::None, name.node_wrapper(), None)
}

fn make_assignment_element(name: &Node /*Identifier*/) -> Gc<Node> {
    name.node_wrapper()
}
