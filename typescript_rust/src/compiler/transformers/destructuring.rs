use std::{
    cell::{Cell, RefCell},
    io,
    rc::Rc,
};

use id_arena::Id;
use itertools::Itertools;

use crate::{
    get_elements_of_binding_or_assignment_pattern, get_factory,
    get_initializer_of_binding_or_assignment_element,
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
    try_visit_node, AllArenas, CoreTransformationContext, Debug_, GetOrInsertDefault, HasArena,
    InArena, Matches, NamedDeclarationInterface, Node, NodeExt, NodeFactory, NodeInterface,
    NonEmpty, Number, OptionTry, ReadonlyTextRange, ReadonlyTextRangeConcrete, TransformFlags,
    TransformNodesTransformationResult, TransformationContext, VecExt, VisitResult, _d,
    impl_has_arena,
};

trait FlattenContext {
    fn context(&self) -> Id<TransformNodesTransformationResult>;
    fn level(&self) -> FlattenLevel;
    fn downlevel_iteration(&self) -> bool;
    fn hoist_temp_variables(&self) -> bool;
    fn has_transformed_prior_element(&self) -> Option<bool>;
    fn set_has_transformed_prior_element(&self, has_transformed_prior_element: Option<bool>);
    fn emit_expression(&self, value: Id<Node> /*Expression*/);
    fn emit_binding_or_assignment(
        &self,
        target: Id<Node>, /*BindingOrAssignmentElementTarget*/
        value: Id<Node>,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<Id<Node>>,
    ) -> io::Result<()>;
    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ArrayBindingOrAssignmentPattern*/>;
    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ObjectBindingOrAssignmentPattern*/>;
    fn create_array_binding_or_assignment_element(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> Id<Node /*BindingOrAssignmentElement*/>;
    fn is_visitor_supported(&self) -> bool;
    fn visitor(&self, _node: Id<Node>) -> Option<io::Result<VisitResult /*<Node>*/>> {
        None
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FlattenLevel {
    All,
    ObjectRest,
}

pub fn flatten_destructuring_assignment<'a, 'b>(
    node: Id<Node>, /*VariableDeclaration | DestructuringAssignment*/
    visitor: Option<impl FnMut(Id<Node>) -> VisitResult + 'a>,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    needs_value: Option<bool>,
    create_assignment_callback: Option<
        impl FnMut(
                Id<Node>, /*Identifier*/
                Id<Node>, /*Expression*/
                Option<&dyn ReadonlyTextRange>,
            ) -> Id<Node /*Expression*/>
            + 'b,
    >,
    arena: &impl HasArena,
) -> Id<Node /*Expression*/> {
    try_flatten_destructuring_assignment(
        node,
        visitor.map(|mut visitor| move |node: Id<Node>| Ok(visitor(node))),
        context,
        level,
        needs_value,
        create_assignment_callback.map(|mut create_assignment_callback| {
            move |a: Id<Node>, b: Id<Node>, c: Option<&dyn ReadonlyTextRange>| {
                Ok(create_assignment_callback(a, b, c))
            }
        }),
        arena,
    )
    .unwrap()
}

fn emit_expression(
    expressions: &mut Option<Vec<Id<Node /*Expression*/>>>,
    expression: Id<Node /*Expression*/>,
) {
    expressions.get_or_insert_default_().push(expression);
}

struct FlattenDestructuringAssignmentFlattenContext<'visitor, 'create_assignment_callback> {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    downlevel_iteration: bool,
    expressions: Id<Option<Vec<Id<Node /*Expression*/>>>>,
    create_assignment_callback: Option<
        Rc<
            RefCell<
                dyn FnMut(
                        Id<Node>,
                        Id<Node>,
                        Option<&dyn ReadonlyTextRange>,
                    ) -> io::Result<Id<Node>>
                    + 'create_assignment_callback,
            >,
        >,
    >,
    visitor: Option<Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>>,
    has_transformed_prior_element: Cell<Option<bool>>,
}

impl<'visitor, 'create_assignment_callback>
    FlattenDestructuringAssignmentFlattenContext<'visitor, 'create_assignment_callback>
{
    pub fn new(
        context: Id<TransformNodesTransformationResult>,
        level: FlattenLevel,
        downlevel_iteration: bool,
        expressions: Id<Option<Vec<Id<Node /*Expression*/>>>>,
        create_assignment_callback: Option<
            Rc<
                RefCell<
                    dyn FnMut(
                            Id<Node>,
                            Id<Node>,
                            Option<&dyn ReadonlyTextRange>,
                        ) -> io::Result<Id<Node>>
                        + 'create_assignment_callback,
                >,
            >,
        >,
        visitor: Option<Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
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
    fn context(&self) -> Id<TransformNodesTransformationResult> {
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

    fn emit_expression(&self, value: Id<Node> /*Expression*/) {
        emit_expression(&mut self.expressions.ref_mut(self), value);
    }

    fn emit_binding_or_assignment(
        &self,
        target: Id<Node>, /*BindingOrAssignmentElementTarget*/
        value: Id<Node>,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<Id<Node>>,
    ) -> io::Result<()> {
        Debug_.assert_node(
            Some(target),
            Some(|node: Id<Node>| {
                if self.create_assignment_callback.is_some() {
                    is_identifier(&node.ref_(self))
                } else {
                    is_expression(node, self)
                }
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
                        .ref_(self)
                        .factory()
                        .ref_(self)
                        .create_assignment(
                            try_visit_node(
                                target,
                                self.visitor
                                    .as_ref()
                                    .map(|visitor| |node: Id<Node>| (visitor.borrow_mut())(node)),
                                Some(|node| is_expression(node, self)),
                                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                            )?,
                            value,
                        )
                        .set_text_range(Some(location), self))
                },
                |create_assignment_callback| {
                    (create_assignment_callback.borrow_mut())(
                        target,
                        value,
                        Some(&ReadonlyTextRangeConcrete::from(location)),
                    )
                },
            )?
            .and_set_original(original, self);
        self.emit_expression(expression);

        Ok(())
    }

    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ArrayBindingOrAssignmentPattern*/> {
        make_array_assignment_pattern(&self.context.ref_(self).factory().ref_(self), elements)
    }

    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ObjectBindingOrAssignmentPattern*/> {
        make_object_assignment_pattern(&self.context.ref_(self).factory().ref_(self), elements)
    }

    fn create_array_binding_or_assignment_element(
        &self,
        node: Id<Node>, /*Identifier*/
    ) -> Id<Node /*BindingOrAssignmentElement*/> {
        make_assignment_element(node)
    }

    fn visitor(&self, node: Id<Node>) -> Option<io::Result<VisitResult /*<Node>*/>> {
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

impl_has_arena!(FlattenDestructuringAssignmentFlattenContext<'_, '_>);

pub fn try_flatten_destructuring_assignment<'visitor, 'create_assignment_callback>(
    mut node: Id<Node>, /*VariableDeclaration | DestructuringAssignment*/
    visitor: Option<impl FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    needs_value: Option<bool>,
    // create_assignment_callback: Option<Id<Box<dyn TryCreateAssignmentCallback>>>,
    create_assignment_callback: Option<
        impl FnMut(
                Id<Node>, /*Identifier*/
                Id<Node>, /*Expression*/
                Option<&dyn ReadonlyTextRange>,
            ) -> io::Result<Id<Node /*Expression*/>>
            + 'create_assignment_callback,
    >,
    arena: &impl HasArena,
) -> io::Result<Id<Node /*Expression*/>> {
    let mut location/*: TextRange*/ = node;
    let mut value: Option<Id<Node /*Expression*/>> = _d();
    let visitor: Option<Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>> =
        visitor.map(|visitor| {
            Rc::new(RefCell::new(visitor))
                as Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>
        });
    let create_assignment_callback: Option<
        Rc<
            RefCell<
                dyn FnMut(
                        Id<Node>,
                        Id<Node>,
                        Option<&dyn ReadonlyTextRange>,
                    ) -> io::Result<Id<Node>>
                    + 'create_assignment_callback,
            >,
        >,
    > = create_assignment_callback.map(|create_assignment_callback| {
        Rc::new(RefCell::new(create_assignment_callback))
            as Rc<
                RefCell<
                    dyn FnMut(
                            Id<Node>,
                            Id<Node>,
                            Option<&dyn ReadonlyTextRange>,
                        ) -> io::Result<Id<Node>>
                        + 'create_assignment_callback,
                >,
            >
    });
    if is_destructuring_assignment(node, arena) {
        value = Some(node.ref_(arena).as_binary_expression().right);
        while is_empty_array_literal(node.ref_(arena).as_binary_expression().left, arena)
            || is_empty_object_literal(node.ref_(arena).as_binary_expression().left, arena)
        {
            if is_destructuring_assignment(value.unwrap(), arena) {
                node = value.unwrap();
                location = value.unwrap();
                value = Some(node.ref_(arena).as_binary_expression().right);
            } else {
                return try_visit_node(
                    value.unwrap(),
                    visitor
                        .as_ref()
                        .map(|visitor| |node: Id<Node>| (visitor.borrow_mut())(node)),
                    Some(|node| is_expression(node, arena)),
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                );
            }
        }
    }

    let expressions: Id<Option<Vec<Id<Node /*Expression*/>>>> = arena.alloc_option_vec_node(_d());
    let flatten_context = FlattenDestructuringAssignmentFlattenContext::new(
        context.clone(),
        level,
        context
            .ref_(arena)
            .get_compiler_options()
            .ref_(arena)
            .downlevel_iteration
            == Some(true),
        expressions.clone(),
        create_assignment_callback.clone(),
        visitor.clone(),
        arena,
    );

    if value.is_some() {
        value = Some(try_visit_node(
            value.unwrap(),
            visitor
                .as_ref()
                .map(|visitor| |node: Id<Node>| (visitor.borrow_mut())(node)),
            Some(|node| is_expression(node, arena)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?);

        if is_identifier(&value.unwrap().ref_(arena))
            && binding_or_assignment_element_assigns_to_name(
                node,
                &value.unwrap().ref_(arena).as_identifier().escaped_text,
                arena,
            )
            || binding_or_assignment_element_contains_non_literal_computed_name(node, arena)
        {
            value = Some(ensure_identifier(
                &flatten_context,
                value.unwrap(),
                false,
                &*location.ref_(arena),
                arena,
            )?);
        } else if needs_value == Some(true) {
            value = Some(ensure_identifier(
                &flatten_context,
                value.unwrap(),
                true,
                &*location.ref_(arena),
                arena,
            )?);
        } else if node_is_synthesized(&*node.ref_(arena)) {
            location = value.unwrap();
        }
    }

    flatten_binding_or_assignment_element(
        &flatten_context,
        node,
        value,
        &*location.ref_(arena),
        Some(is_destructuring_assignment(node, arena)),
        arena,
    )?;

    if let Some(value) = value.filter(|_| needs_value == Some(true)) {
        if !expressions.ref_(arena).as_ref().is_non_empty() {
            return Ok(value);
        }

        expressions.ref_mut(arena).as_mut().unwrap().push(value);
    }

    let ret = context
        .ref_(arena).factory()
        .ref_(arena).inline_expressions(expressions.ref_(arena).as_ref().unwrap()) /* || context.factory.createOmittedExpression()*/;
    Ok(ret)
}

fn binding_or_assignment_element_assigns_to_name(
    element: Id<Node>,  /*BindingOrAssignmentElement*/
    escaped_name: &str, /*__String*/
    arena: &impl HasArena,
) -> bool {
    let target = get_target_of_binding_or_assignment_element(element, arena).unwrap();
    if is_binding_or_assigment_pattern(&target.ref_(arena)) {
        return binding_or_assignment_pattern_assigns_to_name(target, escaped_name, arena);
    } else if is_identifier(&target.ref_(arena)) {
        return target.ref_(arena).as_identifier().escaped_text == escaped_name;
    }
    false
}

fn binding_or_assignment_pattern_assigns_to_name(
    pattern: Id<Node>,  /*BindingOrAssignmentPattern*/
    escaped_name: &str, /*__String*/
    arena: &impl HasArena,
) -> bool {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern, arena);
    for element in elements {
        if binding_or_assignment_element_assigns_to_name(element, escaped_name, arena) {
            return true;
        }
    }
    false
}

fn binding_or_assignment_element_contains_non_literal_computed_name(
    element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> bool {
    let property_name = try_get_property_name_of_binding_or_assignment_element(element, arena);
    if property_name.matches(|property_name| {
        is_computed_property_name(&property_name.ref_(arena))
            && !is_literal_expression(
                &property_name
                    .ref_(arena)
                    .as_computed_property_name()
                    .expression
                    .ref_(arena),
            )
    }) {
        return true;
    }
    let target = get_target_of_binding_or_assignment_element(element, arena);
    target.matches(|target| {
        is_binding_or_assigment_pattern(&target.ref_(arena))
            && binding_or_assignment_pattern_contains_non_literal_computed_name(target, arena)
    })
}

fn binding_or_assignment_pattern_contains_non_literal_computed_name(
    pattern: Id<Node>, /*BindingOrAssignmentPattern*/
    arena: &impl HasArena,
) -> bool {
    get_elements_of_binding_or_assignment_pattern(pattern, arena)
        .into_iter()
        .any(|element| {
            binding_or_assignment_element_contains_non_literal_computed_name(element, arena)
        })
}

pub fn flatten_destructuring_binding(
    node: Id<Node>, /*VariableDeclaration | ParameterDeclaration*/
    mut visitor: impl FnMut(Id<Node>) -> VisitResult,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    rval: Option<Id<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    skip_initializer: Option<bool>,
    arena: &impl HasArena,
) -> Vec<Id<Node /*VariableDeclaration*/>> {
    try_flatten_destructuring_binding(
        node,
        |node: Id<Node>| Ok(visitor(node)),
        context,
        level,
        rval,
        hoist_temp_variables,
        skip_initializer,
        arena,
    )
    .unwrap()
}

pub struct PendingDeclaration {
    pending_expressions: Option<Vec<Id<Node /*Expression*/>>>,
    name: Id<Node /*BindingName*/>,
    value: Id<Node /*Expression*/>,
    location: Option<ReadonlyTextRangeConcrete>,
    original: Option<Id<Node>>,
}

pub fn try_flatten_destructuring_binding<'visitor>(
    mut node: Id<Node>, /*VariableDeclaration | ParameterDeclaration*/
    visitor: impl FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    rval: Option<Id<Node /*Expression*/>>,
    hoist_temp_variables: Option<bool>,
    skip_initializer: Option<bool>,
    arena: &impl HasArena,
) -> io::Result<Vec<Id<Node /*VariableDeclaration*/>>> {
    let hoist_temp_variables = hoist_temp_variables.unwrap_or(false);
    let pending_expressions: Id<Option<Vec<Id<Node /*Expression*/>>>> =
        arena.alloc_option_vec_node(_d());
    let pending_declarations: Id<Vec<PendingDeclaration>> =
        arena.alloc_vec_pending_declaration(_d());
    let mut declarations: Vec<Id<Node /*VariableDeclaration*/>> = _d();
    let visitor: Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>> =
        Rc::new(RefCell::new(visitor));
    // as Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>
    let flatten_context = FlattenDestructuringBindingFlattenContext::new(
        context.clone(),
        level,
        context
            .ref_(arena)
            .get_compiler_options()
            .ref_(arena)
            .downlevel_iteration
            == Some(true),
        hoist_temp_variables,
        pending_expressions.clone(),
        pending_declarations.clone(),
        visitor.clone(),
        arena,
    );

    if is_variable_declaration(&node.ref_(arena)) {
        let initializer = get_initializer_of_binding_or_assignment_element(node, arena);
        if let Some(mut initializer) = initializer.filter(|initializer| {
            is_identifier(&initializer.ref_(arena))
                && binding_or_assignment_element_assigns_to_name(
                    node,
                    &initializer.ref_(arena).as_identifier().escaped_text,
                    arena,
                )
                || binding_or_assignment_element_contains_non_literal_computed_name(node, arena)
        }) {
            initializer = ensure_identifier(
                &flatten_context,
                try_visit_node(
                    initializer,
                    Some(|node: Id<Node>| (flatten_context.visitor.borrow_mut())(node)),
                    Option::<fn(Id<Node>) -> bool>::None,
                    Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                )?,
                false,
                &*initializer.ref_(arena),
                arena,
            )?;
            node = context
                .ref_(arena)
                .factory()
                .ref_(arena)
                .update_variable_declaration(
                    node,
                    node.ref_(arena).as_variable_declaration().maybe_name(),
                    None,
                    None,
                    Some(initializer),
                );
        }
    }

    flatten_binding_or_assignment_element(
        &flatten_context,
        node,
        rval,
        &*node.ref_(arena),
        skip_initializer,
        arena,
    )?;
    if pending_expressions.ref_(arena).is_some() {
        let temp = context
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
        if hoist_temp_variables {
            let value = context
                .ref_(arena)
                .factory()
                .ref_(arena)
                .inline_expressions(pending_expressions.ref_(arena).as_ref().unwrap());
            *pending_expressions.ref_mut(arena) = None;
            emit_binding_or_assignment(
                &mut pending_expressions.ref_mut(arena),
                &*context.ref_(arena),
                &mut pending_declarations.ref_mut(arena),
                temp,
                value,
                Option::<&Node>::None,
                None,
                arena,
            );
        } else {
            context.ref_(arena).hoist_variable_declaration(temp);
            let mut pending_declarations = pending_declarations.ref_mut(arena);
            let pending_declarations_len = pending_declarations.len();
            let pending_declaration = &mut pending_declarations[pending_declarations_len - 1];
            pending_declaration
                .pending_expressions
                .get_or_insert_default_()
                .push(
                    context
                        .ref_(arena)
                        .factory()
                        .ref_(arena)
                        .create_assignment(temp.clone(), pending_declaration.value.clone()),
                );
            pending_declaration
                .pending_expressions
                .as_mut()
                .unwrap()
                .extend(
                    pending_expressions
                        .ref_(arena)
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
    } in pending_declarations.ref_(arena).iter()
    {
        let variable = context
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_variable_declaration(
                Some(name.clone()),
                None,
                None,
                Some(pending_expressions.as_ref().map_or_else(
                    || value.clone(),
                    |pending_expressions| {
                        context
                            .ref_(arena)
                            .factory()
                            .ref_(arena)
                            .inline_expressions(
                                &pending_expressions.clone().and_push(value.clone()),
                            )
                    },
                )),
            );
        variable.ref_(arena).set_original(original.clone());
        set_text_range(&*variable.ref_(arena), location.as_ref());
        declarations.push(variable);
    }
    Ok(declarations)
}

fn emit_binding_or_assignment(
    pending_expressions: &mut Option<Vec<Id<Node /*Expression*/>>>,
    context: &dyn TransformationContext,
    pending_declarations: &mut Vec<PendingDeclaration>,
    target: Id<Node>,    /*BindingOrAssignmentElementTarget*/
    mut value: Id<Node>, /*Expression*/
    location: Option<&(impl ReadonlyTextRange + ?Sized)>,
    original: Option<Id<Node>>,
    arena: &impl HasArena,
) {
    Debug_.assert_node(
        Some(target),
        Some(|node: Id<Node>| is_binding_name(&node.ref_(arena))),
        None,
    );
    if pending_expressions.is_some() {
        pending_expressions.as_mut().unwrap().push(value);
        value = context
            .factory()
            .ref_(arena)
            .inline_expressions(pending_expressions.as_ref().unwrap());
        *pending_expressions = None;
    }
    pending_declarations.push(PendingDeclaration {
        pending_expressions: pending_expressions.clone(),
        name: target,
        value,
        location: location.map(Into::into),
        original,
    });
}

struct FlattenDestructuringBindingFlattenContext<'visitor> {
    arena: *const AllArenas,
    context: Id<TransformNodesTransformationResult>,
    level: FlattenLevel,
    downlevel_iteration: bool,
    hoist_temp_variables: bool,
    pending_expressions: Id<Option<Vec<Id<Node /*Expression*/>>>>,
    pending_declarations: Id<Vec<PendingDeclaration>>,
    visitor: Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>,
    has_transformed_prior_element: Cell<Option<bool>>,
}

impl<'visitor> FlattenDestructuringBindingFlattenContext<'visitor> {
    pub fn new(
        context: Id<TransformNodesTransformationResult>,
        level: FlattenLevel,
        downlevel_iteration: bool,
        hoist_temp_variables: bool,
        pending_expressions: Id<Option<Vec<Id<Node /*Expression*/>>>>,
        pending_declarations: Id<Vec<PendingDeclaration>>,
        visitor: Rc<RefCell<dyn FnMut(Id<Node>) -> io::Result<VisitResult> + 'visitor>>,
        arena: &impl HasArena,
    ) -> Self {
        Self {
            arena: arena.arena(),
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
    fn context(&self) -> Id<TransformNodesTransformationResult> {
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

    fn emit_expression(&self, value: Id<Node> /*Expression*/) {
        self.pending_expressions
            .ref_mut(self)
            .get_or_insert_default_()
            .push(value);
    }

    fn emit_binding_or_assignment(
        &self,
        target: Id<Node>, /*BindingOrAssignmentElementTarget*/
        value: Id<Node>,  /*Expression*/
        location: &(impl ReadonlyTextRange + ?Sized),
        original: Option<Id<Node>>,
    ) -> io::Result<()> {
        emit_binding_or_assignment(
            &mut self.pending_expressions.ref_mut(self),
            &*self.context.ref_(self),
            &mut self.pending_declarations.ref_mut(self),
            target,
            value,
            Some(location),
            original,
            self,
        );

        Ok(())
    }

    fn create_array_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ArrayBindingOrAssignmentPattern*/> {
        make_array_binding_pattern(
            &self.context.ref_(self).factory().ref_(self),
            elements,
            self,
        )
    }

    fn create_object_binding_or_assignment_pattern(
        &self,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    ) -> Id<Node /*ObjectBindingOrAssignmentPattern*/> {
        make_object_binding_pattern(
            &self.context.ref_(self).factory().ref_(self),
            elements,
            self,
        )
    }

    fn create_array_binding_or_assignment_element(
        &self,
        name: Id<Node>, /*Identifier*/
    ) -> Id<Node /*BindingOrAssignmentElement*/> {
        make_binding_element(&self.context.ref_(self).factory().ref_(self), name)
    }

    fn visitor(&self, node: Id<Node>) -> Option<io::Result<VisitResult /*<Node>*/>> {
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

impl_has_arena!(FlattenDestructuringBindingFlattenContext<'_>);

fn flatten_binding_or_assignment_element(
    flatten_context: &impl FlattenContext,
    element: Id<Node>, /*BindingOrAssignmentElement*/
    mut value: Option<Id<Node /*Expression*/>>,
    location: &impl ReadonlyTextRange,
    skip_initializer: Option<bool>,
    arena: &impl HasArena,
) -> io::Result<()> {
    let binding_target = get_target_of_binding_or_assignment_element(element, arena).unwrap();
    if skip_initializer != Some(true) {
        let initializer = try_maybe_visit_node(
            get_initializer_of_binding_or_assignment_element(element, arena),
            flatten_context
                .is_visitor_supported()
                .then(|| |node: Id<Node>| flatten_context.visitor(node).unwrap()),
            Some(|node| is_expression(node, arena)),
            Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
        )?;
        if let Some(initializer) = initializer {
            if value.is_some() {
                value = Some(create_default_value_check(
                    flatten_context,
                    value.unwrap(),
                    initializer,
                    location,
                    arena,
                )?);
                if !is_simple_inlineable_expression(&initializer.ref_(arena))
                    && is_binding_or_assigment_pattern(&binding_target.ref_(arena))
                {
                    value = Some(ensure_identifier(
                        flatten_context,
                        value.unwrap(),
                        true,
                        location,
                        arena,
                    )?);
                }
            } else {
                value = Some(initializer);
            }
        } else if value.is_none() {
            value = Some(
                flatten_context
                    .context()
                    .ref_(arena)
                    .factory()
                    .ref_(arena)
                    .create_void_zero(),
            );
        }
    }
    if is_object_binding_or_assigment_pattern(&binding_target.ref_(arena)) {
        flatten_object_binding_or_assignment_pattern(
            flatten_context,
            element,
            binding_target,
            value.unwrap(),
            location,
            arena,
        )?;
    } else if is_array_binding_or_assigment_pattern(&binding_target.ref_(arena)) {
        flatten_array_binding_or_assignment_pattern(
            flatten_context,
            element,
            binding_target,
            value.unwrap(),
            location,
            arena,
        )?;
    } else {
        flatten_context.emit_binding_or_assignment(
            binding_target,
            value.unwrap(),
            location,
            Some(element),
        )?;
    }

    Ok(())
}

fn flatten_object_binding_or_assignment_pattern(
    flatten_context: &impl FlattenContext,
    parent: Id<Node>,    /*BindingOrAssignmentElement*/
    pattern: Id<Node>,   /*ObjectBindingOrAssignmentPattern*/
    mut value: Id<Node>, /*Expression*/
    location: &impl ReadonlyTextRange,
    arena: &impl HasArena,
) -> io::Result<()> {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern, arena);
    let num_elements = elements.len();
    if num_elements != 1 {
        let reuse_identifier_expressions =
            !is_declaration_binding_element(&parent.ref_(arena)) || num_elements != 0;
        value = ensure_identifier(
            flatten_context,
            value,
            reuse_identifier_expressions,
            location,
            arena,
        )?;
    }
    let mut binding_elements: Option<Vec<Id<Node /*BindingOrAssignmentElement*/>>> = _d();
    let mut computed_temp_variables: Option<Vec<Id<Node /*Expression*/>>> = _d();
    for (i, element) in elements.iter().enumerate() {
        let element = *element;
        if get_rest_indicator_of_binding_or_assignment_element(&element.ref_(arena)).is_none() {
            let property_name =
                get_property_name_of_binding_or_assignment_element(element, arena).unwrap();
            if flatten_context.level() >= FlattenLevel::ObjectRest
                && !element.ref_(arena).transform_flags().intersects(
                    TransformFlags::ContainsRestOrSpread
                        | TransformFlags::ContainsObjectRestOrSpread,
                )
                && !get_target_of_binding_or_assignment_element(element, arena)
                    .unwrap()
                    .ref_(arena)
                    .transform_flags()
                    .intersects(
                        TransformFlags::ContainsRestOrSpread
                            | TransformFlags::ContainsObjectRestOrSpread,
                    )
                && !is_computed_property_name(&property_name.ref_(arena))
            {
                binding_elements
                    .get_or_insert_default_()
                    .push(try_visit_node(
                        element,
                        flatten_context
                            .is_visitor_supported()
                            .then(|| |node: Id<Node>| flatten_context.visitor(node).unwrap()),
                        Option::<fn(Id<Node>) -> bool>::None,
                        Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
                    )?);
            } else {
                if binding_elements.is_some() {
                    flatten_context.emit_binding_or_assignment(
                        flatten_context.create_object_binding_or_assignment_pattern(
                            binding_elements.as_ref().unwrap(),
                        ),
                        value,
                        location,
                        Some(pattern),
                    )?;
                    binding_elements = None;
                }
                let rhs_value = create_destructuring_property_access(
                    flatten_context,
                    value,
                    property_name,
                    arena,
                )?;
                if is_computed_property_name(&property_name.ref_(arena)) {
                    computed_temp_variables.get_or_insert_default_().push(
                        rhs_value
                            .ref_(arena)
                            .as_element_access_expression()
                            .argument_expression,
                    );
                }
                flatten_binding_or_assignment_element(
                    flatten_context,
                    element,
                    Some(rhs_value),
                    &*element.ref_(arena),
                    None,
                    arena,
                )?;
            }
        } else if i == num_elements - 1 {
            if binding_elements.is_some() {
                flatten_context.emit_binding_or_assignment(
                    flatten_context.create_object_binding_or_assignment_pattern(
                        binding_elements.as_ref().unwrap(),
                    ),
                    value,
                    location,
                    Some(pattern),
                )?;
                binding_elements = None;
            }
            let rhs_value = flatten_context
                .context()
                .ref_(arena)
                .get_emit_helper_factory()
                .ref_(arena)
                .create_rest_helper(
                    value,
                    &elements,
                    computed_temp_variables.as_deref(),
                    &*pattern.ref_(arena),
                );
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &*element.ref_(arena),
                None,
                arena,
            )?;
        }
    }
    if let Some(binding_elements) = binding_elements {
        flatten_context.emit_binding_or_assignment(
            flatten_context.create_object_binding_or_assignment_pattern(&binding_elements),
            value,
            location,
            Some(pattern),
        )?;
    }

    Ok(())
}

fn flatten_array_binding_or_assignment_pattern(
    flatten_context: &impl FlattenContext,
    parent: Id<Node>,    /*BindingOrAssignmentElement*/
    pattern: Id<Node>,   /*ArrayBindingOrAssignmentPattern*/
    mut value: Id<Node>, /*Expression*/
    location: &impl ReadonlyTextRange,
    arena: &impl HasArena,
) -> io::Result<()> {
    let elements = get_elements_of_binding_or_assignment_pattern(pattern, arena);
    let num_elements = elements.len();
    if flatten_context.level() < FlattenLevel::ObjectRest && flatten_context.downlevel_iteration() {
        value = ensure_identifier(
            flatten_context,
            flatten_context
                .context()
                .ref_(arena)
                .get_emit_helper_factory()
                .ref_(arena)
                .create_read_helper(
                    value,
                    (!(num_elements > 0
                        && get_rest_indicator_of_binding_or_assignment_element(
                            &elements[num_elements - 1].ref_(arena),
                        )
                        .is_some()))
                    .then_some(num_elements),
                )
                .set_text_range(Some(location), arena),
            false,
            location,
            arena,
        )?;
    } else if num_elements != 1
        && (flatten_context.level() < FlattenLevel::ObjectRest || num_elements == 0)
        || elements
            .iter()
            .all(|element| is_omitted_expression(&element.ref_(arena)))
    {
        let reuse_identifier_expressions =
            !is_declaration_binding_element(&parent.ref_(arena)) || num_elements != 0;
        value = ensure_identifier(
            flatten_context,
            value,
            reuse_identifier_expressions,
            location,
            arena,
        )?;
    }
    let mut binding_elements: Option<Vec<Id<Node /*BindingOrAssignmentElement*/>>> = _d();
    let mut rest_containing_elements: Option<
        Vec<(
            Id<Node /*Identifier*/>,
            Id<Node /*BindingOrAssignmentElement*/>,
        )>,
    > = _d();
    for (i, element) in elements.iter().enumerate() {
        let element = *element;
        if flatten_context.level() >= FlattenLevel::ObjectRest {
            if element
                .ref_(arena)
                .transform_flags()
                .intersects(TransformFlags::ContainsObjectRestOrSpread)
                || flatten_context.has_transformed_prior_element() == Some(true)
                    && !is_simple_binding_or_assignment_element(element, arena)
            {
                flatten_context.set_has_transformed_prior_element(Some(true));
                let temp = flatten_context
                    .context()
                    .ref_(arena)
                    .factory()
                    .ref_(arena)
                    .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
                if flatten_context.hoist_temp_variables() {
                    flatten_context
                        .context()
                        .ref_(arena)
                        .hoist_variable_declaration(temp);
                }

                rest_containing_elements
                    .get_or_insert_default_()
                    .push((temp, element));
                binding_elements
                    .get_or_insert_default_()
                    .push(flatten_context.create_array_binding_or_assignment_element(temp));
            } else {
                binding_elements.get_or_insert_default_().push(element);
            }
        } else if is_omitted_expression(&element.ref_(arena)) {
            continue;
        } else if get_rest_indicator_of_binding_or_assignment_element(&element.ref_(arena))
            .is_none()
        {
            let rhs_value = flatten_context
                .context()
                .ref_(arena)
                .factory()
                .ref_(arena)
                .create_element_access_expression(value, Number::new(i as f64));
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &*element.ref_(arena),
                None,
                arena,
            )?;
        } else if i == num_elements - 1 {
            let rhs_value = flatten_context
                .context()
                .ref_(arena)
                .factory()
                .ref_(arena)
                .create_array_slice_call(value, Some(Number::new(i as f64)));
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(rhs_value),
                &*element.ref_(arena),
                None,
                arena,
            )?;
        }
    }
    if let Some(binding_elements) = binding_elements {
        flatten_context.emit_binding_or_assignment(
            flatten_context.create_array_binding_or_assignment_pattern(&binding_elements),
            value,
            location,
            Some(pattern),
        )?;
    }
    if let Some(rest_containing_elements) = rest_containing_elements {
        for (id, element) in rest_containing_elements {
            flatten_binding_or_assignment_element(
                flatten_context,
                element,
                Some(id),
                &*element.ref_(arena),
                None,
                arena,
            )?;
        }
    }

    Ok(())
}

fn is_simple_binding_or_assignment_element(
    element: Id<Node>, /*BindingOrAssignmentElement*/
    arena: &impl HasArena,
) -> bool {
    let Some(target) = get_target_of_binding_or_assignment_element(element, arena) else {
        return true;
    };
    if is_omitted_expression(&target.ref_(arena)) {
        return true;
    }
    let property_name = try_get_property_name_of_binding_or_assignment_element(element, arena);
    if property_name.matches(|property_name| !is_property_name_literal(&property_name.ref_(arena)))
    {
        return false;
    }
    let initializer = get_initializer_of_binding_or_assignment_element(element, arena);
    if initializer.matches(|initializer| !is_simple_inlineable_expression(&initializer.ref_(arena)))
    {
        return false;
    }
    if is_binding_or_assigment_pattern(&target.ref_(arena)) {
        return get_elements_of_binding_or_assignment_pattern(target, arena)
            .into_iter()
            .all(|element| is_simple_binding_or_assignment_element(element, arena));
    }
    is_identifier(&target.ref_(arena))
}

fn create_default_value_check(
    flatten_context: &impl FlattenContext,
    value: Id<Node>,         /*Expression*/
    default_value: Id<Node>, /*Expression*/
    location: &impl ReadonlyTextRange,
    arena: &impl HasArena,
) -> io::Result<Id<Node /*Expression*/>> {
    let value = ensure_identifier(flatten_context, value, true, location, arena)?;
    Ok(flatten_context
        .context()
        .ref_(arena)
        .factory()
        .ref_(arena)
        .create_conditional_expression(
            flatten_context
                .context()
                .ref_(arena)
                .factory()
                .ref_(arena)
                .create_type_check(value, "undefined"),
            None,
            default_value,
            None,
            value,
        ))
}

fn create_destructuring_property_access(
    flatten_context: &impl FlattenContext,
    value: Id<Node>,         /*Expression*/
    property_name: Id<Node>, /*PropertyName*/
    arena: &impl HasArena,
) -> io::Result<Id<Node /*LeftHandSideExpression*/>> {
    Ok(if is_computed_property_name(&property_name.ref_(arena)) {
        let argument_expression = ensure_identifier(
            flatten_context,
            try_visit_node(
                property_name
                    .ref_(arena)
                    .as_computed_property_name()
                    .expression,
                flatten_context
                    .is_visitor_supported()
                    .then(|| |node: Id<Node>| flatten_context.visitor(node).unwrap()),
                Option::<fn(Id<Node>) -> bool>::None,
                Option::<fn(&[Id<Node>]) -> Id<Node>>::None,
            )?,
            false,
            &*property_name.ref_(arena),
            arena,
        )?;
        flatten_context
            .context()
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_element_access_expression(value, argument_expression)
    } else if is_string_or_numeric_literal_like(&property_name.ref_(arena)) {
        let argument_expression = get_factory(arena).clone_node(property_name);
        flatten_context
            .context()
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_element_access_expression(value, argument_expression)
    } else {
        let name = flatten_context
            .context()
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_identifier(id_text(&property_name.ref_(arena)));
        flatten_context
            .context()
            .ref_(arena)
            .factory()
            .ref_(arena)
            .create_property_access_expression(value, name)
    })
}

fn ensure_identifier(
    flatten_context: &impl FlattenContext,
    value: Id<Node>, /*Expression*/
    reuse_identifier_expressions: bool,
    location: &impl ReadonlyTextRange,
    arena: &impl HasArena,
) -> io::Result<Id<Node>> {
    Ok(
        if is_identifier(&value.ref_(arena)) && reuse_identifier_expressions {
            value
        } else {
            let temp = flatten_context
                .context()
                .ref_(arena)
                .factory()
                .ref_(arena)
                .create_temp_variable(Option::<fn(Id<Node>)>::None, None);
            if flatten_context.hoist_temp_variables() {
                flatten_context
                    .context()
                    .ref_(arena)
                    .hoist_variable_declaration(temp);
                flatten_context.emit_expression(
                    flatten_context
                        .context()
                        .ref_(arena)
                        .factory()
                        .ref_(arena)
                        .create_assignment(temp, value)
                        .set_text_range(Some(location), arena),
                );
            } else {
                flatten_context.emit_binding_or_assignment(
                    temp,
                    value,
                    location,
                    Option::<Id<Node>>::None,
                )?;
            }
            temp
        },
    )
}

fn make_array_binding_pattern(
    factory: &NodeFactory,
    elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    arena: &impl HasArena,
) -> Id<Node> {
    Debug_.assert_each_node(
        elements,
        |node: Id<Node>| is_array_binding_element(&node.ref_(arena)),
        None,
    );
    factory.create_array_binding_pattern(elements.to_owned())
}

fn make_array_assignment_pattern(
    factory: &NodeFactory,
    elements: &[Id<Node /*BindingOrAssignmentElement*/>],
) -> Id<Node> {
    factory.create_array_literal_expression(
        Some(
            elements
                .into_iter()
                .map(|&element| {
                    factory
                        .converters()
                        .convert_to_array_assignment_element(element)
                })
                .collect_vec(),
        ),
        None,
    )
}

fn make_object_binding_pattern(
    factory: &NodeFactory,
    elements: &[Id<Node /*BindingOrAssignmentElement*/>],
    arena: &impl HasArena,
) -> Id<Node> {
    Debug_.assert_each_node(
        elements,
        |node: Id<Node>| is_binding_element(&node.ref_(arena)),
        None,
    );
    factory.create_object_binding_pattern(elements.to_owned())
}

fn make_object_assignment_pattern(
    factory: &NodeFactory,
    elements: &[Id<Node /*BindingOrAssignmentElement*/>],
) -> Id<Node> {
    factory.create_object_literal_expression(
        Some(
            elements
                .into_iter()
                .map(|&element| {
                    factory
                        .converters()
                        .convert_to_object_assignment_element(element)
                })
                .collect_vec(),
        ),
        None,
    )
}

fn make_binding_element(factory: &NodeFactory, name: Id<Node> /*Identifier*/) -> Id<Node> {
    factory.create_binding_element(None, Option::<Id<Node>>::None, name, None)
}

fn make_assignment_element(name: Id<Node> /*Identifier*/) -> Id<Node> {
    name
}
