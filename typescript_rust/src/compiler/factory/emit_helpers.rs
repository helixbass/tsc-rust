use std::borrow::Borrow;

use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    BaseNodeFactorySynthetic, Comparison, EmitFlags, EmitHelper, Node, NodeArray, NodeArrayOrVec,
    NodeExt, NodeFactory, PrivateIdentifierKind, TransformationContext, _d,
    create_expression_from_entity_name, get_emit_script_target,
    get_property_name_of_binding_or_assignment_element, is_computed_property_name, Debug_,
    GeneratedIdentifierFlags, GetOrInsertDefault, MapOrDefault, NodeInterface, ReadonlyTextRange,
    ScriptTarget, SyntaxKind, VecExt,
};

// TODO: remove #[unsafe_ignore_trace] from TransformNodesTransformationResult if this ends up
// needing to be traced
#[derive(Trace, Finalize)]
pub struct EmitHelperFactory {
    factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    context: Gc<Box<dyn TransformationContext>>,
    immutable_true: GcCell<Option<Gc<Node>>>,
    immutable_false: GcCell<Option<Gc<Node>>>,
}

impl EmitHelperFactory {
    fn immutable_true(&self) -> Gc<Node> {
        self.immutable_true
            .borrow_mut()
            .get_or_insert_with(|| {
                self.factory
                    .create_true()
                    .set_emit_flags(EmitFlags::Immutable)
            })
            .clone()
    }

    fn immutable_false(&self) -> Gc<Node> {
        self.immutable_false
            .borrow_mut()
            .get_or_insert_with(|| {
                self.factory
                    .create_false()
                    .set_emit_flags(EmitFlags::Immutable)
            })
            .clone()
    }

    pub fn get_unscoped_helper_name(&self, name: &str) -> Gc<Node /*Identifier*/> {
        self.factory
            .create_identifier(name)
            .set_emit_flags(EmitFlags::HelperName | EmitFlags::AdviseOnEmitNode)
    }

    pub fn create_decorate_helper(
        &self,
        decorator_expressions: impl Into<NodeArrayOrVec /*Expression*/>,
        target: Gc<Node /*Expression*/>,
        member_name: Option<Gc<Node /*Expression*/>>,
        descriptor: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node /*Expression*/> {
        let decorator_expressions = decorator_expressions.into();
        self.context.request_emit_helper(decorate_helper());

        let mut arguments_array: Vec<Gc<Node /*Expression*/>> = _d();
        arguments_array.push(
            self.factory
                .create_array_literal_expression(Some(decorator_expressions), Some(true)),
        );
        arguments_array.push(target);
        if let Some(member_name) = member_name {
            arguments_array.push(member_name);
            if let Some(descriptor) = descriptor {
                arguments_array.push(descriptor);
            }
        }

        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__decorate"),
            Option::<Gc<NodeArray>>::None,
            Some(arguments_array),
        )
    }

    pub fn create_metadata_helper(
        &self,
        metadata_key: &str,
        metadata_value: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(metadata_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__metadata"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory
                    .create_string_literal(metadata_key.to_owned(), None, None),
                metadata_value,
            ]),
        )
    }

    pub fn create_param_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
        parameter_offset: usize,
        /*location?: TextRange*/
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(param_helper());
        /*setTextRange(*/
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__param"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory
                    .create_numeric_literal(format!("{parameter_offset}"), None),
                expression,
            ]),
        ) /*, location);*/
    }

    pub fn create_assign_helper(
        &self,
        attributes_segments: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        let attributes_segments = attributes_segments.into();
        if get_emit_script_target(&self.context.get_compiler_options()) >= ScriptTarget::ES2015 {
            return self.factory.create_call_expression(
                self.factory.create_property_access_expression(
                    self.factory.create_identifier("Object"),
                    "assign",
                ),
                Option::<Gc<NodeArray>>::None,
                Some(attributes_segments),
            );
        }
        self.context.request_emit_helper(assign_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__assign"),
            Option::<Gc<NodeArray>>::None,
            Some(attributes_segments),
        )
    }

    pub fn create_await_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(await_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__await"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_async_generator_helper(
        &self,
        generator_func: Gc<Node /*FunctionExpression*/>,
        has_lexical_this: bool,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(await_helper());
        self.context.request_emit_helper(async_generator_helper());

        let generator_func_emit_node = generator_func
            .maybe_emit_node_mut()
            .get_or_insert_default_()
            .clone();
        let mut generator_func_emit_node = generator_func_emit_node.borrow_mut();
        generator_func_emit_node.flags = Some(
            generator_func_emit_node.flags.unwrap_or_default()
                | EmitFlags::AsyncFunctionBody
                | EmitFlags::ReuseTempVariableScope,
        );

        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__asyncGenerator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                if has_lexical_this {
                    self.factory.create_this()
                } else {
                    self.factory.create_void_zero()
                },
                self.factory.create_identifier("arguments"),
                generator_func,
            ]),
        )
    }

    pub fn create_async_delegator_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(await_helper());
        self.context.request_emit_helper(async_delegator());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__asyncDelegator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_async_values_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(async_values());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__asyncValues"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_rest_helper(
        &self,
        value: Gc<Node /*Expression*/>,
        elements: &[Gc<Node /*BindingOrAssignmentElement*/>],
        computed_temp_variables: Option<&[Gc<Node /*Expression*/>]>,
        location: &impl ReadonlyTextRange,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(rest_helper());
        let mut property_names: Vec<Gc<Node /*Expression*/>> = _d();
        let mut computed_temp_variable_offset = 0;
        for element in elements.into_iter().take(elements.len() - 1) {
            let property_name = get_property_name_of_binding_or_assignment_element(element);
            if let Some(ref property_name) = property_name {
                if is_computed_property_name(property_name) {
                    Debug_.assert_is_defined(
                        &computed_temp_variables,
                        Some("Encountered computed property name but 'computedTempVariables' argument was not provided.")
                    );
                    let computed_temp_variables = computed_temp_variables.unwrap();
                    let temp = &computed_temp_variables[computed_temp_variable_offset];
                    computed_temp_variable_offset += 1;
                    property_names.push(
                        self.factory.create_conditional_expression(
                            self.factory.create_type_check(temp.clone(), "symbol"),
                            None,
                            temp.clone(),
                            None,
                            self.factory.create_add(
                                temp.clone(),
                                self.factory
                                    .create_string_literal("".to_owned(), None, None),
                            ),
                        ),
                    );
                } else {
                    property_names
                        .push(self.factory.create_string_literal_from_node(property_name));
                }
            }
        }
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__rest"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                value,
                self.factory
                    .create_array_literal_expression(Some(property_names), None)
                    .set_text_range(Some(location)),
            ]),
        )
    }

    pub fn create_awaiter_helper(
        &self,
        has_lexical_this: bool,
        has_lexical_arguments: bool,
        promise_constructor: Option<Gc<Node /*EntityName | Expression*/>>,
        body: Gc<Node /*Block*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(awaiter_helper());

        let generator_func = self.factory.create_function_expression(
            Option::<Gc<NodeArray>>::None,
            Some(self.factory.create_token(SyntaxKind::AsteriskToken)),
            Option::<Gc<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(vec![]),
            None,
            body,
        );

        let generator_func_emit_node = generator_func
            .maybe_emit_node_mut()
            .get_or_insert_default_()
            .clone();
        let mut generator_func_emit_node = generator_func_emit_node.borrow_mut();
        generator_func_emit_node.flags = Some(
            generator_func_emit_node.flags.unwrap_or_default()
                | EmitFlags::AsyncFunctionBody
                | EmitFlags::ReuseTempVariableScope,
        );

        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__awaiter"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                if has_lexical_this {
                    self.factory.create_this()
                } else {
                    self.factory.create_void_zero()
                },
                if has_lexical_arguments {
                    self.factory.create_identifier("arguments")
                } else {
                    self.factory.create_void_zero()
                },
                promise_constructor.map_or_else(
                    || self.factory.create_void_zero(),
                    |promise_constructor| {
                        create_expression_from_entity_name(&self.factory, &promise_constructor)
                    },
                ),
                generator_func,
            ]),
        )
    }

    pub fn create_extends_helper(&self, name: Gc<Node /*Identifier*/>) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(extends_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__extends"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                name,
                self.factory.create_unique_name(
                    "_super",
                    Some(
                        GeneratedIdentifierFlags::Optimistic | GeneratedIdentifierFlags::FileLevel,
                    ),
                ),
            ]),
        )
    }

    pub fn create_template_object_helper(
        &self,
        cooked: Gc<Node /*ArrayLiteralExpression*/>,
        raw: Gc<Node /*ArrayLiteralExpression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(template_object_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__makeTemplateObject"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![cooked, raw]),
        )
    }

    pub fn create_spread_array_helper(
        &self,
        to: Gc<Node /*Expression*/>,
        from: Gc<Node /*Expression*/>,
        pack_from: bool,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(spread_array_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__spreadArray"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                to,
                from,
                if pack_from {
                    self.immutable_true()
                } else {
                    self.immutable_false()
                },
            ]),
        )
    }

    pub fn create_values_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(values_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__values"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_read_helper(
        &self,
        iterator_record: Gc<Node /*Expression*/>,
        count: Option<usize>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(read_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__read"),
            Option::<Gc<NodeArray>>::None,
            Some(count.map_or_else(
                || vec![iterator_record.clone()],
                |count| {
                    vec![
                        iterator_record.clone(),
                        self.factory
                            .create_numeric_literal(format!("{count}"), None),
                    ]
                },
            )),
        )
    }

    pub fn create_generator_helper(
        &self,
        body: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(generator_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__generator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![self.factory.create_this(), body]),
        )
    }

    pub fn create_create_binding_helper(
        &self,
        module: Gc<Node /*Expression*/>,
        input_name: Gc<Node /*Expression*/>,
        output_name: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(create_binding_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__createBinding"),
            Option::<Gc<NodeArray>>::None,
            Some(
                vec![
                    self.factory.create_identifier("exports"),
                    module,
                    input_name,
                ]
                .and_extend(output_name.map_or_default(|output_name| vec![output_name])),
            ),
        )
    }

    pub fn create_import_star_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(import_star_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__importStar"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_import_star_callback_helper(&self) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(import_star_helper());
        self.get_unscoped_helper_name("__importStar")
    }

    pub fn create_import_default_helper(
        &self,
        expression: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context.request_emit_helper(import_default_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__importDefault"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_export_star_helper(
        &self,
        module_expression: Gc<Node /*Expression*/>,
        exports_expression: Option<Gc<Node /*Expression*/>>,
    ) -> Gc<Node /*Expression*/> {
        let exports_expression =
            exports_expression.unwrap_or_else(|| self.factory.create_identifier("exports"));
        self.context.request_emit_helper(export_star_helper());
        self.context.request_emit_helper(create_binding_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__exportStar"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![module_expression, exports_expression]),
        )
    }

    pub fn create_class_private_field_get_helper(
        &self,
        receiver: Gc<Node /*Expression*/>,
        state: Gc<Node /*Identifier*/>,
        kind: PrivateIdentifierKind,
        f: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node /*Expression*/> {
        self.context
            .request_emit_helper(class_private_field_get_helper());
        let kind_str: &str = kind.borrow();
        let args = match f {
            None => vec![
                receiver,
                state,
                self.factory
                    .create_string_literal(kind_str.to_owned(), None, None),
            ],
            Some(f) => vec![
                receiver,
                state,
                self.factory
                    .create_string_literal(kind_str.to_owned(), None, None),
                f,
            ],
        };
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldGet"),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        )
    }

    pub fn create_class_private_field_set_helper(
        &self,
        receiver: Gc<Node /*Expression*/>,
        state: Gc<Node /*Identifier*/>,
        value: Gc<Node /*Expression*/>,
        kind: PrivateIdentifierKind,
        f: Option<Gc<Node /*Identifier*/>>,
    ) -> Gc<Node /*Expression*/> {
        self.context
            .request_emit_helper(class_private_field_set_helper());
        let kind_str: &str = kind.borrow();
        let args = match f {
            None => vec![
                receiver,
                state,
                value,
                self.factory
                    .create_string_literal(kind_str.to_owned(), None, None),
            ],
            Some(f) => vec![
                receiver,
                state,
                value,
                self.factory
                    .create_string_literal(kind_str.to_owned(), None, None),
                f,
            ],
        };
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldSet"),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        )
    }

    pub fn create_class_private_field_in_helper(
        &self,
        state: Gc<Node /*Identifier*/>,
        receiver: Gc<Node /*Expression*/>,
    ) -> Gc<Node /*Expression*/> {
        self.context
            .request_emit_helper(class_private_field_in_helper());
        self.factory.create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldIn"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![state, receiver]),
        )
    }
}

pub fn create_emit_helper_factory(
    context: Gc<Box<dyn TransformationContext>>,
) -> EmitHelperFactory {
    EmitHelperFactory {
        factory: context.factory(),
        context,
        immutable_true: _d(),
        immutable_false: _d(),
    }
}

pub(crate) fn compare_emit_helpers(_x: &EmitHelper, _y: &EmitHelper) -> Comparison {
    unimplemented!()
}

pub fn decorate_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn metadata_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn param_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn assign_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn await_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn async_generator_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn async_delegator() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn async_values() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn rest_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn awaiter_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn extends_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn template_object_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn read_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn spread_array_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn values_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn generator_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn create_binding_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn set_module_default_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn import_star_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn import_default_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn export_star_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn class_private_field_get_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn class_private_field_set_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn class_private_field_in_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn advanced_async_super_helper() -> Gc<EmitHelper> {
    unimplemented!()
}

pub fn is_call_to_helper(
    _first_segment: &Node, /*Expression*/
    _helper_name: &str,    /*__String*/
) -> bool {
    unimplemented!()
}
