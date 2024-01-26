use std::borrow::Borrow;

use gc::{Finalize, Gc, GcCell, Trace};
use id_arena::Id;

use crate::{
    BaseNodeFactorySynthetic, Comparison, EmitFlags, EmitHelper, Node, NodeArray, NodeArrayOrVec,
    NodeExt, NodeFactory, PrivateIdentifierKind, TransformationContext, _d, compare_values,
    create_expression_from_entity_name, get_emit_flags, get_emit_script_target,
    get_property_name_of_binding_or_assignment_element, is_call_expression,
    is_computed_property_name, is_identifier, Debug_, EmitHelperBase, EmitHelperTextCallback,
    GeneratedIdentifierFlags, GetOrInsertDefault, MapOrDefault, NodeInterface, ReadonlyTextRange,
    ScopedEmitHelperBuilder, ScriptTarget, SyntaxKind, UnscopedEmitHelperBuilder, VecExt,
    HasArena, AllArenas, InArena,
    TransformNodesTransformationResult, CoreTransformationContext,
};

// TODO: remove #[unsafe_ignore_trace] from TransformNodesTransformationResult if this ends up
// needing to be traced
#[derive(Trace, Finalize)]
pub struct EmitHelperFactory {
    factory: Id<NodeFactory>,
    context: Id<TransformNodesTransformationResult>,
    immutable_true: GcCell<Option<Id<Node>>>,
    immutable_false: GcCell<Option<Id<Node>>>,
}

impl EmitHelperFactory {
    fn immutable_true(&self) -> Id<Node> {
        self.immutable_true
            .borrow_mut()
            .get_or_insert_with(|| {
                self.factory
                    .ref_(self).create_true()
                    .set_emit_flags(EmitFlags::Immutable, self)
            })
            .clone()
    }

    fn immutable_false(&self) -> Id<Node> {
        self.immutable_false
            .borrow_mut()
            .get_or_insert_with(|| {
                self.factory
                    .ref_(self).create_false()
                    .set_emit_flags(EmitFlags::Immutable, self)
            })
            .clone()
    }

    pub fn get_unscoped_helper_name(&self, name: &str) -> Id<Node /*Identifier*/> {
        self.factory
            .ref_(self).create_identifier(name)
            .set_emit_flags(EmitFlags::HelperName | EmitFlags::AdviseOnEmitNode, self)
    }

    pub fn create_decorate_helper(
        &self,
        decorator_expressions: impl Into<NodeArrayOrVec /*Expression*/>,
        target: Id<Node /*Expression*/>,
        member_name: Option<Id<Node /*Expression*/>>,
        descriptor: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node /*Expression*/> {
        let decorator_expressions = decorator_expressions.into();
        self.context.ref_(self).request_emit_helper(decorate_helper(self));

        let mut arguments_array: Vec<Id<Node /*Expression*/>> = _d();
        arguments_array.push(
            self.factory
                .ref_(self).create_array_literal_expression(Some(decorator_expressions), Some(true)),
        );
        arguments_array.push(target);
        if let Some(member_name) = member_name {
            arguments_array.push(member_name);
            if let Some(descriptor) = descriptor {
                arguments_array.push(descriptor);
            }
        }

        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__decorate"),
            Option::<Gc<NodeArray>>::None,
            Some(arguments_array),
        )
    }

    pub fn create_metadata_helper(
        &self,
        metadata_key: &str,
        metadata_value: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(metadata_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__metadata"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory
                    .ref_(self).create_string_literal(metadata_key.to_owned(), None, None),
                metadata_value,
            ]),
        )
    }

    pub fn create_param_helper(
        &self,
        expression: Id<Node /*Expression*/>,
        parameter_offset: usize,
        /*location?: TextRange*/
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(param_helper(self));
        /*setTextRange(*/
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__param"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                self.factory
                    .ref_(self).create_numeric_literal(format!("{parameter_offset}"), None),
                expression,
            ]),
        ) /*, location);*/
    }

    pub fn create_assign_helper(
        &self,
        attributes_segments: impl Into<NodeArrayOrVec /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        let attributes_segments = attributes_segments.into();
        if get_emit_script_target(&self.context.ref_(self).get_compiler_options().ref_(self)) >= ScriptTarget::ES2015 {
            return self.factory.ref_(self).create_call_expression(
                self.factory.ref_(self).create_property_access_expression(
                    self.factory.ref_(self).create_identifier("Object"),
                    "assign",
                ),
                Option::<Gc<NodeArray>>::None,
                Some(attributes_segments),
            );
        }
        self.context.ref_(self).request_emit_helper(assign_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__assign"),
            Option::<Gc<NodeArray>>::None,
            Some(attributes_segments),
        )
    }

    pub fn create_await_helper(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(await_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__await"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_async_generator_helper(
        &self,
        generator_func: Id<Node /*FunctionExpression*/>,
        has_lexical_this: bool,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(await_helper(self));
        self.context.ref_(self).request_emit_helper(async_generator_helper(self));

        let generator_func_emit_node = generator_func
            .ref_(self).maybe_emit_node_mut()
            .get_or_insert_default_()
            .clone();
        let mut generator_func_emit_node = generator_func_emit_node.borrow_mut();
        generator_func_emit_node.flags = Some(
            generator_func_emit_node.flags.unwrap_or_default()
                | EmitFlags::AsyncFunctionBody
                | EmitFlags::ReuseTempVariableScope,
        );

        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__asyncGenerator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                if has_lexical_this {
                    self.factory.ref_(self).create_this()
                } else {
                    self.factory.ref_(self).create_void_zero()
                },
                self.factory.ref_(self).create_identifier("arguments"),
                generator_func,
            ]),
        )
    }

    pub fn create_async_delegator_helper(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(await_helper(self));
        self.context.ref_(self).request_emit_helper(async_delegator(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__asyncDelegator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_async_values_helper(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(async_values(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__asyncValues"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_rest_helper(
        &self,
        value: Id<Node /*Expression*/>,
        elements: &[Id<Node /*BindingOrAssignmentElement*/>],
        computed_temp_variables: Option<&[Id<Node /*Expression*/>]>,
        location: &impl ReadonlyTextRange,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(rest_helper(self));
        let mut property_names: Vec<Id<Node /*Expression*/>> = _d();
        let mut computed_temp_variable_offset = 0;
        for &element in elements.into_iter().take(elements.len() - 1) {
            let property_name = get_property_name_of_binding_or_assignment_element(element, self);
            if let Some(property_name) = property_name {
                if is_computed_property_name(&property_name.ref_(self)) {
                    Debug_.assert_is_defined(
                        &computed_temp_variables,
                        Some("Encountered computed property name but 'computedTempVariables' argument was not provided.")
                    );
                    let computed_temp_variables = computed_temp_variables.unwrap();
                    let temp = &computed_temp_variables[computed_temp_variable_offset];
                    computed_temp_variable_offset += 1;
                    property_names.push(
                        self.factory.ref_(self).create_conditional_expression(
                            self.factory.ref_(self).create_type_check(temp.clone(), "symbol"),
                            None,
                            temp.clone(),
                            None,
                            self.factory.ref_(self).create_add(
                                temp.clone(),
                                self.factory
                                    .ref_(self).create_string_literal("".to_owned(), None, None),
                            ),
                        ),
                    );
                } else {
                    property_names
                        .push(self.factory.ref_(self).create_string_literal_from_node(property_name));
                }
            }
        }
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__rest"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                value,
                self.factory
                    .ref_(self).create_array_literal_expression(Some(property_names), None)
                    .set_text_range(Some(location), self),
            ]),
        )
    }

    pub fn create_awaiter_helper(
        &self,
        has_lexical_this: bool,
        has_lexical_arguments: bool,
        promise_constructor: Option<Id<Node /*EntityName | Expression*/>>,
        body: Id<Node /*Block*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(awaiter_helper(self));

        let generator_func = self.factory.ref_(self).create_function_expression(
            Option::<Gc<NodeArray>>::None,
            Some(self.factory.ref_(self).create_token(SyntaxKind::AsteriskToken)),
            Option::<Id<Node>>::None,
            Option::<Gc<NodeArray>>::None,
            Some(vec![]),
            None,
            body,
        );

        let generator_func_emit_node = generator_func
            .ref_(self).maybe_emit_node_mut()
            .get_or_insert_default_()
            .clone();
        let mut generator_func_emit_node = generator_func_emit_node.borrow_mut();
        generator_func_emit_node.flags = Some(
            generator_func_emit_node.flags.unwrap_or_default()
                | EmitFlags::AsyncFunctionBody
                | EmitFlags::ReuseTempVariableScope,
        );

        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__awaiter"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                if has_lexical_this {
                    self.factory.ref_(self).create_this()
                } else {
                    self.factory.ref_(self).create_void_zero()
                },
                if has_lexical_arguments {
                    self.factory.ref_(self).create_identifier("arguments")
                } else {
                    self.factory.ref_(self).create_void_zero()
                },
                promise_constructor.map_or_else(
                    || self.factory.ref_(self).create_void_zero(),
                    |promise_constructor| {
                        create_expression_from_entity_name(&self.factory.ref_(self), promise_constructor)
                    },
                ),
                generator_func,
            ]),
        )
    }

    pub fn create_extends_helper(&self, name: Id<Node /*Identifier*/>) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(extends_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__extends"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![
                name,
                self.factory.ref_(self).create_unique_name(
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
        cooked: Id<Node /*ArrayLiteralExpression*/>,
        raw: Id<Node /*ArrayLiteralExpression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(template_object_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__makeTemplateObject"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![cooked, raw]),
        )
    }

    pub fn create_spread_array_helper(
        &self,
        to: Id<Node /*Expression*/>,
        from: Id<Node /*Expression*/>,
        pack_from: bool,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(spread_array_helper(self));
        self.factory.ref_(self).create_call_expression(
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
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(values_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__values"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_read_helper(
        &self,
        iterator_record: Id<Node /*Expression*/>,
        count: Option<usize>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(read_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__read"),
            Option::<Gc<NodeArray>>::None,
            Some(count.map_or_else(
                || vec![iterator_record.clone()],
                |count| {
                    vec![
                        iterator_record.clone(),
                        self.factory
                            .ref_(self).create_numeric_literal(format!("{count}"), None),
                    ]
                },
            )),
        )
    }

    pub fn create_generator_helper(
        &self,
        body: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(generator_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__generator"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![self.factory.ref_(self).create_this(), body]),
        )
    }

    pub fn create_create_binding_helper(
        &self,
        module: Id<Node /*Expression*/>,
        input_name: Id<Node /*Expression*/>,
        output_name: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(create_binding_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__createBinding"),
            Option::<Gc<NodeArray>>::None,
            Some(
                vec![
                    self.factory.ref_(self).create_identifier("exports"),
                    module,
                    input_name,
                ]
                .and_extend(output_name.map_or_default(|output_name| vec![output_name])),
            ),
        )
    }

    pub fn create_import_star_helper(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(import_star_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__importStar"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_import_star_callback_helper(&self) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(import_star_helper(self));
        self.get_unscoped_helper_name("__importStar")
    }

    pub fn create_import_default_helper(
        &self,
        expression: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context.ref_(self).request_emit_helper(import_default_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__importDefault"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![expression]),
        )
    }

    pub fn create_export_star_helper(
        &self,
        module_expression: Id<Node /*Expression*/>,
        exports_expression: Option<Id<Node /*Expression*/>>,
    ) -> Id<Node /*Expression*/> {
        let exports_expression =
            exports_expression.unwrap_or_else(|| self.factory.ref_(self).create_identifier("exports"));
        self.context.ref_(self).request_emit_helper(export_star_helper(self));
        self.context.ref_(self).request_emit_helper(create_binding_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__exportStar"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![module_expression, exports_expression]),
        )
    }

    pub fn create_class_private_field_get_helper(
        &self,
        receiver: Id<Node /*Expression*/>,
        state: Id<Node /*Identifier*/>,
        kind: PrivateIdentifierKind,
        f: Option<Id<Node /*Identifier*/>>,
    ) -> Id<Node /*Expression*/> {
        self.context
            .ref_(self).request_emit_helper(class_private_field_get_helper(self));
        let kind_str: &str = kind.borrow();
        let args = match f {
            None => vec![
                receiver,
                state,
                self.factory
                    .ref_(self).create_string_literal(kind_str.to_owned(), None, None),
            ],
            Some(f) => vec![
                receiver,
                state,
                self.factory
                    .ref_(self).create_string_literal(kind_str.to_owned(), None, None),
                f,
            ],
        };
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldGet"),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        )
    }

    pub fn create_class_private_field_set_helper(
        &self,
        receiver: Id<Node /*Expression*/>,
        state: Id<Node /*Identifier*/>,
        value: Id<Node /*Expression*/>,
        kind: PrivateIdentifierKind,
        f: Option<Id<Node /*Identifier*/>>,
    ) -> Id<Node /*Expression*/> {
        self.context
            .ref_(self).request_emit_helper(class_private_field_set_helper(self));
        let kind_str: &str = kind.borrow();
        let args = match f {
            None => vec![
                receiver,
                state,
                value,
                self.factory
                    .ref_(self).create_string_literal(kind_str.to_owned(), None, None),
            ],
            Some(f) => vec![
                receiver,
                state,
                value,
                self.factory
                    .ref_(self).create_string_literal(kind_str.to_owned(), None, None),
                f,
            ],
        };
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldSet"),
            Option::<Gc<NodeArray>>::None,
            Some(args),
        )
    }

    pub fn create_class_private_field_in_helper(
        &self,
        state: Id<Node /*Identifier*/>,
        receiver: Id<Node /*Expression*/>,
    ) -> Id<Node /*Expression*/> {
        self.context
            .ref_(self).request_emit_helper(class_private_field_in_helper(self));
        self.factory.ref_(self).create_call_expression(
            self.get_unscoped_helper_name("__classPrivateFieldIn"),
            Option::<Gc<NodeArray>>::None,
            Some(vec![state, receiver]),
        )
    }
}

impl HasArena for EmitHelperFactory {
    fn arena(&self) -> &AllArenas {
        unimplemented!()
    }
}

pub fn create_emit_helper_factory(
    context: Id<TransformNodesTransformationResult>,
    arena: &impl HasArena,
) -> EmitHelperFactory {
    EmitHelperFactory {
        factory: context.ref_(arena).factory(),
        context,
        immutable_true: _d(),
        immutable_false: _d(),
    }
}

pub(crate) fn compare_emit_helpers(x: Id<EmitHelper>, y: Id<EmitHelper>, arena: &impl HasArena) -> Comparison {
    if x == y {
        return Comparison::EqualTo;
    }
    if x.ref_(arena).priority() == y.ref_(arena).priority() {
        return Comparison::EqualTo;
    }
    if x.ref_(arena).priority().is_none() {
        return Comparison::GreaterThan;
    }
    if y.ref_(arena).priority().is_none() {
        return Comparison::LessThan;
    }
    compare_values(x.ref_(arena).priority(), y.ref_(arena).priority())
}

pub fn helper_string(
    input: &[&'static str], /*TemplateStringsArray*/
    args: &[&'static str],
) -> Gc<Box<dyn EmitHelperTextCallback>> {
    Gc::new(Box::new(HelperString {
        input: input.to_owned(),
        args: args.to_owned(),
    }))
}

#[derive(Trace, Finalize)]
struct HelperString {
    input: Vec<&'static str>,
    args: Vec<&'static str>,
}

impl EmitHelperTextCallback for HelperString {
    fn call(&self, unique_name: &dyn Fn(&str) -> String) -> String {
        let mut result = "".to_owned();
        for (i, arg) in self.args.iter().enumerate() {
            result.push_str(&self.input[i]);
            result.push_str(&unique_name(arg));
        }
        result.push_str(&self.input[self.input.len() - 1]);
        result
    }
}

macro_rules! lazy_emit_helper {
    ($initializer:expr, $arena:expr $(,)?) => {{
        use std::cell::OnceCell;
        use id_arena::Id;

        thread_local! {
            static LAZY_HELPER: OnceCell<Id<EmitHelper>> = OnceCell::new();
        }
        LAZY_HELPER.with(|lazy_helper| {
            *lazy_helper.get_or_init(|| {
                $arena.alloc_emit_helper($initializer)
            })
        })
    }};
}

pub fn decorate_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:decorate")
        .import_name("__decorate")
        .priority(2_usize)
        .text(r#"
            var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
                var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
                if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
                else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
                return c > 3 && r && Object.defineProperty(target, key, r), r;
            };"#)
        .build().unwrap().into(), arena)
}

pub fn metadata_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:metadata")
        .import_name("__metadata")
        .priority(3_usize)
        .text(r#"
            var __metadata = (this && this.__metadata) || function (k, v) {
                if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
            };"#)
        .build().unwrap().into(), arena)
}

pub fn param_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:param")
        .import_name("__param")
        .priority(4_usize)
        .text(
            r#"
            var __param = (this && this.__param) || function (paramIndex, decorator) {
                return function (target, key) { decorator(target, key, paramIndex); }
            };"#,
        )
        .build()
        .unwrap()
        .into(), arena)
}

pub fn assign_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:assign")
        .import_name("__assign")
        .priority(1_usize)
        .text(
            r#"
            var __assign = (this && this.__assign) || function () {
                __assign = Object.assign || function(t) {
                    for (var s, i = 1, n = arguments.length; i < n; i++) {
                        s = arguments[i];
                        for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                            t[p] = s[p];
                    }
                    return t;
                };
                return __assign.apply(this, arguments);
            };"#,
        )
        .build()
        .unwrap()
        .into(), arena)
}

pub fn await_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:await")
        .import_name("__await")
        .text(r#"
            var __await = (this && this.__await) || function (v) { return this instanceof __await ? (this.v = v, this) : new __await(v); }"#)
        .build().unwrap().into(), arena)
}

pub fn async_generator_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:asyncGenerator")
        .import_name("__asyncGenerator")
        .dependencies([await_helper(arena)])
        .text(r#"
            var __asyncGenerator = (this && this.__asyncGenerator) || function (thisArg, _arguments, generator) {
                if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
                var g = generator.apply(thisArg, _arguments || []), i, q = [];
                return i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i;
                function verb(n) { if (g[n]) i[n] = function (v) { return new Promise(function (a, b) { q.push([n, v, a, b]) > 1 || resume(n, v); }); }; }
                function resume(n, v) { try { step(g[n](v)); } catch (e) { settle(q[0][3], e); } }
                function step(r) { r.value instanceof __await ? Promise.resolve(r.value.v).then(fulfill, reject) : settle(q[0][2], r); }
                function fulfill(value) { resume("next", value); }
                function reject(value) { resume("throw", value); }
                function settle(f, v) { if (f(v), q.shift(), q.length) resume(q[0][0], q[0][1]); }
            };"#)
        .build().unwrap().into(), arena)
}

pub fn async_delegator(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:asyncDelegator")
        .import_name("__asyncDelegator")
        .dependencies([await_helper(arena)])
        .text(r#"
            var __asyncDelegator = (this && this.__asyncDelegator) || function (o) {
                var i, p;
                return i = {}, verb("next"), verb("throw", function (e) { throw e; }), verb("return"), i[Symbol.iterator] = function () { return this; }, i;
                function verb(n, f) { i[n] = o[n] ? function (v) { return (p = !p) ? { value: __await(o[n](v)), done: n === "return" } : f ? f(v) : v; } : f; }
            };"#)
        .build().unwrap().into(), arena)
}

pub fn async_values(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:asyncValues")
        .import_name("__asyncValues")
        .text(r#"
            var __asyncValues = (this && this.__asyncValues) || function (o) {
                if (!Symbol.asyncIterator) throw new TypeError("Symbol.asyncIterator is not defined.");
                var m = o[Symbol.asyncIterator], i;
                return m ? m.call(o) : (o = typeof __values === "function" ? __values(o) : o[Symbol.iterator](), i = {}, verb("next"), verb("throw"), verb("return"), i[Symbol.asyncIterator] = function () { return this; }, i);
                function verb(n) { i[n] = o[n] && function (v) { return new Promise(function (resolve, reject) { v = o[n](v), settle(resolve, reject, v.done, v.value); }); }; }
                function settle(resolve, reject, d, v) { Promise.resolve(v).then(function(v) { resolve({ value: v, done: d }); }, reject); }
            };"#)
        .build().unwrap().into(), arena)
}

pub fn rest_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:rest")
        .import_name("__rest")
        .text(r#"
            var __rest = (this && this.__rest) || function (s, e) {
                var t = {};
                for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
                    t[p] = s[p];
                if (s != null && typeof Object.getOwnPropertySymbols === "function")
                    for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
                        if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                            t[p[i]] = s[p[i]];
                    }
                return t;
            };"#)
        .build().unwrap().into(), arena)
}

pub fn awaiter_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
            .name("typescript:awaiter")
            .import_name("__awaiter")
            .priority(5_usize)
            .text(r#"
                var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
                    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
                    return new (P || (P = Promise))(function (resolve, reject) {
                        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
                        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
                        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
                        step((generator = generator.apply(thisArg, _arguments || [])).next());
                    });
                };"#)
            .build().unwrap().into(), arena)
}

pub fn extends_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:extends")
        .import_name("__extends")
        .priority(0_usize)
        .text(r#"
            var __extends = (this && this.__extends) || (function () {
                var extendStatics = function (d, b) {
                    extendStatics = Object.setPrototypeOf ||
                        ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
                        function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
                    return extendStatics(d, b);
                };

                return function (d, b) {
                    if (typeof b !== "function" && b !== null)
                        throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
                    extendStatics(d, b);
                    function __() { this.constructor = d; }
                    d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
                };
            })();"#)
        .build().unwrap().into(), arena)
}

pub fn template_object_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:makeTemplateObject")
        .import_name("__makeTemplateObject")
        .priority(0_usize)
        .text(r#"
            var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
                if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
                return cooked;
            };"#)
        .build().unwrap().into(), arena)
}

pub fn read_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:read")
        .import_name("__read")
        .text(
            r#"
            var __read = (this && this.__read) || function (o, n) {
                var m = typeof Symbol === "function" && o[Symbol.iterator];
                if (!m) return o;
                var i = m.call(o), r, ar = [], e;
                try {
                    while ((n === void 0 || n-- > 0) && !(r = i.next()).done) ar.push(r.value);
                }
                catch (error) { e = { error: error }; }
                finally {
                    try {
                        if (r && !r.done && (m = i["return"])) m.call(i);
                    }
                    finally { if (e) throw e.error; }
                }
                return ar;
            };"#,
        )
        .build()
        .unwrap()
        .into(), arena)
}

pub fn spread_array_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:spreadArray")
        .import_name("__spreadArray")
        .text(r#"
            var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
                if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
                    if (ar || !(i in from)) {
                        if (!ar) ar = Array.prototype.slice.call(from, 0, i);
                        ar[i] = from[i];
                    }
                }
                return to.concat(ar || Array.prototype.slice.call(from));
            };"#)
        .build().unwrap().into(), arena)
}

pub fn values_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:values")
        .import_name("__values")
        .text(r#"
            var __values = (this && this.__values) || function(o) {
                var s = typeof Symbol === "function" && Symbol.iterator, m = s && o[s], i = 0;
                if (m) return m.call(o);
                if (o && typeof o.length === "number") return {
                    next: function () {
                        if (o && i >= o.length) o = void 0;
                        return { value: o && o[i++], done: !o };
                    }
                };
                throw new TypeError(s ? "Object is not iterable." : "Symbol.iterator is not defined.");
            };"#)
        .build().unwrap().into(), arena)
}

pub fn generator_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:generator")
        .import_name("__generator")
        .priority(6_usize)
        .text(r#"
            var __generator = (this && this.__generator) || function (thisArg, body) {
                var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
                return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
                function verb(n) { return function (v) { return step([n, v]); }; }
                function step(op) {
                    if (f) throw new TypeError("Generator is already executing.");
                    while (_) try {
                        if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
                        if (y = 0, t) op = [op[0] & 2, t.value];
                        switch (op[0]) {
                            case 0: case 1: t = op; break;
                            case 4: _.label++; return { value: op[1], done: false };
                            case 5: _.label++; y = op[1]; op = [0]; continue;
                            case 7: op = _.ops.pop(); _.trys.pop(); continue;
                            default:
                                if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                                if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                                if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                                if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                                if (t[2]) _.ops.pop();
                                _.trys.pop(); continue;
                        }
                        op = body.call(thisArg, _);
                    } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
                    if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
                }
            };"#)
        .build().unwrap().into(), arena)
}

pub fn create_binding_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:commonjscreatebinding")
        .import_name("__createBinding")
        .priority(1_usize)
        .text(r#"
            var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
                if (k2 === undefined) k2 = k;
                Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
            }) : (function(o, m, k, k2) {
                if (k2 === undefined) k2 = k;
                o[k2] = m[k];
            }));"#)
        .build().unwrap().into(), arena)
}

pub fn set_module_default_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:commonjscreatevalue")
        .import_name("__setModuleDefault")
        .priority(1_usize)
        .text(r#"
            var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
                Object.defineProperty(o, "default", { enumerable: true, value: v });
            }) : function(o, v) {
                o["default"] = v;
            });"#)
        .build().unwrap().into(), arena)
}

pub fn import_star_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:commonjsimportstar")
        .import_name("__importStar")
        .priority(2_usize)
        .dependencies([create_binding_helper(arena), set_module_default_helper(arena)])
        .text(r#"
            var __importStar = (this && this.__importStar) || function (mod) {
                if (mod && mod.__esModule) return mod;
                var result = {};
                if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
                __setModuleDefault(result, mod);
                return result;
            };"#)
        .build().unwrap().into(), arena)
}

pub fn import_default_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:commonjsimportdefault")
        .import_name("__importDefault")
        .text(
            r#"
            var __importDefault = (this && this.__importDefault) || function (mod) {
                return (mod && mod.__esModule) ? mod : { "default": mod };
            };"#,
        )
        .build()
        .unwrap()
        .into(), arena)
}

pub fn export_star_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:export-star")
        .import_name("__exportStar")
        .priority(2_usize)
        .dependencies([create_binding_helper(arena)])
        .text(r#"
            var __exportStar = (this && this.__exportStar) || function(m, exports) {
                for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
            };"#)
        .build().unwrap().into(), arena)
}

pub fn class_private_field_get_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:classPrivateFieldGet")
        .import_name("__classPrivateFieldGet")
        .text(r#"
            var __classPrivateFieldGet = (this && this.__classPrivateFieldGet) || function (receiver, state, kind, f) {
                if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a getter");
                if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot read private member from an object whose class did not declare it");
                return kind === "m" ? f : kind === "a" ? f.call(receiver) : f ? f.value : state.get(receiver);
            };"#)
        .build().unwrap().into(), arena)
}

pub fn class_private_field_set_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:classPrivateFieldSet")
        .import_name("__classPrivateFieldSet")
        .text(r#"
            var __classPrivateFieldSet = (this && this.__classPrivateFieldSet) || function (receiver, state, value, kind, f) {
                if (kind === "m") throw new TypeError("Private method is not writable");
                if (kind === "a" && !f) throw new TypeError("Private accessor was defined without a setter");
                if (typeof state === "function" ? receiver !== state || !f : !state.has(receiver)) throw new TypeError("Cannot write private member to an object whose class did not declare it");
                return (kind === "a" ? f.call(receiver, value) : f ? f.value = value : state.set(receiver, value)), value;
            };"#)
        .build().unwrap().into(), arena)
}

pub fn class_private_field_in_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(UnscopedEmitHelperBuilder::default()
        .name("typescript:classPrivateFieldIn")
        .import_name("__classPrivateFieldIn")
        .text(r#"
            var __classPrivateFieldIn = (this && this.__classPrivateFieldIn) || function(state, receiver) {
                if (receiver === null || (typeof receiver !== "object" && typeof receiver !== "function")) throw new TypeError("Cannot use 'in' operator on non-object");
                return typeof state === "function" ? receiver === state : state.has(receiver);
            };"#)
        .build().unwrap().into(), arena)
}

pub fn async_super_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(ScopedEmitHelperBuilder::default()
        .name("typescript:async-super")
        .text(helper_string(
            &[
                r#"
            const "#,
                r#" = name => super[name];"#,
            ],
            &["_superIndex"],
        ))
        .build()
        .unwrap()
        .into(), arena)
}

pub fn advanced_async_super_helper(arena: &impl HasArena) -> Id<EmitHelper> {
    lazy_emit_helper!(ScopedEmitHelperBuilder::default()
        .name("typescript:advanced-async-super")
        .text(helper_string(
            &[
                r#"
                    const "#,
                r#" = (function (geti, seti) {
                        const cache = Object.create(null);
                        return name => cache[name] || (cache[name] = { get value() { return geti(name); }, set value(v) { seti(name, v); } });
                    })(name => super[name], (name, value) => super[name] = value);"#,
            ],
            &["_superIndex"],
        ))
        .build()
        .unwrap()
        .into(), arena)
}

pub fn is_call_to_helper(
    first_segment: Id<Node>, /*Expression*/
    helper_name: &str,       /*__String*/
    arena: &impl HasArena,
) -> bool {
    is_call_expression(&first_segment.ref_(arena)) && {
        let first_segment_ref = first_segment.ref_(arena);
        let first_segment_as_call_expression = first_segment_ref.as_call_expression();
        is_identifier(&first_segment_as_call_expression.expression.ref_(arena))
            && get_emit_flags(&first_segment_as_call_expression.expression.ref_(arena))
                .intersects(EmitFlags::HelperName)
            && first_segment_as_call_expression
                .expression
                .ref_(arena).as_identifier()
                .escaped_text
                == helper_name
    }
}
