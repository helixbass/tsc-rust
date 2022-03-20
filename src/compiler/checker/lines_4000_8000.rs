#![allow(non_upper_case_globals)]

use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    get_emit_script_target, is_expression, is_identifier_text, unescape_leading_underscores,
    using_single_line_string_writer, BaseIntrinsicType, BaseNodeFactorySynthetic, BaseObjectType,
    BaseType, CharacterCodes, Debug_, EmitHint, EmitTextWriter, IndexInfo, KeywordTypeNode, Node,
    NodeArray, NodeBuilderFlags, NodeInterface, ObjectFlags, PrinterOptions,
    ResolvableTypeInterface, ResolvedTypeInterface, Signature, SignatureKind, SourceFile, Symbol,
    SymbolFlags, SymbolFormatFlags, SymbolInterface, SymbolTable, SymbolTracker, SyntaxKind, Type,
    TypeChecker, TypeFlags, TypeFormatFlags, TypeInterface, TypeParameter, TypePredicate, __String,
    create_printer, create_text_writer, factory, get_object_flags, get_source_file_of_node,
    synthetic_factory,
};

impl TypeChecker {
    pub(super) fn get_export_symbol_of_value_symbol_if_exported<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbolRef>,
    ) -> Option<Rc<Symbol>> {
        self.get_merged_symbol(if let Some(symbol) = symbol {
            let symbol = symbol.borrow();
            if symbol.flags().intersects(SymbolFlags::ExportValue) {
                unimplemented!()
            } else {
                Some(symbol.symbol_wrapper())
            }
        } else {
            None
        })
    }

    pub(super) fn symbol_is_value(&self, symbol: &Symbol) -> bool {
        symbol.flags().intersects(SymbolFlags::Value)
    }

    pub(super) fn create_type(&self, flags: TypeFlags) -> BaseType {
        let mut result = (self.Type)(flags);
        self.increment_type_count();
        result.id = Some(self.type_count());
        result
    }

    pub(super) fn create_intrinsic_type(
        &self,
        kind: TypeFlags,
        intrinsic_name: &str,
        object_flags: Option<ObjectFlags>,
    ) -> BaseIntrinsicType {
        let object_flags = object_flags.unwrap_or(ObjectFlags::None);
        let type_ = self.create_type(kind);
        let type_ = BaseIntrinsicType::new(type_, intrinsic_name.to_string(), object_flags);
        type_
    }

    pub(super) fn create_object_type<TSymbol: Borrow<Symbol>>(
        &self,
        object_flags: ObjectFlags,
        symbol: Option<TSymbol>,
    ) -> BaseObjectType {
        let mut type_ = self.create_type(TypeFlags::Object);
        type_.set_symbol(symbol.map(|symbol| symbol.borrow().symbol_wrapper()));
        let type_ = BaseObjectType::new(type_, object_flags);
        type_
    }

    pub(super) fn create_typeof_type(&self) -> Rc<Type> {
        self.get_union_type(vec![], None)
    }

    pub(super) fn create_type_parameter<TSymbolRef: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbolRef>,
    ) -> TypeParameter {
        let mut type_ = self.create_type(TypeFlags::TypeParameter);
        if let Some(symbol) = symbol {
            let symbol = symbol.borrow();
            type_.set_symbol(Some(symbol.symbol_wrapper()));
        }
        let type_ = TypeParameter::new(type_);
        type_
    }

    pub(super) fn is_reserved_member_name(&self, name: &__String) -> bool {
        let mut chars = name.chars();
        let mut current_char: Option<char> = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char != CharacterCodes::underscore {
                return false;
            }
        } else {
            return false;
        }
        current_char = chars.next();
        if let Some(current_char) = current_char {
            if current_char == CharacterCodes::underscore
                || current_char == CharacterCodes::at
                || current_char == CharacterCodes::hash
            {
                return false;
            }
        } else {
            return false;
        }
        true
    }

    pub(super) fn get_named_members(&self, members: &SymbolTable) -> Vec<Rc<Symbol>> {
        members
            .iter()
            .filter(|(id, symbol)| self.is_named_member(&symbol, id))
            .map(|(_, symbol)| symbol.clone())
            .collect()
    }

    pub(super) fn is_named_member(&self, member: &Symbol, escaped_name: &__String) -> bool {
        !self.is_reserved_member_name(escaped_name) && self.symbol_is_value(member)
    }

    pub(super) fn set_structured_type_members<
        TType: ResolvableTypeInterface + ResolvedTypeInterface,
    >(
        &self,
        type_: &TType,
        members: Rc<RefCell<SymbolTable>>,
        call_signatures: Vec<Rc<Signature>>,
        construct_signatures: Vec<Rc<Signature>>,
    ) /*-> BaseObjectType*/
    {
        type_.resolve(
            members.clone(),
            vec![],
            call_signatures,
            construct_signatures,
        );
        if true {
            type_.set_properties(self.get_named_members(&*(*members).borrow()));
        }
        // type_
    }

    pub(super) fn create_anonymous_type<TSymbol: Borrow<Symbol>>(
        &self,
        symbol: Option<TSymbol>,
        members: Rc<RefCell<SymbolTable>>,
        call_signatures: Vec<Rc<Signature>>,
        construct_signatures: Vec<Rc<Signature>>,
        index_infos: Vec<Rc<IndexInfo>>,
    ) -> BaseObjectType {
        let type_ = self.create_object_type(ObjectFlags::Anonymous, symbol);
        // TODO: pass index_infos to set_structured_type_members()
        self.set_structured_type_members(&type_, members, call_signatures, construct_signatures);
        type_
    }

    pub(super) fn symbol_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        meaning: Option<SymbolFlags>,
        flags: Option<SymbolFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(SymbolFormatFlags::AllowAnyNodeKind);
        let mut node_flags = NodeBuilderFlags::IgnoreErrors;
        if flags.intersects(SymbolFormatFlags::UseOnlyExternalAliasing) {
            node_flags |= NodeBuilderFlags::UseOnlyExternalAliasing;
        }
        if flags.intersects(SymbolFormatFlags::WriteTypeParametersOrArguments) {
            node_flags |= NodeBuilderFlags::WriteTypeParametersInQualifiedName;
        }
        if flags.intersects(SymbolFormatFlags::UseAliasDefinedOutsideCurrentScope) {
            node_flags |= NodeBuilderFlags::UseAliasDefinedOutsideCurrentScope;
        }
        if flags.intersects(SymbolFormatFlags::DoNotIncludeSymbolChain) {
            node_flags |= NodeBuilderFlags::DoNotIncludeSymbolChain;
        }
        let builder = if flags.intersects(SymbolFormatFlags::AllowAnyNodeKind) {
            NodeBuilder::symbol_to_expression
        } else {
            unimplemented!()
        };
        let symbol_to_string_worker = |writer: Rc<RefCell<dyn EmitTextWriter>>| {
            let entity = builder(
                &self.node_builder,
                self,
                symbol,
                // meaning.unwrap() TODO: this is ! in the Typescript code but would be undefined at runtime when called from propertyRelatedTo()?
                meaning,
                Option::<&Node>::None, // TODO: this is wrong
                Some(node_flags),
                None,
            )
            .unwrap();
            let entity: Rc<Node> = entity.into();
            let mut printer = if false {
                unimplemented!()
            } else {
                create_printer(PrinterOptions {/*remove_comments: true*/})
            };
            let source_file = if let Some(enclosing_declaration) = enclosing_declaration {
                Some(get_source_file_of_node(Some(enclosing_declaration.borrow())).unwrap())
            } else {
                None
            };
            printer.write_node(EmitHint::Unspecified, &*entity, source_file, writer);
            // writer
        };
        if let Some(writer) = writer {
            unimplemented!()
        } else {
            using_single_line_string_writer(symbol_to_string_worker)
        }
    }

    pub(super) fn signature_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        kind: Option<SignatureKind>,
        writer: Option<&dyn EmitTextWriter>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        unimplemented!()
    }

    pub(super) fn type_to_string_<TNodeRef: Borrow<Node>>(
        &self,
        type_: &Type,
        enclosing_declaration: Option<TNodeRef>,
        flags: Option<TypeFormatFlags>,
        writer: Option<Rc<RefCell<dyn EmitTextWriter>>>,
    ) -> String {
        let flags = flags.unwrap_or(
            TypeFormatFlags::AllowUniqueESSymbolType
                | TypeFormatFlags::UseAliasDefinedOutsideCurrentScope,
        );
        let writer = writer.unwrap_or_else(|| Rc::new(RefCell::new(create_text_writer(""))));
        let no_truncation = false || flags.intersects(TypeFormatFlags::NoTruncation);
        let type_node = self.node_builder.type_to_type_node(
            self,
            type_,
            Option::<&Node>::None, // TODO: this is wrong
            Some(
                self.to_node_builder_flags(Some(flags))
                    | NodeBuilderFlags::IgnoreErrors
                    | if no_truncation {
                        NodeBuilderFlags::NoTruncation
                    } else {
                        NodeBuilderFlags::None
                    },
            ),
            Some((*writer).borrow().as_symbol_tracker()),
        );
        let type_node: Rc<Node> = match type_node {
            None => Debug_.fail(Some("should always get typenode")),
            Some(type_node) => type_node.wrap(),
        };
        let options = PrinterOptions {};
        let mut printer = create_printer(options);
        let source_file: Option<Rc<Node /*SourceFile*/>> =
            if let Some(enclosing_declaration) = enclosing_declaration {
                let enclosing_declaration = enclosing_declaration.borrow();
                Some(get_source_file_of_node(Some(enclosing_declaration)).unwrap())
            } else {
                None
            };
        printer.write_node(
            EmitHint::Unspecified,
            &*type_node,
            source_file,
            writer.clone(),
        );
        let result = (*writer).borrow().get_text();

        result
    }

    pub(super) fn get_type_names_for_error_display(
        &self,
        left: &Type,
        right: &Type,
    ) -> (String, String) {
        let left_str = if let Some(symbol) = left.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(&symbol) {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(left, enclosing_declaration, None, None)
            } else {
                self.type_to_string_(left, Option::<&Node>::None, None, None)
            }
        } else {
            self.type_to_string_(left, Option::<&Node>::None, None, None)
        };
        let right_str = if let Some(symbol) = right.maybe_symbol() {
            if self.symbol_value_declaration_is_context_sensitive(&symbol) {
                let enclosing_declaration = (*symbol.maybe_value_declaration().borrow()).clone();
                self.type_to_string_(right, enclosing_declaration, None, None)
            } else {
                self.type_to_string_(right, Option::<&Node>::None, None, None)
            }
        } else {
            self.type_to_string_(right, Option::<&Node>::None, None, None)
        };
        (left_str, right_str)
    }

    pub(super) fn get_type_name_for_error_display(&self, type_: &Type) -> String {
        self.type_to_string_(
            type_,
            Option::<&Node>::None,
            Some(TypeFormatFlags::UseFullyQualifiedType),
            None,
        )
    }

    pub(super) fn symbol_value_declaration_is_context_sensitive(&self, symbol: &Symbol) -> bool {
        match symbol.maybe_value_declaration() {
            Some(value_declaration) => {
                is_expression(&value_declaration) && !self.is_context_sensitive(&value_declaration)
            }
            None => false,
        }
    }

    pub(super) fn to_node_builder_flags(&self, flags: Option<TypeFormatFlags>) -> NodeBuilderFlags {
        let flags = flags.unwrap_or(TypeFormatFlags::None);
        NodeBuilderFlags::from_bits((flags & TypeFormatFlags::NodeBuilderFlagsMask).bits()).unwrap()
    }

    pub fn type_predicate_to_string_<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_predicate: &TypePredicate,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<TypeFormatFlags>,
        writer: Option<&dyn EmitTextWriter>,
    ) -> String {
        let flags = flags.unwrap_or(TypeFormatFlags::UseAliasDefinedOutsideCurrentScope);
        unimplemented!()
    }
}

pub(super) fn create_node_builder() -> NodeBuilder {
    NodeBuilder::new()
}

#[derive(Debug)]
pub struct NodeBuilder {}

impl NodeBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn type_to_type_node<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        type_: &Type,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        self.with_context(flags, tracker, |context| {
            self.type_to_type_node_helper(type_checker, type_, context)
        })
    }

    pub fn index_info_to_index_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        index_info: &IndexInfo,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        unimplemented!()
    }

    pub fn signature_to_signature_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        signature: &Signature,
        kind: SyntaxKind,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*SignatureDeclaration & {typeArguments?: NodeArray<TypeNode>}*/> {
        unimplemented!()
    }

    pub fn symbol_to_entity_name<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        meaning: SymbolFlags,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*EntityName*/> {
        unimplemented!()
    }

    pub fn symbol_to_expression<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node> {
        self.with_context(flags, tracker, |context| {
            self._symbol_to_expression(type_checker, symbol, context, meaning)
        })
    }

    pub fn symbol_to_type_parameter_declarations<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<NodeArray /*<TypeParameterDeclaration>*/> {
        unimplemented!()
    }

    pub fn symbol_to_parameter_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        symbol: &Symbol,
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*ParameterDeclaration*/> {
        unimplemented!()
    }

    pub fn type_parameter_to_declaration<TEnclosingDeclaration: Borrow<Node>>(
        &self,
        parameter: &Node, /*TypeParameter*/
        enclosing_declaration: Option<TEnclosingDeclaration>,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
    ) -> Option<Node /*TypeParameterDeclaration*/> {
        unimplemented!()
    }

    fn with_context<TReturn, TCallback: FnOnce(&NodeBuilderContext) -> TReturn>(
        &self,
        flags: Option<NodeBuilderFlags>,
        tracker: Option<&dyn SymbolTracker>,
        cb: TCallback,
    ) -> Option<TReturn> {
        let default_tracker: Option<DefaultNodeBuilderContextSymbolTracker> = match tracker {
            Some(_) => None,
            None => Some(DefaultNodeBuilderContextSymbolTracker::new()),
        };
        let context = NodeBuilderContext::new(
            flags.unwrap_or(NodeBuilderFlags::None),
            tracker.unwrap_or_else(|| default_tracker.as_ref().unwrap()),
        );
        let resulting_node = cb(&context);
        Some(resulting_node)
    }

    pub fn type_to_type_node_helper(
        &self,
        type_checker: &TypeChecker,
        type_: &Type,
        context: &NodeBuilderContext,
    ) -> Node {
        if type_.flags().intersects(TypeFlags::String) {
            return Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_keyword_type_node(synthetic_factory_, SyntaxKind::StringKeyword)
                })
            }))
            .into();
        }
        if type_.flags().intersects(TypeFlags::Number) {
            return Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_keyword_type_node(synthetic_factory_, SyntaxKind::NumberKeyword)
                })
            }))
            .into();
        }
        if type_.flags().intersects(TypeFlags::BigInt) {
            return Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_keyword_type_node(synthetic_factory_, SyntaxKind::BigIntKeyword)
                })
            }))
            .into();
        }
        if type_.flags().intersects(TypeFlags::Boolean) {
            return Into::<KeywordTypeNode>::into(synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_
                        .create_keyword_type_node(synthetic_factory_, SyntaxKind::BooleanKeyword)
                })
            }))
            .into();
        }
        if type_.flags().intersects(TypeFlags::StringLiteral) {
            return synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_literal_type_node(
                            synthetic_factory_,
                            factory_
                                .create_string_literal(
                                    synthetic_factory_,
                                    type_.as_string_literal_type().value.clone(),
                                    Some(context.flags.intersects(
                                        NodeBuilderFlags::UseSingleQuotesForStringLiteralType,
                                    )),
                                    None,
                                )
                                .into(),
                        )
                    })
                })
                .into();
        }
        if type_.flags().intersects(TypeFlags::NumberLiteral) {
            let value = type_.as_number_literal_type().value.value();
            return synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_literal_type_node(
                            synthetic_factory_,
                            if value < 0.0 {
                                factory_
                                    .create_prefix_unary_expression(
                                        synthetic_factory_,
                                        SyntaxKind::MinusToken,
                                        factory_
                                            .create_numeric_literal(
                                                synthetic_factory_,
                                                (-value).to_string(),
                                                None,
                                            )
                                            .into(),
                                    )
                                    .into()
                            } else {
                                factory_
                                    .create_numeric_literal(
                                        synthetic_factory_,
                                        value.to_string(),
                                        None,
                                    )
                                    .into()
                            },
                        )
                    })
                })
                .into();
        }
        if type_.flags().intersects(TypeFlags::BigIntLiteral) {
            return synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_literal_type_node(
                            synthetic_factory_,
                            factory_
                                .create_big_int_literal(
                                    synthetic_factory_,
                                    type_.as_big_int_literal_type().value.clone(),
                                )
                                .into(),
                        )
                    })
                })
                .into();
        }
        if type_.flags().intersects(TypeFlags::BooleanLiteral) {
            return synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_literal_type_node(
                            synthetic_factory_,
                            if type_.as_intrinsic_type().intrinsic_name() == "true" {
                                factory_.create_true(synthetic_factory_)
                            } else {
                                factory_.create_false(synthetic_factory_)
                            }
                            .into(),
                        )
                    })
                })
                .into();
        }

        let object_flags = get_object_flags(&*type_);

        if type_.flags().intersects(TypeFlags::TypeParameter)
            || object_flags.intersects(ObjectFlags::ClassOrInterface)
        {
            return if let Some(symbol) = type_.maybe_symbol() {
                self.symbol_to_type_node(type_checker, &symbol, context, SymbolFlags::Type)
            } else {
                unimplemented!()
            };
        }
        if type_
            .flags()
            .intersects(TypeFlags::Union | TypeFlags::Intersection)
        {
            let types = {
                let types = type_.as_union_or_intersection_type_interface().types();
                if type_.flags().intersects(TypeFlags::Union) {
                    type_checker.format_union_types(types)
                } else {
                    types.to_vec()
                }
            };
            if types.len() == 1 {
                return self.type_to_type_node_helper(type_checker, &types[0], context);
            }
            let type_nodes =
                self.map_to_type_nodes(type_checker, Some(&types), context, Some(true));
            if let Some(type_nodes) = type_nodes {
                if !type_nodes.is_empty() {
                    return if type_.flags().intersects(TypeFlags::Union) {
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_.create_union_type_node(synthetic_factory_, type_nodes)
                            })
                        })
                    } else {
                        synthetic_factory.with(|synthetic_factory_| {
                            factory.with(|factory_| {
                                factory_
                                    .create_intersection_type_node(synthetic_factory_, type_nodes)
                            })
                        })
                    };
                }
            }
            unimplemented!()
        }
        if object_flags.intersects(ObjectFlags::Anonymous | ObjectFlags::Mapped) {
            Debug_.assert(type_.flags().intersects(TypeFlags::Object), None);
            return self.create_anonymous_type_node(type_checker, context, type_);
        }

        unimplemented!()
    }

    fn create_anonymous_type_node(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Node {
        let symbol = type_.maybe_symbol();
        if let Some(symbol) = symbol {
            if false {
                unimplemented!()
            } else {
                return self.visit_and_transform_type(
                    type_checker,
                    context,
                    type_,
                    NodeBuilder::create_type_node_from_object_type,
                );
            }
        } else {
            unimplemented!()
        }
    }

    fn visit_and_transform_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type,
        transform: fn(&NodeBuilder, &TypeChecker, &NodeBuilderContext, &Type) -> Node,
    ) -> Node {
        let result = transform(self, type_checker, context, type_);
        result
    }

    fn create_type_node_from_object_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type, /*ObjectType*/
    ) -> Node {
        let resolved = type_checker.resolve_structured_type_members(type_);

        let members = self.create_type_nodes_from_resolved_type(type_checker, context, &resolved);
        let type_literal_node = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| factory_.create_type_literal_node(synthetic_factory_, members))
        });
        type_literal_node.into()
    }

    fn create_type_nodes_from_resolved_type(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        resolved_type: &Type, /*ResolvedType*/
    ) -> Option<Vec<Rc<Node /*TypeElement*/>>> {
        let mut type_elements: Vec<Rc<Node>> = vec![];

        let properties = resolved_type.as_resolved_type().properties();

        for property_symbol in &*properties {
            self.add_property_to_element_list(
                type_checker,
                &property_symbol,
                context,
                &mut type_elements,
            );
        }
        if !type_elements.is_empty() {
            Some(type_elements)
        } else {
            None
        }
    }

    fn add_property_to_element_list(
        &self,
        type_checker: &TypeChecker,
        property_symbol: &Symbol,
        context: &NodeBuilderContext,
        type_elements: &mut Vec<Rc<Node /*TypeElement*/>>,
    ) {
        let property_type = if false {
            unimplemented!()
        } else {
            type_checker.get_non_missing_type_of_symbol(property_symbol)
        };
        let property_name =
            self.get_property_name_node_for_symbol(type_checker, property_symbol, context);
        let optional_token = if property_symbol.flags().intersects(SymbolFlags::Optional) {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    Some(factory_.create_token(synthetic_factory_, SyntaxKind::QuestionToken))
                })
            })
        } else {
            None
        };
        if false {
            unimplemented!()
        } else {
            let property_type_node: Node;
            if false {
                unimplemented!()
            } else {
                property_type_node = if true {
                    self.serialize_type_for_declaration(
                        type_checker,
                        context,
                        &property_type,
                        property_symbol,
                    )
                } else {
                    unimplemented!()
                };
            }

            let modifiers = if false {
                unimplemented!()
            } else {
                Option::<NodeArray>::None
            };
            let property_signature = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_property_signature(
                        synthetic_factory_,
                        modifiers,
                        property_name,
                        optional_token.map(Into::into),
                        Some(property_type_node.wrap()),
                    )
                })
            });

            type_elements.push(property_signature.into());
        }
    }

    fn map_to_type_nodes(
        &self,
        type_checker: &TypeChecker,
        types: Option<&[Rc<Type>]>,
        context: &NodeBuilderContext,
        is_bare_list: Option<bool>,
    ) -> Option<Vec<Rc<Node /*TypeNode*/>>> {
        if let Some(types) = types {
            if !types.is_empty()
            /*some(types)*/
            {
                let may_have_name_collisions = !context
                    .flags
                    .intersects(NodeBuilderFlags::UseFullyQualifiedType);
                let mut result: Vec<Rc<Node>> = vec![];
                for (i, type_) in types.iter().enumerate() {
                    let type_node = self.type_to_type_node_helper(type_checker, type_, context);
                    result.push(type_node.into());
                }

                return Some(result);
            }
        }
        None
    }

    fn lookup_symbol_chain(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        self.lookup_symbol_chain_worker(symbol, context, meaning)
    }

    fn lookup_symbol_chain_worker(
        &self,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Vec<Rc<Symbol>> {
        let chain: Vec<Rc<Symbol>>;
        if false {
            unimplemented!()
        } else {
            chain = vec![symbol.symbol_wrapper()];
        }
        chain
    }

    fn symbol_to_type_node(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: SymbolFlags,
    ) -> Node {
        let chain = self.lookup_symbol_chain(symbol, context, Some(meaning));

        let chain_index = chain.len() - 1;
        let entity_name =
            self.create_access_from_symbol_chain(type_checker, context, chain, chain_index, 0);
        if false {
            unimplemented!()
        } else {
            // let last_id = if is_identifier(entity_name) {
            //     entity_name
            // } else {
            //     unimplemented!()
            // };
            let last_type_args: Option<NodeArray> = None;
            synthetic_factory
                .with(|synthetic_factory_| {
                    factory.with(|factory_| {
                        factory_.create_type_reference_node(
                            synthetic_factory_,
                            entity_name,
                            last_type_args,
                        )
                    })
                })
                .into()
        }
    }

    fn create_access_from_symbol_chain(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
        stopper: usize,
    ) -> Rc<Node> {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = chain[index].clone();

        let mut symbol_name: Option<Cow<'static, str>>;
        if index == 0 {
            symbol_name = Some(type_checker.get_name_of_symbol_as_written(&symbol, Some(context)));
        } else {
            unimplemented!()
        }
        if symbol_name.is_none() {
            symbol_name = Some(type_checker.get_name_of_symbol_as_written(&symbol, Some(context)));
        }
        let symbol_name = symbol_name.unwrap();

        let identifier = synthetic_factory.with(|synthetic_factory_| {
            factory.with(|factory_| {
                factory_.create_identifier(
                    synthetic_factory_,
                    &symbol_name,
                    type_parameter_nodes,
                    None,
                )
            })
        });
        identifier.set_symbol(symbol);

        identifier.into()
    }

    fn _symbol_to_expression(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
        meaning: /*SymbolFlags*/ Option<SymbolFlags>,
    ) -> Node {
        let chain = self.lookup_symbol_chain(symbol, context, meaning);
        let index = chain.len() - 1;
        self.create_expression_from_symbol_chain(type_checker, context, chain, index)
    }

    fn get_property_name_node_for_symbol(
        &self,
        type_checker: &TypeChecker,
        symbol: &Symbol,
        context: &NodeBuilderContext,
    ) -> Rc<Node> {
        let single_quote = false;
        let string_named = false;
        let raw_name = unescape_leading_underscores(symbol.escaped_name());
        self.create_property_name_node_for_identifier_or_literal(
            type_checker,
            raw_name,
            Some(string_named),
            Some(single_quote),
        )
    }

    fn create_property_name_node_for_identifier_or_literal(
        &self,
        type_checker: &TypeChecker,
        name: String,
        string_named: Option<bool>,
        single_quote: Option<bool>,
    ) -> Rc<Node> {
        if is_identifier_text(
            &name,
            Some(get_emit_script_target(&type_checker.compiler_options)),
            None,
        ) {
            synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_identifier(
                        synthetic_factory_,
                        &name,
                        Option::<NodeArray>::None,
                        None,
                    )
                })
            })
        } else {
            unimplemented!()
        }
        .into()
    }

    fn create_expression_from_symbol_chain(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        chain: Vec<Rc<Symbol>>,
        index: usize,
    ) -> Node {
        let type_parameter_nodes = Option::<NodeArray>::None; // TODO: this is wrong
        let symbol = &*(&chain)[index];

        let symbol_name = type_checker.get_name_of_symbol_as_written(symbol, Some(context));

        if index == 0 || false {
            let identifier = synthetic_factory.with(|synthetic_factory_| {
                factory.with(|factory_| {
                    factory_.create_identifier(
                        synthetic_factory_,
                        &symbol_name,
                        type_parameter_nodes,
                        None,
                    )
                })
            });
            identifier.set_symbol(symbol.symbol_wrapper());
            return identifier.into();
        } else {
            unimplemented!()
        }
    }

    fn serialize_type_for_declaration(
        &self,
        type_checker: &TypeChecker,
        context: &NodeBuilderContext,
        type_: &Type,
        symbol: &Symbol,
    ) -> Node {
        let result = self.type_to_type_node_helper(type_checker, type_, context);
        result
    }
}

struct DefaultNodeBuilderContextSymbolTracker {}

impl DefaultNodeBuilderContextSymbolTracker {
    pub fn new() -> Self {
        Self {}
    }
}

impl SymbolTracker for DefaultNodeBuilderContextSymbolTracker {}

pub struct NodeBuilderContext<'symbol_tracker> {
    flags: NodeBuilderFlags,
    tracker: &'symbol_tracker dyn SymbolTracker,
}

impl<'symbol_tracker> NodeBuilderContext<'symbol_tracker> {
    pub fn new(flags: NodeBuilderFlags, tracker: &'symbol_tracker dyn SymbolTracker) -> Self {
        Self { flags, tracker }
    }
}
