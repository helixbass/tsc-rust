use std::{cell::Cell, collections::HashMap, io, mem};

use bitflags::bitflags;
use gc::{Finalize, Gc, GcCell, Trace};

use crate::{
    chain_bundle, get_emit_script_target, get_use_define_for_class_fields,
    BaseNodeFactorySynthetic, CompilerOptions, EmitHint, EmitResolver, Node, NodeFactory, NodeId,
    ScriptTarget, TransformationContext, TransformationContextOnEmitNodeOverrider,
    TransformationContextOnSubstituteNodeOverrider, Transformer, TransformerFactory,
    TransformerFactoryInterface, TransformerInterface, UnderscoreEscapedMap,
};

bitflags! {
    pub(super) struct ClassPropertySubstitutionFlags: u32 {
        const None = 0;
        const ClassAliases = 1 << 0;
        const ClassStaticThisOrSuperReference = 1 << 1;
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum PrivateIdentifierKind {
    Field,    /*= "f"*/
    Method,   /*= "m"*/
    Accessor, /*= "a"*/
}

pub(super) trait PrivateIdentifierInfoInterface {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/>;
    fn is_static(&self) -> bool;
    fn is_valid(&self) -> bool;
    fn kind(&self) -> PrivateIdentifierKind;
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierAccessorInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Accessor*/
    pub getter_name: Option<Gc<Node /*Identifier*/>>,
    pub setter_name: Option<Gc<Node /*Identifier*/>>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierAccessorInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierMethodInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Method*/
    pub method_name: Gc<Node /*Identifier*/>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierMethodInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierInstanceFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInstanceFieldInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierStaticFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
    #[unsafe_ignore_trace]
    kind: PrivateIdentifierKind, /*PrivateIdentifierKind.Field*/
    pub variable_name: Gc<Node /*Identifier*/>,
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierStaticFieldInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
        self.brand_check_identifier.clone()
    }

    fn is_static(&self) -> bool {
        self.is_static
    }

    fn is_valid(&self) -> bool {
        self.is_valid
    }

    fn kind(&self) -> PrivateIdentifierKind {
        self.kind
    }
}

#[derive(Trace, Finalize)]
pub(super) enum PrivateIdentifierInfo {
    PrivateIdentifierMethodInfo(PrivateIdentifierMethodInfo),
    PrivateIdentifierInstanceFieldInfo(PrivateIdentifierInstanceFieldInfo),
    PrivateIdentifierStaticFieldInfo(PrivateIdentifierStaticFieldInfo),
    PrivateIdentifierAccessorInfo(PrivateIdentifierAccessorInfo),
}

impl PrivateIdentifierInfoInterface for PrivateIdentifierInfo {
    fn brand_check_identifier(&self) -> Gc<Node /*Identifier*/> {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.brand_check_identifier(),
            Self::PrivateIdentifierAccessorInfo(value) => value.brand_check_identifier(),
        }
    }

    fn is_static(&self) -> bool {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.is_static(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.is_static(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.is_static(),
            Self::PrivateIdentifierAccessorInfo(value) => value.is_static(),
        }
    }

    fn is_valid(&self) -> bool {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.is_valid(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.is_valid(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.is_valid(),
            Self::PrivateIdentifierAccessorInfo(value) => value.is_valid(),
        }
    }

    fn kind(&self) -> PrivateIdentifierKind {
        match self {
            Self::PrivateIdentifierMethodInfo(value) => value.kind(),
            Self::PrivateIdentifierInstanceFieldInfo(value) => value.kind(),
            Self::PrivateIdentifierStaticFieldInfo(value) => value.kind(),
            Self::PrivateIdentifierAccessorInfo(value) => value.kind(),
        }
    }
}

#[derive(Trace, Finalize)]
pub(super) struct PrivateIdentifierEnvironment {
    pub class_name: String,
    pub weak_set_name: Option<Gc<Node /*Identifier*/>>,
    pub identifiers: UnderscoreEscapedMap<PrivateIdentifierInfo>,
}

#[derive(Trace, Finalize)]
pub(super) struct ClassLexicalEnvironment {
    #[unsafe_ignore_trace]
    pub facts: ClassFacts,
    pub class_constructor: Option<Gc<Node /*Identifier*/>>,
    pub super_class_reference: Option<Gc<Node /*Identifier*/>>,
    pub private_identifier_environment: Option<PrivateIdentifierEnvironment>,
}

bitflags! {
    pub(super) struct ClassFacts: u32 {
        const None = 0;
        const ClassWasDecorated = 1 << 0;
        const NeedsClassConstructorReference = 1 << 1;
        const NeedsClassSuperReference = 1 << 2;
        const NeedsSubstitutionForThisInClassStaticField = 1 << 3;
    }
}

#[derive(Trace, Finalize)]
struct TransformClassFields {
    pub(super) _transformer_wrapper: GcCell<Option<Transformer>>,
    pub(super) context: Gc<Box<dyn TransformationContext>>,
    pub(super) factory: Gc<NodeFactory<BaseNodeFactorySynthetic>>,
    pub(super) resolver: Gc<Box<dyn EmitResolver>>,
    pub(super) compiler_options: Gc<CompilerOptions>,
    #[unsafe_ignore_trace]
    pub(super) language_version: ScriptTarget,
    pub(super) use_define_for_class_fields: bool,
    pub(super) should_transform_private_elements_or_class_static_blocks: bool,
    pub(super) should_transform_super_in_static_initializers: bool,
    pub(super) should_transform_this_in_static_initializers: bool,
    #[unsafe_ignore_trace]
    pub(super) enabled_substitutions: Cell<Option<ClassPropertySubstitutionFlags>>,
    pub(super) class_aliases: GcCell<Option<Vec<Gc<Node /*Identifier*/>>>>,
    pub(super) pending_expressions: GcCell<Option<Vec<Gc<Node /*Expression*/>>>>,
    pub(super) pending_statements: GcCell<Option<Vec<Gc<Node /*Statement*/>>>>,
    pub(super) class_lexical_environment_stack: GcCell<Vec<Option<Gc<ClassLexicalEnvironment>>>>,
    pub(super) class_lexical_environment_map: GcCell<HashMap<NodeId, Gc<ClassLexicalEnvironment>>>,
    pub(super) current_class_lexical_environment: GcCell<Option<Gc<ClassLexicalEnvironment>>>,
    pub(super) current_computed_property_name_class_lexical_environment:
        GcCell<Option<Gc<ClassLexicalEnvironment>>>,
    pub(super) current_static_property_declaration_or_static_block:
        GcCell<Option<Gc<Node /*PropertyDeclaration | ClassStaticBlockDeclaration*/>>>,
}

impl TransformClassFields {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Gc<Box<Self>> {
        let compiler_options = context.get_compiler_options();
        let language_version = get_emit_script_target(&compiler_options);
        let use_define_for_class_fields = get_use_define_for_class_fields(&compiler_options);
        let transformer_wrapper: Transformer = Gc::new(Box::new(Self {
            _transformer_wrapper: Default::default(),
            factory: context.factory(),
            resolver: context.get_emit_resolver(),
            context: context.clone(),
            use_define_for_class_fields,
            compiler_options,
            should_transform_private_elements_or_class_static_blocks: language_version
                < ScriptTarget::ESNext,
            should_transform_super_in_static_initializers: (language_version
                <= ScriptTarget::ES2021
                || !use_define_for_class_fields)
                && language_version >= ScriptTarget::ES2015,
            should_transform_this_in_static_initializers: language_version <= ScriptTarget::ES2021
                || !use_define_for_class_fields,
            language_version,
            enabled_substitutions: Default::default(),
            class_aliases: Default::default(),
            pending_expressions: Default::default(),
            pending_statements: Default::default(),
            class_lexical_environment_stack: Default::default(),
            class_lexical_environment_map: Default::default(),
            current_class_lexical_environment: Default::default(),
            current_computed_property_name_class_lexical_environment: Default::default(),
            current_static_property_declaration_or_static_block: Default::default(),
        }));
        let downcasted: Gc<Box<Self>> = unsafe { mem::transmute(transformer_wrapper.clone()) };
        *downcasted._transformer_wrapper.borrow_mut() = Some(transformer_wrapper);
        context.override_on_emit_node(&mut |previous_on_emit_node| {
            Gc::new(Box::new(TransformClassFieldsOnEmitNodeOverrider::new(
                downcasted.clone(),
                previous_on_emit_node,
            )))
        });
        context.override_on_substitute_node(&mut |previous_on_substitute_node| {
            Gc::new(Box::new(
                TransformClassFieldsOnSubstituteNodeOverrider::new(
                    downcasted.clone(),
                    previous_on_substitute_node,
                ),
            ))
        });
        downcasted
    }

    pub(super) fn as_transformer(&self) -> Transformer {
        self._transformer_wrapper.borrow().clone().unwrap()
    }
}

impl TransformerInterface for TransformClassFields {
    fn call(&self, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformClassFieldsOnEmitNodeOverrider {
    transform_class_fields: Gc<Box<TransformClassFields>>,
    previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
}

impl TransformClassFieldsOnEmitNodeOverrider {
    fn new(
        transform_class_fields: Gc<Box<TransformClassFields>>,
        previous_on_emit_node: Gc<Box<dyn TransformationContextOnEmitNodeOverrider>>,
    ) -> Self {
        Self {
            transform_class_fields,
            previous_on_emit_node,
        }
    }
}

impl TransformationContextOnEmitNodeOverrider for TransformClassFieldsOnEmitNodeOverrider {
    fn on_emit_node(
        &self,
        _hint: EmitHint,
        _node: &Node,
        _emit_callback: &dyn Fn(EmitHint, &Node) -> io::Result<()>,
    ) -> io::Result<()> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformClassFieldsOnSubstituteNodeOverrider {
    transform_class_fields: Gc<Box<TransformClassFields>>,
    previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
}

impl TransformClassFieldsOnSubstituteNodeOverrider {
    pub(super) fn new(
        transform_class_fields: Gc<Box<TransformClassFields>>,
        previous_on_substitute_node: Gc<Box<dyn TransformationContextOnSubstituteNodeOverrider>>,
    ) -> Self {
        Self {
            transform_class_fields,
            previous_on_substitute_node,
        }
    }
}

impl TransformationContextOnSubstituteNodeOverrider
    for TransformClassFieldsOnSubstituteNodeOverrider
{
    fn on_substitute_node(&self, _hint: EmitHint, _node: &Node) -> io::Result<Gc<Node>> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformClassFieldsFactory {}

impl TransformClassFieldsFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformClassFieldsFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        chain_bundle().call(
            context.clone(),
            TransformClassFields::new(context).as_transformer(),
        )
    }
}

pub fn transform_class_fields() -> TransformerFactory {
    Gc::new(Box::new(TransformClassFieldsFactory::new()))
}
