use std::io;

use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface, UnderscoreEscapedMap,
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

pub(super) struct PrivateIdentifierAccessorInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
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

pub(super) struct PrivateIdentifierMethodInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
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

pub(super) struct PrivateIdentifierInstanceFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
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

pub(super) struct PrivateIdentifierStaticFieldInfo {
    brand_check_identifier: Gc<Node /*Identifier*/>,
    is_static: bool,
    is_valid: bool,
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

pub(super) struct PrivateIdentifierEnvironment {
    pub class_name: String,
    pub weak_set_name: Option<Gc<Node /*Identifier*/>>,
    pub identifiers: UnderscoreEscapedMap<PrivateIdentifierInfo>,
}

pub(super) struct ClassLexicalEnvironment {
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
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformClassFields {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformClassFields {
    fn call(&self, _node: &Node) -> io::Result<Gc<Node>> {
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
        Gc::new(Box::new(TransformClassFields::new(context)))
    }
}

pub fn transform_class_fields() -> TransformerFactory {
    Gc::new(Box::new(TransformClassFieldsFactory::new()))
}
