use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

const USE_MEM_TYPE_METADATA_FORMAT: bool = false;

bitflags! {
    struct TypeScriptSubstitutionFlags: u32 {
        const None = 0;
        const ClassAliases = 1 << 0;
        const NamespaceExports = 1 << 1;
        const NonQualifiedEnumMembers = 1 << 2;
    }
}

bitflags! {
    struct ClassFacts: u32 {
        const None = 0;
        const HasStaticInitializedProperties = 1 << 0;
        const HasConstructorDecorators = 1 << 1;
        const HasMemberDecorators = 1 << 2;
        const IsExportOfNamespace = 1 << 3;
        const IsNamedExternalExport = 1 << 4;
        const IsDefaultExternalExport = 1 << 5;
        const IsDerivedClass = 1 << 6;
        const UseImmediatelyInvokedFunctionExpression = 1 << 7;

        const HasAnyDecorators = Self::HasConstructorDecorators.bits | Self::HasMemberDecorators.bits;
        const NeedsName = Self::HasStaticInitializedProperties.bits | Self::HasMemberDecorators.bits;
        const MayNeedImmediatelyInvokedFunctionExpression = Self::HasAnyDecorators.bits | Self::HasStaticInitializedProperties.bits;
        const IsExported = Self::IsExportOfNamespace.bits | Self::IsDefaultExternalExport.bits | Self::IsNamedExternalExport.bits;
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScript {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformTypeScript {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }

    fn transform_source_file_or_bundle(
        &self,
        node: &Node, /*SourceFile | Bundle*/
    ) -> Gc<Node> {
        unimplemented!()
    }
}

impl TransformerInterface for TransformTypeScript {
    fn call(&self, node: &Node) -> Gc<Node> {
        self.transform_source_file_or_bundle(node)
    }
}

#[derive(Trace, Finalize)]
struct TransformTypeScriptFactory {}

impl TransformTypeScriptFactory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformTypeScriptFactory {
    fn call(&self, context: Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformTypeScript::new(context)))
    }
}

pub fn transform_type_script() -> TransformerFactory {
    Gc::new(Box::new(TransformTypeScriptFactory::new()))
}
