use bitflags::bitflags;
use gc::{Finalize, Gc, Trace};

use crate::{
    Node, TransformationContext, Transformer, TransformerFactory, TransformerFactoryInterface,
    TransformerInterface,
};

bitflags! {
    struct ESNextSubstitutionFlags: u32 {
        const None = 0;
        const AsyncMethodsWithSuper = 1 << 0;
    }
}

bitflags! {
    struct HierarchyFacts: u32 {
        const None = 0;

        const HasLexicalThis = 1 << 0;
        const IterationContainer = 1 << 1;

        const AncestorFactsMask = (Self::IterationContainer.bits << 1) - 1;

        const SourceFileIncludes = Self::HasLexicalThis.bits;
        const SourceFileExcludes = Self::IterationContainer.bits;
        const StrictModeSourceFileIncludes = Self::None.bits;

        const ClassOrFunctionIncludes = Self::HasLexicalThis.bits;
        const ClassOrFunctionExcludes = Self::IterationContainer.bits;

        const ArrowFunctionIncludes = Self::None.bits;
        const ArrowFunctionExcludes = Self::ClassOrFunctionExcludes.bits;

        const IterationStatementIncludes = Self::IterationContainer.bits;
        const IterationStatementExcludes = Self::None.bits;
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018 {
    context: Gc<Box<dyn TransformationContext>>,
}

impl TransformES2018 {
    fn new(context: Gc<Box<dyn TransformationContext>>) -> Self {
        Self { context }
    }
}

impl TransformerInterface for TransformES2018 {
    fn call(&self, node: &crate::Node) -> Gc<Node> {
        unimplemented!()
    }
}

#[derive(Trace, Finalize)]
struct TransformES2018Factory {}

impl TransformES2018Factory {
    fn new() -> Self {
        Self {}
    }
}

impl TransformerFactoryInterface for TransformES2018Factory {
    fn call(&self, context: gc::Gc<Box<dyn TransformationContext>>) -> Transformer {
        Gc::new(Box::new(TransformES2018::new(context)))
    }
}

pub fn transform_es2018() -> TransformerFactory {
    Gc::new(Box::new(TransformES2018Factory::new()))
}
