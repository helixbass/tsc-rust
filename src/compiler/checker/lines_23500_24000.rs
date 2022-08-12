#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::rc::Rc;

use crate::{Node, SymbolInterface, Type, TypeChecker, TypeFlags, TypeInterface};

impl TypeChecker {
    pub(super) fn extract_types_of_kind(&self, type_: &Type, kind: TypeFlags) -> Rc<Type> {
        self.filter_type(type_, |t: &Type| t.flags().intersects(kind))
    }

    pub(super) fn get_flow_type_of_reference<
        TInitialType: Borrow<Type>,
        TFlowContainer: Borrow<Node>,
    >(
        &self,
        reference: &Node,
        declared_type: &Type,
        initial_type: Option<TInitialType>,
        flow_container: Option<TFlowContainer>,
    ) -> Rc<Type> {
        let initial_type = initial_type.map_or_else(
            || declared_type.type_wrapper(),
            |initial_type| initial_type.borrow().type_wrapper(),
        );
        unimplemented!()
    }
}
