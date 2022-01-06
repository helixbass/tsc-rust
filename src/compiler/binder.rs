#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    Symbol, SymbolTable, SyntaxKind, VariableDeclaration, __String, append_if_unique,
    create_symbol_table, for_each, for_each_child, get_escaped_text_of_identifier_or_literal,
    get_name_of_declaration, is_binding_pattern, is_property_name_literal, object_allocator,
    set_parent, set_value_declaration, BaseSymbol, Expression, ExpressionStatement,
    InternalSymbolName, NamedDeclarationInterface, Node, NodeArray, NodeInterface,
    ObjectLiteralExpression, PropertySignature, Statement, SymbolFlags, SymbolInterface,
    TypeElement, TypeParameterDeclaration,
};

bitflags! {
    struct ContainerFlags: u32 {
        const None = 0;

        const IsContainer = 1 << 0;

        const IsControlFlowContainer = 1 << 2;

        const HasLocals = 1 << 5;
        const IsInterface = 1 << 6;
    }
}

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: &Node) {
    // binder.call(file);
    create_binder().call(file);
}

#[allow(non_snake_case)]
struct BinderType {
    file: RefCell<Option<Rc</*SourceFile*/ Node>>>,
    parent: RefCell<Option<Rc<Node>>>,
    container: RefCell<Option<Rc<Node>>>,
    block_scope_container: RefCell<Option<Rc<Node>>>,
    Symbol: RefCell<Option<fn(SymbolFlags, __String) -> BaseSymbol>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: RefCell::new(None),
        parent: RefCell::new(None),
        container: RefCell::new(None),
        block_scope_container: RefCell::new(None),
        Symbol: RefCell::new(None),
    }
}

impl BinderType {
    fn call(&self, f: &Node) {
        self.bind_source_file(f);
    }

    fn file(&self) -> Rc<Node> {
        self.file.borrow().as_ref().unwrap().clone()
    }

    fn set_file(&self, file: Option<Rc<Node>>) {
        *self.file.borrow_mut() = file;
    }

    fn maybe_parent(&self) -> Option<Rc<Node>> {
        self.parent.borrow().as_ref().map(Clone::clone)
    }

    fn parent(&self) -> Rc<Node> {
        self.parent.borrow().as_ref().unwrap().clone()
    }

    fn set_parent(&self, parent: Option<Rc<Node>>) {
        *self.parent.borrow_mut() = parent;
    }

    fn container(&self) -> Rc<Node> {
        self.container.borrow().as_ref().unwrap().clone()
    }

    fn maybe_container(&self) -> Option<Rc<Node>> {
        self.container.borrow().as_ref().map(Clone::clone)
    }

    fn set_container(&self, container: Option<Rc<Node>>) {
        *self.container.borrow_mut() = container;
    }

    fn maybe_block_scope_container(&self) -> Option<Rc<Node>> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .map(Clone::clone)
    }

    fn block_scope_container(&self) -> Rc<Node> {
        self.block_scope_container
            .borrow()
            .as_ref()
            .unwrap()
            .clone()
    }

    fn set_block_scope_container(&self, block_scope_container: Option<Rc<Node>>) {
        *self.block_scope_container.borrow_mut() = block_scope_container;
    }

    #[allow(non_snake_case)]
    fn Symbol(&self) -> fn(SymbolFlags, __String) -> BaseSymbol {
        self.Symbol.borrow().unwrap()
    }

    #[allow(non_snake_case)]
    fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> BaseSymbol) {
        *self.Symbol.borrow_mut() = Some(Symbol);
    }

    fn bind_source_file(&self, f: &Node) {
        self.set_file(Some(f.node_wrapper()));

        self.set_Symbol(object_allocator.get_symbol_constructor());

        if true {
            self.bind(Some(&*self.file()));
        }

        self.set_file(None);
        self.set_parent(None);
        self.set_container(None);
    }

    fn create_symbol(&self, flags: SymbolFlags, name: __String) -> Symbol {
        self.Symbol()(flags, name).into()
    }

    fn add_declaration_to_symbol<TNode: NodeInterface>(
        &self,
        symbol: Rc<Symbol>,
        node: &TNode, /*Declaration*/
        symbol_flags: SymbolFlags,
    ) {
        symbol.set_flags(symbol.flags() | symbol_flags);

        node.set_symbol(symbol.clone());
        let declarations = append_if_unique(
            symbol.maybe_declarations().as_ref().map(|vec| vec.clone()),
            node.node_wrapper(),
        );
        symbol.set_declarations(declarations);

        if symbol_flags.intersects(
            SymbolFlags::Class
                | SymbolFlags::Interface
                | SymbolFlags::TypeLiteral
                | SymbolFlags::ObjectLiteral,
        ) {
            let mut members = symbol.maybe_members();
            if members.is_none() {
                *members = Some(Rc::new(RefCell::new(create_symbol_table())));
            }
        }

        if symbol_flags.intersects(SymbolFlags::Value) {
            set_value_declaration(&*symbol, node);
        }
    }

    fn get_declaration_name<TNode: NodeInterface>(&self, node: &TNode) -> Option<__String> {
        let name = get_name_of_declaration(node);
        if let Some(name) = name {
            return if is_property_name_literal(&*name) {
                Some(get_escaped_text_of_identifier_or_literal(&*name))
            } else {
                None
            };
        }
        unimplemented!()
    }

    fn declare_symbol<TNode: NodeInterface>(
        &self,
        symbol_table: &mut SymbolTable,
        parent: Option<Rc<Symbol>>,
        node: &TNode, /*Declaration*/
        includes: SymbolFlags,
        excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        let name = self.get_declaration_name(node);

        let mut symbol = None;
        match name {
            None => unimplemented!(),
            Some(name) => {
                if true {
                    symbol = Some(Rc::new(self.create_symbol(SymbolFlags::None, name.clone())));
                    symbol_table.insert(name, symbol.as_ref().unwrap().clone());
                }
            }
        }
        let symbol = symbol.unwrap();

        self.add_declaration_to_symbol(symbol.clone(), node, includes);

        symbol
    }

    fn bind_container(&self, node: &Node, container_flags: ContainerFlags) {
        let save_container = self.maybe_container();
        let saved_block_scope_container = self.maybe_block_scope_container();

        if container_flags.intersects(ContainerFlags::IsContainer) {
            self.set_container(Some(node.node_wrapper()));
            self.set_block_scope_container(Some(node.node_wrapper()));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container().set_locals(create_symbol_table());
            }
        }

        if false {
        } else if container_flags.intersects(ContainerFlags::IsInterface) {
            self.bind_children(node);
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
        self.set_block_scope_container(saved_block_scope_container);
    }

    fn bind_each_functions_first(&self, nodes: &NodeArray) {
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() == SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
        BinderType::bind_each_callback(nodes, |n| {
            if n.kind() != SyntaxKind::FunctionDeclaration {
                self.bind(Some(n))
            }
        });
    }

    fn bind_each(&self, nodes: &NodeArray) {
        for_each(nodes, |node, _| {
            self.bind(Some(node.clone()));
            Option::<()>::None
        });
    }

    fn bind_each_callback<TNodeCallback: FnMut(&Node)>(
        nodes: &NodeArray,
        mut bind_function: TNodeCallback,
    ) {
        for_each(nodes, |node, _| {
            bind_function(&*node);
            Option::<()>::None
        });
    }

    fn bind_each_child(&self, node: &Node) {
        for_each_child(node, |node| self.bind(node), |nodes| self.bind_each(nodes));
    }

    fn bind_children(&self, node: &Node) {
        match node {
            Node::Statement(Statement::ExpressionStatement(expression_statement)) => {
                self.bind_expression_statement(expression_statement);
            }
            Node::Expression(Expression::PrefixUnaryExpression(_)) => {
                self.bind_prefix_unary_expression_flow(node);
            }
            Node::Expression(Expression::BinaryExpression(_)) => unimplemented!(),
            Node::VariableDeclaration(_) => {
                self.bind_variable_declaration_flow(node);
            }
            Node::SourceFile(source_file) => {
                self.bind_each_functions_first(&source_file.statements);
            }
            Node::Expression(Expression::ArrayLiteralExpression(_))
            | Node::Expression(Expression::ObjectLiteralExpression(_))
            | Node::PropertyAssignment(_) => {
                // self.set_in_assignment_pattern(save_in_assignment_pattern);
                self.bind_each_child(node);
            }
            _ => {
                self.bind_each_child(node);
            }
        };
    }

    fn bind_expression_statement(&self, node: &ExpressionStatement) {
        self.bind(Some(node.expression.clone()));
    }

    fn bind_prefix_unary_expression_flow(&self, node: &Node) {
        if false {
        } else {
            self.bind_each_child(node);
        }
    }

    fn bind_variable_declaration_flow(&self, node: &Node /*VariableDeclaration*/) {
        self.bind_each_child(node);
    }

    fn get_container_flags(&self, node: &Node) -> ContainerFlags {
        match node.kind() {
            SyntaxKind::InterfaceDeclaration => {
                return ContainerFlags::IsContainer | ContainerFlags::IsInterface;
            }
            SyntaxKind::SourceFile => {
                return ContainerFlags::IsContainer
                    | ContainerFlags::IsControlFlowContainer
                    | ContainerFlags::HasLocals;
            }
            _ => (),
        }

        ContainerFlags::None
    }

    fn declare_symbol_and_add_to_symbol_table<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::SourceFile => {
                Some(self.declare_source_file_member(node, symbol_flags, symbol_excludes))
            }
            SyntaxKind::InterfaceDeclaration => Some(self.declare_symbol(
                &mut *self.container().symbol().members().borrow_mut(),
                Some(self.container().symbol()),
                node,
                symbol_flags,
                symbol_excludes,
            )),
            _ => unimplemented!(),
        }
    }

    fn declare_source_file_member<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) -> Rc<Symbol> {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol(
                &mut *self.file().locals(),
                None,
                node,
                symbol_flags,
                symbol_excludes,
            )
        }
    }

    fn bind_object_literal_expression(&self, node: &ObjectLiteralExpression) {
        self.bind_anonymous_declaration(
            node,
            SymbolFlags::ObjectLiteral,
            InternalSymbolName::Object(),
        );
    }

    fn bind_anonymous_declaration<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol_flags: SymbolFlags,
        name: __String,
    ) -> Rc<Symbol> {
        let symbol = Rc::new(self.create_symbol(symbol_flags, name));
        match &*self.file() {
            Node::SourceFile(source_file) => {
                source_file.keep_strong_reference_to_symbol(symbol.clone());
            }
            _ => panic!("Expected SourceFile"),
        }
        self.add_declaration_to_symbol(symbol.clone(), node, symbol_flags);
        symbol
    }

    fn bind_block_scoped_declaration<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        let block_scope_container = self.block_scope_container();
        {
            let mut block_scope_container_locals = block_scope_container.maybe_locals();
            if block_scope_container_locals.is_none() {
                *block_scope_container_locals = Some(create_symbol_table());
            }
        }
        self.declare_symbol(
            &mut *block_scope_container.locals(),
            None,
            node,
            symbol_flags,
            symbol_excludes,
        );
    }

    fn bind<TNodeRef: Borrow<Node>>(&self, node: Option<TNodeRef>) {
        if node.is_none() {
            return;
        }
        let node = node.unwrap();
        let node = node.borrow();
        set_parent(node, self.maybe_parent());

        self.bind_worker(node);

        if node.kind() > SyntaxKind::LastToken {
            let save_parent = self.maybe_parent();
            self.set_parent(Some(node.node_wrapper()));
            let container_flags = self.get_container_flags(node);
            if container_flags == ContainerFlags::None {
                self.bind_children(node);
            } else {
                self.bind_container(node, container_flags);
            }
            self.set_parent(save_parent);
        }
    }

    fn bind_worker(&self, node: &Node) {
        match &*node {
            Node::TypeParameterDeclaration(type_parameter_declaration) => {
                self.bind_type_parameter(type_parameter_declaration)
            }
            Node::VariableDeclaration(variable_declaration) => {
                self.bind_variable_declaration_or_binding_element(variable_declaration)
            }
            Node::TypeElement(TypeElement::PropertySignature(property_signature)) => {
                self.bind_property_worker(property_signature)
            }
            Node::PropertyAssignment(property_assignment) => self
                .bind_property_or_method_or_accessor(
                    property_assignment,
                    SymbolFlags::Property,
                    SymbolFlags::PropertyExcludes,
                ),
            Node::Expression(Expression::ObjectLiteralExpression(object_literal_expression)) => {
                self.bind_object_literal_expression(object_literal_expression)
            }
            Node::Statement(Statement::InterfaceDeclaration(interface_declaration)) => self
                .bind_block_scoped_declaration(
                    interface_declaration,
                    SymbolFlags::Interface,
                    SymbolFlags::InterfaceExcludes,
                ),
            _ => (),
        }
    }

    fn bind_property_worker(&self, node: &PropertySignature) {
        self.bind_property_or_method_or_accessor(
            node,
            SymbolFlags::Property
                | if false {
                    unimplemented!()
                } else {
                    SymbolFlags::None
                },
            SymbolFlags::PropertyExcludes,
        )
    }

    fn bind_variable_declaration_or_binding_element(&self, node: &VariableDeclaration) {
        if !is_binding_pattern(&*node.name()) {
            if false {
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                    SymbolFlags::FunctionScopedVariableExcludes,
                );
            }
        }
    }

    fn bind_property_or_method_or_accessor<TNode: NodeInterface>(
        &self,
        node: &TNode,
        symbol_flags: SymbolFlags,
        symbol_excludes: SymbolFlags,
    ) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(node, symbol_flags, symbol_excludes);
        }
    }

    fn bind_type_parameter(&self, node: &TypeParameterDeclaration) {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol_and_add_to_symbol_table(
                node,
                SymbolFlags::TypeParameter,
                SymbolFlags::TypeParameterExcludes,
            );
        }
    }
}
