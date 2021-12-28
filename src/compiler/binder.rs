#![allow(non_upper_case_globals)]

use bitflags::bitflags;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    Symbol, SymbolTable, SyntaxKind, VariableDeclaration, __String, append_if_unique,
    create_symbol_table, for_each, for_each_child, get_escaped_text_of_identifier_or_literal,
    get_name_of_declaration, is_binding_pattern, is_property_name_literal, object_allocator,
    set_parent, set_value_declaration, Expression, ExpressionStatement, NamedDeclarationInterface,
    Node, NodeArray, NodeInterface, Statement, SymbolFlags,
};

bitflags! {
    struct ContainerFlags: u32 {
        const None = 0;

        const IsContainer = 1 << 0;

        const IsControlFlowContainer = 1 << 2;

        const HasLocals = 1 << 5;
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
    Symbol: RefCell<Option<fn(SymbolFlags, __String) -> Symbol>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: RefCell::new(None),
        parent: RefCell::new(None),
        container: RefCell::new(None),
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

    #[allow(non_snake_case)]
    fn Symbol(&self) -> fn(SymbolFlags, __String) -> Symbol {
        self.Symbol.borrow().unwrap()
    }

    #[allow(non_snake_case)]
    fn set_Symbol(&self, Symbol: fn(SymbolFlags, __String) -> Symbol) {
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
        self.Symbol()(flags, name)
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
        node: &TNode, /*Declaration*/
        includes: SymbolFlags,
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

        if container_flags.intersects(ContainerFlags::IsContainer) {
            self.set_container(Some(node.node_wrapper()));
            if container_flags.intersects(ContainerFlags::HasLocals) {
                self.container().set_locals(create_symbol_table());
            }
        }

        if false {
        } else {
            self.bind_children(node);
        }

        self.set_container(save_container);
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
            Node::Expression(Expression::ArrayLiteralExpression(_)) => {
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
    ) -> Option<Rc<Symbol>> {
        match self.container().kind() {
            SyntaxKind::SourceFile => Some(self.declare_source_file_member(node, symbol_flags)),
            _ => unimplemented!(),
        }
    }

    fn declare_source_file_member<TNode: NodeInterface>(
        &self,
        node: &TNode, /*Declaration*/
        symbol_flags: SymbolFlags,
    ) -> Rc<Symbol> {
        if false {
            unimplemented!()
        } else {
            self.declare_symbol(&mut *self.file().locals(), node, symbol_flags)
        }
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
            Node::VariableDeclaration(variable_declaration) => {
                return self.bind_variable_declaration_or_binding_element(variable_declaration);
            }
            _ => (),
        }
    }

    fn bind_variable_declaration_or_binding_element(&self, node: &VariableDeclaration) {
        if !is_binding_pattern(&*node.name()) {
            if false {
            } else {
                self.declare_symbol_and_add_to_symbol_table(
                    node,
                    SymbolFlags::FunctionScopedVariable,
                );
            }
        }
    }
}
