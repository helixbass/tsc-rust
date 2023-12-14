use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
    fmt,
    rc::Rc,
};

use gc::Gc;
use id_arena::Id;

use crate::{
    maybe_map, unescape_leading_underscores, AssertionLevel, Node, NodeArray, NodeInterface,
    Symbol, SymbolFlags, SymbolInterface, SyntaxKind,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum LogLevel {
    Off,
    Error,
    Warning,
    Info,
    Verbose,
}

pub trait LoggingHost {
    fn log(&self, level: LogLevel, s: &str);
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum AssertionKeys {
    AssertEachNode,
    AssertNode,
    AssertNotNode,
}

thread_local! {
    static current_assertion_level: Cell<AssertionLevel> = Cell::new(AssertionLevel::None);
}
thread_local! {
    pub static current_log_level: Cell<LogLevel> = Cell::new(LogLevel::Warning);
}
thread_local! {
    pub static is_debugging: Cell<bool> = Cell::new(false);
}
thread_local! {
    pub static logging_host: RefCell<Option<Rc<dyn LoggingHost>>> = RefCell::new(None);
}

pub struct DebugType {}

impl DebugType {
    fn new() -> Self {
        Self {}
    }

    fn current_assertion_level(&self) -> AssertionLevel {
        current_assertion_level.with(|current_assertion_level_| current_assertion_level_.get())
    }
    pub fn current_log_level(&self) -> LogLevel {
        current_log_level.with(|current_log_level_| current_log_level_.get())
    }
    pub fn is_debugging(&self) -> bool {
        is_debugging.with(|is_debugging_| is_debugging_.get())
    }

    pub fn should_log(&self, level: LogLevel) -> bool {
        self.current_log_level() <= level
    }

    pub fn set_logging_host(&self, host: Option<Rc<dyn LoggingHost>>) {
        logging_host.with(|logging_host_| {
            *logging_host_.borrow_mut() = host;
        });
    }

    pub fn log_message(&self, level: LogLevel, s: &str) {
        logging_host.with(|logging_host_| {
            let logging_host_ = logging_host_.borrow();
            if let Some(logging_host_) = logging_host_.as_ref() {
                if self.should_log(level) {
                    logging_host_.log(level, s);
                }
            }
        })
    }

    pub fn log(&self, s: &str) {
        self.log_message(LogLevel::Info, s)
    }

    pub fn should_assert(&self, level: AssertionLevel) -> bool {
        self.current_assertion_level() >= level
    }

    fn should_assert_function(&self, level: AssertionLevel, _name: AssertionKeys) -> bool {
        if !self.should_assert(level) {
            // assertionCache[name] = { level, assertion: Debug[name] };
            // (Debug as any)[name] = noop;
            return false;
        }
        true
    }

    pub fn fail(&self, message: Option<&str>) -> ! {
        let message = match message {
            Some(message) => format!("Debug failure. {}", message),
            None => "Debug failure.".to_owned(),
        };
        panic!("{}", message);
    }

    pub fn fail_bad_syntax_kind(&self, node: Id<Node>, message: Option<&str>) -> ! {
        self.fail(Some(&format!(
            "{}\r\nNode {} was unexpected.",
            &message.map_or_else(
                || "Unexpected node.".to_owned(),
                |message| message.to_owned()
            ),
            self.format_syntax_kind(Some(node.kind()))
        )))
    }

    pub fn assert(&self, expression: bool, message: Option<&str>) {
        if !expression {
            let message = message.map_or("False expression.".to_string(), |message| {
                format!("False expression: {}", message)
            });
            self.fail(Some(&message));
        }
    }

    pub fn assert_equal<TValue: Eq + fmt::Debug>(
        &self,
        a: &TValue,
        b: &TValue,
        msg: Option<&str>,
        msg2: Option<&str>,
    ) {
        if a != b {
            let message = match (msg, msg2) {
                (Some(msg), Some(msg2)) => format!("{} {}", msg, msg2),
                (Some(msg), _) => msg.to_string(),
                _ => "".to_string(),
            };
            self.fail(Some(&format!("Expected {:?} === {:?}. {}", a, b, &message)));
        }
    }

    pub fn assert_less_than<TValue: Ord + fmt::Debug>(
        &self,
        a: TValue,
        b: TValue,
        msg: Option<&str>,
    ) {
        if a > b {
            self.fail(Some(&format!(
                "Expected {:?} < {:?}. {}",
                a,
                b,
                msg.unwrap_or("")
            )));
        }
    }

    pub fn assert_less_than_or_equal<TValue: Ord + fmt::Debug>(&self, a: TValue, b: TValue) {
        if a > b {
            self.fail(Some(&format!("Expected {:?} <= {:?}", a, b)));
        }
    }

    pub fn assert_greater_than_or_equal<TValue: Ord + fmt::Debug>(&self, a: TValue, b: TValue) {
        if a < b {
            self.fail(Some(&format!("Expected {:?} >= {:?}", a, b)));
        }
    }

    pub fn assert_is_defined<TValue>(&self, value: &Option<TValue>, message: Option<&str>) {
        if value.is_none() {
            self.fail(message);
        }
    }

    pub fn check_defined<TValue>(&self, value: Option<TValue>, message: Option<&str>) -> TValue {
        self.assert_is_defined(&value, message);
        value.unwrap()
    }

    pub fn assert_never<TValue>(&self, _value: TValue, message: Option<&str>) -> ! {
        let message = message.unwrap_or("Illegal value:");
        self.fail(Some(message));
    }

    pub fn assert_each_node(
        &self,
        nodes: &[Id<Node>],
        mut test: impl FnMut(Id<Node>) -> bool,
        message: Option<&str>,
    ) {
        if self.should_assert_function(AssertionLevel::Normal, AssertionKeys::AssertEachNode) {
            self.assert(
                /*test === undefined ||*/ nodes.into_iter().all(|node| test(node)),
                Some(message.unwrap_or("Unexpected node.")),
            );
        }
    }

    pub fn assert_node(
        &self,
        node: Option<Id<Node>>,
        test: Option<impl FnOnce(Id<Node>) -> bool>,
        message: Option<&str>,
    ) {
        if self.should_assert_function(AssertionLevel::Normal, AssertionKeys::AssertNode) {
            self.assert(
                node.is_some() && (test.is_none() || (test.unwrap())(node.unwrap().borrow())),
                Some(message.unwrap_or("Unexpected node.")),
            );
        }
    }

    pub fn assert_not_node(
        &self,
        node: Option<Id<Node>>,
        test: Option<impl FnOnce(Id<Node>) -> bool>,
        message: Option<&str>,
    ) {
        if self.should_assert_function(AssertionLevel::Normal, AssertionKeys::AssertNotNode) {
            self.assert(
                node.is_none() || test.is_none() || !(test.unwrap())(node.unwrap().borrow()),
                Some(message.unwrap_or("Unexpected node.")),
            );
        }
    }

    pub fn format_symbol(&self, symbol: &Symbol) -> String {
        format!(
            "{{ name: {}; flags: {}; declarations: {:?} }}",
            unescape_leading_underscores(symbol.escaped_name()),
            self.format_symbol_flags(Some(symbol.flags())),
            maybe_map(
                symbol.maybe_declarations().as_deref(),
                |node: &Id<Node>, _| self.format_syntax_kind(Some(node.kind()))
            )
        )
    }

    pub fn format_syntax_kind(&self, kind: Option<SyntaxKind>) -> String {
        format!("{:?}", kind)
    }

    pub fn format_symbol_flags(&self, _flags: Option<SymbolFlags>) -> String {
        unimplemented!()
    }

    pub fn attach_node_array_debug_info(&self, _array: &NodeArray) {
        // TODO: implement this?
    }

    pub fn enable_debug_info(&self) {
        unimplemented!()
    }
}

lazy_static! {
    pub static ref Debug_: DebugType = DebugType::new();
}
