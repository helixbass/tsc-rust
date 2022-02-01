#![allow(non_upper_case_globals)]

use std::borrow::Borrow;
use std::cell::Cell;
use std::fmt;

use crate::{AssertionLevel, Node, NodeArray, SyntaxKind};

enum AssertionKeys {
    AssertNode,
}

thread_local! {
    static current_assertion_level: Cell<AssertionLevel> = Cell::new(AssertionLevel::None);
}

pub struct DebugType {}

impl DebugType {
    fn new() -> Self {
        Self {}
    }

    fn current_assertion_level(&self) -> AssertionLevel {
        current_assertion_level.with(|current_assertion_level_| current_assertion_level_.get())
    }

    fn should_assert(&self, level: AssertionLevel) -> bool {
        self.current_assertion_level() >= level
    }

    fn should_assert_function(&self, level: AssertionLevel, name: AssertionKeys) -> bool {
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
            None => "Debug failure.".to_string(),
        };
        panic!("{}", message);
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

    pub fn assert_less_than_or_equal<TValue: Ord + fmt::Debug>(&self, a: TValue, b: TValue) {
        if a > b {
            self.fail(Some(&format!("Expected {:?} <= {:?}", a, b)));
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

    pub fn assert_never<TValue>(&self, value: TValue, message: Option<&str>) -> ! {
        let message = message.unwrap_or("Illegal value:");
        self.fail(Some(message));
    }

    pub fn assert_node<TNode: Borrow<Node>, TTest: FnOnce(&Node) -> bool>(
        &self,
        node: Option<TNode>,
        test: Option<TTest>,
        message: Option<&str>,
    ) {
        if self.should_assert_function(AssertionLevel::Normal, AssertionKeys::AssertNode) {
            self.assert(
                node.is_some() && (test.is_none() || (test.unwrap())(node.unwrap().borrow())),
                Some(message.unwrap_or("Unexpected node.")),
            );
        }
    }

    pub fn format_syntax_kind(&self, kind: Option<SyntaxKind>) -> String {
        format!("{:?}", kind)
    }

    pub fn attach_node_array_debug_info(&self, array: &mut NodeArray) {
        // TODO: implement this?
    }
}

lazy_static! {
    pub static ref Debug_: DebugType = DebugType::new();
}
