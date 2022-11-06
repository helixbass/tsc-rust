use std::cell::RefCell;

struct DescribeContext {
    its: Vec<Box<dyn FnOnce()>>,
    befores: Vec<Box<dyn FnOnce()>>,
    afters: Vec<Box<dyn FnOnce()>>,
}

impl DescribeContext {
    pub fn new() -> Self {
        Self {
            its: vec![],
            befores: vec![],
            afters: vec![],
        }
    }
}

thread_local! {
    static describe_contexts_: RefCell<Vec<DescribeContext>> = RefCell::new(vec![]);
}

pub fn describe<TCallback: FnOnce()>(description: &str, callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        describe_contexts.borrow_mut().push(DescribeContext::new());
    });
    callback();
    let DescribeContext {
        its,
        befores,
        afters,
    } = describe_contexts_.with(|describe_contexts| describe_contexts.borrow_mut().pop().unwrap());
    for before in befores {
        before()
    }
    for it in its {
        it()
    }
    for after in afters {
        after()
    }
}

pub fn it<TCallback: FnOnce() + 'static>(description: &str, callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].its.push(Box::new(callback));
    });
}

pub fn before<TCallback: FnOnce() + 'static>(callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].befores.push(Box::new(callback));
    });
}

pub fn after<TCallback: FnOnce() + 'static>(callback: TCallback) {
    describe_contexts_.with(|describe_contexts| {
        let mut describe_contexts = describe_contexts.borrow_mut();
        let index = describe_contexts.len() - 1;
        describe_contexts[index].afters.push(Box::new(callback));
    });
}