#![allow(non_upper_case_globals)]

use std::rc::Rc;

use crate::{set_parent, Node, SourceFile};

// lazy_static! {
//     static ref binder: BinderType = create_binder();
// }

pub fn bind_source_file(file: Rc<SourceFile>) {
    // binder.call(file);
    create_binder().call(file);
}

struct BinderType {
    file: Option<Rc<SourceFile>>,
    parent: Option<Rc<Node>>,
}

fn create_binder() -> BinderType {
    BinderType {
        file: None,
        parent: None,
    }
}

impl BinderType {
    fn call(&mut self, f: Rc<SourceFile>) {
        self.bind_source_file(f);
    }

    fn file(&self) -> Rc<SourceFile> {
        self.file.as_ref().unwrap().clone()
    }

    fn bind_source_file(&mut self, f: Rc<SourceFile>) {
        self.file = Some(f);

        if true {
            self.bind(Some(Rc::new(self.file().into())));
        }

        self.file = None;
        self.parent = None;
    }

    fn bind(&self, node: Option<Rc<Node>>) {
        let node = match node {
            None => {
                return;
            }
            Some(node) => node,
        };
        set_parent(
            &*node,
            match &self.parent {
                None => None,
                Some(parent) => Some(parent.clone()),
            },
        );
    }
}
