// copied from https://github.com/eun-ice/weak-self

use std::cell::UnsafeCell;
use std::fmt;
use std::rc::{Rc, Weak};

pub struct WeakSelf<T: ?Sized> {
    cell: UnsafeCell<Option<Weak<T>>>,
}

impl<T: ?Sized> Clone for WeakSelf<T> {
    fn clone(&self) -> Self {
        WeakSelf {
            cell: UnsafeCell::new(Some(self.get())),
        }
    }
}

impl<T: ?Sized> WeakSelf<T> {
    /// Constructs a new empty WeakSelf<T>
    pub fn new() -> WeakSelf<T> {
        WeakSelf {
            cell: UnsafeCell::new(None),
        }
    }

    /// Initialize the WeakSelf<T> with an Rc.
    ///
    /// Note: content must point be the only existing Rc, otherwise this method will panic
    pub fn init(&self, content: &Rc<T>, require_exclusive_access: bool) {
        if require_exclusive_access {
            if Rc::strong_count(content) != 1 || Rc::weak_count(content) != 0 {
                panic!("Exclusive access to Rc<T> is required while initializing WeakSelf<T>");
            }
        }
        let weak = Rc::downgrade(content);
        unsafe {
            *self.cell.get() = Some(weak);
        }
    }

    /// get Some Weak<T> pointer to the content, or None if not yet initialized
    pub fn try_get(&self) -> Option<&Weak<T>> {
        unsafe {
            match *self.cell.get() {
                Some(ref weak) => Some(&weak),
                None => None,
            }
        }
    }

    /// get a Weak<T> pointer to the content, or panic if not yet initialized
    pub fn get(&self) -> Weak<T> {
        self.try_get()
            .expect("expected WeakSelf to be initialized")
            .clone()
    }
}

unsafe impl<T: ?Sized + Sync + Send> Sync for WeakSelf<T> {}

unsafe impl<T: ?Sized + Sync + Send> Send for WeakSelf<T> {}

impl<T: ?Sized + fmt::Debug> fmt::Debug for WeakSelf<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.try_get() {
            None => {
                write!(f, "Empty WeakSelf<T>")
            }
            Some(weak) => fmt::Debug::fmt(weak, f),
        }
    }
}

impl<T: ?Sized> Default for WeakSelf<T> {
    fn default() -> Self {
        WeakSelf::new()
    }
}
