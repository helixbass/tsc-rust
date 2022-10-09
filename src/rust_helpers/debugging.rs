use std::cell::Cell;

thread_local! {
    static is_debugging: Cell<bool> = Cell::new(false);
}

pub fn if_debugging<TCallback: FnMut()>(mut callback: TCallback) {
    is_debugging.with(|is_debugging_| {
        if is_debugging_.get() {
            callback();
        }
    })
}

pub fn start_debugging() {
    println!("starting debugging");
    is_debugging.with(|is_debugging_| {
        is_debugging_.set(true);
    });
}

pub fn stop_debugging() {
    println!("stopping debugging");
    is_debugging.with(|is_debugging_| {
        is_debugging_.set(false);
    });
}

pub fn while_debugging<TReturn, TCallback: FnMut() -> TReturn>(mut callback: TCallback) -> TReturn {
    start_debugging();
    let ret = callback();
    stop_debugging();
    ret
}
