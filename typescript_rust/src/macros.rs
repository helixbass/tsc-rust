#[macro_export]
macro_rules! regex {
    ($re:literal $(,)?) => {{
        static RE: once_cell::sync::OnceCell<regex::Regex> = once_cell::sync::OnceCell::new();
        RE.get_or_init(|| regex::Regex::new($re).unwrap())
    }};
}

#[macro_export]
macro_rules! return_ok_none_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                return Ok(None);
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! return_ok_false_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                return Ok(false);
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! return_ok_default_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                return Ok(Default::default());
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! break_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                break;
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! continue_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                continue;
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! debug_fail_if_none {
    ($expr:expr $(,)?) => {
        match $expr {
            None => {
                crate::Debug_.fail(None);
            }
            Some(expr) => expr,
        }
    };
    ($expr:expr , $message:expr $(,)?) => {
        match $expr {
            None => {
                crate::Debug_.fail(Some($message));
            }
            Some(expr) => expr,
        }
    };
}

#[macro_export]
macro_rules! continue_if_err {
    ($expr:expr $(,)?) => {
        match $expr {
            Err(_) => {
                continue;
            }
            Ok(expr) => expr,
        }
    };
}
