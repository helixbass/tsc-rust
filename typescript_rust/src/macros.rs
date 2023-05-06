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
