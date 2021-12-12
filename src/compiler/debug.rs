#![allow(non_upper_case_globals)]

pub struct DebugType {}

impl DebugType {
    fn fail(&self, message: Option<&str>) -> ! {
        let message = match message {
            Some(message) => format!("Debug failure. {}", message),
            None => "Debug failure.".to_string(),
        };
        panic!("{}", message);
    }

    pub fn assert(&self, expression: bool) {
        if !expression {
            let message = "False expression.";
            self.fail(Some(message));
        }
    }
}

lazy_static! {
    pub static ref Debug_: DebugType = DebugType {};
}
