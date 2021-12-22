#![allow(non_upper_case_globals)]

pub struct DebugType {}

impl DebugType {
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

    pub fn assert_is_defined<TValue>(&self, value: Option<TValue>, message: Option<&str>) {
        if value.is_none() {
            self.fail(message);
        }
    }
}

lazy_static! {
    pub static ref Debug_: DebugType = DebugType {};
}
