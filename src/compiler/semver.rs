#[derive(Clone)]
pub struct Version {
    pub version: &'static str,
}

impl Version {
    pub fn new(version: &'static str) -> Self {
        Self { version }
    }
}

pub struct VersionRange {}

impl VersionRange {
    pub fn try_parse(text: &str) -> Option<Self> {
        unimplemented!()
    }

    pub fn test<TVersion: Into<VersionOrString>>(&self, version: TVersion) -> bool {
        unimplemented!()
    }
}

pub enum VersionOrString {
    Version(Version),
    String(String),
}

impl From<Version> for VersionOrString {
    fn from(value: Version) -> Self {
        Self::Version(value)
    }
}

impl From<String> for VersionOrString {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}
