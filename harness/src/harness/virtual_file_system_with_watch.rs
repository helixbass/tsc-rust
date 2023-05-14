pub mod TestFSWithWatch {
    use derive_builder::Builder;

    pub fn lib_file() -> File {
        FileBuilder::default()
            .path("/a/lib/lib.d.ts")
            .content(
                r#"/// <reference no-default-lib="true"/>
interface Boolean {}
interface Function {}
interface CallableFunction {}
interface NewableFunction {}
interface IArguments {}
interface Number { toExponential: any; }
interface Object {}
interface RegExp {}
interface String { charAt: any; }
interface Array<T> { length: number; [n: number]: T; }"#,
            )
            .build()
            .unwrap()
    }

    #[derive(Builder)]
    #[builder(setter(strip_option, into))]
    pub struct File {
        pub path: String,
        pub content: String,
        #[builder(default)]
        pub file_size: Option<usize>,
    }
}
