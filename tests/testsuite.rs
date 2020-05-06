extern crate lit;

use std::env;
use std::path::PathBuf;

fn bin_dir() -> PathBuf {
    env::current_exe()
        .ok()
        .map(|mut path| {
            path.pop();
            path.pop();
            path
        })
        .unwrap()
}

fn bin_path() -> String {
    bin_dir()
        .join(format!("lox{}", env::consts::EXE_SUFFIX))
        .to_str()
        .unwrap()
        .to_string()
}

#[test]
fn run() {
    lit::run::tests(lit::event_handler::Default::new(), |config| {
        config.add_search_path("tests/lox/");
        config.add_extension("lox");
        config
            .constants
            .insert("lox".to_owned(), bin_path());
    }).expect("LIT tests failed");
}
