extern crate lalrpop;

fn main() {
    println!("cargo:rerun-if-changed=src/parser/grammar.lalrpop");
    println!("cargo:rerun-if-changed=build.rs");
    lalrpop::process_root().unwrap();
}
