#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate tony_frontend;

use tony_frontend::{lexer::Lexer, parser};

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let _ = parser::ProgramParser::new().parse(Lexer::new(input));
    }
});
