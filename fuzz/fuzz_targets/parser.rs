#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate tony_frontend;

use libfuzzer_sys::arbitrary::*;
use tony_frontend::{lexer::Token, parser};

fuzz_target!(|data: Vec<(usize, Token, usize)>| {
    use tony_frontend::{lexer::Lexer, parser};
    let _ = parser::ProgramParser::new().parse(data.into_iter().map(|d| {
        let ok: std::result::Result<_, tony_frontend::TonyError> = Ok(d);
        ok
    }));
});
