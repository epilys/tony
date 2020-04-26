extern crate inkwell;
#[cfg(feature = "fuzzing")]
extern crate libfuzzer_sys;
use std::collections::HashMap;
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

#[macro_use]
extern crate lalrpop_util;

pub mod error;
pub use error::*;

pub mod ast;

pub mod lexer;

pub mod parser;

pub mod symbols;

pub mod builtins;

pub mod llvm;
