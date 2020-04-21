//! This example is supposed to be ran as a executable.
//! The source code is in the following order:
//! - Lexer,
//! - Parser,
//! - Compiler,
//! - Program.
//!
//! Both the `Parser` and the `Compiler` may fail, in which case they would return
//! an error represented by `Result<T, TonyError>`, for easier error reporting.

extern crate inkwell;
use std::collections::HashMap;
use std::io::{self, Read};
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

use crate::Token::*;
#[macro_use]
extern crate lalrpop_util;

mod error;
pub use error::*;

mod ast;

mod lexer;
use lexer::*;

mod parser;

mod symbols;

mod builtins;

mod llvm;
use llvm::*;

//const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

struct RunConfig {
    display_lexer_output: bool,
    display_parser_output: bool,
    display_compiler_output: bool,
}

/// Entry point of the program; acts as a REPL.
pub fn main() {
    // use self::inkwell::support::add_symbol;
    let mut runtime_config = RunConfig {
        display_lexer_output: false,
        display_parser_output: false,
        display_compiler_output: false,
    };

    for arg in std::env::args() {
        match arg.as_str() {
            "--dl" => runtime_config.display_lexer_output = true,
            "--dp" => runtime_config.display_parser_output = true,
            "--dc" => runtime_config.display_compiler_output = true,
            _ => (),
        }
    }

    /*
        let context = Context::create();
        let module = context.create_module("repl");
        let builder = context.create_builder();

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();

        let mut previous_exprs = Vec::new();
        let mut ctr = 0;
        loop {
            if ctr > 0 {
                break;
            }
            ctr += 1;
            println!();
            //print_flush!("?> ");

            // Read input from stdin
            let mut input = String::new();
            io::stdin()
                .read_to_string(&mut input)
                .expect("Could not read from standard input.");

            if input.starts_with("exit") || input.starts_with("quit") {
                break;
            } else if input.chars().all(char::is_whitespace) {
                continue;
            }

            // Build precedence map
            let mut prec = HashMap::with_capacity(6);

            prec.insert('=', 2);
            prec.insert('<', 10);
            prec.insert('+', 20);
            prec.insert('-', 20);
            prec.insert('*', 40);
            prec.insert('/', 40);

            // Parse and (optionally) display input

            // make module
            let module = context.create_module("tmp");

            // recompile every previously parsed function into the new module
            for prev in &previous_exprs {
                Compiler::compile(&context, &builder, &fpm, &module, prev)
                    .expect("Cannot re-add previously compiled function.");
            }

            let (name, is_anonymous) = match Parser::new(input, &mut prec).parse() {
                Ok(fun) => {
                    let is_anon = fun.is_anon;

                    if display_parser_output {
                        if is_anon {
                            println!("-> Expression parsed: \n{:?}\n", fun.body);
                        } else {
                            println!("-> Function parsed: \n{:?}\n", fun);
                        }
                    }

                    match Compiler::compile(&context, &builder, &fpm, &module, &fun) {
                        Ok(function) => {
                            if display_compiler_output {
                                // Not printing a new line since LLVM automatically
                                // prefixes the generated string with one
                                //print_flush!("-> Expression compiled to IR:");
                                function.print_to_stderr();
                            }

                            if !is_anon {
                                // only add it now to ensure it is correct
                                previous_exprs.push(fun);
                            }

                            (function.get_name().to_str().unwrap().to_string(), is_anon)
                        }
                        Err(err) => {
                            println!("!> Error compiling function: {}", err);
                            continue;
                        }
                    }
                }
                Err(err) => {
                    println!("!> Error parsing expression: {}", err);
                    continue;
                }
            };

            if is_anonymous {
                let ee = module
                    .create_jit_execution_engine(OptimizationLevel::None)
                    .unwrap();

                let maybe_fn =
                    unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>(name.as_str()) };
                let compiled_fn = match maybe_fn {
                    Ok(f) => f,
                    Err(err) => {
                        println!("!> Error during execution: {:?}", err);
                        continue;
                    }
                };

                unsafe {
                    println!("=> {}", compiled_fn.call());
                }
            }
        }
    */
    if let Err(exit_code) = run_app(runtime_config) {
        std::process::exit(exit_code);
    }
}

fn run_app(conf: RunConfig) -> Result<(), i32> {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Could not read from standard input.");

    if conf.display_lexer_output {
        println!(
            "-> Lexer output: \n{:?}\n",
            Lexer::new(input.as_str()).collect::<Vec<LexResult>>()
        );
    }
    let ast = match parser::ProgramParser::new().parse(Lexer::new(input.as_str())) {
        Ok(res) => res,
        Err(err) => {
            match err {
                lalrpop_util::ParseError::InvalidToken { ref location } => {
                    println!(
                        "{}",
                        TonyError::with_offset("Invalid token", *location)
                            .display(input.to_string())
                    );
                }
                lalrpop_util::ParseError::UnrecognizedEOF {
                    ref location,
                    ref expected,
                } => {
                    println!(
                        "{}",
                        TonyError::with_offset("Unrecognized EOF", *location)
                            .display(input.to_string())
                    );
                    println!("Expected tokens: {}", expected.join(", "));
                }
                lalrpop_util::ParseError::UnrecognizedToken {
                    token: (ref l, ref _t, ref r),
                    ref expected,
                } => {
                    println!(
                        "{}",
                        TonyError::with_span(
                            format!("Unrecognized token: \"{}\"", &input[*l..*r]),
                            (*l, *r)
                        )
                        .display(input.to_string())
                    );
                    println!("Expected tokens: {}", expected.join(", "));
                }
                lalrpop_util::ParseError::ExtraToken {
                    token: (ref l, ref t, ref r),
                } => {
                    println!(
                        "{}",
                        TonyError::with_span(
                            format!("Extra token of type {:?}: \"{}\"", t, &input[*l..*r]),
                            (*l, *r)
                        )
                        .display(input.to_string())
                    );
                }
                lalrpop_util::ParseError::User { error: err } => {
                    println!(
                        "{}",
                        err.set_parser_kind().display(input.to_string()).to_string()
                    );
                }
            }
            return Err(-1);
        }
    };
    if conf.display_parser_output {
        println!("-> Parser output: \n{:#?}\n", &ast);
    }

    if !ast
        .0
        .iter()
        .map(|span| span.into_inner().header.0.var.id.into_inner())
        .any(|id| id.0 == "main")
    {
        println!("No main function found.");
        return Err(-1);
    }

    let mut env = symbols::ProgramEnvironment::new_environment();

    for funcdef in ast.0.iter() {
        if let Err(err) = env.insert_global_func(funcdef.into_inner().clone()) {
            println!("{}", err.display(input.to_string()));
            //eprintln!("{:#?}", &env);
            return Err(-1);
        }
    }
    let context = Context::create();
    let module = context.create_module("bin");
    let builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    /*
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    */

    fpm.initialize();
    // make module
    for funcdef in crate::builtins::builtins_to_funcdef() {
        match Compiler::compile(&context, &builder, &fpm, &module, &funcdef) {
            Ok(function) => {
                if conf.display_compiler_output {
                    // Not printing a new line since LLVM automatically
                    // prefixes the generated string with one
                    print!("Expression compiled to IR:");
                    function.print_to_stderr();
                }
            }
            Err(err) => {
                println!(
                    "!> Error compiling function: {}",
                    err.display(input.to_string())
                );
            }
        }
    }

    for funcdef in ast.0.iter() {
        if conf.display_parser_output {
            println!("Function parsed: \n{}\n", &funcdef.into_inner());
        }

        match Compiler::compile(&context, &builder, &fpm, &module, &funcdef) {
            Ok(function) => {
                if conf.display_compiler_output {
                    // Not printing a new line since LLVM automatically
                    // prefixes the generated string with one
                    print!("Expression compiled to IR:");
                    function.print_to_stderr();
                }
            }
            Err(err) => {
                println!(
                    "!> Error compiling function: {}",
                    err.display(input.to_string())
                );
                Err(1)?
            }
        }
    }
    println!("cool beans");
    use inkwell::targets::{CodeModel, RelocMode, Target, TargetMachine};
    use inkwell::OptimizationLevel;

    use std::path::Path;

    let target_config = inkwell::targets::InitializationConfig::default();
    /*
        asm_parser: true,
        asm_printer: true,
        base: true,
        disassembler: true,
        info: true,
        machine_code: true,
    };*/
    inkwell::targets::Target::initialize_native(&target_config).unwrap();
    let opt = OptimizationLevel::None;
    let reloc = RelocMode::PIC;
    let model = CodeModel::Default;
    let path = Path::new("./output.ll");
    let target = Target::get_first().expect("Could not get native target");
    let default_triple = TargetMachine::get_default_triple();
    let target_machine = target
        .create_target_machine(&default_triple, "generic", "", opt, reloc, model)
        .unwrap();
    let data_layout = target_machine.get_target_data().get_data_layout();

    module.set_data_layout(&data_layout);
    /*
    target_machine
        .write_to_file(&module, inkwell::targets:FileType::Object, &path)
        .unwrap();
    */
    module.print_to_file(&path).unwrap();
    println!(
        "compiling.. {}",
        String::from_utf8_lossy(
            &std::process::Command::new("llc-8")
                .args(&["-relocation-model=pic", "-filetype=obj", "output.ll"])
                .output()
                .unwrap()
                .stderr
        )
    );
    println!(
        "linking.. {}",
        String::from_utf8_lossy(
            &std::process::Command::new("gcc")
                .args(&[
                    "output.o",
                    "target/debug/libruntime.a",
                    "-lpthread",
                    "-ldl",
                    "-o",
                    "main"
                ])
                .output()
                .unwrap()
                .stderr
        )
    );
    Ok(())
}
