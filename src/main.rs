//! This example is supposed to be ran as a executable.
//! The source code is in the following order:
//! - Lexer,
//! - Parser,
//! - Compiler,
//! - Program.
//!
//! Both the `Parser` and the `Compiler` may fail, in which case they would return
//! an error represented by `Result<T, TonyError>`, for easier error reporting.

extern crate tony_frontend;
use std::io::{self, Read};
use tony_frontend::lexer::*;
use tony_frontend::TonyError;
use tony_frontend::{llvm::*, parser, symbols};
//const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

struct RunConfig {
    display_lexer_output: bool,
    display_parser_output: bool,
    display_compiler_output: bool,
    file: String,
}

/// Entry point of the program; acts as a REPL.
pub fn main() {
    // use self::inkwell::support::add_symbol;
    let mut runtime_config = RunConfig {
        display_lexer_output: false,
        display_parser_output: false,
        display_compiler_output: false,
        file: String::new(),
    };

    for arg in std::env::args() {
        match arg.as_str() {
            "--dl" => runtime_config.display_lexer_output = true,
            "--dp" => runtime_config.display_parser_output = true,
            "--dc" => runtime_config.display_compiler_output = true,
            _ => {
                runtime_config.file = arg;
            }
        }
    }

    if let Err(exit_code) = run_app(runtime_config) {
        std::process::exit(exit_code);
    }
}

fn run_app(conf: RunConfig) -> Result<(), i32> {
    let mut input = String::new();
    let mut file = std::fs::File::open(conf.file.as_str()).unwrap();
    file.read_to_string(&mut input)
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
    let debug_metadata_version = context.i32_type().const_int(3, false);
    module.add_basic_value_flag(
        "Debug Info Version",
        inkwell::module::FlagBehavior::Warning,
        debug_metadata_version,
    );

    let dibuilder = module.create_debug_info_builder(true);

    let compile_unit = dibuilder.create_compile_unit(
        inkwell::debug_info::DWARFSourceLanguage::C,
        dibuilder.create_file(conf.file.as_str(), "."),
        /* producer */ "tony toy compiler",
        /* is_optimized */ false,
        /*flags*/ "",
        /* runtime_ver */ 1,
        /*split_name */ "debug_file",
        /* kind */ inkwell::debug_info::DWARFEmissionKind::Full,
        /*dwo_id */ 0x0,
        /* split_debug_inling */ true,
        /* debug_info_for_profiling */ false,
    );
    let debug_helper = DebugHelper {
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        input: input.as_str(),
    };

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
    for funcdef in tony_frontend::builtins::builtins_to_funcdef() {
        match Compiler::compile(
            &context,
            &builder,
            &debug_helper,
            &fpm,
            &module,
            &funcdef,
            &env,
            &env.global_scope_uuid,
        ) {
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

        match Compiler::compile(
            &context,
            &builder,
            &debug_helper,
            &fpm,
            &module,
            &funcdef,
            &env,
            &env.get_funcscope(Some(&env.global_scope_uuid), funcdef.ident())
                .unwrap(),
        ) {
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
    dibuilder.finalize();
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
