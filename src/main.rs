//! This is an example of the [Kaleidoscope tutorial](https://llvm.org/docs/tutorial/)
//! made in Rust, using Inkwell.
//! Currently, all features up to the [7th chapter](https://llvm.org/docs/tutorial/LangImpl07.html)
//! are available.
//! This example is supposed to be ran as a executable, which launches a REPL.
//! The source code is in the following order:
//! - Lexer,
//! - Parser,
//! - Compiler,
//! - Program.
//!
//! Both the `Parser` and the `Compiler` may fail, in which case they would return
//! an error represented by `Result<T, &'static str>`, for easier error reporting.

extern crate inkwell;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::iter::Peekable;
use std::ops::DerefMut;
use std::str::Chars;

use self::inkwell::builder::Builder;
use self::inkwell::context::Context;
use self::inkwell::module::Module;
use self::inkwell::passes::PassManager;
use self::inkwell::types::BasicTypeEnum;
use self::inkwell::values::{BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use self::inkwell::FloatPredicate; //, OptimizationLevel};

use crate::Token::*;
#[macro_use]
extern crate lalrpop_util;

mod error;
pub use error::*;

mod ast;

mod lexer;
use lexer::*;

mod parser;
use parser::*;

mod symbols;

const ANONYMOUS_FUNCTION_NAME: &str = "anonymous";

// ======================================================================================
// COMPILER =============================================================================
// ======================================================================================

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue<'ctx>, &'static str> {
        match *expr {
            Expr::Number(nb) => Ok(self.context.f64_type().const_float(nb)),

            Expr::Variable(ref name) => match self.variables.get(name.as_str()) {
                Some(var) => Ok(self
                    .builder
                    .build_load(*var, name.as_str())
                    .into_float_value()),
                None => Err("Could not find a matching variable."),
            },

            Expr::VarIn {
                ref variables,
                ref body,
            } => {
                let mut old_bindings = Vec::new();

                for &(ref var_name, ref initializer) in variables {
                    let var_name = var_name.as_str();

                    let initial_val = match *initializer {
                        Some(ref init) => self.compile_expr(init)?,
                        None => self.context.f64_type().const_float(0.),
                    };

                    let alloca = self.create_entry_block_alloca(var_name);

                    self.builder.build_store(alloca, initial_val);

                    if let Some(old_binding) = self.variables.remove(var_name) {
                        old_bindings.push(old_binding);
                    }

                    self.variables.insert(var_name.to_string(), alloca);
                }

                let body = self.compile_expr(body)?;

                for binding in old_bindings {
                    self.variables
                        .insert(binding.get_name().to_str().unwrap().to_string(), binding);
                }

                Ok(body)
            }

            Expr::Binary {
                op,
                ref left,
                ref right,
            } => {
                if op == '=' {
                    // handle assignement
                    let var_name = match *left.borrow() {
                        Expr::Variable(ref var_name) => var_name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignement.");
                        }
                    };

                    let var_val = self.compile_expr(right)?;
                    let var = self
                        .variables
                        .get(var_name.as_str())
                        .ok_or("Undefined variable.")?;

                    self.builder.build_store(*var, var_val);

                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?;
                    let rhs = self.compile_expr(right)?;

                    match op {
                        '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                        '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                        '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                        '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
                        '<' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                lhs,
                                rhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),
                        '>' => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                rhs,
                                lhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),

                        custom => {
                            let mut name = String::from("binary");

                            name.push(custom);

                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self
                                        .builder
                                        .build_call(fun, &[lhs.into(), rhs.into()], "tmpbin")
                                        .try_as_basic_value()
                                        .left()
                                    {
                                        Some(value) => Ok(value.into_float_value()),
                                        None => Err("Invalid call produced."),
                                    }
                                }

                                None => Err("Undefined binary operator."),
                            }
                        }
                    }
                }
            }

            Expr::Call {
                ref fn_name,
                ref args,
            } => match self.get_function(fn_name.as_str()) {
                Some(fun) => {
                    let mut compiled_args = Vec::with_capacity(args.len());

                    for arg in args {
                        compiled_args.push(self.compile_expr(arg)?);
                    }

                    let argsv: Vec<BasicValueEnum> = compiled_args
                        .iter()
                        .by_ref()
                        .map(|&val| val.into())
                        .collect();

                    match self
                        .builder
                        .build_call(fun, argsv.as_slice(), "tmp")
                        .try_as_basic_value()
                        .left()
                    {
                        Some(value) => Ok(value.into_float_value()),
                        None => Err("Invalid call produced."),
                    }
                }
                None => Err("Unknown function."),
            },

            Expr::Conditional {
                ref cond,
                ref consequence,
                ref alternative,
            } => {
                let parent = self.fn_value();
                let zero_const = self.context.f64_type().const_float(0.0);

                // create condition by comparing without 0.0 and returning an int
                let cond = self.compile_expr(cond)?;
                let cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    cond,
                    zero_const,
                    "ifcond",
                );

                // build branch
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                // build then block
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expr(consequence)?;
                self.builder.build_unconditional_branch(cont_bb);

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                let else_val = self.compile_expr(alternative)?;
                self.builder.build_unconditional_branch(cont_bb);

                let else_bb = self.builder.get_insert_block().unwrap();

                // emit merge block
                self.builder.position_at_end(cont_bb);

                let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value().into_float_value())
            }

            Expr::For {
                ref var_name,
                ref start,
                ref end,
                ref step,
                ref body,
            } => {
                let parent = self.fn_value();

                let start_alloca = self.create_entry_block_alloca(var_name);
                let start = self.compile_expr(start)?;

                self.builder.build_store(start_alloca, start);

                // go from current block to loop block
                let loop_bb = self.context.append_basic_block(parent, "loop");

                self.builder.build_unconditional_branch(loop_bb);
                self.builder.position_at_end(loop_bb);

                let old_val = self.variables.remove(var_name.as_str());

                self.variables.insert(var_name.to_owned(), start_alloca);

                // emit body
                self.compile_expr(body)?;

                // emit step
                let step = match *step {
                    Some(ref step) => self.compile_expr(step)?,
                    None => self.context.f64_type().const_float(1.0),
                };

                // compile end condition
                let end_cond = self.compile_expr(end)?;

                let curr_var = self.builder.build_load(start_alloca, var_name);
                let next_var =
                    self.builder
                        .build_float_add(curr_var.into_float_value(), step, "nextvar");

                self.builder.build_store(start_alloca, next_var);

                let end_cond = self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    end_cond,
                    self.context.f64_type().const_float(0.0),
                    "loopcond",
                );
                let after_bb = self.context.append_basic_block(parent, "afterloop");

                self.builder
                    .build_conditional_branch(end_cond, loop_bb, after_bb);
                self.builder.position_at_end(after_bb);

                self.variables.remove(var_name);

                if let Some(val) = old_val {
                    self.variables.insert(var_name.to_owned(), val);
                }

                Ok(self.context.f64_type().const_float(0.0))
            }
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(proto.args[i].clone(), alloca);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        // return the whole thing after verification and optimization
        if function.verify(true) {
            self.fpm.run_on(&function);

            Ok(function)
        } else {
            unsafe {
                function.delete();
            }

            Err("Invalid generated function.")
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
            function: function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}

// ======================================================================================
// PROGRAM ==============================================================================
// ======================================================================================

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

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
            "-> Attempting to parse lexed input: \n{:?}\n",
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
                        TonyError::with_offset(input.to_string(), "Invalid token", *location)
                    );
                }
                lalrpop_util::ParseError::UnrecognizedEOF {
                    ref location,
                    ref expected,
                } => {
                    println!(
                        "{}",
                        TonyError::with_offset(input.to_string(), "Unrecognized EOF", *location)
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
                            input.to_string(),
                            format!("Unrecognized token: \"{}\"", &input[*l..*r]),
                            (*l, *r)
                        )
                    );
                    println!("Expected tokens: {}", expected.join(", "));
                }
                lalrpop_util::ParseError::ExtraToken {
                    token: (ref l, ref t, ref r),
                } => {
                    println!(
                        "{}",
                        TonyError::with_span(
                            input.to_string(),
                            format!("Extra token of type {:?}: \"{}\"", t, &input[*l..*r]),
                            (*l, *r)
                        )
                    );
                }
                lalrpop_util::ParseError::User { error: err } => {
                    println!("{}", err.set_parser_kind().to_string());
                }
            }
            return Err(-1);
        }
    };
    if !ast
        .0
        .iter()
        .map(|span| span.into_inner().header.0.var.id.into_inner())
        .any(|id| id.0 == "main")
        && ast.0.len() != 1
    {
        println!("No main function found.");
        return Err(-1);
    }

    let mut env = symbols::ProgramEnvironment::new_environment(input.to_string());

    for funcdef in ast.0.iter() {
        if let Err(err) = env.insert_global_func(funcdef.into_inner().clone()) {
            println!("{}", err.to_string());
            //eprintln!("{:#?}", &env);
            return Err(-1);
        }
    }
    println!("cool beans");
    Ok(())
}
