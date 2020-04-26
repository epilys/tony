#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate tony_frontend;

use libfuzzer_sys::arbitrary::*;
use tony_frontend::ast::*;
use tony_frontend::llvm::*;
use tony_frontend::{lexer::Lexer, lexer::Token, parser, symbols, TonyError};

fuzz_target!(|data: Program| {
    let _: std::result::Result<_, TonyError> = (|ast: Program| {
        let mut env = symbols::ProgramEnvironment::new_environment();
        if !ast
            .0
            .iter()
            .map(|span| span.into_inner().header.0.var.id.into_inner())
            .any(|id| id.0 == "main")
        {
            return Err(TonyError::new(""));
        }

        for funcdef in ast.0.iter() {
            env.insert_global_func(funcdef.into_inner().clone())?;
        }
        let context = Context::create();
        let module = context.create_module("bin");
        let builder = context.create_builder();
        // Create FPM
        let fpm = PassManager::create(&module);
        fpm.initialize();
        for funcdef in tony_frontend::builtins::builtins_to_funcdef() {
            Compiler::compile(
                &context,
                &builder,
                &fpm,
                &module,
                &funcdef,
                &env,
                &env.global_scope_uuid,
            )?;
        }

        for funcdef in ast.0.iter() {
            Compiler::compile(
                &context,
                &builder,
                &fpm,
                &module,
                &funcdef,
                &env,
                &env.get_funcscope(Some(&env.global_scope_uuid), funcdef.ident())
                    .unwrap(),
            )?;
        }
        Ok(())
    })(data);
});
