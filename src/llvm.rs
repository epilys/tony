use super::*;
use crate::ast::*;
use std::borrow::Borrow;

use inkwell::builder::Builder;
pub use inkwell::context::Context;
use inkwell::module::Module;
pub use inkwell::passes::PassManager;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, FunctionValue, PointerValue};
use inkwell::FloatPredicate; //, OptimizationLevel};

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a FuncDef,

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
    fn create_entry_block_alloca(&self, name: &str, t: &TonyType) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(Compiler::tonytype_into_basictypenum(&self.context, t), name)
    }

    /// Compiles the specified `Expr`
    fn compile_expr(&'_ self, expr: &Expr) -> Result<Box<dyn BasicValue<'_> + '_>, &'static str> {
        match expr {
            Expr::Atom(_) => {}
            Expr::IntConst(IntConst(v, _)) => {
                return Ok(Box::new(self.context.f64_type().const_float(*v)));
            }
            Expr::CharConst(CharConst(v)) => {
                return Ok(Box::new(
                    self.context.i32_type().const_int(*v as u32 as u64, false),
                ));
            }
            Expr::True => {
                return Ok(Box::new(self.context.bool_type().const_int(1, false)));
            }
            Expr::False => {
                return Ok(Box::new(self.context.bool_type().const_zero()));
            }
            Expr::Not(expr_span) => {}
            Expr::And(expr_1_span, expr_2_span) => {}
            Expr::Or(expr_1_span, expr_2_span) => {}
            Expr::Minus(expr_span) => {}
            Expr::Op(expr_1_span, op, expr_2_span) => {}
            Expr::New(type_span, expr_span) => {}
            Expr::Nil => {}
        }
        todo!()
    }

    /// Compiles the specified `Stmt` into an LLVM `FloatValue`.
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), &'static str> {
        println!("compiling stmt {:?}", stmt);
        match stmt {
            Stmt::Simple(Simple::Skip) => {}
            Stmt::Simple(Simple::Call(call)) => {}
            Stmt::Simple(Simple::Assignment(atom, expr_span)) => {}
            Stmt::Exit => {}
            Stmt::Return(expr_span) => {
                let ret = self.compile_expr(expr_span)?;
                self.builder.build_return(Some(&(*ret)));
                return Ok(());
            }
            Stmt::Control(StmtType::If {
                condition: condition_expr_span,
                body: body_stmt_spans,
                _else: _else_stmt_spans,
            }) => {}
            Stmt::Control(StmtType::For {
                init,
                condition: condition_expr_span,
                eval,
                body: body_stmt_spans,
            }) => {}
        }
        todo!() /*
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
                                    Some(ref init) => self.compile_stmt(init)?,
                                    None => self.context.f64_type().const_float(0.),
                                };

                                let alloca = self.create_entry_block_alloca(var_name);

                                self.builder.build_store(alloca, initial_val);

                                if let Some(old_binding) = self.variables.remove(var_name) {
                                    old_bindings.push(old_binding);
                                }

                                self.variables.insert(var_name.to_string(), alloca);
                            }

                            let body = self.compile_stmt(body)?;

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

                                let var_val = self.compile_stmt(right)?;
                                let var = self
                                    .variables
                                    .get(var_name.as_str())
                                    .ok_or("Undefined variable.")?;

                                self.builder.build_store(*var, var_val);

                                Ok(var_val)
                            } else {
                                let lhs = self.compile_stmt(left)?;
                                let rhs = self.compile_stmt(right)?;

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
                                    compiled_args.push(self.compile_stmt(arg)?);
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
                            let cond = self.compile_stmt(cond)?;
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
                            let then_val = self.compile_stmt(consequence)?;
                            self.builder.build_unconditional_branch(cont_bb);

                            let then_bb = self.builder.get_insert_block().unwrap();

                            // build else block
                            self.builder.position_at_end(else_bb);
                            let else_val = self.compile_stmt(alternative)?;
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
                            let start = self.compile_stmt(start)?;

                            self.builder.build_store(start_alloca, start);

                            // go from current block to loop block
                            let loop_bb = self.context.append_basic_block(parent, "loop");

                            self.builder.build_unconditional_branch(loop_bb);
                            self.builder.position_at_end(loop_bb);

                            let old_val = self.variables.remove(var_name.as_str());

                            self.variables.insert(var_name.to_owned(), start_alloca);

                            // emit body
                            self.compile_stmt(body)?;

                            // emit step
                            let step = match *step {
                                Some(ref step) => self.compile_stmt(step)?,
                                None => self.context.f64_type().const_float(1.0),
                            };

                            // compile end condition
                            let end_cond = self.compile_stmt(end)?;

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
                */
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(
        &self,
        (fn_vardef, arg_decls): &(Formal, Vec<Formal>),
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let args_types = arg_decls
            .iter()
            .map(|f| {
                Compiler::tonytype_into_basictypenum(&self.context, f.var.tony_type.into_inner())
            })
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = Compiler::tonytype_into_fn_typenum(
            &self.context,
            fn_vardef.var.tony_type.into_inner(),
            args_types,
        );
        let fn_val =
            self.module
                .add_function(fn_vardef.var.id.into_inner().0.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            if arg_decls[i].is_ref {
                arg.into_pointer_value().set_name(arg_decls[i].var.as_str());
            } else {
                match arg_decls[i].var.tony_type.into_inner() {
                    TonyType::Int => arg.into_float_value().set_name(arg_decls[i].var.as_str()),
                    TonyType::Bool | TonyType::Char => {
                        arg.into_int_value().set_name(arg_decls[i].var.as_str())
                    }
                    TonyType::Unit => todo!(),
                    TonyType::Array(_) => {
                        arg.into_array_value().set_name(arg_decls[i].var.as_str())
                    }
                    TonyType::List(_) => {
                        arg.into_vector_value().set_name(arg_decls[i].var.as_str())
                    }
                };
            }
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.header;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_empty() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve((proto.1).len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = (proto.1)[i].var.as_str();
            let arg_type = (proto.1)[i].var.tony_type.into_inner();
            let alloca = self.create_entry_block_alloca(arg_name, arg_type);

            self.builder.build_store(alloca, arg);

            self.variables
                .insert((proto.1)[i].var.as_str().to_string(), alloca);
        }

        for stmt in self.function.body.iter() {
            self.compile_stmt(stmt)?;
        }
        if self.function.body.is_empty() || proto.0.var.tony_type.into_inner() == &TonyType::Unit {
            self.builder.build_return(None);
        }
        /*
        // compile body
        let body = self.compile_stmt(self.function.body.as_ref().unwrap())?;
        self.builder.build_return(Some(&body));
        */

        // return the whole thing after verification and optimization
        if function.verify(true) {
            //self.fpm.run_on(&function);

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
        function: &FuncDef,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }

    fn tonytype_into_basictypenum(context: &'ctx Context, t: &TonyType) -> BasicTypeEnum<'ctx> {
        match t {
            TonyType::Int => context.f64_type().into(),
            TonyType::Bool => context.bool_type().into(),
            TonyType::Char => context.i32_type().into(),
            TonyType::Unit => todo!(),
            TonyType::Array(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_array_type()
                    .into()
            }
            TonyType::List(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_vector_type()
                    .into()
            }
        }
    }

    fn tonytype_into_fn_typenum(
        context: &'ctx Context,
        t: &TonyType,
        args: &[BasicTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        match t {
            TonyType::Int => context.f64_type().fn_type(args, false),
            TonyType::Bool => context.bool_type().fn_type(args, false),
            TonyType::Char => context.i32_type().fn_type(args, false),
            TonyType::Unit => context.void_type().fn_type(args, false),
            TonyType::Array(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_array_type()
                    .fn_type(args, false)
            }
            TonyType::List(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_vector_type()
                    .fn_type(args, false)
            }
        }
    }
}
