use super::*;
use crate::ast::*;

use inkwell::builder::Builder;
pub use inkwell::context::Context;
use inkwell::module::Module;
pub use inkwell::passes::PassManager;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::IntValue;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{types::BasicType, AddressSpace, IntPredicate};

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a FuncDef,

    variables: HashMap<String, PointerValue<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
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

    fn compile_bool_expr(&self, expr: &Span<Expr>) -> Result<IntValue<'ctx>, TonyError> {
        match expr.into_inner() {
            Expr::True => Ok(self.context.bool_type().const_int(1, false)),
            Expr::False => Ok(self.context.bool_type().const_zero()),
            Expr::Not(expr_span) => {
                let v = self.compile_bool_expr(expr_span)?;
                Ok(self.builder.build_not(v, "tmpnot"))
            }
            Expr::And(left, right) => {
                let lhs = self.compile_bool_expr(left)?;
                let rhs = self.compile_bool_expr(right)?;
                Ok(self.builder.build_and(lhs, rhs, "tmpand"))
            }
            Expr::Or(left, right) => {
                let lhs = self.compile_bool_expr(left)?;
                let rhs = self.compile_bool_expr(right)?;
                Ok(self.builder.build_or(lhs, rhs, "tmpor"))
            }
            Expr::Atom(Atom::Id(id_span)) => {
                let name = &id_span.into_inner().0.to_string();
                let var = self.variables.get(name.as_str()).ok_or_else(|| {
                    TonyError::with_span(
                        format!("Undefined variable, found {:?}", id_span.into_inner()),
                        id_span.span(),
                    )
                    .set_typecheck_kind()
                })?;
                //TODO typecheck
                Ok(self
                    .builder
                    .build_load(*var, name.as_str())
                    .into_int_value())
            }
            Expr::Op(left, Operator::Equals, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::NotEquals, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::Less, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::Greater, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::LessOrEqual, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::GreaterOrEqual, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(_, op, _) => Err(TonyError::with_span(
                format!("expected bool expression, found {:?} operation", op),
                expr.span(),
            )
            .set_typecheck_kind()),
            Expr::Atom(Atom::Call(_)) => Ok(self.compile_expr(expr)?.into_int_value()),
            _ => Err(TonyError::with_span(
                format!("expected bool, found {:?}", expr.into_inner()),
                expr.span(),
            )
            .set_typecheck_kind()),
        }
    }

    /// Compiles the specified `Expr`
    fn compile_expr(&self, expr: &Span<Expr>) -> Result<BasicValueEnum<'ctx>, TonyError> {
        match expr.into_inner() {
            Expr::Atom(Atom::Call(Call(ident_span, arg_spans))) => {
                match self.get_function(ident_span.as_str()) {
                    Some(fun) => {
                        let mut compiled_args = Vec::with_capacity(arg_spans.len());

                        for arg in arg_spans {
                            compiled_args.push(self.compile_expr(arg)?);
                        }

                        let argsv: Vec<BasicValueEnum> =
                            compiled_args.into_iter().map(|val| val.into()).collect();

                        let val = match self
                            .builder
                            .build_call(fun, argsv.as_slice(), "tmp")
                            .try_as_basic_value()
                            .left()
                        {
                            Some(_value) => _value,
                            None => {
                                return Err(TonyError::with_span(
                                    format!(
                                        "Function {:?} returns void. ",
                                        ident_span.into_inner()
                                    ),
                                    ident_span.span(),
                                )
                                .set_typecheck_kind())
                            }
                        };
                        Ok(val.into())
                    }
                    None => Err(TonyError::with_span(
                        format!("Undefined function, found {:?}", ident_span.into_inner()),
                        ident_span.span(),
                    )
                    .set_symbol_table_kind()),
                }
            }
            Expr::Atom(Atom::Id(id_span)) => {
                let name = &id_span.into_inner().0.to_string();
                let var = self.variables.get(name.as_str()).ok_or_else(|| {
                    TonyError::with_span(
                        format!("Undefined variable, found {:?}", id_span.into_inner()),
                        id_span.span(),
                    )
                    .set_symbol_table_kind()
                })?;
                Ok(self.builder.build_load(*var, name.as_str()))
            }
            Expr::Atom(Atom::StringLiteral(string_literal)) => {
                /* build_global_string panics if string_literal contains nul bytes! */
                let global = unsafe {
                    self.builder
                        .build_global_string(string_literal.0.as_str(), string_literal.0.as_str())
                };
                let zero = self.context.i32_type().const_zero();
                Ok(unsafe {
                    self.builder
                        .build_gep(global.as_pointer_value(), &[zero, zero], "litptr_")
                        .as_basic_value_enum()
                })
            }
            Expr::Atom(Atom::AtomIndex(_, _)) => todo!(),
            Expr::IntConst(IntConst(v, _)) => Ok(self
                .context
                .i64_type()
                .const_int(*v as u64, false)
                .as_basic_value_enum()),
            Expr::CharConst(CharConst(v)) => Ok(self
                .context
                .i8_type()
                .const_int(*v as u32 as u64, false)
                .as_basic_value_enum()),
            Expr::True | Expr::False | Expr::Not(_) | Expr::And(_, _) | Expr::Or(_, _) => {
                Ok(self.compile_bool_expr(expr)?.as_basic_value_enum())
            }
            Expr::Minus(expr_span) => {
                let val = self.compile_expr(expr_span)?.into_int_value();
                Ok(self
                    .builder
                    .build_int_neg(val, "tmpneg")
                    .as_basic_value_enum())
            }
            Expr::Op(_, Operator::Equals, _)
            | Expr::Op(_, Operator::NotEquals, _)
            | Expr::Op(_, Operator::Less, _)
            | Expr::Op(_, Operator::Greater, _)
            | Expr::Op(_, Operator::LessOrEqual, _)
            | Expr::Op(_, Operator::GreaterOrEqual, _) => {
                Ok(self.compile_bool_expr(expr)?.as_basic_value_enum())
            }
            Expr::Op(left, op, right) => {
                let lhs = self.compile_expr(left)?.into_int_value();
                let rhs = self.compile_expr(right)?.into_int_value();
                match op {
                    Operator::Div => Ok(self
                        .builder
                        .build_int_signed_div(lhs, rhs, "tmp_sub")
                        .as_basic_value_enum()),
                    Operator::Mod => Ok(self
                        .builder
                        .build_int_signed_rem(lhs, rhs, "tmp_sub")
                        .as_basic_value_enum()),
                    Operator::Times => Ok(self
                        .builder
                        .build_int_mul(lhs, rhs, "tmp_sub")
                        .as_basic_value_enum()),
                    Operator::Plus => Ok(self
                        .builder
                        .build_int_add(lhs, rhs, "tmp_sub")
                        .as_basic_value_enum()),
                    Operator::Minus => Ok(self
                        .builder
                        .build_int_sub(lhs, rhs, "tmp_sub")
                        .as_basic_value_enum()),
                    Operator::Equals
                    | Operator::NotEquals
                    | Operator::Less
                    | Operator::Greater
                    | Operator::LessOrEqual
                    | Operator::GreaterOrEqual => unsafe { std::hint::unreachable_unchecked() },
                }
            }
            Expr::New(_type_span, _expr_span) => todo!(),
            Expr::Nil => todo!(),
        }
    }

    fn compile_simple(&mut self, simple: &Span<Simple>) -> Result<(), TonyError> {
        match simple.into_inner() {
            Simple::Skip => Ok(()), // Skip is NOP
            Simple::Call(Call(ident_span, arg_spans)) => {
                return match self.get_function(ident_span.as_str()) {
                    Some(fun) => {
                        let mut compiled_args = Vec::with_capacity(arg_spans.len());

                        for arg in arg_spans {
                            compiled_args.push(self.compile_expr(arg)?);
                        }

                        let argsv: Vec<BasicValueEnum> =
                            compiled_args.into_iter().map(|val| val.into()).collect();

                        self.builder.build_call(fun, argsv.as_slice(), "tmp");
                        Ok(())
                    }
                    None => Err(TonyError::with_span(
                        format!("Undefined function, found {:?}", ident_span.into_inner()),
                        ident_span.span(),
                    )
                    .set_typecheck_kind()),
                };
            }
            Simple::Assignment(Atom::StringLiteral(_), _) => todo!(), // NOP?
            Simple::Assignment(Atom::AtomIndex(_, _), _) => todo!(),
            Simple::Assignment(Atom::Id(id_span), expr_span) => {
                let var_name = id_span.into_inner().0.as_str();

                let var_val = self.compile_expr(expr_span)?;
                let var = self.variables.get(var_name).ok_or_else(|| {
                    TonyError::with_span(
                        format!("Undefined variable, found {:?}", id_span.into_inner()),
                        id_span.span(),
                    )
                    .set_typecheck_kind()
                })?;

                self.builder.build_store(*var, var_val);

                return Ok(());
            }
            Simple::Assignment(Atom::Call(_), _) => todo!(),
        }
    }

    /// Compiles the specified `Stmt` into an LLVM `IntValue`.
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<(), TonyError> {
        //println!("compiling stmt {:?}", stmt);
        match stmt {
            Stmt::Simple(simple_span) => self.compile_simple(simple_span),
            Stmt::Exit => {
                self.builder.build_return(None);
                Ok(())
            }
            Stmt::Return(expr_span) => {
                let ret = self.compile_expr(expr_span)?;
                self.builder.build_return(Some(&ret));
                Ok(())
            }
            Stmt::Control(StmtType::If {
                condition: condition_expr_span,
                body: body_stmt_spans,
                _else: else_stmt_spans,
            }) => {
                let parent = self.fn_value();

                let cond = self.compile_bool_expr(&condition_expr_span)?;

                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let mut cont_bb = None; //self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb);

                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(then_bb);
                }
                self.builder.position_at_end(then_bb);
                for stmt in body_stmt_spans.iter() {
                    self.compile_stmt(stmt.into_inner())?;
                }
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    if cont_bb.is_none() {
                        cont_bb = Some(self.context.append_basic_block(parent, "ifcont"));
                    }
                    self.builder.build_unconditional_branch(cont_bb.unwrap());
                }
                //let then_bb = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(else_bb);
                for stmt in else_stmt_spans.iter() {
                    self.compile_stmt(stmt.into_inner())?;
                }
                if else_bb.get_terminator().is_none() {
                    if cont_bb.is_none() {
                        cont_bb = Some(self.context.append_basic_block(parent, "ifcont"));
                    }
                    self.builder.build_unconditional_branch(cont_bb.unwrap());
                }
                //let else_bb = self.builder.get_insert_block().unwrap();

                if cont_bb.is_some() {
                    self.builder.position_at_end(cont_bb.unwrap());
                }
                //let phi = self.builder.build_phi(self.context.i64_type(), "iftmp");

                //let zero_const = self.context.i64_type().const_zero();
                //phi.add_incoming(&[(&zero_const, then_bb), (&zero_const, else_bb)]);

                //Ok(phi.as_basic_value().into_int_value())

                Ok(())
            }
            Stmt::Control(StmtType::For {
                init: init_list,
                condition: condition_expr_span,
                eval: eval_list,
                body: body_stmt_spans,
            }) => {
                for s in init_list {
                    self.compile_simple(s)?;
                }
                let parent = self.fn_value();

                /*
                 * - append loop block
                 * - branch to loop block
                 * - position to loop block
                 * - check condition
                 * - branch if condition is false to merge block
                 * - execute body
                 * - execute eval
                 * - unconditional jump to loop block
                 * - append merge block
                 */
                // go from current block to loop block
                let cond_bb = self.context.append_basic_block(parent, "forcond");
                let loop_bb = self.context.append_basic_block(parent, "forloop");
                let eval_bb = self.context.append_basic_block(parent, "foreval");
                let cont_bb = self.context.append_basic_block(parent, "forcont");

                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(cond_bb);
                // check condition
                let cond = self.compile_bool_expr(&condition_expr_span)?;

                // - branch if condition is false to merge block

                self.builder
                    .build_conditional_branch(cond, loop_bb, cont_bb);
                // - execute body
                self.builder.position_at_end(loop_bb);

                // emit body
                for stmt in body_stmt_spans.iter() {
                    self.compile_stmt(stmt.into_inner())?;
                }
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(eval_bb);
                }
                self.builder.position_at_end(eval_bb);

                // eval
                for s in eval_list {
                    self.compile_simple(s)?;
                }
                self.builder.build_unconditional_branch(cond_bb);
                self.builder.position_at_end(cont_bb);
                Ok(())
            }
        }
        /*
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
    ) -> Result<FunctionValue<'ctx>, TonyError> {
        let args_types = arg_decls
            .iter()
            .map(|f| {
                Compiler::tonytype_into_basictypenum(&self.context, f.var.tony_type.into_inner())
            })
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = if fn_vardef.var.as_str() == "main" {
            self.context.i64_type().fn_type(&[], false)
        } else {
            Compiler::tonytype_into_fn_typenum(
                &self.context,
                fn_vardef.var.tony_type.into_inner(),
                args_types,
            )
        };
        let fn_val = self
            .module
            .add_function(fn_vardef.var.as_str(), fn_type, None);

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            if arg_decls[i].is_ref {
                arg.into_pointer_value().set_name(arg_decls[i].var.as_str());
            } else {
                match arg_decls[i].var.tony_type.into_inner() {
                    TonyType::Int => arg.into_int_value().set_name(arg_decls[i].var.as_str()),
                    TonyType::Bool | TonyType::Char => {
                        arg.into_int_value().set_name(arg_decls[i].var.as_str())
                    }
                    TonyType::Unit => todo!(),
                    TonyType::Array(_) => {
                        arg.into_pointer_value().set_name(arg_decls[i].var.as_str())
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
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, TonyError> {
        let proto = &self.function.header;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.is_extern {
            return Ok(function);
        }
        for decl_span in self.function.declarations.iter() {
            match decl_span.into_inner() {
                crate::ast::Decl::Func(funcdef_span) => {
                    let function = Self::compile(
                        self.context,
                        self.builder,
                        self.fpm,
                        self.module,
                        funcdef_span.into_inner(),
                    )?;
                    self.functions.insert(
                        funcdef_span.into_inner().header.0.var.as_str().to_string(),
                        function,
                    );
                }
                crate::ast::Decl::Var(_) => {}
            }
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

        for decl_span in self.function.declarations.iter() {
            match decl_span.into_inner() {
                crate::ast::Decl::Func(_) => {}
                crate::ast::Decl::Var(vardefs) => {
                    for vardef in vardefs {
                        let var_name = vardef.as_str();
                        let var_type = vardef.tony_type.into_inner();
                        let alloca = self.create_entry_block_alloca(var_name, var_type);

                        self.variables.insert(var_name.to_string(), alloca);
                    }
                }
            }
        }
        for stmt in self.function.body.iter() {
            self.compile_stmt(stmt)?;
        }
        let entry = self.builder.get_insert_block().unwrap();
        if entry.get_terminator().is_none() {
            if self.function.header.0.var.as_str() == "main" {
                self.builder.position_at_end(entry);
                self.builder
                    .build_return(Some(&self.context.i64_type().const_zero()));
            } else if self.function.body.is_empty()
                || proto.0.var.tony_type.into_inner() == &TonyType::Unit
            {
                self.builder.build_return(None);
            }
        }

        // return the whole thing after verification and optimization
        if function.verify(true) {
            //self.fpm.run_on(&function);

            Ok(function)
        } else {
            function.print_to_stderr();
            unsafe {
                function.delete();
            }

            Err(TonyError::new("Invalid generated function.").set_llvm_verify_kind()).unwrap()
        }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &FuncDef,
    ) -> Result<FunctionValue<'ctx>, TonyError> {
        let mut compiler = Compiler {
            context,
            builder,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
        };

        compiler.compile_fn()
    }

    fn tonytype_into_basictypenum(context: &'ctx Context, t: &TonyType) -> BasicTypeEnum<'ctx> {
        match t {
            TonyType::Int => context.i64_type().into(),
            TonyType::Bool => context.bool_type().into(),
            TonyType::Char => context.i8_type().into(),
            TonyType::Unit => todo!(),
            TonyType::Array(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .ptr_type(AddressSpace::Generic)
                    .into()
            }
            TonyType::List(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_pointer_type()
                    .vec_type(0)
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
            TonyType::Int => context.i64_type().fn_type(args, false),
            TonyType::Bool => context.bool_type().fn_type(args, false),
            TonyType::Char => context.i8_type().fn_type(args, false),
            TonyType::Unit => context.void_type().fn_type(args, false),
            TonyType::Array(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .ptr_type(AddressSpace::Generic)
                    .fn_type(args, false)
            }
            TonyType::List(span_type) => {
                Compiler::tonytype_into_basictypenum(context, span_type.into_inner())
                    .into_pointer_type()
                    .vec_type(0)
                    .fn_type(args, false)
            }
        }
    }
}
