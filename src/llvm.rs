use super::*;
use crate::ast::*;
use crate::symbols::{ProgramEnvironment, ScopeUuid};

use inkwell::builder::Builder;
pub use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DICompileUnit, DIFlags, DILexicalBlock, DIScope, DISubProgram, DebugInfoBuilder,
};
use inkwell::module::Module;
pub use inkwell::passes::PassManager;
use inkwell::types::{BasicTypeEnum, FunctionType};
use inkwell::values::IntValue;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{types::BasicType, AddressSpace, IntPredicate};

macro_rules! auto_deref {
    ($expr:expr, $builder:expr) => {{
        if $expr.is_pointer_value() {
            $builder.build_load($expr.into_pointer_value(), "tmpderef")
        } else {
            $expr
        }
    }};
}

mod gc;
use gc::*;

struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    def: Formal,
}

#[derive(Copy, Clone)]
pub struct DebugHelper<'a, 'ctx> {
    pub dibuilder: &'a DebugInfoBuilder<'ctx>,
    pub compile_unit: &'a DICompileUnit<'ctx>,
    pub input: &'a str,
}

impl<'a, 'ctx> DebugHelper<'a, 'ctx> {
    fn span_to_line(&self, span: (usize, usize)) -> (u32, u32) {
        use std::convert::TryInto;
        let (left, _) = span;
        let mut lines = 1_usize;
        let mut line_offset = 0_usize;
        let prev_line = self
            .input
            .char_indices()
            .filter(|(pos, _)| *pos <= left)
            .filter(|(_, c)| *c == '\n')
            .map(|(pos, _)| pos)
            .max()
            .unwrap_or(0);
        for l in self.input[..=prev_line].split_n() {
            if l.ends_with("\n") {
                lines += 1;
                line_offset += l.len();
            }
        }
        (
            lines.try_into().unwrap(),
            left.saturating_sub(line_offset).try_into().unwrap(),
        )
    }

    fn create_function(&self, funcdef: &FuncDef, func_scope: DIScope<'ctx>) -> DISubProgram<'ctx> {
        let ditype = self.dibuilder.create_basic_type(
            &format!("{:?}", funcdef.return_type()),
            64_u64,
            0x05,
            DIFlags::Public,
        );
        let subr_type = self.dibuilder.create_subroutine_type(
            self.compile_unit.get_file(),
            ditype,
            vec![],
            DIFlags::Public,
        );

        let flags = if funcdef.ident().0.as_str() == "main" {
            DIFlags::Public
        } else {
            DIFlags::Public
        };

        let ret = self.dibuilder.create_function(
            /* scope */ func_scope,
            /* func name */ funcdef.ident().0.as_str(),
            /* linkage_name */ None,
            /* file */ self.compile_unit.get_file(),
            /* line_no */ self.span_to_line(funcdef.header.0.var.id.span()).0,
            /* DIType */ subr_type,
            /* is_local_to_unit */ false,
            /* is_definition */ true,
            /* scope_line */ self.span_to_line(funcdef.header.0.var.id.span()).0,
            /* flags */ flags,
            /* is_optimized */ false,
        );
        ret
    }

    fn create_lexical_block(
        &self,
        scope: DISubProgram<'ctx>,
        start: (usize, usize),
    ) -> DILexicalBlock<'ctx> {
        let func_scope: DIScope<'ctx> = scope.as_debug_info_scope();
        self.dibuilder.create_lexical_block(
            /* scope */ func_scope,
            /* file */ self.compile_unit.get_file(),
            /* line_no */ self.span_to_line(start).0,
            /* column_no */ 0,
        )
    }

    fn create_local_variable(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        scope: DIScope<'ctx>,
        alloca: PointerValue<'ctx>,
        vardef: &VarDef,
        block: inkwell::basic_block::BasicBlock<'ctx>,
    ) {
        let (line, column) = self.span_to_line(vardef.id.span());
        let divar = self.dibuilder.create_parameter_variable(
            /* scope */ scope,
            /* name: */ vardef.id.0.as_str(),
            /* arg_no: u32, */ 0,
            /* file: DIFile<'ctx>*/ self.compile_unit.get_file(),
            /* line_no: u32,*/ line,
            /* ty: DIType<'ctx>*/
            self.dibuilder
                .create_basic_type("int", 64_u64, 0x05, DIFlags::Public),
            /* always_preserve: bool*/ true,
            DIFlags::Public,
        );

        let loc = self
            .dibuilder
            .create_debug_location(context, line, column, scope, None);
        self.dibuilder.insert_declare_at_end(
            /*storage */ alloca,
            /*var_info*/ Some(divar),
            /* expr */ None,
            /* debug_loc */ loc,
            /* block */ block,
        );
    }

    fn set_current_debug_location(
        &self,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        scope: DIScope<'ctx>,
        span: (usize, usize),
    ) {
        let (line, column) = self.span_to_line(span);
        let loc = self
            .dibuilder
            .create_debug_location(context, line, column, scope, None);
        builder.set_current_debug_location(context, loc);
    }
}

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    debug_helper: &'a DebugHelper<'a, 'ctx>,
    debug_info_function_scope: DISubProgram<'ctx>,
    debug_info_function_lexical_block: DILexicalBlock<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub function: &'a FuncDef,

    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    symbol_tables: &'a ProgramEnvironment,
    scope_uuid: &'a ScopeUuid,
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
    fn create_entry_block_alloca(
        &self,
        name: &str,
        t: &TonyType,
        is_ref: bool,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        let ret = if is_ref {
            builder.build_alloca(
                Compiler::tonytype_into_basictypenum_ref(&self.context, t),
                name,
            )
        } else {
            builder.build_alloca(Compiler::tonytype_into_basictypenum(&self.context, t), name)
        };
        builder.build_store(ret, Compiler::tonytype_default_value(&self.context, t, ret));
        if t.is_array() {
            HeapArray::incref(self, ret).unwrap();
        }
        ret
    }

    fn compile_bool_expr(&mut self, expr: &Span<Expr>) -> Result<IntValue<'ctx>, TonyError> {
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
                    .build_load(var.ptr, name.as_str())
                    .into_int_value())
            }
            Expr::Op(left, Operator::Equals, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::NotEquals, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::Less, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::Greater, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::LessOrEqual, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
                Ok({
                    self.builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "tmpcmp")
                })
            }
            Expr::Op(left, Operator::GreaterOrEqual, right) => {
                let lhs = auto_deref!(self.compile_expr(left)?, self.builder).into_int_value();
                let rhs = auto_deref!(self.compile_expr(right)?, self.builder).into_int_value();
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
            Expr::Atom(Atom::Call(_)) => {
                Ok(auto_deref!(self.compile_expr(expr)?, self.builder).into_int_value())
            }
            Expr::Atom(Atom::ListOp(list_op_span)) if list_op_span.is_nilq() => {
                if let ListOp::NilQ(expr_span) = list_op_span.into_inner() {
                    let ptr = self.compile_expr(expr_span)?.into_pointer_value();
                    Ok(self.builder.build_is_null(ptr, "isnullcheck"))
                } else {
                    unreachable!()
                }
            }
            _ => Err(TonyError::with_span(
                format!("expected bool, found {:?}", expr.into_inner()),
                expr.span(),
            )
            .set_typecheck_kind()),
        }
    }

    /// Compiles the specified `Expr`
    fn compile_expr(&mut self, expr: &Span<Expr>) -> Result<BasicValueEnum<'ctx>, TonyError> {
        match expr.into_inner() {
            Expr::Atom(Atom::Call(call @ Call(_, _))) => Ok(self.compile_call(call)?.unwrap()),
            Expr::Atom(Atom::Id(id_span)) => {
                let name = &id_span.into_inner().0.to_string();
                let var = self.variables.get(name.as_str()).ok_or_else(|| {
                    TonyError::with_span(
                        format!("Undefined variable, found {:?}", id_span.into_inner()),
                        id_span.span(),
                    )
                    .set_symbol_table_kind()
                })?;
                if var.def.var.tony_type.is_array() {
                    HeapArray::incref(self, var.ptr)?;
                }
                Ok(self.builder.build_load(var.ptr, name.as_str()))
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
            Expr::Atom(Atom::AtomIndex(box_atom, ref expr_span)) => {
                if let Atom::Id(ref id_span) = **box_atom {
                    let idx = self.compile_expr(expr_span)?.into_int_value();
                    HeapArray::index(self, id_span, idx).map(|ptr_val| ptr_val.into())
                } else {
                    todo!()
                }
            }
            Expr::Atom(Atom::ListOp(list_op_span)) => match list_op_span.into_inner() {
                ListOp::Head(expr_span) => {
                    let ptr = self.compile_expr(expr_span)?.into_pointer_value();
                    HeapList::head(self, ptr).map(|p| p.as_basic_value_enum())
                }
                ListOp::Tail(expr_span) => {
                    let ptr = self.compile_expr(expr_span)?.into_pointer_value();
                    HeapList::tail(self, ptr).map(|p| p.as_basic_value_enum())
                }
                ListOp::NilQ(expr_span) => {
                    let ptr = self.compile_expr(expr_span)?.into_pointer_value();
                    Ok(self
                        .builder
                        .build_is_null(ptr, "isnullcheck")
                        .as_basic_value_enum())
                }
                ListOp::Cons(head_span, tail_span) => {
                    let head_val = self.compile_expr(head_span)?;
                    if head_val.is_pointer_value() {
                        let tail_ptr = self.compile_expr(tail_span)?.into_pointer_value();
                        Ok(HeapList::cons(self, head_val.into_pointer_value(), tail_ptr).into())
                    } else {
                        let head_ptr = self
                            .builder
                            .build_alloca(head_val.get_type(), "tmp_ref_cons");
                        self.builder.build_store(head_ptr, head_val);
                        let tail_ptr = self.compile_expr(tail_span)?.into_pointer_value();
                        Ok(HeapList::cons(self, head_ptr, tail_ptr).into())
                    }
                }
            },
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
            Expr::New(type_span, expr_span) => {
                let len = self.compile_expr(expr_span)?.into_int_value();
                Ok(HeapArray::new(self, type_span, len).into())
            }
            Expr::Nil => Ok(self
                .context
                .i64_type()
                .ptr_type(AddressSpace::Generic)
                .const_null()
                .into()),
        }
    }

    fn compile_simple(&mut self, simple: &Span<Simple>) -> Result<(), TonyError> {
        match simple.into_inner() {
            Simple::Skip => Ok(()), // Skip is NOP
            Simple::Call(call @ Call(_, _)) => {
                let _ = self.compile_call(call)?;
                Ok(())
            }
            Simple::Assignment(Atom::StringLiteral(_), _) => Ok(()), // NOP?
            Simple::Assignment(Atom::AtomIndex(box_atom, index_expr_span), expr_span) => {
                if let Atom::Id(ref id_span) = **box_atom {
                    let idx = self.compile_expr(index_expr_span)?.into_int_value();
                    let ptr = HeapArray::index(self, id_span, idx)?;
                    let var_val = self.compile_expr(expr_span)?;
                    self.builder
                        .build_store(ptr, auto_deref!(var_val, self.builder));
                    Ok(())
                } else {
                    todo!()
                }
            }
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
                match (
                    var.ptr.get_type().get_element_type().is_pointer_type(),
                    var_val.is_pointer_value(),
                ) {
                    (true, true) if var.def.var.tony_type.is_array() => {
                        HeapArray::decref(self, var.ptr)?;
                        self.builder.build_store(var.ptr, var_val);
                    }
                    (true, true) => {
                        self.builder.build_store(
                            self.builder
                                .build_load(var.ptr, "tmpload")
                                .into_pointer_value(),
                            self.builder
                                .build_load(var_val.into_pointer_value(), "tmpload"),
                        );
                    }
                    (true, false) => {
                        self.builder.build_store(
                            self.builder
                                .build_load(var.ptr, "tmpload")
                                .into_pointer_value(),
                            var_val,
                        );
                    }
                    (false, true) => {
                        self.builder.build_store(
                            var.ptr,
                            self.builder
                                .build_load(var_val.into_pointer_value(), "tmpload"),
                        );
                    }
                    (false, false) => {
                        self.builder.build_store(var.ptr, var_val);
                    }
                }

                return Ok(());
            }
            Simple::Assignment(Atom::Call(call), expr_span) => {
                let var_val = self.compile_expr(expr_span)?;
                let var = self.compile_call(call)?.unwrap();
                if !var.get_type().is_pointer_type() {
                    return Ok(());
                }
                let var = var.into_pointer_value();
                let var_type = self.symbol_tables.expr_type_check(
                    Some(self.scope_uuid),
                    expr_span.into_inner(),
                    self.function,
                )?;
                match (
                    var.get_type().get_element_type().is_pointer_type(),
                    var_val.is_pointer_value(),
                ) {
                    (true, true) if var_type.is_array() => {
                        HeapArray::decref(self, var)?;
                        self.builder.build_store(var, var_val);
                    }
                    (true, true) => {
                        self.builder.build_store(
                            self.builder.build_load(var, "tmpload").into_pointer_value(),
                            self.builder
                                .build_load(var_val.into_pointer_value(), "tmpload"),
                        );
                    }
                    (true, false) => {
                        self.builder.build_store(
                            self.builder.build_load(var, "tmpload").into_pointer_value(),
                            var_val,
                        );
                    }
                    (false, true) => {
                        self.builder.build_store(
                            var,
                            self.builder
                                .build_load(var_val.into_pointer_value(), "tmpload"),
                        );
                    }
                    (false, false) => {
                        self.builder.build_store(var, var_val);
                    }
                }

                return Ok(());
            }
            Simple::Assignment(Atom::ListOp(_), _) => todo!(),
        }
    }

    /// Compiles the specified `Stmt` into an LLVM `IntValue`.
    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<bool, TonyError> {
        //println!("compiling stmt {:?}", stmt);
        match stmt {
            Stmt::Simple(simple_span) => {
                self.compile_simple(simple_span)?;
                Ok(false)
            }
            Stmt::Exit => {
                self.compile_return_block()?;
                if self.function.header.0.var.as_str() == "main" {
                    self.builder
                        .build_return(Some(&self.context.i64_type().const_zero()));
                } else {
                    self.builder.build_return(None);
                }
                Ok(true)
            }
            Stmt::Return(expr_span) => {
                let ret = self.compile_expr(expr_span)?;
                self.compile_return_block()?;
                self.builder.build_return(Some(&ret));
                Ok(true)
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
                    self.debug_helper.set_current_debug_location(
                        self.context,
                        self.builder,
                        self.debug_info_function_lexical_block.as_debug_info_scope(),
                        stmt.span(),
                    );
                    if self.compile_stmt(stmt.into_inner())? {
                        break;
                    };
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
                    self.debug_helper.set_current_debug_location(
                        self.context,
                        self.builder,
                        self.debug_info_function_lexical_block.as_debug_info_scope(),
                        stmt.span(),
                    );
                    if self.compile_stmt(stmt.into_inner())? {
                        break;
                    };
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

                let current_block = self.builder.get_insert_block().unwrap();
                Ok(current_block.get_terminator().is_some())
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
                    self.debug_helper.set_current_debug_location(
                        self.context,
                        self.builder,
                        self.debug_info_function_lexical_block.as_debug_info_scope(),
                        stmt.span(),
                    );
                    if self.compile_stmt(stmt.into_inner())? {
                        break;
                    };
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
                Ok(cont_bb.get_terminator().is_some())
            }
        }
    }

    fn compile_call(&mut self, call: &Call) -> Result<Option<BasicValueEnum<'ctx>>, TonyError> {
        let Call(ident_span, arg_spans) = call;
        match self.get_function(ident_span.as_str()) {
            Some(fun) => {
                let mut compiled_args = Vec::with_capacity(arg_spans.len());
                let param_types = fun.get_type().get_param_types();
                let funcdef = self
                    .symbol_tables
                    .get_funcdef(Some(self.scope_uuid), ident_span)
                    .unwrap();
                let mut stack_allocations = vec![];

                for (i, arg) in arg_spans.into_iter().enumerate() {
                    if param_types[i].is_pointer_type() {
                        let ref_arg = match arg.into_inner() {
                            Expr::Atom(Atom::Id(id_span)) => {
                                let name = &id_span.into_inner().0.to_string();
                                let var = self.variables.get(name.as_str()).ok_or_else(|| {
                                    TonyError::with_span(
                                        format!(
                                            "Undefined variable, found {:?}",
                                            id_span.into_inner()
                                        ),
                                        id_span.span(),
                                    )
                                    .set_symbol_table_kind()
                                })?;
                                if var.def.var.tony_type.is_array() {
                                    HeapArray::incref(self, var.ptr)?;
                                }
                                if var.ptr.get_type().get_element_type().is_pointer_type() {
                                    self.builder
                                        .build_load(var.ptr, name.as_str())
                                        .into_pointer_value()
                                } else {
                                    var.ptr
                                }
                            }
                            Expr::Atom(Atom::AtomIndex(box_atom, ref expr_span)) => {
                                if let Atom::Id(ref id_span) = **box_atom {
                                    let idx = self.compile_expr(expr_span)?.into_int_value();
                                    HeapArray::index(self, id_span, idx)?
                                } else {
                                    todo!()
                                }
                            }
                            _ => {
                                let evaluation = self.compile_expr(arg)?;
                                if evaluation.is_pointer_value() {
                                    evaluation.into_pointer_value()
                                } else {
                                    let alloc =
                                        self.builder.build_alloca(param_types[i], "tmp_ref");
                                    self.builder.build_store(alloc, evaluation);
                                    stack_allocations.push(alloc);

                                    alloc
                                }
                            }
                        };
                        compiled_args.push(ref_arg.as_basic_value_enum());
                    } else {
                        compiled_args.push(auto_deref!(self.compile_expr(arg)?, self.builder));
                    }
                }

                let argsv: Vec<BasicValueEnum> = compiled_args
                    .iter()
                    .cloned()
                    .map(|val| val.into())
                    .collect();
                for (i, ptr) in compiled_args.iter().enumerate() {
                    if (funcdef.header.1)[i].var.tony_type.into_inner().is_array() {
                        HeapArray::incref2(self, ptr.into_pointer_value())?;
                    }
                }

                let val = self.builder.build_call(fun, argsv.as_slice(), "tmp");
                for alloc in stack_allocations {
                    self.builder.build_free(alloc);
                }
                for (i, ptr) in compiled_args.into_iter().enumerate() {
                    if (funcdef.header.1)[i].var.tony_type.into_inner().is_array() {
                        HeapArray::decref2(self, ptr.into_pointer_value())?;
                    }
                }
                if funcdef.return_type().is_array() {
                    /* Take ownership */
                    let ptr =
                        self.create_entry_block_alloca("retval", funcdef.return_type(), false);
                    self.builder
                        .build_store(ptr, val.try_as_basic_value().left().unwrap());
                    self.variables.insert(
                        "retval".into(),
                        Variable {
                            ptr,
                            def: (funcdef.header.0).clone(),
                        },
                    );
                }
                Ok(val.try_as_basic_value().left())
            }
            None => Err(TonyError::with_span(
                format!("Undefined function, found {:?}", ident_span.into_inner()),
                ident_span.span(),
            )
            .set_typecheck_kind()),
        }
    }

    fn compile_return_block(&self) -> Result<(), TonyError> {
        for Variable { ptr, def } in self.variables.values() {
            if def.var.tony_type.is_array() {
                HeapArray::decref(self, *ptr)?;
            }
        }
        Ok(())
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(
        &self,
        (fn_vardef, arg_decls): &(Formal, Vec<Formal>),
    ) -> Result<FunctionValue<'ctx>, TonyError> {
        let args_types = arg_decls
            .iter()
            .map(|f| {
                if f.is_ref {
                    Compiler::tonytype_into_basictypenum_ref(
                        &self.context,
                        f.var.tony_type.into_inner(),
                    )
                } else {
                    Compiler::tonytype_into_basictypenum(
                        &self.context,
                        f.var.tony_type.into_inner(),
                    )
                }
            })
            .collect::<Vec<BasicTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = if fn_vardef.var.as_str() == "main" {
            self.context.i64_type().fn_type(&[], false)
        } else {
            Compiler::tonytype_into_fn_typenum(
                &self.context,
                fn_vardef.var.tony_type.into_inner(),
                fn_vardef.is_ref,
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
                    TonyType::Unit => unreachable!(),
                    TonyType::Any => unreachable!(),
                    TonyType::Array(_) => {
                        arg.into_pointer_value().set_name(arg_decls[i].var.as_str())
                    }
                    TonyType::List(_) => {
                        arg.into_pointer_value().set_name(arg_decls[i].var.as_str())
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
        function.set_subprogram(self.debug_info_function_scope);
        assert_eq!(
            Some(self.debug_info_function_scope),
            function.get_subprogram()
        );
        self.debug_helper.set_current_debug_location(
            self.context,
            self.builder,
            self.debug_info_function_lexical_block.as_debug_info_scope(),
            self.function.header.0.var.id.span(),
        );
        for decl_span in self.function.declarations.iter() {
            match decl_span.into_inner() {
                crate::ast::Decl::Func(funcdef_span) => {
                    let function = Self::compile(
                        self.context,
                        self.builder,
                        &self.debug_helper,
                        self.debug_info_function_scope.as_debug_info_scope(),
                        self.fpm,
                        self.module,
                        funcdef_span.into_inner(),
                        self.symbol_tables,
                        self.symbol_tables
                            .get_funcscope(Some(self.scope_uuid), funcdef_span.ident())
                            .unwrap(),
                    )?;
                    self.functions.insert(
                        funcdef_span.into_inner().header.0.var.as_str().to_string(),
                        function,
                    );
                }
                crate::ast::Decl::Var(_) => {}
            }
        }
        self.debug_helper.set_current_debug_location(
            self.context,
            self.builder,
            self.debug_info_function_lexical_block.as_debug_info_scope(),
            self.function.header.0.var.id.span(),
        );

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve((proto.1).len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = (proto.1)[i].var.as_str();
            let arg_type = (proto.1)[i].var.tony_type.into_inner();
            let is_ref = (proto.1)[i].is_ref;
            let alloca = self.create_entry_block_alloca(arg_name, arg_type, is_ref);

            self.builder.build_store(alloca, arg);

            self.variables.insert(
                (proto.1)[i].var.as_str().to_string(),
                Variable {
                    ptr: alloca,
                    def: (proto.1)[i].clone(),
                },
            );
        }

        for decl_span in self.function.declarations.iter() {
            match decl_span.into_inner() {
                crate::ast::Decl::Func(_) => {}
                crate::ast::Decl::Var(vardefs) => {
                    for vardef in vardefs {
                        let var_name = vardef.as_str();
                        let var_type = vardef.tony_type.into_inner();
                        let alloca = self
                            .create_entry_block_alloca(var_name, var_type, false /* is_ref */);

                        self.debug_helper.create_local_variable(
                            self.context,
                            self.builder,
                            self.debug_info_function_lexical_block.as_debug_info_scope(),
                            alloca,
                            vardef,
                            self.builder.get_insert_block().unwrap(),
                        );
                        self.variables.insert(
                            var_name.to_string(),
                            Variable {
                                ptr: alloca,
                                def: Formal {
                                    is_ref: false,
                                    var: vardef.clone(),
                                },
                            },
                        );
                    }
                }
            }
        }
        for stmt in self.function.body.iter() {
            self.debug_helper.set_current_debug_location(
                self.context,
                self.builder,
                self.debug_info_function_lexical_block.as_debug_info_scope(),
                stmt.span(),
            );
            if self.compile_stmt(stmt)? {
                break;
            };
        }
        let entry = self.builder.get_insert_block().unwrap();
        if entry.get_terminator().is_none() {
            if self.function.header.0.var.as_str() == "main" {
                self.builder.position_at_end(entry);
                self.compile_return_block()?;
                self.builder
                    .build_return(Some(&self.context.i64_type().const_zero()));
            } else if self.function.body.is_empty()
                || proto.0.var.tony_type.into_inner() == &TonyType::Unit
            {
                self.compile_return_block()?;
                self.builder.build_return(None);
            }
        }

        // return the whole thing after verification and optimization
        if true {
            //function.verify(true) {
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
        debug_helper: &'a DebugHelper<'a, 'ctx>,
        func_scope: DIScope<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        function: &'a FuncDef,
        env: &'a ProgramEnvironment,
        scope_uuid: &'a ScopeUuid,
    ) -> Result<FunctionValue<'ctx>, TonyError> {
        //builder.unset_current_debug_location();
        debug_helper.set_current_debug_location(
            context,
            builder,
            func_scope,
            function.header.0.var.id.span(),
        );
        let debug_info_function_scope = debug_helper.create_function(function, func_scope);
        let debug_info_function_lexical_block = debug_helper
            .create_lexical_block(debug_info_function_scope, function.header.0.var.id.span());
        let mut compiler = Compiler {
            context,
            builder,
            debug_helper,
            debug_info_function_scope,
            debug_info_function_lexical_block,
            fpm: pass_manager,
            module,
            function,
            fn_value_opt: None,
            variables: HashMap::new(),
            functions: HashMap::new(),
            symbol_tables: env,
            scope_uuid,
        };

        compiler.compile_fn()
    }

    fn tonytype_into_basictypenum_ref(context: &'ctx Context, t: &TonyType) -> BasicTypeEnum<'ctx> {
        Compiler::tonytype_into_basictypenum(context, t)
            .ptr_type(AddressSpace::Generic)
            .into()
    }

    fn tonytype_into_basictypenum(context: &'ctx Context, t: &TonyType) -> BasicTypeEnum<'ctx> {
        match t {
            TonyType::Any => unreachable!(),
            TonyType::Int => context.i64_type().into(),
            TonyType::Bool => context.bool_type().into(),
            TonyType::Char => context.i8_type().into(),
            TonyType::Unit => todo!(),
            TonyType::Array(type_span) | TonyType::List(type_span) => {
                Compiler::tonytype_into_basictypenum_ref(context, type_span.into_inner())
            }
        }
    }

    fn tonytype_into_fn_typenum(
        context: &'ctx Context,
        t: &TonyType,
        is_ref: bool,
        args: &[BasicTypeEnum<'ctx>],
    ) -> FunctionType<'ctx> {
        if is_ref {
            match t {
                TonyType::Any => unreachable!(),
                TonyType::Int => context
                    .i64_type()
                    .ptr_type(AddressSpace::Generic)
                    .fn_type(args, false),
                TonyType::Bool => context
                    .bool_type()
                    .ptr_type(AddressSpace::Generic)
                    .fn_type(args, false),
                TonyType::Char => context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .fn_type(args, false),
                TonyType::Unit => context.void_type().fn_type(args, false),
                TonyType::Array(type_span) | TonyType::List(type_span) => {
                    Compiler::tonytype_into_basictypenum_ref(context, type_span.into_inner())
                        .ptr_type(AddressSpace::Generic)
                        .fn_type(args, false)
                }
            }
        } else {
            match t {
                TonyType::Any => unreachable!(),
                TonyType::Int => context.i64_type().fn_type(args, false),
                TonyType::Bool => context.bool_type().fn_type(args, false),
                TonyType::Char => context.i8_type().fn_type(args, false),
                TonyType::Unit => context.void_type().fn_type(args, false),
                TonyType::Array(type_span) | TonyType::List(type_span) => {
                    Compiler::tonytype_into_basictypenum_ref(context, type_span.into_inner())
                        .fn_type(args, false)
                }
            }
        }
    }

    fn tonytype_default_value(
        context: &'ctx Context,
        t: &TonyType,
        ptr: PointerValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        if ptr.get_type().get_element_type().is_pointer_type() {
            ptr.get_type()
                .get_element_type()
                .into_pointer_type()
                .const_null()
                .into()
        } else {
            match t {
                TonyType::Any => unreachable!(),
                TonyType::Unit => unreachable!(),
                TonyType::Bool | TonyType::Int | TonyType::Char => ptr
                    .get_type()
                    .get_element_type()
                    .into_int_type()
                    .const_zero()
                    .into(),
                TonyType::Array(type_span) => {
                    Compiler::tonytype_default_value(context, type_span.into_inner(), ptr)
                }
                TonyType::List(_) => context
                    .i64_type()
                    .ptr_type(AddressSpace::Generic)
                    .const_null()
                    .into(),
            }
        }
    }
}
