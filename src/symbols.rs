use crate::ast;
use crate::span;
use crate::TonyError;
use std::collections::HashMap;
use std::fmt;
use uuid::Uuid;

macro_rules! uuid_hash_type {
    ($n:ident) => {
        #[derive(PartialEq, Hash, Eq, Copy, Clone, Default)]
        pub struct $n(Uuid);

        impl fmt::Debug for $n {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl fmt::Display for $n {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl $n {
            fn new() -> Self {
                $n(Uuid::new_v4())
            }
            /*
            pub fn null() -> Self {
                $n(Uuid::nil())
            }
            */
        }
    };
}

uuid_hash_type!(VarUuid);
uuid_hash_type!(FuncUuid);
uuid_hash_type!(ScopeUuid);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    Variable {
        is_ref: bool,
        def: ast::VarDef,
        uuid: VarUuid,
    },
    Function {
        def: ast::FuncDef,
        uuid: FuncUuid,
        scope_uuid: ScopeUuid,
    },
    BuiltinFunction {
        def: ast::FuncDef,
    },
}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Variable { ref uuid, .. } => uuid.hash(state),
            Symbol::Function { ref uuid, .. } => uuid.hash(state),
            Symbol::BuiltinFunction { ref def, .. } => def.ident().hash(state),
        }
    }
}

#[derive(Debug)]
pub struct ProgramEnvironment {
    pub symbol_tables: HashMap<ScopeUuid, SymbolTable>,
    pub global_scope_uuid: ScopeUuid,
}

macro_rules! this_scope {
    ($symbol_tables:ident[$scope_key:expr]) => {
        $symbol_tables.get_mut($scope_key).unwrap()
    };
}
macro_rules! expected_type {
    ($t:expr, $expected:expr, $expr_span:expr) => {
        if $t != $expected {
            return Err(TonyError::with_span(
                format!("Expected type {:?}, found {:?}", $expected, $t),
                $expr_span.span(),
            )
            .set_typecheck_kind());
        }
    };
}
impl ProgramEnvironment {
    pub fn new_environment() -> Self {
        let global_scope = SymbolTable::new_scope(None);
        let global_scope_uuid = global_scope.uuid.clone();
        let symbol_tables: HashMap<ScopeUuid, SymbolTable> =
            vec![(global_scope.uuid.clone(), global_scope)]
                .into_iter()
                .collect();
        assert!(symbol_tables.contains_key(&global_scope_uuid));

        let mut ret = ProgramEnvironment {
            symbol_tables,
            global_scope_uuid,
        };
        for value in crate::builtins::builtins_to_funcdef() {
            ret.insert_builtin_func(value);
        }

        ret
    }

    fn insert_builtin_func(&mut self, value: ast::FuncDef) {
        let ident = value.ident().clone();
        let symbol = Symbol::BuiltinFunction { def: value };
        let len = self.symbol_tables[&self.global_scope_uuid].symbols.len();
        self.global_scope_mut()
            .symbols_index
            .insert(symbol.clone(), len);
        self.global_scope_mut().ident_index.insert(ident, len);
        self.global_scope_mut().symbols.push(symbol);
    }

    pub fn global_scope_mut(&mut self) -> &mut SymbolTable {
        self.symbol_tables.get_mut(&self.global_scope_uuid).unwrap()
    }

    pub fn insert_var(
        &mut self,
        scope_uuid: Option<&ScopeUuid>,
        value: ast::VarDef,
        is_ref: bool,
    ) -> Result<(), TonyError> {
        let Self {
            ref mut symbol_tables,
            ref global_scope_uuid,
            ..
        } = self;
        let scope_key = scope_uuid.unwrap_or(global_scope_uuid);
        let ident = value.id.into_inner().clone();
        //println!("inserting var {:?} to {}", &ident, scope_key);
        let symbol = Symbol::Variable {
            uuid: VarUuid::new(),
            def: value,
            is_ref,
        };
        // TODO check type
        let len = this_scope!(symbol_tables[&scope_key]).symbols.len();
        this_scope!(symbol_tables[&scope_key])
            .symbols_index
            .insert(symbol.clone(), len);
        this_scope!(symbol_tables[&scope_key])
            .ident_index
            .insert(ident, len);
        this_scope!(symbol_tables[&scope_key]).symbols.push(symbol);
        Ok(())
    }

    pub fn insert_global_func(&mut self, value: ast::FuncDef) -> Result<(), TonyError> {
        self.insert_func(None, value)
    }

    fn insert_func(
        &mut self,
        scope_uuid: Option<&ScopeUuid>,
        value: ast::FuncDef,
    ) -> Result<(), TonyError> {
        let new_scope_uuid;
        {
            let Self {
                ref mut symbol_tables,
                ref global_scope_uuid,
                ..
            } = self;
            let scope_key = scope_uuid.unwrap_or(global_scope_uuid);
            let new_scope =
                SymbolTable::new_scope(Some(this_scope!(symbol_tables[scope_key]).uuid.clone()));
            new_scope_uuid = new_scope.uuid.clone();
            {
                symbol_tables.insert(new_scope.uuid.clone(), new_scope);
                //println!(
                //    "{:?} inserting func {:#?}",
                //    this_scope!(symbol_tables[scope_key]),
                //    &value
                //);
                //for v in value.header.1.iter() {
                //println!(
                //    "scope {} inserting header arg {:#?}",
                //    this_scope!(symbol_tables[scope_key]).uuid,
                //    v
                //);
                //}
            }
        }
        {
            for (is_ref, vardef) in value.header.1.iter().map(|f| (f.is_ref, &f.var)) {
                self.insert_var(Some(&new_scope_uuid), vardef.clone(), is_ref)?;
            }
            for decl in value.declarations.iter().map(|span| span.into_inner()) {
                ////println!("inserting var/def {:#?}", decl);
                match decl {
                    ast::Decl::Func(span) => {
                        let func = span.into_inner();
                        self.insert_func(Some(&new_scope_uuid), func.clone())?; //new_scope.insert_func();
                    }
                    ast::Decl::Var(v) => {
                        for var in v {
                            self.insert_var(Some(&new_scope_uuid), var.clone(), false)?;
                        }
                    }
                }
            }
        }
        {
            let Self {
                ref mut symbol_tables,
                ref global_scope_uuid,
                ..
            } = self;
            let scope_key = scope_uuid.unwrap_or(global_scope_uuid);
            let ident = value.header.0.var.id.into_inner().clone();
            let symbol = Symbol::Function {
                uuid: FuncUuid::new(),
                def: value.clone(),
                scope_uuid: new_scope_uuid,
            };

            //println!("inserting func {:?} to {}", &ident, scope_key);
            let len = this_scope!(symbol_tables[scope_key]).symbols.len();
            this_scope!(symbol_tables[scope_key])
                .symbols_index
                .insert(symbol.clone(), len);
            this_scope!(symbol_tables[scope_key])
                .ident_index
                .insert(ident, len);
            this_scope!(symbol_tables[scope_key]).symbols.push(symbol);
        }
        {
            for stmt in value.body.iter() {
                //println!("examining stmt {:#?} ", stmt);
                // Check that all statements refer to in-scope symbols.
                self.contains_stmt_symbol(Some(&new_scope_uuid), stmt.into_inner())?;
                self.type_check(Some(&new_scope_uuid), stmt, &value)?;
            }
            terminating_analysis(&value, &value.body)?;
        }
        Ok(())
    }

    fn contains_stmt_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        stmt: &ast::Stmt,
    ) -> Result<(), TonyError> {
        match stmt {
            ast::Stmt::Simple(simple) => self.contains_simple_symbol(scope_uuid, simple),
            ast::Stmt::Exit => Ok(()),
            ast::Stmt::Return(expr_span) => {
                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())
            }
            ast::Stmt::Control(ast::StmtType::If {
                condition: condition_expr_span,
                body: body_stmt_spans,
                _else: _else_stmt_spans,
            }) => {
                self.contains_expr_symbol(scope_uuid, condition_expr_span.into_inner())?;
                body_stmt_spans
                    .iter()
                    .map(ast::Span::into_inner)
                    .map(|stmt| self.contains_stmt_symbol(scope_uuid, stmt))
                    .collect::<Result<Vec<()>, TonyError>>()?;
                _else_stmt_spans
                    .iter()
                    .map(ast::Span::into_inner)
                    .map(|stmt| self.contains_stmt_symbol(scope_uuid, stmt))
                    .collect::<Result<Vec<()>, TonyError>>()?;
                Ok(())
            }
            ast::Stmt::Control(ast::StmtType::For {
                init,
                condition: condition_expr_span,
                eval,
                body: body_stmt_spans,
            }) => {
                self.contains_expr_symbol(scope_uuid, condition_expr_span.into_inner())?;
                init.iter()
                    .map(|simple| self.contains_simple_symbol(scope_uuid, simple))
                    .collect::<Result<Vec<()>, TonyError>>()?;
                eval.iter()
                    .map(|simple| self.contains_simple_symbol(scope_uuid, simple))
                    .collect::<Result<Vec<()>, TonyError>>()?;

                body_stmt_spans
                    .iter()
                    .map(ast::Span::into_inner)
                    .map(|stmt| self.contains_stmt_symbol(scope_uuid, stmt))
                    .collect::<Result<Vec<()>, TonyError>>()?;
                Ok(())
            }
        }
    }

    fn contains_simple_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        simple: &ast::Simple,
    ) -> Result<(), TonyError> {
        match simple {
            ast::Simple::Skip => Ok(()),
            ast::Simple::Call(ast::Call(ident_span, exprs)) => {
                self.contains_func_symbol(scope_uuid, ident_span)?;
                exprs
                    .iter()
                    .map(|expr_span| self.contains_expr_symbol(scope_uuid, expr_span.into_inner()))
                    .collect::<Result<Vec<()>, TonyError>>()
                    .map(|_| ())?;
                Ok(())
            }
            ast::Simple::Assignment(atom, expr_span) => {
                self.contains_atom_symbol(scope_uuid, atom)?;
                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())?;
                Ok(())
            }
        }
    }

    fn contains_atom_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        atom: &ast::Atom,
    ) -> Result<(), TonyError> {
        match atom {
            ast::Atom::Id(ident_span) => self.contains_var_symbol(scope_uuid, ident_span),
            ast::Atom::StringLiteral(_) => Ok(()),
            ast::Atom::AtomIndex(atom, expr) => {
                self.contains_atom_symbol(scope_uuid, atom)?;
                self.contains_expr_symbol(scope_uuid, expr)
            }
            ast::Atom::Call(ast::Call(ident_span, exprs)) => {
                self.contains_func_symbol(scope_uuid, ident_span)?;
                exprs
                    .iter()
                    .map(|expr_span| self.contains_expr_symbol(scope_uuid, expr_span.into_inner()))
                    .collect::<Result<Vec<()>, TonyError>>()
                    .map(|_| ())?;
                Ok(())
            }
            ast::Atom::ListOp(list_op_span) => match list_op_span.into_inner() {
                ast::ListOp::Head(expr_span)
                | ast::ListOp::Tail(expr_span)
                | ast::ListOp::NilQ(expr_span) => self.contains_expr_symbol(scope_uuid, expr_span),
                ast::ListOp::Cons(left_span, right_span) => {
                    self.contains_expr_symbol(scope_uuid, left_span)?;
                    self.contains_expr_symbol(scope_uuid, right_span)
                }
            },
        }
    }

    fn contains_func_symbol<'global>(
        &'global self,
        mut scope_uuid: Option<&'global ScopeUuid>,
        ident: &ast::Span<ast::Identifier>,
    ) -> Result<(), TonyError> {
        loop {
            let scope_key = scope_uuid.unwrap_or(&self.global_scope_uuid);
            //println!("contains_func_symbol {:?}", ident);
            let ret = self.symbol_tables[scope_key]
                .ident_index
                .get(ident.into_inner())
                .and_then(|&idx| self.symbol_tables[scope_key].symbols.get(idx))
                .and_then(|symbol| match symbol {
                    Symbol::Function { .. } | Symbol::BuiltinFunction { .. } => Some(()),
                    Symbol::Variable { .. } => None,
                })
                .ok_or_else(|| {
                    TonyError::with_span(
                        format!("func ident {:?} not found in scope", ident.into_inner()),
                        ident.span(),
                    )
                    .set_symbol_table_kind()
                });
            if ret.is_ok() || scope_uuid.is_none() {
                return ret;
            }
            scope_uuid = self.symbol_tables[scope_key].parent_scope.as_ref();
        }
    }

    fn contains_var_symbol<'global>(
        &'global self,
        mut scope_uuid: Option<&'global ScopeUuid>,
        ident: &ast::Span<ast::Identifier>,
    ) -> Result<(), TonyError> {
        loop {
            let scope_key = scope_uuid.unwrap_or(&self.global_scope_uuid);
            let ret = self.symbol_tables[scope_key]
                .ident_index
                .get(ident.into_inner())
                .and_then(|&idx| self.symbol_tables[scope_key].symbols.get(idx))
                .and_then(|symbol| match symbol {
                    Symbol::Variable { .. } => Some(()),
                    Symbol::BuiltinFunction { .. } | Symbol::Function { .. } => None,
                })
                .ok_or_else(|| {
                    TonyError::with_span(
                        format!("var ident {:?} not found in scope", ident.into_inner()),
                        ident.span(),
                    )
                    .set_symbol_table_kind()
                });
            if ret.is_ok() || scope_uuid.is_none() {
                return ret;
            }
            scope_uuid = self.symbol_tables[scope_key].parent_scope.as_ref();
        }
    }

    fn contains_expr_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        expr: &ast::Expr,
    ) -> Result<(), TonyError> {
        match expr {
            ast::Expr::Atom(atom) => self.contains_atom_symbol(scope_uuid, atom),
            ast::Expr::IntConst(_)
            | ast::Expr::CharConst(_)
            | ast::Expr::True
            | ast::Expr::False
            | ast::Expr::Nil => Ok(()),
            ast::Expr::Not(expr_span) => {
                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())
            }
            ast::Expr::And(expr_span_1, expr_span_2) => {
                self.contains_expr_symbol(scope_uuid, expr_span_1.into_inner())?;
                self.contains_expr_symbol(scope_uuid, expr_span_2.into_inner())
            }
            ast::Expr::Or(expr_span_1, expr_span_2) => {
                self.contains_expr_symbol(scope_uuid, expr_span_1.into_inner())?;
                self.contains_expr_symbol(scope_uuid, expr_span_2.into_inner())
            }
            ast::Expr::Minus(expr_span) => {
                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())
            }
            ast::Expr::Op(expr_span_1, _, expr_span_2) => {
                self.contains_expr_symbol(scope_uuid, expr_span_1.into_inner())?;
                self.contains_expr_symbol(scope_uuid, expr_span_2.into_inner())
            }
            ast::Expr::New(_type_span, expr_span) => {
                // TODO: check type

                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())
            }
        }
    }

    fn type_check(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        stmt: &ast::Span<ast::Stmt>,
        func_def: &ast::FuncDef,
    ) -> Result<(), TonyError> {
        match stmt.into_inner() {
            ast::Stmt::Simple(simple) => {
                self.simple_type_check(scope_uuid, &simple, func_def)?;
            }
            ast::Stmt::Exit => {
                if func_def.return_type() != &ast::TonyType::Unit {
                    return Err(TonyError::with_span(
                        format!(
                            "Function {} can't use 'exit'; its return type is {:?} ",
                            func_def.ident().0,
                            func_def.return_type()
                        ),
                        func_def.header.0.var.id.span(),
                    )
                    .set_typecheck_kind());
                }
            }
            ast::Stmt::Return(expr_span) => {
                if func_def.return_type()
                    != &self.expr_type_check(scope_uuid, &expr_span, func_def)?
                {
                    return Err(TonyError::with_span(
                        format!(
                            "Function {} has a {:?} return type",
                            func_def.ident().0,
                            func_def.return_type()
                        ),
                        func_def.header.0.var.id.span(),
                    )
                    .set_typecheck_kind());
                }
                if func_def.return_type() == &ast::TonyType::Unit {
                    return Err(TonyError::with_span(
                        format!(
                            "Function {} has a {:?} return type, but uses a `return` statement. (Use `exit`)",
                            func_def.ident().0,
                            func_def.return_type()
                        ),
                        func_def.header.0.var.id.span(),
                    )
                    .set_typecheck_kind());
                }
            }
            ast::Stmt::Control(ast::StmtType::If {
                condition: condition_expr_span,
                body: body_stmt_spans,
                _else: _else_stmt_spans,
            }) => {
                if self.expr_type_check(scope_uuid, &condition_expr_span, func_def)?
                    != ast::TonyType::Bool
                {
                    return Err(TonyError::with_span(
                        format!("If condition must be a bool expression"),
                        condition_expr_span.span(),
                    )
                    .set_typecheck_kind());
                }
                for stmt in body_stmt_spans {
                    self.type_check(scope_uuid, stmt, func_def)?;
                }
                for stmt in _else_stmt_spans {
                    self.type_check(scope_uuid, stmt, func_def)?;
                }
            }
            ast::Stmt::Control(ast::StmtType::For {
                init,
                condition: condition_expr_span,
                eval,
                body: body_stmt_spans,
            }) => {
                for simple in init {
                    self.simple_type_check(scope_uuid, simple, func_def)?;
                }
                if self.expr_type_check(scope_uuid, &condition_expr_span, func_def)?
                    != ast::TonyType::Bool
                {
                    return Err(TonyError::with_span(
                        format!("For condition must be a bool expression"),
                        func_def.header.0.var.id.span(),
                    )
                    .set_typecheck_kind());
                }
                for simple in eval {
                    self.simple_type_check(scope_uuid, simple, func_def)?;
                }
                for stmt in body_stmt_spans {
                    self.type_check(scope_uuid, stmt, func_def)?;
                }
            }
        }
        Ok(())
    }

    fn atom_type_check(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        atom: &ast::Atom,
        func_def: &ast::FuncDef,
    ) -> Result<ast::TonyType, TonyError> {
        match atom {
            ast::Atom::Id(ident_span) => Ok(self
                .get_vardef(scope_uuid, ident_span.into_inner())
                .unwrap()
                .tony_type
                .into_inner()
                .clone()),
            ast::Atom::StringLiteral(_) => Ok(ast::TonyType::Array(Box::new(
                span![ast::TonyType::Char; 0,0],
            ))),
            ast::Atom::AtomIndex(atom, expr) => {
                let t = self.atom_type_check(scope_uuid, atom, func_def)?;
                let inner_type: ast::TonyType;
                match t {
                    ast::TonyType::Array(i) => {
                        inner_type = i.into_inner().clone();
                    }
                    _ => {
                        return Err(TonyError::with_span(
                            format!("You cannot index var {:?}, it is of type {:?}.", &atom, t),
                            expr.span(),
                        )
                        .set_symbol_table_kind());
                    }
                }
                let idx_t = self.expr_type_check(scope_uuid, expr, func_def)?;
                expected_type!(idx_t, ast::TonyType::Int, expr);
                Ok(inner_type)
            }
            ast::Atom::Call(ast::Call(ident_span, exprs)) => {
                let def = self
                    .get_funcdef(scope_uuid, ident_span.into_inner())
                    .unwrap();
                if def.header.1.len() != exprs.len() {
                    return Err(TonyError::with_span(
                        format!(
                            "Func {} expects {} arguments, found {}.",
                            &ident_span.into_inner().0,
                            def.header.1.len(),
                            exprs.len()
                        ),
                        ident_span.span(),
                    )
                    .set_symbol_table_kind());
                }
                for (i, formal) in def.header.1.iter().enumerate() {
                    let expr_type = self.expr_type_check(scope_uuid, &exprs[i], func_def)?;
                    expected_type!(expr_type, *formal.var.tony_type.into_inner(), &exprs[i]);
                }
                Ok(def.return_type().clone())
            }
            ast::Atom::ListOp(list_op_span) => match list_op_span.into_inner() {
                ast::ListOp::Head(expr_span) => {
                    let expr_type = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                    let inner_type = match expr_type {
                        ast::TonyType::List(inner_type) => inner_type,
                        other => {
                            return Err(TonyError::with_span(
                                format!("Expected type List[_], found {:?}", other),
                                expr_span.span(),
                            )
                            .set_typecheck_kind());
                        }
                    };
                    Ok(inner_type.into_inner().clone())
                }
                ast::ListOp::NilQ(expr_span) => {
                    let expr_type = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                    match expr_type {
                        ast::TonyType::List(_) => {}
                        other => {
                            return Err(TonyError::with_span(
                                format!("Expected type List[_], found {:?}", other),
                                expr_span.span(),
                            )
                            .set_typecheck_kind());
                        }
                    };
                    Ok(ast::TonyType::Bool)
                }
                ast::ListOp::Tail(expr_span) => {
                    let expr_type = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                    match expr_type {
                        ast::TonyType::List(_) => {}
                        other => {
                            return Err(TonyError::with_span(
                                format!("Expected type List[_], found {:?}", other),
                                expr_span.span(),
                            )
                            .set_typecheck_kind());
                        }
                    };
                    Ok(expr_type.clone())
                }
                ast::ListOp::Cons(left_span, right_span) => {
                    let left_type = self.expr_type_check(scope_uuid, left_span, func_def)?;
                    let right_type = self.expr_type_check(scope_uuid, right_span, func_def)?;
                    let inner_type = match right_type {
                        ast::TonyType::List(inner_type) => inner_type,
                        other => {
                            return Err(TonyError::with_span(
                                format!("Expected type List[_], found {:?}", other),
                                right_span.span(),
                            )
                            .set_typecheck_kind());
                        }
                    };
                    expected_type!(left_type, *inner_type.into_inner(), left_span);
                    Ok(ast::TonyType::List(inner_type.clone()))
                }
            },
        }
    }

    pub fn expr_type_check(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        expr: &ast::Expr,
        func_def: &ast::FuncDef,
    ) -> Result<ast::TonyType, TonyError> {
        match expr {
            ast::Expr::Atom(atom) => self.atom_type_check(scope_uuid, atom, func_def),
            ast::Expr::IntConst(_) => Ok(ast::TonyType::Int),
            ast::Expr::CharConst(_) => Ok(ast::TonyType::Char),
            ast::Expr::True | ast::Expr::False => Ok(ast::TonyType::Bool),
            ast::Expr::Nil => Ok(ast::TonyType::List(Box::new(
                span![ast::TonyType::Any; 0,0],
            ))),
            ast::Expr::Not(expr_span) => {
                let t = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                expected_type!(t, ast::TonyType::Bool, expr_span);
                Ok(ast::TonyType::Bool)
            }
            ast::Expr::And(expr_span_1, expr_span_2) => {
                let t_1 = self.expr_type_check(scope_uuid, expr_span_1, func_def)?;
                expected_type!(t_1, ast::TonyType::Bool, expr_span_1);
                let t_2 = self.expr_type_check(scope_uuid, expr_span_2, func_def)?;
                expected_type!(t_2, ast::TonyType::Bool, expr_span_2);
                Ok(ast::TonyType::Bool)
            }
            ast::Expr::Or(expr_span_1, expr_span_2) => {
                let t_1 = self.expr_type_check(scope_uuid, expr_span_1, func_def)?;
                expected_type!(t_1, ast::TonyType::Bool, expr_span_1);
                let t_2 = self.expr_type_check(scope_uuid, expr_span_2, func_def)?;
                expected_type!(t_2, ast::TonyType::Bool, expr_span_2);
                Ok(ast::TonyType::Bool)
            }
            ast::Expr::Minus(expr_span) => {
                let t = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                expected_type!(t, ast::TonyType::Int, expr_span);
                Ok(t)
            }
            ast::Expr::Op(left, ast::Operator::Equals, right)
            | ast::Expr::Op(left, ast::Operator::NotEquals, right)
            | ast::Expr::Op(left, ast::Operator::Less, right)
            | ast::Expr::Op(left, ast::Operator::Greater, right)
            | ast::Expr::Op(left, ast::Operator::LessOrEqual, right)
            | ast::Expr::Op(left, ast::Operator::GreaterOrEqual, right) => {
                let t_1 = self.expr_type_check(scope_uuid, left, func_def)?;
                expected_type!(t_1, ast::TonyType::Int, left);
                let t_2 = self.expr_type_check(scope_uuid, right, func_def)?;
                expected_type!(t_2, ast::TonyType::Int, right);
                Ok(ast::TonyType::Bool)
            }
            ast::Expr::Op(left, _, right) => {
                let t_1 = self.expr_type_check(scope_uuid, left, func_def)?;
                expected_type!(t_1, ast::TonyType::Int, left);
                let t_2 = self.expr_type_check(scope_uuid, right, func_def)?;
                expected_type!(t_2, ast::TonyType::Int, right);
                Ok(ast::TonyType::Int)
            }
            ast::Expr::New(type_span, expr_span) => {
                let index_t = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                expected_type!(index_t, ast::TonyType::Int, expr_span);
                Ok(ast::TonyType::Array(Box::new(
                    span![type_span.into_inner().clone(); 0,0],
                )))
            }
        }
    }

    fn simple_type_check(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        simple: &ast::Span<ast::Simple>,
        func_def: &ast::FuncDef,
    ) -> Result<ast::TonyType, TonyError> {
        match simple.into_inner() {
            ast::Simple::Skip => Ok(ast::TonyType::Unit),
            ast::Simple::Call(ast::Call(ident_span, exprs)) => {
                let def = self
                    .get_funcdef(scope_uuid, ident_span.into_inner())
                    .unwrap();
                if def.header.1.len() != exprs.len() {
                    return Err(TonyError::with_span(
                        format!(
                            "Func {} expects {} arguments, found {}.",
                            &ident_span.into_inner().0,
                            def.header.1.len(),
                            exprs.len()
                        ),
                        ident_span.span(),
                    )
                    .set_symbol_table_kind());
                }
                for (i, formal) in def.header.1.iter().enumerate() {
                    let expr_type = self.expr_type_check(scope_uuid, &exprs[i], func_def)?;
                    expected_type!(expr_type, *formal.var.tony_type.into_inner(), &exprs[i]);
                }
                Ok(def.return_type().clone())
            }
            ast::Simple::Assignment(atom, expr_span) => {
                let atom_type = self.atom_type_check(scope_uuid, atom, func_def)?;
                if atom_type == ast::TonyType::Unit {
                    return Err(TonyError::with_span(
                        format!(
                            "Cannot assign to type {:?} in function {:?}",
                            atom_type,
                            func_def.ident().0,
                        ),
                        func_def.header.0.var.id.span(),
                    )
                    .set_typecheck_kind());
                }
                let t = self.expr_type_check(scope_uuid, expr_span, func_def)?;
                expected_type!(t, atom_type, expr_span);
                Ok(ast::TonyType::Unit)
            }
        }
    }

    pub fn get_funcdef<'global>(
        &'global self,
        mut scope_uuid: Option<&'global ScopeUuid>,
        ident: &ast::Identifier,
    ) -> Option<&ast::FuncDef> {
        loop {
            let scope_key = scope_uuid.unwrap_or(&self.global_scope_uuid);
            let table = &self.symbol_tables[scope_key];
            if let Some(&idx) = table.ident_index.get(ident) {
                match &table.symbols[idx] {
                    Symbol::Function { ref def, .. } | Symbol::BuiltinFunction { ref def, .. } => {
                        return Some(def);
                    }
                    _ => {}
                }
            }
            if scope_uuid.is_none() {
                return None;
            }
            scope_uuid = self.symbol_tables[scope_key].parent_scope.as_ref();
        }
    }

    fn get_vardef<'global>(
        &'global self,
        mut scope_uuid: Option<&'global ScopeUuid>,
        ident: &ast::Identifier,
    ) -> Option<&ast::VarDef> {
        loop {
            let scope_key = scope_uuid.unwrap_or(&self.global_scope_uuid);
            let table = &self.symbol_tables[scope_key];
            if let Some(&idx) = table.ident_index.get(ident) {
                if let Symbol::Variable { ref def, .. } = &table.symbols[idx] {
                    return Some(def);
                }
            }
            if scope_uuid.is_none() {
                return None;
            }
            scope_uuid = self.symbol_tables[scope_key].parent_scope.as_ref();
        }
    }

    pub fn get_funcscope<'global>(
        &'global self,
        mut scope_uuid: Option<&'global ScopeUuid>,
        ident: &ast::Identifier,
    ) -> Option<&'global ScopeUuid> {
        loop {
            let scope_key = scope_uuid.unwrap_or(&self.global_scope_uuid);
            let table = &self.symbol_tables[scope_key];
            if let Some(&idx) = table.ident_index.get(ident) {
                match &table.symbols[idx] {
                    Symbol::Function { ref scope_uuid, .. } => {
                        return Some(scope_uuid);
                    }
                    _ => {}
                }
            }
            if scope_uuid.is_none() {
                return None;
            }
            scope_uuid = self.symbol_tables[scope_key].parent_scope.as_ref();
        }
    }
}

pub struct SymbolTable {
    symbols: Vec<Symbol>,
    symbols_index: HashMap<Symbol, usize>,
    ident_index: HashMap<ast::Identifier, usize>,
    types: Vec<ast::TonyType>,
    uuid: ScopeUuid,
    parent_scope: Option<ScopeUuid>,
}

impl SymbolTable {
    pub fn new_scope(parent_scope: Option<ScopeUuid>) -> Self {
        SymbolTable {
            symbols: vec![],
            symbols_index: HashMap::default(),
            ident_index: HashMap::default(),
            types: vec![
                ast::TonyType::Int,
                ast::TonyType::Bool,
                ast::TonyType::Char,
                ast::TonyType::Unit,
            ],
            uuid: ScopeUuid::new(),
            parent_scope,
        }
    }
}

impl fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref p) = self.parent_scope {
            f.debug_struct("symbol table")
                .field("symbols", &self.symbols)
                .field("symbols_index", &self.symbols_index)
                .field("types", &self.types)
                .field("parent_scope", p)
                .field("uuid", &self.uuid)
                .finish()
        } else {
            f.debug_struct("global symbol table")
                .field("symbols", &self.symbols)
                .field("symbols_index", &self.symbols_index)
                .field("types", &self.types)
                .field("uuid", &self.uuid)
                .finish()
        }
    }
}

fn terminating_analysis(
    value: &ast::FuncDef,
    stmts: &[ast::Span<ast::Stmt>],
) -> Result<(), TonyError> {
    let last_stmt = stmts.last().map(|stmt_span| stmt_span.into_inner());
    match last_stmt {
        Some(ast::Stmt::Exit) | Some(ast::Stmt::Return(_)) => Ok(()),
        Some(ast::Stmt::Control(ast::StmtType::If { body, _else, .. })) => {
            let b = terminating_analysis(value, body);
            let e = terminating_analysis(value, _else);
            if b.is_ok() && e.is_ok() {
                return Ok(());
            }
            /* This is not a terminating block. */
            if b.is_err() {
                b
            } else {
                e
            }
        }
        Some(ast::Stmt::Control(ast::StmtType::For { body, .. })) => {
            terminating_analysis(value, body)
        }
        Some(_) | None => {
            if value.return_type() != &ast::TonyType::Unit {
                return Err(TonyError::with_span(
                    format!(
                        "Function {} has no return statement; its return type is {:?} ",
                        value.ident().0,
                        value.return_type()
                    ),
                    value.header.0.var.tony_type.span(),
                )
                .set_typecheck_kind());
            }
            Ok(())
        }
    }
}
