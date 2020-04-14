use crate::ast;
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
        ident: ast::Identifier,
        tony_type: ast::TonyType,
    },
}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Variable { ref uuid, .. } => uuid.hash(state),
            Symbol::Function { ref uuid, .. } => uuid.hash(state),
            Symbol::BuiltinFunction { ref ident, .. } => ident.hash(state),
        }
    }
}

#[derive(Debug)]
pub struct ProgramEnvironment {
    source_code: String,
    symbol_tables: HashMap<ScopeUuid, SymbolTable>,
    global_scope_uuid: ScopeUuid,
}

macro_rules! this_scope {
    ($symbol_tables:ident[$scope_key:expr]) => {
        $symbol_tables.get_mut($scope_key).unwrap()
    };
}
impl ProgramEnvironment {
    pub fn new_environment(source_code: String) -> Self {
        let global_scope = SymbolTable::new_scope(None);
        let global_scope_uuid = global_scope.uuid.clone();
        let symbol_tables: HashMap<ScopeUuid, SymbolTable> =
            vec![(global_scope.uuid.clone(), global_scope)]
                .into_iter()
                .collect();
        assert!(symbol_tables.contains_key(&global_scope_uuid));

        let mut ret = ProgramEnvironment {
            source_code,
            symbol_tables,
            global_scope_uuid,
        };
        ret.insert_builtin_func("puti");
        ret.insert_builtin_func("putb");
        ret.insert_builtin_func("putc");
        ret.insert_builtin_func("puts");
        ret.insert_builtin_func("geti");
        ret.insert_builtin_func("getb");
        ret.insert_builtin_func("getc");
        ret.insert_builtin_func("gets");
        ret.insert_builtin_func("abs");
        ret.insert_builtin_func("ord");
        ret.insert_builtin_func("chr");
        ret.insert_builtin_func("strlen");
        ret.insert_builtin_func("strcmp");
        ret.insert_builtin_func("strcpy");
        ret.insert_builtin_func("strcat");
        ret.insert_builtin_func("readInteger");

        ret
    }

    #[inline(always)]
    fn insert_builtin_func(&mut self, value: &'static str) {
        let ident = ast::Identifier(value.to_string());
        let symbol = Symbol::BuiltinFunction {
            ident: ident.clone(),
            tony_type: ast::TonyType::Unit,
        };
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
            for stmt in value.statements.iter().map(|span| span.into_inner()) {
                //println!("examining stmt {:#?} ", stmt);
                // Check that all statements refer to in-scope symbols.
                self.contains_stmt_symbol(Some(&new_scope_uuid), stmt)?;
            }
        }
        Ok(())
    }

    fn contains_stmt_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        stmt: &ast::Stmt,
    ) -> Result<(), TonyError> {
        match stmt {
            ast::Stmt::Simple(ast::Simple::Skip) => Ok(()),
            ast::Stmt::Simple(ast::Simple::Call(ast::Call(ident_span, exprs))) => {
                self.contains_func_symbol(scope_uuid, ident_span)?;
                exprs
                    .iter()
                    .map(|expr_span| self.contains_expr_symbol(scope_uuid, expr_span.into_inner()))
                    .collect::<Result<Vec<()>, TonyError>>()
                    .map(|_| ())?;
                Ok(())
            }
            ast::Stmt::Simple(ast::Simple::Assignment(atom, expr_span)) => {
                self.contains_atom_symbol(scope_uuid, atom)?;
                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())?;
                Ok(())
            }
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
            ast::Stmt::Control(ast::StmtType::For) => todo!(),
        }
    }

    fn contains_atom_symbol(
        &self,
        scope_uuid: Option<&ScopeUuid>,
        atom: &ast::Atom,
    ) -> Result<(), TonyError> {
        let ret = match atom {
            ast::Atom::Id(ident_span) => self.contains_var_symbol(scope_uuid, ident_span),
            ast::Atom::StringLiteral(_) => Ok(()),
            ast::Atom::AtomIndex(_expr) => todo!(),
            ast::Atom::Call(ast::Call(ident_span, exprs)) => {
                self.contains_func_symbol(scope_uuid, ident_span)?;
                exprs
                    .iter()
                    .map(|expr_span| self.contains_expr_symbol(scope_uuid, expr_span.into_inner()))
                    .collect::<Result<Vec<()>, TonyError>>()
                    .map(|_| ())?;
                Ok(())
            }
        };
        ret
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
                        self.source_code.clone(),
                        format!("func ident {:?} not found in scope", ident.into_inner()),
                        (ident.left, ident.right),
                    )
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
                        self.source_code.clone(),
                        format!("var ident {:?} not found in scope", ident.into_inner()),
                        (ident.left, ident.right),
                    )
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
        let ret = match expr {
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
            ast::Expr::New(type_span, expr_span) => {
                // TODO: check type

                self.contains_expr_symbol(scope_uuid, expr_span.into_inner())
            }
        };
        ret
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
