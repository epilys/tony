/*
 * meli -
 *
 * Copyright  Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

#[derive(Debug, Clone)]
pub struct Identifier(pub String);
#[derive(Debug, Clone)]
pub struct StringLiteral(pub String);
#[derive(Debug, Clone)]
pub struct CharConst(pub char);
#[derive(Debug, Clone)]
pub struct IntConst(pub f64);

#[derive(Debug, Clone)]
pub struct Span<T: std::fmt::Debug + Clone> {
    left: usize,
    right: usize,
    inner: T,
}

impl<T: std::fmt::Debug + Clone> Span<T> {
    pub fn into_inner(&self) -> &T {
        &self.inner
    }
    pub fn span(&self) -> (usize, usize) {
        (self.left, self.right)
    }

    pub fn new(inner: T, left: usize, right: usize) -> Span<T> {
        Span { left, right, inner }
    }
}

#[macro_export]
macro_rules! span {
    [$T:expr; $l:expr, $r:expr] => {
        crate::ast::Span::new($T, $l, $r)
    }
}

#[derive(Debug, Clone)]
pub struct Program(pub Vec<Span<FuncDef>>);

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub header: (Formal, Vec<Formal>),
    pub declarations: Vec<Span<Decl>>,
    pub statements: Vec<Span<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct Formal {
    pub is_ref: bool,
    pub var: VarDef,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub tony_type: Span<TonyType>,
    pub id: Span<Identifier>,
}

#[derive(Debug, Clone)]
pub enum TonyType {
    Int,
    Bool,
    Char,
    Unit,
    Array(Box<Span<TonyType>>),
    List(Box<Span<TonyType>>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Simple(Simple),
    Exit,
    Return(Box<Span<Expr>>),
    Control(StmtType),
}

#[derive(Debug, Clone)]
pub enum StmtType {
    If {
        condition: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
        _else: Vec<Span<Stmt>>,
    },
    For,
}

#[derive(Debug, Clone)]
pub enum Simple {
    Skip,
    Call(Call),
    Assignment(Atom, Box<Span<Expr>>),
}

#[derive(Debug, Clone)]
pub struct Call(pub Span<Identifier>, pub Vec<Span<Expr>>);

#[derive(Debug, Clone)]
pub enum Atom {
    Id(Span<Identifier>),
    StringLiteral(StringLiteral),
    AtomIndex(Box<Span<Expr>>),
    Call(Call),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    IntConst(IntConst),
    CharConst(CharConst),
    True,
    False,
    Not(Box<Span<Expr>>),
    And(Box<Span<Expr>>, Box<Span<Expr>>),
    Or(Box<Span<Expr>>, Box<Span<Expr>>),
    Minus(Box<Span<Expr>>),
    Op(Box<Span<Expr>>, Operator, Box<Span<Expr>>),
    New(Span<TonyType>, Box<Span<Expr>>),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Decl {
    Func(Span<FuncDef>),
    Var(VarDef),
}

#[derive(Debug, Clone)]
pub enum Operator {
    Equals,
    NotEquals,
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    Div,
    Mod,
    Times,
    Plus,
    Minus,
}
