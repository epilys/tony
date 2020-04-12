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

#[derive(Debug)]
pub struct Identifier(pub String);
#[derive(Debug)]
pub struct StringLiteral(pub String);
#[derive(Debug)]
pub struct CharConst(pub char);
#[derive(Debug)]
pub struct IntConst(pub f64);

#[derive(Debug)]
pub struct Program(pub Vec<FuncDef>);

#[derive(Debug)]
pub struct FuncDef {
    pub header: (Formal, Vec<Formal>),
    pub declarations: Vec<Decl>,
    pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Formal {
    pub is_ref: bool,
    pub var: VarDef,
}

#[derive(Debug)]
pub struct VarDef {
    pub tony_type: TonyType,
    pub id: Identifier,
}

#[derive(Debug, Clone)]
pub enum TonyType {
    Int,
    Bool,
    Char,
    Unit,
    Array(Box<TonyType>),
    List(Box<TonyType>),
}

#[derive(Debug)]
pub enum Stmt {
    Simple(Simple),
    Exit,
    Return(Box<Expr>),
    Control(StmtType),
}

#[derive(Debug)]
pub enum StmtType {
    If {
        condition: Box<Expr>,
        body: Vec<Stmt>,
        _else: Vec<Stmt>,
    },
    For,
}

#[derive(Debug)]
pub enum Simple {
    Skip,
    Call(Call),
    Assignment(Atom, Box<Expr>),
}

#[derive(Debug)]
pub struct Call(pub Identifier, pub Vec<Expr>);

#[derive(Debug)]
pub enum Atom {
    Id(Identifier),
    StringLiteral(StringLiteral),
    AtomIndex(Box<Expr>),
    Call(Call),
}

#[derive(Debug)]
pub enum Expr {
    Atom(Atom),
    IntConst(IntConst),
    CharConst(CharConst),
    True,
    False,
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>),
    Op(Box<Expr>, Operator, Box<Expr>),
    New(TonyType, Box<Expr>),
    Nil,
}

#[derive(Debug)]
pub enum Decl {
    Func(FuncDef),
    Var(VarDef),
}

#[derive(Debug)]
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
