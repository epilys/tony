#[cfg(feature = "fuzzing")]
use libfuzzer_sys::arbitrary::{self, Arbitrary};
use std::fmt;
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct Identifier(pub String);
impl Identifier {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct StringLiteral(pub String);
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct CharConst(pub char);
#[derive(Debug, Clone)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct IntConst(pub i64, pub String);

use std::ops::Deref;

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> Deref for Span<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::cmp::PartialEq for IntConst {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl std::cmp::Eq for IntConst {}

#[derive(Clone)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct Span<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    pub left: usize,
    pub right: usize,
    pub inner: T,
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> std::cmp::PartialEq for Span<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}
impl<T: std::fmt::Debug + Clone + PartialEq + Eq> std::cmp::Eq for Span<T> {}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> fmt::Debug for Span<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.into_inner(), f)
    }
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> Span<T> {
    #[inline(always)]
    pub fn into_inner(&self) -> &T {
        &self.inner
    }

    #[inline(always)]
    pub fn span(&self) -> (usize, usize) {
        (self.left, self.right)
    }

    #[inline(always)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct Program(pub Vec<Span<FuncDef>>);

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct FuncDef {
    pub header: (Formal, Vec<Formal>),
    pub declarations: Vec<Span<Decl>>,
    pub body: Vec<Span<Stmt>>,
    pub is_extern: bool,
}

impl FuncDef {
    #[inline(always)]
    pub fn return_type(&self) -> &TonyType {
        &self.header.0.var.tony_type
    }
    #[inline(always)]
    pub fn ident(&self) -> &Identifier {
        self.header.0.var.id.into_inner()
    }
}

impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}({})",
            self.header.0,
            self.header
                .1
                .iter()
                .map(|f| format!("{:?}", f))
                .fold(String::new(), |mut acc, x| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(&x);
                    acc
                })
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct Formal {
    pub is_ref: bool,
    pub var: VarDef,
}

impl fmt::Debug for Formal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{:?}", if self.is_ref { "ref " } else { "" }, self.var)
    }
}

#[derive(PartialEq, Eq, Clone)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct VarDef {
    pub tony_type: Span<TonyType>,
    pub id: Span<Identifier>,
}

impl VarDef {
    pub fn as_str(&self) -> &str {
        self.id.0.as_str()
    }
}

impl fmt::Debug for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?} {:?}",
            self.tony_type.into_inner(),
            self.id.into_inner()
        )
    }
}

#[derive(Clone)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum TonyType {
    Int,
    Bool,
    Char,
    Unit,
    Any,
    Array(Box<Span<TonyType>>),
    List(Box<Span<TonyType>>),
}

impl std::cmp::PartialEq for TonyType {
    fn eq(&self, other: &Self) -> bool {
        use TonyType::*;
        match (self, other) {
            (Any, _) | (_, Any) => true,
            (Int, Int) => true,
            (Bool, Bool) => true,
            (Char, Char) => true,
            (Unit, Unit) => true,
            (Array(left_span), Array(right_span)) => {
                left_span.into_inner() == right_span.into_inner()
            }
            (List(left_span), List(right_span)) => {
                left_span.into_inner() == right_span.into_inner()
            }
            _ => false,
        }
    }
}
impl std::cmp::Eq for TonyType {}

impl fmt::Debug for TonyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TonyType::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            Char => write!(f, "char"),
            Unit => write!(f, "()"),
            Any => write!(f, "Any"),
            Array(inner) => write!(f, "array {:?}[]", inner.into_inner()),
            List(inner) => write!(f, "list[{:?}]", inner.into_inner()),
        }
    }
}

impl TonyType {
    pub fn is_array(&self) -> bool {
        match self {
            TonyType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            TonyType::List(_) => true,
            _ => false,
        }
    }

    pub fn is_any(&self) -> bool {
        match self {
            TonyType::Any => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Stmt {
    Simple(Span<Simple>),
    Exit,
    Return(Box<Span<Expr>>),
    Control(StmtType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum StmtType {
    If {
        condition: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
        _else: Vec<Span<Stmt>>,
    },
    For {
        init: Vec<Span<Simple>>,
        condition: Box<Span<Expr>>,
        eval: Vec<Span<Simple>>,
        body: Vec<Span<Stmt>>,
    },
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Simple {
    Skip,
    Call(Call),
    Assignment(Atom, Box<Span<Expr>>),
}

impl fmt::Debug for Simple {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Simple::Skip => write!(f, "skip"),
            Simple::Call(call) => call.fmt(f),
            Simple::Assignment(a, expr) => {
                let expr = expr.into_inner();

                write!(f, "{:?} := {:?}", a, expr)
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub struct Call(pub Span<Identifier>, pub Vec<Span<Expr>>);

impl fmt::Debug for Call {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Call(Span { ref inner, .. }, ref arg_spans) = self;
        write!(
            f,
            "{:?}({})",
            inner,
            arg_spans
                .iter()
                .map(Span::into_inner)
                .map(|expr| format!("{:?}", expr))
                .fold(String::new(), |mut acc, el| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.extend(format!("{:?}", el).chars());
                    acc
                })
        )
    }
}

#[derive(Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Atom {
    Id(Span<Identifier>),
    StringLiteral(StringLiteral),
    AtomIndex(Box<Atom>, Box<Span<Expr>>),
    Call(Call),
    ListOp(Span<ListOp>),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Id(Span { inner: ident, .. }) => ident.fmt(f),
            Atom::StringLiteral(lit) => lit.fmt(f),
            Atom::AtomIndex(atom, expr_span) => write!(f, "{:?}[{:?}]", atom, expr_span),
            Atom::Call(call) => call.fmt(f),
            Atom::ListOp(listop) => listop.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum ListOp {
    Cons(Box<Span<Expr>>, Box<Span<Expr>>),
    Head(Box<Span<Expr>>),
    Tail(Box<Span<Expr>>),
    NilQ(Box<Span<Expr>>),
}

impl ListOp {
    pub fn is_nilq(&self) -> bool {
        match self {
            ListOp::NilQ(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Decl {
    Func(Span<FuncDef>),
    Var(Vec<VarDef>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
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
