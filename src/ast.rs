use std::fmt;
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Identifier(pub String);
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct StringLiteral(pub String);
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct CharConst(pub char);
#[derive(Debug, Clone)]
pub struct IntConst(pub f64, pub String);

impl std::cmp::PartialEq for IntConst {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl std::cmp::Eq for IntConst {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    pub left: usize,
    pub right: usize,
    inner: T,
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> Span<T> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program(pub Vec<Span<FuncDef>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncDef {
    pub header: (Formal, Vec<Formal>),
    pub declarations: Vec<Span<Decl>>,
    pub statements: Vec<Span<Stmt>>,
}

#[derive(Clone, PartialEq, Eq)]
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
pub struct VarDef {
    pub tony_type: Span<TonyType>,
    pub id: Span<Identifier>,
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

#[derive(PartialEq, Eq, Clone)]
pub enum TonyType {
    Int,
    Bool,
    Char,
    Unit,
    Array(Box<Span<TonyType>>),
    List(Box<Span<TonyType>>),
}

impl fmt::Debug for TonyType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TonyType::*;
        match self {
            Int => write!(f, "int"),
            Bool => write!(f, "bool"),
            Char => write!(f, "char"),
            Unit => write!(f, "()"),
            Array(inner) => write!(f, "array {:?}[]", inner.into_inner()),
            List(inner) => write!(f, "list {:?}[]", inner.into_inner()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Simple(Simple),
    Exit,
    Return(Box<Span<Expr>>),
    Control(StmtType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StmtType {
    If {
        condition: Box<Span<Expr>>,
        body: Vec<Span<Stmt>>,
        _else: Vec<Span<Stmt>>,
    },
    For {
        init: Vec<Simple>,
        condition: Box<Span<Expr>>,
        eval: Vec<Simple>,
        body: Vec<Span<Stmt>>,
    },
}

#[derive(Clone, PartialEq, Eq)]
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
pub enum Atom {
    Id(Span<Identifier>),
    StringLiteral(StringLiteral),
    AtomIndex(Box<Span<Expr>>),
    Call(Call),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Id(Span { inner: ident, .. }) => ident.fmt(f),
            Atom::StringLiteral(lit) => lit.fmt(f),
            Atom::AtomIndex(sp) => sp.into_inner().fmt(f),
            Atom::Call(call) => call.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
pub enum Decl {
    Func(Span<FuncDef>),
    Var(Vec<VarDef>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
