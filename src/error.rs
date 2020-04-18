#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Pos {
    Index(usize, usize),
    Span(usize, usize),
    Offset(usize),
}

#[derive(Debug, PartialEq, Eq)]
enum TonyErrorKind {
    Lexer,
    Parser,
    SymbolTableCheck,
    TypeCheck,
}

/// Defines an error encountered by the `Tonyer`.
#[derive(Debug, PartialEq, Eq)]
pub struct TonyError {
    kind: TonyErrorKind,
    pub error: String,
    pos: Pos,
}
macro_rules! set_fn {
        ($set_name:ident, $kind:expr) => {
            pub fn $set_name(self) -> Self {
                TonyError {
                    kind: $kind,
                    .. self
                }
            }
        }
    }

impl TonyError {
    pub fn new<I: Into<String>>(msg: I) -> TonyError {
        TonyError {
            kind: TonyErrorKind::Lexer,
            error: msg.into(),
            pos: Pos::Index(0, 0),
        }
    }

    pub fn with_index<I: Into<String>>(msg: I, index: (usize, usize)) -> TonyError {
        TonyError {
            kind: TonyErrorKind::Lexer,
            error: msg.into(),
            pos: Pos::Index(index.0, index.1),
        }
    }

    pub fn with_offset<I: Into<String>>(msg: I, offset: usize) -> TonyError {
        TonyError {
            kind: TonyErrorKind::Lexer,
            error: msg.into(),
            pos: Pos::Offset(offset),
        }
    }

    pub fn with_span<I: Into<String>>(msg: I, span: (usize, usize)) -> TonyError {
        TonyError {
            kind: TonyErrorKind::Lexer,
            error: msg.into(),
            pos: Pos::Span(span.0, span.1),
        }
    }

    pub fn display(self, source_code: String) -> TonyErrorDisplay {
        TonyErrorDisplay {
            inner: self,
            source_code,
        }
    }

    set_fn!(set_parser_kind, TonyErrorKind::Parser);
    set_fn!(set_symbol_table_kind, TonyErrorKind::SymbolTableCheck);
    set_fn!(set_typecheck_kind, TonyErrorKind::TypeCheck);
}

#[derive(Debug)]
pub struct TonyErrorDisplay {
    inner: TonyError,
    source_code: String,
}
impl std::fmt::Display for TonyErrorDisplay {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { source_code, inner } = self;
        let index = match inner.pos {
            Pos::Index(line, col) => (line, col),
            Pos::Span(left, _right) => {
                let mut lines = 0;
                let mut line_offset = 0;
                let prev_line = source_code[..left].rfind("\n").unwrap_or(0);
                for l in source_code[..prev_line].split_n() {
                    lines += 1;
                    line_offset += l.len();
                }
                (lines, left.saturating_sub(line_offset + 1))
            }
            Pos::Offset(offset) => {
                let mut lines = 0;
                let mut line_offset = 0;
                let prev_line = source_code[..offset].rfind("\n").unwrap_or(0);
                for l in source_code[..prev_line].split_n() {
                    lines += 1;
                    line_offset += l.len();
                }
                (lines, offset.saturating_sub(line_offset + 1))
            }
        };
        let line_num_s = (index.0 + 1).to_string();
        let indent_length = line_num_s.len() + 3;
        let indent = " ".repeat(indent_length);
        match inner.pos {
            Pos::Index(_, _) | Pos::Offset(_) => write!(
                fmt,
                "{bold}{red}{:?} Error{reset}{bold} Line {}, Column {}: {error}\n{blue}{indent}|\n   {line_num_s}|{reset}{line}\n{bold}{blue}{indent}|{space}{red}{pointer}{reset}",
                inner.kind,
                index.0 + 1,
                index.1 + 1,
                line = source_code.lines().nth(index.0).unwrap().trim_end(),
                error = &inner.error,
                space = " ".repeat(index.1),
                pointer = "^",
                line_num_s = line_num_s,
                indent = indent,
                reset = "\x1b[m",
                blue = "\x1b[34m",
                red = "\x1b[31m",
                bold = "\x1b[1m",
            ),
            Pos::Span(l, r) => write!(
                fmt,
                "{bold}{red}{:?} Error{reset}{bold} Line {}, Column {}: {error}\n{blue}{indent}|\n   {line_num_s}|{reset}{line}\n{bold}{blue}{indent}|{space}{red}{pointer}{reset}",
                inner.kind,
                index.0 + 1,
                index.1 + 1,
                line = source_code.lines().nth(index.0).unwrap().trim_end(),
                error = &inner.error,
                space = " ".repeat(index.1),
                pointer = "^".repeat(r - l),
                line_num_s = line_num_s,
                indent = indent,
                reset = "\x1b[m",
                blue = "\x1b[34m",
                red = "\x1b[31m",
                bold = "\x1b[1m",
            ),
        }
    }
}

pub struct LineIterator<'a> {
    slice: &'a str,
}

impl<'a> Iterator for LineIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        if self.slice.is_empty() {
            None
        } else if let Some(pos) = self.slice.find("\n") {
            let ret = &self.slice[..pos + 1];
            self.slice = &self.slice[pos + 1..];
            Some(ret)
        } else {
            let ret = self.slice;
            self.slice = &self.slice[ret.len()..];
            Some(ret)
        }
    }
}

pub trait LineSplit {
    fn split_n(&self) -> LineIterator;
}

impl LineSplit for str {
    fn split_n(&self) -> LineIterator {
        LineIterator { slice: self }
    }
}
