use super::*;

// κτικές μονάδες της γλώσσας Tony χωρίζονται στις παρακάτω κατηγορίες:
/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    // Τις λέξεις κλειδιά, οι οποίες είναι οι παρακάτω:
    // and end list ref char false new skip decl for nil tail def head nil?  true else if not elif int or
    And,
    End,
    List,
    Ref,
    Exit,
    Return,
    Char,
    False,
    New,
    Skip,
    Decl,
    For,
    Nil,
    Tail,
    Def,
    Head,
    NilQ,
    True,
    Else,
    If,
    Not,
    Elif,
    Mod,
    Int,
    Bool,
    Or,
    // Τους σταθερούς χαρακτήρες, που αποτελούνται από ένα χαρακτήρα μέσα σε απλά εισαγωγικά. Ο χαρακτήρας αυτός μπορεί να είναι οποιοσδήποτε κοινός χαρακτήρας ή ακολουθία διαφυγής (escape sequence). Κοινοί χαρακτήρες είναι όλοι οι εκτυπώσιμοι χαρακτήρες πλην των απλών και διπλών εισαγωγικών και του χαρακτήρα \ (backslash). Οι ακολουθίες διαφυγής ξεκινούν με το χαρακτήρα \ (backslash) και περιγράφονται στον πίνακα 1. Παραδείγματα σταθερών χαρακτήρων είναι οι ακόλουθες: ’a’ ’1’ ’\n’ ’\”’ \x1d’
    CChar(char),
    // Τις σταθερές συμβολοσειρές, που αποτελούνται από μια ακολουθία κοινών χαρακτήρων ή ακο- λουθιών διαφυγής μέσα σε διπλά εισαγωγικά. Οι συμβολοσειρές δεν μπορούν να εκτείνονται σε περισσότερες από μια γραμμές προγράμματος. Παραδείγματα σταθερών συμβολοσειρών είναι οι ακόλουθες:
    //•,Τους συμβολικούς τελεστές, οι οποίοι είναι οι παρακάτω:
    Plus,       // +
    Minus,      // -
    Times,      // *
    Backslash,  // /
    Octothorpe, // #
    Equals,     // =
    NotEquals,  // <>
    Lthan,      // <
    Gthan,      // >
    Lequals,    // <=
    Gequals,    // >=
    // Διαχωριστές
    LParen,     // (
    RParen,     // )
    LSqBracket, // [
    RSqBracket, // ]
    Comma,      // ,
    Semicolon,  // ;
    Colon,      // :
    Assignment, // :=

    // • Κενούς χαρακτήρες, δηλαδή ακολουθίες αποτελούμενες από κενά διαστήματα (space), χαρακτήρες στηλοθέτησης (tab), χαρακτήρες αλλαγής γραμμής (line feed) ή χαρακτήρες επιστροφής στην αρχή της γραμμής (carriage return).

    // Σχόλια μιας γραμμής, τα οποία αρχίζουν με το χαρακτήρα % και τερματίζονται με το τέλος της τρέχουσας γραμμής.
    // Σχόλια πολλών γραμμών, τα οποία αρχίζουν με την ακολουθία χαρακτήρων <* και τερματίζονται με την ακολουθία χαρακτήρων *> . Τα σχόλια αυτής της μορφής επιτρέπεται να είναι φωλιασμένα.
    Comment,
    Binary,
    EOF,
    Extern,
    Identifier(String),
    StringLiteral(String),
    In,
    Number(f64),
    Op(char),
    Then,
    Unary,
    Var,
}

impl std::fmt::Display for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

/// Defines an error encountered by the `Lexer`.
#[derive(Debug)]
pub struct LexError {
    pub error: String,
    pub source_code: String,
    pub index: (usize, usize),
}

impl LexError {
    pub fn new<I: Into<String>>(source_code: String, msg: I) -> LexError {
        LexError {
            error: msg.into(),
            source_code,
            index: (0, 0),
        }
    }

    pub fn with_index<I: Into<String>>(
        source_code: String,
        msg: I,
        index: (usize, usize),
    ) -> LexError {
        LexError {
            error: msg.into(),
            source_code,
            index,
        }
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let line_num_s = (self.index.0 + 1).to_string();
        let indent_length = line_num_s.len() + 3;
        let indent = " ".repeat(indent_length);
        write!(
            fmt,
            "Line {}, Column {}: {}\n{indent}|\n   {line_num_s}|{}\n{indent}|{}{}",
            self.index.0 + 1,
            self.index.1 + 1,
            &self.error,
            self.source_code
                .lines()
                .nth(self.index.0)
                .unwrap()
                .trim_right(),
            " ".repeat(self.index.1),
            "^",
            line_num_s = line_num_s,
            indent = indent,
        )
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexResult = Result<(usize, Token, usize), LexError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
    col: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            chars: Box::new(input.chars().peekable()),
            pos: 0,
            col: 0,
            line: 0,
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut pos = self.pos;

        macro_rules! advance_pos {
            ($ch:expr) => {{
                pos += $ch.len_utf8();
                self.col += 1;
            }};
        }

        // Skip whitespaces
        loop {
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;

                    return Ok((pos, Token::EOF, pos));
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
                let ch = *ch.unwrap();
                advance_pos!(ch);
                if ch == '\n' {
                    self.line += 1;
                    self.col = 0;
                }
            }

            chars.next();
        }

        let start = pos;
        let next = chars.next();
        let follows: Option<char> = chars.peek().map(|&c| c);

        if next.is_none() {
            return Ok((start, Token::EOF, start));
        }

        let c = next.unwrap();
        advance_pos!(c);

        println!("Next char is {:?} {:?}", next, src[start..].chars().next());
        // Actually get the next token.
        let result = match c {
            '+' => Ok((start, Token::Plus, pos)),
            '-' => Ok((start, Token::Minus, pos)),
            '*' => Ok((start, Token::Times, pos)),
            '/' => Ok((start, Token::Backslash, pos)),
            '#' => Ok((start, Token::Octothorpe, pos)),
            '=' => Ok((start, Token::Equals, pos)),
            '%' => {
                // Comment
                loop {
                    let ch = match chars.next() {
                        Some(ch) => ch,
                        None => break,
                    };
                    advance_pos!(ch);

                    if ch == '\n' {
                        self.line += 1;
                        self.col = 0;
                        break;
                    }
                }

                Ok((start, Token::Comment, pos))
            }
            '<' if follows == Some('*') => todo!(),
            '*' if follows == Some('>') => todo!(),
            '<' if follows == Some('>') => {
                chars.next();
                advance_pos!(follows.unwrap());
                Ok((start, Token::NotEquals, pos))
            }
            '<' if follows == Some('=') => {
                chars.next();
                advance_pos!(follows.unwrap());
                Ok((start, Token::Lequals, pos))
            }
            '>' if follows == Some('=') => {
                chars.next();
                advance_pos!(follows.unwrap());
                Ok((start, Token::Gequals, pos))
            }
            '<' => Ok((start, Token::Lthan, pos)),
            '>' => Ok((start, Token::Gthan, pos)),
            // Διαχωριστές
            '(' => Ok((start, Token::LParen, pos)),
            ')' => Ok((start, Token::RParen, pos)),
            '[' => Ok((start, Token::LSqBracket, pos)),
            ']' => Ok((start, Token::RSqBracket, pos)),
            ',' => Ok((start, Token::Comma, pos)),
            ';' => Ok((start, Token::Semicolon, pos)),
            ':' if follows == Some('=') => {
                chars.next();
                advance_pos!(follows.unwrap());
                Ok((start, Token::Assignment, pos))
            }
            ':' => Ok((start, Token::Colon, pos)),

            '0'..='9' => {
                // Parse number literal
                println!("Parse number literal {:?}", &src[start..pos]);
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };
                    println!("let ch = {:?}", ch);

                    // Parse float.
                    if ch != '.' && !ch.is_digit(16) {
                        break;
                    }

                    println!("Next char is {:?}", chars.next());
                    advance_pos!(ch);
                }
                println!("parse number {:?}", &src[start..pos]);

                Ok((start, Token::Number(src[start..pos].parse().unwrap()), pos))
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                // Parse identifier
                println!("Parse identifier");
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    advance_pos!(ch);
                }
                println!("token = {:?}", &src[start..pos]);

                match &src[start..pos] {
                    "def" => Ok((start, Token::Def, pos)),
                    "extern" => Ok((start, Token::Extern, pos)),
                    "if" => Ok((start, Token::If, pos)),
                    "then" => Ok((start, Token::Then, pos)),
                    "else" => Ok((start, Token::Else, pos)),
                    "for" => Ok((start, Token::For, pos)),
                    "var" => Ok((start, Token::Var, pos)),
                    "decl" => Ok((start, Token::Decl, pos)),
                    "and" => Ok((start, Token::And, pos)),
                    "end" => Ok((start, Token::End, pos)),
                    "list" => Ok((start, Token::List, pos)),
                    "ref" => Ok((start, Token::Ref, pos)),
                    "char" => Ok((start, Token::Char, pos)),
                    "false" => Ok((start, Token::False, pos)),
                    "new" => Ok((start, Token::New, pos)),
                    "skip" => Ok((start, Token::Skip, pos)),
                    "decl" => Ok((start, Token::Decl, pos)),
                    "for" => Ok((start, Token::For, pos)),
                    "nil" => Ok((start, Token::Nil, pos)),
                    "nil?" => Ok((start, Token::NilQ, pos)),
                    "tail" => Ok((start, Token::Tail, pos)),
                    "def" => Ok((start, Token::Def, pos)),
                    "head" => Ok((start, Token::Head, pos)),
                    "true" => Ok((start, Token::True, pos)),
                    "else" => Ok((start, Token::Else, pos)),
                    "if" => Ok((start, Token::If, pos)),
                    "mod" => Ok((start, Token::Mod, pos)),
                    "not" => Ok((start, Token::Not, pos)),
                    "elif" => Ok((start, Token::Elif, pos)),
                    "int" => Ok((start, Token::Int, pos)),
                    "bool" => Ok((start, Token::Bool, pos)),
                    "or" => Ok((start, Token::Or, pos)),
                    "exit" => Ok((start, Token::Exit, pos)),
                    "return" => Ok((start, Token::Return, pos)),

                    ident => Ok((start, Token::Identifier(ident.to_string()), pos)),
                }
            }
            quot @ '”' | quot @ '"' => {
                // Parse string constant
                println!("Parse string constant");
                let mut prev = quot;
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };
                    if ch == quot && prev != '\\' {
                        chars.next();
                        prev = ch;
                        break;
                    } else if ch == '\n' {
                        return Err(LexError::with_index(
                            self.input.to_string(),
                            format!("Encountered new line while parsing string literal",),
                            (self.line, self.col),
                        ));
                    }

                    chars.next();
                    prev = ch;
                    advance_pos!(ch);
                }
                let s = src[start + quot.len_utf8()..pos].to_string();
                advance_pos!(prev);
                Ok((start, Token::StringLiteral(s), pos))
            }

            op => {
                // FIXME
                // Parse operator
                Ok((start, Token::Op(op), pos))
            }
        };

        // Update stored position, and return
        self.pos = pos;

        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(usize, Token, usize), LexError>;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok((_, EOF, _)) => None,
            Ok((_, Token::Comment, _)) => self.next(),
            ok @ Ok(_) => Some(ok),
            err @ Err(_) => Some(err),
        }
    }
}
