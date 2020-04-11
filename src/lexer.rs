use super::*;

// κτικές μονάδες της γλώσσας Tony χωρίζονται στις παρακάτω κατηγορίες:
/// Represents a primitive syntax token.
#[derive(Debug, Clone)]
pub enum Token {
    // Τις λέξεις κλειδιά, οι οποίες είναι οι παρακάτω:
    // and end list ref char false new skip decl for nil tail def head nil?  true else if not elsif int or
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
    Elsif,
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
    Ident(String),
    In,
    Number(f64),
    Op(char),
    Then,
    Unary,
    Var,
}

/// Defines an error encountered by the `Lexer`.
pub struct LexError {
    pub error: &'static str,
    pub index: usize,
}

impl LexError {
    pub fn new(msg: &'static str) -> LexError {
        LexError {
            error: msg,
            index: 0,
        }
    }

    pub fn with_index(msg: &'static str, index: usize) -> LexError {
        LexError {
            error: msg,
            index: index,
        }
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `LexError` on failure.
pub type LexResult = Result<Token, LexError>;

/// Defines a lexer which transforms an input `String` into
/// a `Token` stream.
pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new `Lexer`, given its source `input`.
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            input: input,
            chars: Box::new(input.chars().peekable()),
            pos: 0,
        }
    }

    /// Lexes and returns the next `Token` from the source code.
    pub fn lex(&mut self) -> LexResult {
        let chars = self.chars.deref_mut();
        let src = self.input;

        let mut pos = self.pos;

        // Skip whitespaces
        loop {
            // Note: the following lines are in their own scope to
            // limit how long 'chars' is borrowed, and in order to allow
            // it to be borrowed again in the loop by 'chars.next()'.
            {
                let ch = chars.peek();

                if ch.is_none() {
                    self.pos = pos;

                    return Ok(Token::EOF);
                }

                if !ch.unwrap().is_whitespace() {
                    break;
                }
            }

            chars.next();
            pos += 1;
        }

        let start = pos;
        let next = chars.next();
        let follows: Option<char> = chars.peek().map(|&c| c);

        if next.is_none() {
            return Ok(Token::EOF);
        }

        pos += 1;

        println!("Next char is {:?} {:?}", next, &src[start..pos]);
        // Actually get the next token.
        let result = match next.unwrap() {
            '+' => Ok(Token::Plus),
            '-' => Ok(Token::Minus),
            '*' => Ok(Token::Times),
            '/' => Ok(Token::Backslash),
            '#' => Ok(Token::Octothorpe),
            '=' => Ok(Token::Equals),
            '%' => {
                // Comment
                loop {
                    let ch = chars.next();
                    pos += 1;

                    if ch == Some('\n') {
                        break;
                    }
                }

                Ok(Token::Comment)
            }
            '<' if follows == Some('*') => todo!(),
            '*' if follows == Some('>') => todo!(),
            '<' if follows == Some('>') => {
                chars.next();
                pos += 1;
                Ok(Token::NotEquals)
            }
            '<' if follows == Some('=') => {
                chars.next();
                pos += 1;
                Ok(Token::Lequals)
            }
            '>' if follows == Some('=') => {
                chars.next();
                pos += 1;
                Ok(Token::Gequals)
            }
            '<' => Ok(Token::Lthan),
            '>' => Ok(Token::Gthan),
            // Διαχωριστές
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '[' => Ok(Token::LSqBracket),
            ']' => Ok(Token::RSqBracket),
            ',' => Ok(Token::Comma),
            ';' => Ok(Token::Semicolon),
            ':' if follows == Some('=') => {
                chars.next();
                pos += 1;
                Ok(Token::Assignment)
            }
            ':' => Ok(Token::Colon),

            '.' | '0'..='9' => {
                // Parse number literal
                println!("Parse number literal {:?}", &src[start..pos]);
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };
                    println!("let ch = {:?}", ch);

                    // Parse float.
                    if ch != '.' && !ch.is_digit(16) {
                        break;
                    }

                    println!("Next char is {:?}", chars.next());
                    pos += 1;
                }
                println!("parse number {:?}", &src[start..pos]);

                Ok(Token::Number(src[start..pos].parse().unwrap()))
            }

            'a'..='z' | 'A'..='Z' | '_' => {
                // Parse identifier
                println!("Parse identifier");
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => return Ok(Token::EOF),
                    };

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !ch.is_alphanumeric() {
                        break;
                    }

                    chars.next();
                    pos += 1;
                }
                println!("token = {:?}", &src[start..pos]);

                match &src[start..pos] {
                    "def" => Ok(Token::Def),
                    "extern" => Ok(Token::Extern),
                    "if" => Ok(Token::If),
                    "then" => Ok(Token::Then),
                    "else" => Ok(Token::Else),
                    "for" => Ok(Token::For),
                    "var" => Ok(Token::Var),
                    "decl" => Ok(Token::Decl),
                    "and" => Ok(Token::And),
                    "end" => Ok(Token::End),
                    "list" => Ok(Token::List),
                    "ref" => Ok(Token::Ref),
                    "char" => Ok(Token::Char),
                    "false" => Ok(Token::False),
                    "new" => Ok(Token::New),
                    "skip" => Ok(Token::Skip),
                    "decl" => Ok(Token::Decl),
                    "for" => Ok(Token::For),
                    "nil" => Ok(Token::Nil),
                    "nil?" => Ok(Token::NilQ),
                    "tail" => Ok(Token::Tail),
                    "def" => Ok(Token::Def),
                    "head" => Ok(Token::Head),
                    "true" => Ok(Token::True),
                    "else" => Ok(Token::Else),
                    "if" => Ok(Token::If),
                    "not" => Ok(Token::Not),
                    "elsif" => Ok(Token::Elsif),
                    "int" => Ok(Token::Int),
                    "bool" => Ok(Token::Bool),
                    "or" => Ok(Token::Or),
                    "exit" => Ok(Token::Exit),
                    "return" => Ok(Token::Return),

                    ident => Ok(Token::Ident(ident.to_string())),
                }
            }
            '”' | '"' => todo!(),

            op => {
                // Parse operator
                Ok(Token::Op(op))
            }
        };

        // Update stored position, and return
        self.pos = pos;

        result
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(EOF) | Err(_) => None,
            Ok(token) => Some(token),
        }
    }
}
