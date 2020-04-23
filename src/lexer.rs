use super::*;

// κτικές μονάδες της γλώσσας Tony χωρίζονται στις παρακάτω κατηγορίες:
/// Represents a primitive syntax token.
#[derive(Debug, PartialEq, Eq, Clone)]
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
    EOF,
    Extern,
    Identifier(String),
    StringLiteral(String),
    Number(i64, String),
    Op(char),
    Var,
}

impl std::fmt::Display for Token {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:?}", self)
    }
}

/// Defines the result of a lexing operation; namely a
/// `Token` on success, or a `TonyError` on failure.
pub type LexResult = Result<(usize, Token, usize), TonyError>;

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

        //println!("Next char is {:?} {:?}", next, src[start..].chars().next());
        // Actually get the next token.
        let result = match c {
            '+' => Ok((start, Token::Plus, pos)),
            '-' => Ok((start, Token::Minus, pos)),
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
            '<' if follows == Some('*') => {
                // Comment start.
                let mut nested_level = 1;
                loop {
                    let next = chars.next();
                    let peek = chars.peek();
                    let ch = match (next, peek) {
                        (Some(ch @ '<'), Some(&'*')) => {
                            nested_level += 1;
                            ch
                        }
                        (Some('*'), Some(&'>')) if nested_level == 1 => {
                            advance_pos!('*');
                            let _ = chars.next();
                            advance_pos!('>');
                            break;
                        }
                        (Some(ch @ '*'), Some(&'>')) => {
                            nested_level -= 1;
                            ch
                        }
                        (Some(ch), _) => ch,
                        (None, _) => {
                            return Err(TonyError::with_span(
                                format!("Encountered EOF while parsing multiline comment",),
                                (start, pos),
                            ))
                        }
                    };
                    advance_pos!(ch);

                    if ch == '\n' {
                        self.line += 1;
                        self.col = 0;
                    }
                }

                Ok((start, Token::Comment, pos))
            }
            '*' => Ok((start, Token::Times, pos)),
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
                //println!("Parse number literal {:?}", &src[start..pos]);
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    if !ch.is_digit(10) {
                        break;
                    }

                    //println!("Next char is {:?}", ch);
                    chars.next();
                    advance_pos!(ch);
                }
                //println!("parse number {:?}", &src[start..pos]);

                match src[start..pos].parse() {
                    Err(err) => Err(TonyError::with_span(
                        format!("Error while parsing number literal: {}", err),
                        (start, pos),
                    )),

                    Ok(ok) => Ok((start, Token::Number(ok, src[start..pos].to_string()), pos)),
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                // Parse identifier
                //println!("Parse identifier");
                loop {
                    let ch = match chars.peek() {
                        Some(ch) => *ch,
                        None => break,
                    };

                    // A word-like identifier only contains underscores and alphanumeric characters.
                    if ch != '_' && !ch.is_alphanumeric() && ch != '?' {
                        break;
                    }

                    chars.next();
                    advance_pos!(ch);
                }
                //println!("token = {:?}", &src[start..pos]);
                match &src[start..pos] {
                    "and" => Ok((start, Token::And, pos)),
                    "bool" => Ok((start, Token::Bool, pos)),
                    "char" => Ok((start, Token::Char, pos)),
                    "decl" => Ok((start, Token::Decl, pos)),
                    "def" => Ok((start, Token::Def, pos)),
                    "elif" => Ok((start, Token::Elif, pos)),
                    "elsif" => Ok((start, Token::Elif, pos)),
                    "else" => Ok((start, Token::Else, pos)),
                    "end" => Ok((start, Token::End, pos)),
                    "exit" => Ok((start, Token::Exit, pos)),
                    "extern" => Ok((start, Token::Extern, pos)),
                    "false" => Ok((start, Token::False, pos)),
                    "for" => Ok((start, Token::For, pos)),
                    "head" => Ok((start, Token::Head, pos)),
                    "if" => Ok((start, Token::If, pos)),
                    "int" => Ok((start, Token::Int, pos)),
                    "list" => Ok((start, Token::List, pos)),
                    "mod" => Ok((start, Token::Mod, pos)),
                    "new" => Ok((start, Token::New, pos)),
                    "nil?" => Ok((start, Token::NilQ, pos)),
                    "nil" => Ok((start, Token::Nil, pos)),
                    "not" => Ok((start, Token::Not, pos)),
                    "or" => Ok((start, Token::Or, pos)),
                    "ref" => Ok((start, Token::Ref, pos)),
                    "return" => Ok((start, Token::Return, pos)),
                    "skip" => Ok((start, Token::Skip, pos)),
                    "tail" => Ok((start, Token::Tail, pos)),
                    "true" => Ok((start, Token::True, pos)),
                    "var" => Ok((start, Token::Var, pos)),
                    _ => Ok((start, Token::Identifier(src[start..pos].to_string()), pos)),
                }
            }
            quot @ '\'' | quot @ '’' => {
                // Parse Char constant
                let mut ch = match chars.next() {
                    Some(ch) => ch,
                    None => {
                        return Err(TonyError::with_index(
                            format!("Encountered EOF while parsing char literal",),
                            (self.line, self.col),
                        ));
                    }
                };
                advance_pos!(ch);
                /* Ακολουθία διαφυγής
                 *  \n   ο χαρακτήρας αλλαγής γραμμής (line feed)
                 *  \t   ο χαρακτήρας στηλοθέτησης (tab)
                 *  \r   ο χαρακτήρας επιστροφής στην αρχή της γραμμής (carriage return)
                 *  \0   ο χαρακτήρας με ASCII κωδικό 0
                 *  \\   ο χαρακτήρας \ (backslash)
                 *  \’   ο χαρακτήρας ’ (απλό εισαγωγικό)
                 *  \”   ο χαρακτήρας ” (διπλό εισαγωγικό)
                 *  \xnn ο χαρακτήρας με ASCII κωδικό nn στο δεκαεξαδικό σύστημα
                 */
                if ch == '\\' {
                    ch = match chars.next() {
                        Some('n') => '\n',
                        Some('t') => '\t',
                        Some('r') => '\r',
                        Some('0') => '\0',
                        Some('\\') => '\\',
                        Some('’') => '’',
                        Some('\'') => '\'',
                        Some('”') => '”',
                        Some('"') => '"',
                        Some('x') => {
                            if src[start..].len() < 2 {
                                return Err(TonyError::with_index(
                                    format!("Encountered EOF while parsing char literal.",),
                                    (self.line, self.col),
                                ));
                            }
                            use std::convert::TryFrom;
                            match u32::from_str_radix(&src[start..start + 2], 16)
                                .ok()
                                .and_then(|hex_num| char::try_from(hex_num).ok())
                            {
                                Some(ascii) => {
                                    advance_pos!(chars.next().unwrap());
                                    advance_pos!(chars.next().unwrap());
                                    ascii
                                }
                                None => {
                                    return Err(TonyError::with_index(
                                        format!("Invalid hex while parsing char literal.",),
                                        (self.line, self.col),
                                    ));
                                }
                            }
                        }
                        Some(ch) => {
                            return Err(TonyError::with_index(
                                format!("Unrecognized start of escape sequence: {}", ch),
                                (self.line, self.col),
                            ));
                        }
                        None => {
                            return Err(TonyError::with_index(
                                format!("Encountered EOF while parsing char literal.",),
                                (self.line, self.col),
                            ));
                        }
                    };
                    advance_pos!(ch);
                }

                let next = match chars.next() {
                    Some(ch) if ch != quot => {
                        return Err(TonyError::with_index(
                            format!("Char literal not properly closed with {}", quot),
                            (self.line, self.col),
                        ));
                    }
                    Some(ch) => ch,
                    None => {
                        return Err(TonyError::with_index(
                            format!("Encountered EOF while parsing char literal"),
                            (self.line, self.col),
                        ));
                    }
                };
                advance_pos!(next);
                Ok((start, Token::CChar(ch), pos))
            }
            quot @ '”' | quot @ '"' => {
                // Parse string constant
                //println!("Parse string constant");
                let mut escape_next = false;
                let mut s = String::new();
                'string_literal_loop: loop {
                    let ch;
                    match chars.peek() {
                        Some(ch) if *ch == quot && !escape_next => {
                            chars.next();
                            advance_pos!(quot);
                            break 'string_literal_loop;
                        }
                        Some(&'n') if escape_next => {
                            ch = '\n';
                            escape_next = false;
                        }
                        Some(&'t') if escape_next => {
                            ch = '\t';
                            escape_next = false;
                        }
                        Some(&'r') if escape_next => {
                            ch = '\r';
                            escape_next = false;
                        }
                        Some(&'0') if escape_next => {
                            ch = '\0';
                            escape_next = false;
                        }
                        Some(&'\\') => {
                            ch = '\\';
                            if !escape_next {
                                escape_next = true;
                            } else {
                                escape_next = false;
                            }
                        }
                        Some(&'’') if escape_next => {
                            ch = '’';
                            escape_next = false;
                        }
                        Some(&'\'') if escape_next => {
                            ch = '\'';
                            escape_next = false;
                        }
                        Some(&'”') if escape_next => {
                            ch = '”';
                            escape_next = false;
                        }
                        Some(&'"') if escape_next => {
                            ch = '"';
                            escape_next = false;
                        }
                        Some(&'x') if escape_next => {
                            escape_next = false;
                            if src[pos + 1..].len() < 2 {
                                return Err(TonyError::with_index(
                                    format!("Encountered EOF while parsing string literal.",),
                                    (self.line, self.col),
                                ));
                            }
                            use std::convert::TryFrom;
                            match u32::from_str_radix(&src[pos + 1..pos + 3], 16)
                                .ok()
                                .and_then(|hex_num| char::try_from(hex_num).ok())
                            {
                                Some(ascii) => {
                                    advance_pos!(chars.next().unwrap());
                                    advance_pos!(chars.next().unwrap());
                                    ch = ascii;
                                }
                                None => {
                                    return Err(TonyError::with_index(
                                        format!("Invalid hex in parsing string literal.",),
                                        (self.line, self.col),
                                    ));
                                }
                            }
                        }
                        Some(ch) if escape_next => {
                            return Err(TonyError::with_index(
                                format!(
                                    "Invalid escape sequence in parsing string literal.: \\{}",
                                    *ch
                                ),
                                (self.line, self.col),
                            ));
                        }
                        Some(&'\n') => {
                            return Err(TonyError::with_index(
                                format!("Encountered new line while parsing string literal",),
                                (self.line, self.col),
                            ));
                        }
                        Some(p) => {
                            ch = *p;
                        }
                        None => {
                            return Err(TonyError::with_index(
                                format!("Encountered EOF while parsing string literal",),
                                (self.line, self.col),
                            ));
                        }
                    };
                    advance_pos!(chars.next().unwrap());
                    if !escape_next {
                        s.push(ch);
                    }
                }
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
    type Item = Result<(usize, Token, usize), TonyError>;

    /// Lexes the next `Token` and returns it.
    /// On EOF or failure, `None` will be returned.
    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok((_, Token::EOF, _)) => None,
            Ok((_, Token::Comment, _)) => self.next(),
            ok @ Ok(_) => Some(ok),
            err @ Err(_) => Some(err),
        }
    }
}

#[test]
fn test_lexer_constants() {
    let input = "'\\n'";
    assert_eq!(Lexer::new(input).lex(), Ok((0, Token::CChar('\n'), 4)));
    let input = "'\\z'";
    assert!(Lexer::new(input).lex().is_err());
    let input = "'c'";
    assert_eq!(Lexer::new(input).lex(), Ok((0, Token::CChar('c'), 3)));
    let input = "'c";
    assert!(Lexer::new(input).lex().is_err());
    let input = r#""Hello world.""#;
    assert_eq!(
        Lexer::new(input).lex(),
        Ok((0, Token::StringLiteral("Hello world.".to_string()), 14))
    );
    let input = r#""Hello world.\n""#;
    assert_eq!(
        Lexer::new(input).lex(),
        Ok((0, Token::StringLiteral("Hello world.\n".to_string()), 16))
    );
    let input = r#""Hello world.\x0A""#;
    assert_eq!(
        Lexer::new(input).lex(),
        Ok((0, Token::StringLiteral("Hello world.\n".to_string()), 18))
    );
    let input = r#""Hello world.\x0z""#;
    assert!(Lexer::new(input).lex().is_err());
    let input = r#""Hello world.
    ""#;
    assert!(Lexer::new(input).lex().is_err());
}
