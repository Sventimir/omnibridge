use std::{
    fmt::{self, Debug, Display, Formatter},
    str::Utf8Error,
};

#[derive(Clone, PartialEq)]
pub enum Sexpr {
    Symbol(String),
    Str(String),
    Nat(u64),
    Float(f64),
    List(Vec<Sexpr>),
    Quoted(Box<Sexpr>),
    QuasiQuoted(Box<Sexpr>),
    UnQuoted(Box<Sexpr>),
}

impl Display for Sexpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Sexpr::Symbol(s) => write!(f, "{}", s),
            Sexpr::Str(s) => write!(f, "\"{}\"", s),
            Sexpr::Nat(i) => write!(f, "{}", i),
            Sexpr::Float(fl) => write!(f, "{}", fl),
            Sexpr::List(l) => {
                write!(f, "(")?;
                let mut items = l.iter();
                match items.next() {
                    Some(e) => {
                        write!(f, "{}", e)?;
                        for i in items {
                            write!(f, " {}", i)?;
                        }
                    }
                    None => (),
                }
                write!(f, ")")
            }
            Sexpr::Quoted(sexp) => write!(f, "'{}", sexp),
            Sexpr::QuasiQuoted(sexp) => write!(f, "`{}", sexp),
            Sexpr::UnQuoted(sexp) => write!(f, ",{}", sexp),
        }
    }
}

impl Debug for Sexpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn symbol(s: &str) -> Sexpr {
    Sexpr::Symbol(s.to_string())
}

pub fn string(s: &str) -> Sexpr {
    Sexpr::Str(s.to_string())
}

pub fn list<I>(iter: I) -> Sexpr
where
    I: IntoIterator<Item = Sexpr>,
{
    Sexpr::List(iter.into_iter().collect())
}

pub fn quoted(sexp: Sexpr) -> Sexpr {
    Sexpr::Quoted(Box::new(sexp))
}

pub fn quasi_quoted(sexp: Sexpr) -> Sexpr {
    Sexpr::QuasiQuoted(Box::new(sexp))
}

pub fn unquoted(sexp: Sexpr) -> Sexpr {
    Sexpr::UnQuoted(Box::new(sexp))
}

pub const NIL: Sexpr = Sexpr::List(vec![]);


#[derive(Clone, PartialEq)]
pub enum ParseError {
    SyntaxError,
    UtfError(Utf8Error),
    UnexpectedNode(&'static str),
    UnexpectedEndOfParsing,
    InvalidNumber(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::SyntaxError => write!(f, "Syntax error"),
            ParseError::UtfError(e) => write!(f, "UTF-8 error: {}", e),
            ParseError::UnexpectedNode(node) => write!(f, "Unexpected node: '{}'", node),
            ParseError::UnexpectedEndOfParsing => write!(f, "Unexpected end of parsing"),
            ParseError::InvalidNumber(s) => write!(f, "Invalid number: '{}'", s),
        }
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

struct ParserState<'a> {
    cursor: tree_sitter::TreeCursor<'a>,
    source: &'a [u8],
}

fn parse_node<'a>(
    node: &'a tree_sitter::Node,
    state: &ParserState<'a>,
) -> Result<Sexpr, ParseError> {
    match node.kind() {
        "sym_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .map(|s| Sexpr::Symbol(s.to_string())),
        "num_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .and_then(|s| {
                // Negative nuymbers shoud be created with `-` function.
                if s.starts_with('-') {
                    return Err(ParseError::InvalidNumber(s.to_string()));
                }
                s.parse::<u64>()
                    .map(Sexpr::Nat)
                    .or_else(|_| s.parse::<f64>().map(Sexpr::Float))
                    .map_err(|_| ParseError::InvalidNumber(s.to_string()))
            }),
        "str_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .map(|s| Sexpr::Str(s[1..s.len() - 1].to_string())),
        "quoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(Sexpr::Quoted(Box::new(quoted)))
        }
        "syn_quoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(Sexpr::QuasiQuoted(Box::new(quoted)))
        }
        "unquoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(Sexpr::UnQuoted(Box::new(quoted)))
        }
        "list_lit" => {
            let mut items: Vec<Sexpr> = Vec::with_capacity(node.child_count());
            let mut cursor = state.cursor.clone();
            let children = node
                .children(&mut cursor)
                .skip(1)
                .take_while(|n| n.kind() != ")");
            for child in children {
                items.push(parse_node(&child, state)?);
            }
            Ok(Sexpr::List(items))
        }
        k => Err(ParseError::UnexpectedNode(k)),
    }
}

pub fn parse(s: &str) -> Result<Sexpr, ParseError> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_commonlisp::language())
        .expect("Error loading Lisp grammar.");
    let expr = parser.parse(s, None).ok_or(ParseError::SyntaxError)?;
    let state = ParserState {
        cursor: expr.walk(),
        source: s.as_bytes(),
    };
    // The root does not contain any uselful info.
    if let Some(root) = state.cursor.node().child(0) {
        parse_node(&root, &state)
    } else {
        Err(ParseError::UnexpectedEndOfParsing)
    }
}
