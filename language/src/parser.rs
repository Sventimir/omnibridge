use super::{src_location::WithLocation, IntoSexp, Lisp, Sexp};
use std::{
    fmt::{self, Debug, Display, Formatter},
    str::Utf8Error,
};

#[derive(Clone, PartialEq)]
pub enum ParseError {
    SyntaxError,
    UtfError(Utf8Error),
    UnexpectedNode(&'static str),
    InvalidNumber(String),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ParseError::SyntaxError => write!(f, "Syntax error"),
            ParseError::UtfError(e) => write!(f, "UTF-8 error: {}", e),
            ParseError::UnexpectedNode(node) => write!(f, "Unexpected node: '{}'", node),
            ParseError::InvalidNumber(s) => write!(f, "Invalid number: '{}'", s),
        }
    }
}

impl Debug for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl IntoSexp for ParseError {
    fn into_sexp<S: Sexp>(self) -> S {
        match self {
            ParseError::SyntaxError => S::list(vec![
                S::symbol("error".to_string()),
                S::symbol("syntax".to_string()),
            ]),
            ParseError::UtfError(e) => S::list(vec![
                S::symbol("error".to_string()),
                S::symbol("utf-8".to_string()),
                S::string(e.to_string()),
            ]),
            ParseError::InvalidNumber(s) => S::list(vec![
                S::symbol("error".to_string()),
                S::symbol("invalid-number".to_string()),
                S::string(s),
            ]),
            ParseError::UnexpectedNode(node) => S::list(vec![
                S::symbol("error".to_string()),
                S::symbol("unexpected-node".to_string()),
                S::symbol(node.to_string()),
            ]),
        }
    }
}

struct ParserState<'a> {
    cursor: tree_sitter::TreeCursor<'a>,
    source: &'a [u8],
}

fn parse_node<'a, L: Lisp + WithLocation>(
    node: &'a tree_sitter::Node,
    state: &ParserState<'a>,
) -> Result<L, ParseError> {
    let mut ret = match node.kind() {
        "sym_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .map(|s| L::symbol(s.to_string())),
        "num_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .and_then(|s| {
                s.parse::<u64>()
                    .map(L::nat)
                    .or_else(|_| s.parse::<f64>().map(L::float))
                    .map_err(|_| ParseError::InvalidNumber(s.to_string()))
            }),
        "str_lit" => node
            .utf8_text(&state.source)
            .map_err(ParseError::UtfError)
            .map(|s| L::string(s[1..s.len() - 1].to_string())),
        "quoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(L::quoted(quoted))
        }
        "syn_quoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(L::quasiquoted(quoted))
        }
        "unquoting_lit" => {
            let quoted = parse_node(&node.child(1).unwrap(), state)?;
            Ok(L::unquoted(quoted))
        }
        "list_lit" => {
            let mut items: Vec<L> = Vec::with_capacity(node.child_count());
            let mut cursor = state.cursor.clone();
            let children = node
                .children(&mut cursor)
                .skip(1)
                .take_while(|n| n.kind() != ")");
            for child in children {
                items.push(parse_node(&child, state)?);
            }
            Ok(L::list(items))
        }
        "nil_lit" => Ok(L::list(vec![])),
        k => Err(ParseError::UnexpectedNode(k)),
    }?;
    ret.annot(node.byte_range());
    Ok(ret)
}

pub fn parse<L: Lisp + WithLocation>(s: &str) -> Result<Vec<L>, ParseError> {
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
    let mut result = Vec::with_capacity(expr.root_node().child_count());
    for child in expr.root_node().children(&mut state.cursor.clone()) {
        result.push(parse_node(&child, &state)?);
    }
    Ok(result)
}
