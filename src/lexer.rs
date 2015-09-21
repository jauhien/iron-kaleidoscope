//< ch-0 lexer-tokens-use
pub use self::Token::{
    Def,
    Extern,
//> ch-0 lexer-tokens-use
    If,
    Then,
    Else,
    For,
    In,
    Binary,
    Unary,
    Var,
//< ch-0 lexer-tokens-use
    Delimiter,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident,
    Number,
    Operator
};
//> lexer-tokens-use

//< lexer-tokens if-lexer
#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,
//> ch-0 lexer-tokens
    If,
    Then,
    Else,
//> if-lexer
    For,
    In,
    Binary,
    Unary,
    Var,
//< ch-0 lexer-tokens if-lexer
    Delimiter, //';' character
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(String)
}
//> lexer-tokens

//< lexer-tokenize
pub fn tokenize(input: &str) -> Vec<Token> {
    // regex for commentaries (start with #, end with the line end)
    let comment_re = regex!(r"(?m)#.*\n");
    // remove commentaries from the input stream
    let preprocessed = comment_re.replace_all(input, "\n");

    let mut result = Vec::new();

    // regex for token, just union of straightforward regexes for different token types
    // operators are parsed the same way as identifier and separated later
    let token_re = regex!(concat!(
        r"(?P<ident>\p{Alphabetic}\w*)|",
        r"(?P<number>\d+\.?\d*)|",
        r"(?P<delimiter>;)|",
        r"(?P<oppar>\()|",
        r"(?P<clpar>\))|",
        r"(?P<comma>,)|",
        r"(?P<operator>\S)"));

    for cap in token_re.captures_iter(preprocessed.as_str()) {
        let token = if cap.name("ident").is_some() {
            match cap.name("ident").unwrap() {
                "def" => Def,
                "extern" => Extern,
//> ch-0 lexer-tokenize
                "if" => If,
                "then" => Then,
                "else" => Else,
//> if-lexer
                "for" => For,
                "in" => In,
                "binary" => Binary,
                "unary" => Unary,
                "var" => Var,
//< ch-0 lexer-tokenize if-lexer
                ident => Ident(ident.to_string())
            }
        } else if cap.name("number").is_some() {
            match cap.name("number").unwrap().parse() {
                Ok(number) => Number(number),
                Err(_) => panic!("Lexer failed trying to parse number")
            }
        } else if cap.name("delimiter").is_some() {
            Delimiter
        } else if cap.name("oppar").is_some() {
            OpeningParenthesis
        } else if cap.name("clpar").is_some() {
            ClosingParenthesis
        } else if cap.name("comma").is_some() {
            Comma
        } else {
            Operator(cap.name("operator").unwrap().to_string())
        };

        result.push(token)
    }

    result
}
//> ch-0 lexer-tokenize if-lexer
