//< ch-0 ch-4 ch-5 ch-6 lexer-tokens-use
pub use self::Token::{
    //> ch-4
    Binary,
    ClosingParenthesis,
    Comma,
    Def,
    //< ch-0 ch-4 ch-5 lexer-tokens-use
    Delimiter,
    Else,
    Extern,
    For,
    Ident,
    //> ch-0 lexer-tokens-use
    If,
    In,
    Number,
    OpeningParenthesis,
    Operator,
    Then,
    Unary,
    //> ch-5
    Var,
};
//> lexer-tokens-use

//< lexer-tokens if-lexer for-lexer mutable-var-lexer
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
    //> ch-4 for-lexer
    Binary,
    Unary,
    //> ch-5
    Var,
    //< ch-0 ch-4 ch-5 lexer-tokens if-lexer for-lexer
    Delimiter, //';' character
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(String),
}
//> lexer-tokens

macro_rules! regex {
    ($re:expr) => {
        regex::Regex::new($re).unwrap()
    };
}

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
        r"(?P<operator>\S)"
    ));

    for cap in token_re.captures_iter(preprocessed.as_ref()) {
        let token = if cap.name("ident").is_some() {
            match cap.name("ident").unwrap().as_str() {
                "def" => Def,
                "extern" => Extern,
                //> ch-0 lexer-tokenize
                "if" => If,
                "then" => Then,
                "else" => Else,
                //> if-lexer
                "for" => For,
                "in" => In,
                //> ch-4 for-lexer
                "binary" => Binary,
                "unary" => Unary,
                //> ch-5
                "var" => Var,
                //< ch-0 ch-4 ch-5 lexer-tokenize if-lexer for-lexer
                ident => Ident(ident.to_string()),
            }
        } else if cap.name("number").is_some() {
            match cap.name("number").unwrap().as_str().parse() {
                Ok(number) => Number(number),
                Err(_) => panic!("Lexer failed trying to parse number"),
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
            Operator(cap.name("operator").unwrap().as_str().to_string())
        };

        result.push(token)
    }

    result
}
//> ch-0 ch-4 ch-5 ch-6 lexer-tokenize if-lexer for-lexer mutable-var-lexer
