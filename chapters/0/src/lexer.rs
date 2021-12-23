pub use self::Token::{
    ClosingParenthesis, Comma, Def, Delimiter, Extern, Ident, Number, OpeningParenthesis, Operator,
};

macro_rules! regex {
    ($re:expr) => {
        regex::Regex::new($re).unwrap()
    };
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Def,
    Extern,
    Delimiter, //';' character
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(String),
}

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
