#[deriving(PartialEq, Clone, Show)]
pub enum Token {
    Def,
    Extern,
    Delimiter,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(String)
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let comment_re = regex!(r"(?m)#.*\n");
    let preprocessed = comment_re.replace_all(input, "\n");

    let mut result = Vec::new();

    let token_re = regex!(r"(?P<ident>\p{Alphabetic}\w*)|(?P<number>\d+\.?\d*)|(?P<delimiter>;)|(?P<oppar>\()|(?P<clpar>\))|(?P<comma>,)|(?P<operator>\S)");
    for cap in token_re.captures_iter(preprocessed.as_slice()) {

        let token = if !cap.name("ident").is_empty() {
            match cap.name("ident") {
                "def" => Def,
                "extern" => Extern,
                ident => Ident(ident.to_string())
            }
        } else if !cap.name("number").is_empty() {
            match from_str::<f64>(cap.name("number")) {
                Some(number) => Number(number),
                None => fail!("Lexer failed trying to parse number")
            }
        } else if !cap.name("delimiter").is_empty() {
            Delimiter
        } else if !cap.name("oppar").is_empty() {
            OpeningParenthesis
        } else if !cap.name("clpar").is_empty() {
            ClosingParenthesis
        } else if !cap.name("comma").is_empty() {
            Comma
        } else {
            Operator(cap.name("operator").to_string())
        };

        result.push(token)
    }

    result
}

#[test]
fn test_tokenize() {
    let result = tokenize("#testing example\nextern sin(ar_g1);#comment\n\tdef \ta 1.1, 2 2.2.2");
    assert_eq!(vec![Extern,
                    Ident("sin".to_string()),
                    OpeningParenthesis,
                    Ident("ar_g1".to_string()),
                    ClosingParenthesis,
                    Delimiter,
                    Def,
                    Ident("a".to_string()),
                    Number(1.1),
                    Comma,
                    Number(2.0),
                    Number(2.2),
                    Operator(".".to_string()),
                    Number(2.0)],
               result);
}
