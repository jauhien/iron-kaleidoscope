use std::collections::HashMap;

use lexer::*;

pub struct ParserSettings {
    operator_precedence: HashMap<String, i32>
}

#[deriving(PartialEq, Clone, Show)]
pub enum Expression {
    Literal(f64),
    Variable(String),
    Binary(String, Box<Expression>, Box<Expression>),
    Call(String, Vec<Expression>),
    Conditional{pub cond_expr: Box<Expression>, pub then_expr: Box<Expression>, pub else_expr: Box<Expression>}
}

#[deriving(PartialEq, Clone, Show)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>
}

#[deriving(PartialEq, Clone, Show)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Expression
}

#[deriving(PartialEq, Clone, Show)]
pub enum ASTNode {
    ExternNode(Prototype),
    FunctionNode(Function)
}

pub type ParsingResult = Result<(Vec<ASTNode>, Vec<Token>), String>;

pub fn default_parser_settings() -> ParserSettings {
    let mut operator_precedence = HashMap::new();
    operator_precedence.insert("<".to_string(), 10);
    operator_precedence.insert("+".to_string(), 20);
    operator_precedence.insert("-".to_string(), 20);
    operator_precedence.insert("*".to_string(), 40);

    ParserSettings{operator_precedence: operator_precedence}
}

pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &ParserSettings) -> ParsingResult {
    let mut rest = tokens.to_vec();
    rest.reverse();

    let mut ast = parsed_tree.to_vec();

    loop {
        let cur_token =
            match rest.last() {
                Some(token) => token.clone(),
                None => break
            };
        let result = match cur_token {
            Def => parse_function(&mut rest, settings),
            Extern => parse_extern(&mut rest, settings),
            Delimiter => {rest.pop(); continue},
            _ => parse_expression(&mut rest, settings)
        };
        match result {
            Good(ast_node, _) => ast.push(ast_node),
            NotComplete => break,
            Bad(message) => return Err(message)
        }
    }

    rest.reverse();
    Ok((ast, rest))
}

enum PartParsingResult<T> {
    Good(T, Vec<Token>),
    NotComplete,
    Bad(String)
}

fn error<T>(message : &str) -> PartParsingResult<T> {
    Bad(message.to_string())
}

macro_rules! parse_try(
    ($function:ident, $tokens:ident, $settings:ident, $parsed_tokens:ident) => (
        parse_try!($function, $tokens, $settings, $parsed_tokens,)
    );

    ($function:ident, $tokens:ident, $settings:ident, $parsed_tokens:ident, $($arg:expr),*) => (
        match $function($tokens, $settings, $($arg),*) {
            Good(ast, toks) => {
                $parsed_tokens.extend(toks.into_iter());
                ast
            },
            NotComplete => {
                $parsed_tokens.reverse();
                $tokens.extend($parsed_tokens.into_iter());
                return NotComplete;
            },
            Bad(message) => return Bad(message)
        }
    )
)

macro_rules! expect_token (
    ([ $($token:pat, $value:expr, $result:stmt);+ ] <= $tokens:ident, $parsed_tokens:ident, $error:expr) => (
        match $tokens.pop() {
            $(
                Some($token) => {
                    $parsed_tokens.push($value);
                    $result
                },
             )+
             None => {
                 $parsed_tokens.reverse();
                 $tokens.extend($parsed_tokens.into_iter());
                 return NotComplete;
             },
            _ => return error($error)
        }
    );
    ([ $($token:pat, $value:expr, $result:stmt);+ ] else $not_matched:block <= $tokens:ident, $parsed_tokens:ident) => (
        match $tokens.last().map(|i| {i.clone()}) {
            $(
                Some($token) => {
                    $tokens.pop();
                    $parsed_tokens.push($value);
                    $result
                },
             )+
            _ => {$not_matched}
        }
    )
)

fn parse_function(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<ASTNode> {
    tokens.pop();
    let mut parsed_tokens = vec!(Def);
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
    let body = parse_try!(parse_expr, tokens, settings, parsed_tokens);
    Good(FunctionNode(Function{prototype: prototype, body: body}), parsed_tokens)
}

fn parse_extern(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<ASTNode> {
    tokens.pop();
    let mut parsed_tokens = vec![Extern];
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
    Good(ExternNode(prototype), parsed_tokens)
}

fn parse_expression(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<ASTNode> {
    let mut parsed_tokens = Vec::new();
    let expression = parse_try!(parse_expr, tokens, settings, parsed_tokens);
    let prototype = Prototype{name: "".to_string(), args: vec![]};
    let lambda = Function{prototype: prototype, body: expression};
    Good(FunctionNode(lambda), parsed_tokens)
}

#[allow(unused_variable)]
fn parse_prototype(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Prototype> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!(
        [Ident(name), Ident(name.clone()), name] <= tokens,
        parsed_tokens, "expected function name in prototype");

    expect_token!(
        [OpeningParenthesis, OpeningParenthesis, ()] <= tokens,
        parsed_tokens, "expected '(' in prototype");

    let mut args = Vec::new();
    loop {
        expect_token!([
            Ident(arg), Ident(arg.clone()), args.push(arg.clone());
            Comma, Comma, continue;
            ClosingParenthesis, ClosingParenthesis, break
        ] <= tokens, parsed_tokens, "expected ')' in prototype");
    }

    let prototype = Prototype{name: name, args: args};
    Good(prototype, parsed_tokens)
}

fn parse_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();
    let lhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);
    let expr = parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, 0, &lhs);
    Good(expr, parsed_tokens)
}

fn parse_primary_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    match tokens.last() {
        Some(&Ident(_)) => parse_ident_expr(tokens, settings),
        Some(&Number(_)) => parse_literal_expr(tokens, settings),
        Some(&If) => parse_conditional_expr(tokens, settings),
        Some(&OpeningParenthesis) => parse_parenthesis_expr(tokens, settings),
        None => return NotComplete,
        _ => error("unknow token when expecting an expression")
    }
}

fn parse_ident_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!(
        [Ident(name), Ident(name.clone()), name] <= tokens,
        parsed_tokens, "identificator expected");

    expect_token!(
        [OpeningParenthesis, OpeningParenthesis, ()]
        else {return Good(Variable(name), parsed_tokens)}
        <= tokens, parsed_tokens);

    let mut args = Vec::new();
    loop {
        expect_token!(
            [ClosingParenthesis, ClosingParenthesis, break;
             Comma, Comma, continue]
            else {
                args.push(parse_try!(parse_expr, tokens, settings, parsed_tokens));
            }
            <= tokens, parsed_tokens);
    }

    Good(Call(name, args), parsed_tokens)
}

#[allow(unused_variable)]
fn parse_literal_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let value = expect_token!(
        [Number(val), Number(val), val] <= tokens,
        parsed_tokens, "literal expected");

    Good(Literal(value), parsed_tokens)
}

fn parse_conditional_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    tokens.pop();
    let mut parsed_tokens = vec![If];
    let cond_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [Then, Then, ()] <= tokens,
        parsed_tokens, "expected then");
    let then_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [Else, Else, ()] <= tokens,
        parsed_tokens, "expected else");
    let else_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    Good(Conditional{cond_expr: box cond_expr, then_expr: box then_expr, else_expr: box else_expr}, parsed_tokens)
}

fn parse_parenthesis_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    tokens.pop();
    let mut parsed_tokens = vec![OpeningParenthesis];

    let expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [ClosingParenthesis, ClosingParenthesis, ()] <= tokens,
        parsed_tokens, "')' expected");

    Good(expr, parsed_tokens)
}

fn parse_binary_expr(tokens : &mut Vec<Token>, settings : &ParserSettings, expr_precedence : i32, lhs : &Expression) -> PartParsingResult<Expression> {
    let mut result = lhs.clone();
    let mut parsed_tokens = Vec::new();

    loop {
        let (operator, precedence) = match tokens.last() {
            Some(&Operator(ref op)) => match settings.operator_precedence.find(op) {
                Some(pr) if *pr >= expr_precedence => (op.clone(), *pr),
                None => return error("unknown operator found"),
                _ => break
            },
            _ => break
        };
        tokens.pop();
        parsed_tokens.push(Operator(operator.clone()));

        let mut rhs;
        let primary_rhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);
        rhs = primary_rhs;

        loop {
            let binary_rhs = match tokens.last().map(|i| {i.clone()}) {
                Some(Operator(ref op)) => match settings.operator_precedence.find(op) {
                    Some(pr) if *pr > precedence => {
                        parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, *pr, &rhs)
                    },
                    None => return error("unknown operator found"),
                    _ => break
                },
                _ => break
            };

            rhs = binary_rhs;
        }

        result = Binary(operator, box result, box rhs);
    }

    Good(result, parsed_tokens)
}
