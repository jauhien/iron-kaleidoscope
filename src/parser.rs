//< ch-1 ch-4
use std::collections::HashMap;

use lexer::*;

pub use self::ASTNode::{
    ExternNode,
    FunctionNode
};

pub use self::Expression::{
    LiteralExpr,
    VariableExpr,
//> ch-1 ch-4
    UnaryExpr,
//< ch-1 ch-4
    BinaryExpr,
//> ch-1
    ConditionalExpr,
    LoopExpr,
//> ch-4
    VarExpr,
//< ch-1 ch-4
    CallExpr
};
//> ch-1 ch-4

pub use self::FunctionType::{
    Normal,
    UnaryOp,
    BinaryOp
};
//< ch-1 ch-4

use self::PartParsingResult::{
    Good,
    NotComplete,
    Bad
};

//< parser-astnode
#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode {
    ExternNode(Prototype),
    FunctionNode(Function)
}
//> parser-astnode

//< parser-defproto
#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Expression
}

//< binary-proto
#[derive(PartialEq, Clone, Debug)]
pub struct Prototype {
    pub name: String,
//> ch-1 ch-4 parser-defproto
    pub ftype: FunctionType,
//< ch-1 ch-4 parser-defproto
    pub args: Vec<String>
}
//> parser-defproto  binary-proto

//< parser-expr if-parser for-parser
#[derive(PartialEq, Clone, Debug)]
pub enum Expression {
    LiteralExpr(f64),
    VariableExpr(String),
//> ch-1  ch-4 parser-expr if-parser for-parser
//< unary-ast
    UnaryExpr(String, Box<Expression>),
//> unary-ast
//< ch-1  ch-4 parser-expr if-parser for-parser
    BinaryExpr(String, Box<Expression>, Box<Expression>),
//> ch-1 parser-expr
    ConditionalExpr{cond_expr: Box<Expression>, then_expr: Box<Expression>, else_expr: Box<Expression>},
//> if-parser
    LoopExpr{var_name: String, start_expr: Box<Expression>, end_expr: Box<Expression>, step_expr: Box<Expression>, body_expr: Box<Expression>},
//> ch-4 for-parser
    VarExpr{vars: Vec<(String, Expression)>, body_expr: Box<Expression>},
//< ch-1 ch-4 parser-expr if-parser for-parser
    CallExpr(String, Vec<Expression>)
}
//> ch-1 ch-4 parser-expr if-parser for-parser
//<  binary-proto

//< unary-ftype
#[derive(PartialEq, Clone, Debug)]
pub enum FunctionType {
    Normal,
//>  binary-proto
    UnaryOp(String),
//<  binary-proto
    BinaryOp(String, i32)
}
//>  binary-proto unary-ftype

//< ch-1 ch-4 parser-result
pub type ParsingResult = Result<(Vec<ASTNode>, Vec<Token>), String>;
//> parser-result

//< parser-part-result
enum PartParsingResult<T> {
    Good(T, Vec<Token>),
    NotComplete,
    Bad(String)
}
//> parser-part-result

//< parser-error
fn error<T>(message : &str) -> PartParsingResult<T> {
    Bad(message.to_string())
}
//> parser-error

//< parser-settings
pub struct ParserSettings {
    operator_precedence: HashMap<String, i32>
}
//> parser-settings

//< parser-default-settings
pub fn default_parser_settings() -> ParserSettings {
    let mut operator_precedence = HashMap::new();
    operator_precedence.insert("=".to_string(), 2);
    operator_precedence.insert("<".to_string(), 10);
    operator_precedence.insert("+".to_string(), 20);
    operator_precedence.insert("-".to_string(), 20);
    operator_precedence.insert("*".to_string(), 40);

    ParserSettings{operator_precedence: operator_precedence}
}
//> parser-default-settings

//< parser-parse parser-parse-sign
pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &mut ParserSettings) -> ParsingResult
//> parser-parse-sign
{
    let mut rest = tokens.to_vec();
    // we read tokens from the end of the vector
    // using it as a stack
    rest.reverse();

    // we will add new AST nodes to already parsed ones
    let mut ast = parsed_tree.to_vec();

    loop {
        // look at the current token and determine what to parse
        // based on its value
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

    // unparsed tokens
    rest.reverse();
    Ok((ast, rest))
}
//> parser-parse

//< parser-parse-try
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
);
//> parser-parse-try

//< parser-expect-token
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
);
//> parser-expect-token

//< parser-parse-extern
fn parse_extern(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    // eat Extern token
    tokens.pop();
    let mut parsed_tokens = vec![Extern];
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
    Good(ExternNode(prototype), parsed_tokens)
}
//> parser-parse-extern

//< parser-parse-function ops-parse-func
fn parse_function(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    // eat Def token
    tokens.pop();
    let mut parsed_tokens = vec!(Def);
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
//> ch-1 ch-4 parser-parse-function

    match prototype.ftype {
        BinaryOp(ref symbol, precedence) => {
            settings.operator_precedence.insert(symbol.clone(), precedence);
        },
        _ => ()
    };

//< ch-1 ch-4 parser-parse-function
    let body = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    Good(FunctionNode(Function{prototype: prototype, body: body}), parsed_tokens)
}
//> parser-parse-function ops-parse-func

//< parser-parse-prototype binary-parse-proto unary-parse-proto
fn parse_prototype(tokens : &mut Vec<Token>, _settings : &mut ParserSettings) -> PartParsingResult<Prototype> {
    let mut parsed_tokens = Vec::new();

    let 
//> ch-1 ch-4 parser-parse-prototype
/*j*/   (
//< ch-1 ch-4 parser-parse-prototype
/*j*/    name
//> ch-1 ch-4 parser-parse-prototype
/*j*/    , ftype
/*j*/   )
//< ch-1 ch-4 parser-parse-prototype
/*jw*/  = expect_token!([
            Ident(name), Ident(name.clone()), 
//> ch-1 ch-4 parser-parse-prototype
/*j*/       (
//< ch-1 ch-4 parser-parse-prototype
/*j*/        name
//> ch-1 ch-4 parser-parse-prototype
/*j*/        , Normal
/*j*/       );
//> binary-parse-proto
            Unary, Unary, {
                let op = expect_token!([
                        Operator(op), Operator(op.clone()), op
                    ] <= tokens, parsed_tokens, "expected unary operator");
                ("unary".to_string() + &op, UnaryOp(op))
            };
//< binary-parse-proto
            Binary, Binary, {
                let op = expect_token!([
                        Operator(op), Operator(op.clone()), op
                    ] <= tokens, parsed_tokens, "expected binary operator");
                let precedence = expect_token!(
                    [Number(value), Number(value), value as i32]
                    else {30}
                    <= tokens, parsed_tokens);

                if precedence < 1 || precedence > 100 {
                    return error("invalid precedecnce: must be 1..100");
                }

                ("binary".to_string() + &op, BinaryOp(op, precedence))
            }
//< ch-1 ch-4 parser-parse-prototype
        ] <= tokens, parsed_tokens, "expected function name in prototype");

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
//> ch-1 ch-4 parser-parse-prototype

    match ftype {
//> binary-parse-proto
        UnaryOp(_) => if args.len() != 1 {
            return error("invalid number of operands for unary operator")
        },
//< binary-parse-proto
        BinaryOp(_, _) => if args.len() != 2 {
            return error("invalid number of operands for binary operator")
        },
        _ => ()
    };

//< ch-1 ch-4 parser-parse-prototype

    Good(Prototype{name: name, args: args
//> ch-1 ch-4 parser-parse-prototype
/*j*/              , ftype: ftype
//< ch-1 ch-4 parser-parse-prototype
/*j*/             }, parsed_tokens)
}
//> parser-parse-prototype binary-parse-proto unary-parse-proto

//< parser-parse-expression
fn parse_expression(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    let mut parsed_tokens = Vec::new();
    let expression = parse_try!(parse_expr, tokens, settings, parsed_tokens);
    let prototype = Prototype{name: "".to_string(), args: vec![]
//> ch-1 ch-4 parser-parse-expression
                              , ftype: Normal
//< ch-1 ch-4 parser-parse-expression
/*j*/                         };
    let lambda = Function{prototype: prototype, body: expression};
    Good(FunctionNode(lambda), parsed_tokens)
}
//> parser-parse-expression
//< if-parser for-parser

//< parser-parse-primary-expr unary-parse-expr
fn parse_primary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    match tokens.last() {
        Some(&Ident(_)) => parse_ident_expr(tokens, settings),
        Some(&Number(_)) => parse_literal_expr(tokens, settings),
//> ch-1 parser-parse-primary-expr
        Some(&If) => parse_conditional_expr(tokens, settings),
//> if-parser
        Some(&For) => parse_loop_expr(tokens, settings),
//> ch-4 for-parser unary-parse-expr
        Some(&Var) => parse_var_expr(tokens, settings),
//< unary-parse-expr
        Some(&Operator(_)) => parse_unary_expr(tokens, settings),
//< ch-1 ch-4 parser-parse-primary-expr if-parser for-parser
        Some(&OpeningParenthesis) => parse_parenthesis_expr(tokens, settings),
        None => return NotComplete,
        _ => error("unknow token when expecting an expression")
    }
}
//> parser-parse-primary-expr if-parser for-parser unary-parse-expr

//< parser-parse-ident-expr
fn parse_ident_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!(
        [Ident(name), Ident(name.clone()), name] <= tokens,
        parsed_tokens, "identificator expected");

    expect_token!(
        [OpeningParenthesis, OpeningParenthesis, ()]
        else {return Good(VariableExpr(name), parsed_tokens)}
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

    Good(CallExpr(name, args), parsed_tokens)
}
//> parser-parse-ident-expr

//< parser-parse-literal-expr
fn parse_literal_expr(tokens : &mut Vec<Token>, _settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let value = expect_token!(
        [Number(val), Number(val), val] <= tokens,
        parsed_tokens, "literal expected");

    Good(LiteralExpr(value), parsed_tokens)
}
//> parser-parse-literal-expr

//< parser-parse-parenthesis-expr
fn parse_parenthesis_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    // eat the opening parenthesis
    tokens.pop();
    let mut parsed_tokens = vec![OpeningParenthesis];

    let expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [ClosingParenthesis, ClosingParenthesis, ()] <= tokens,
        parsed_tokens, "')' expected");

    Good(expr, parsed_tokens)
}
//> parser-parse-parenthesis-expr

//< parser-parse-expr
fn parse_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();
    let lhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);
    let expr = parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, 0, &lhs);
    Good(expr, parsed_tokens)
}
//> parser-parse-expr

//< parser-parse-binary-expr
fn parse_binary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings, expr_precedence : i32, lhs : &Expression) -> PartParsingResult<Expression> {
    // start with LHS value
    let mut result = lhs.clone();
    let mut parsed_tokens = Vec::new();

    loop {
        // continue until the current token is not an operator
        // or it is an operator with precedence lesser than expr_precedence
        let (operator, precedence) = match tokens.last() {
            Some(&Operator(ref op)) => match settings.operator_precedence.get(op) {
                Some(pr) if *pr >= expr_precedence => (op.clone(), *pr),
                None => return error("unknown operator found"),
                _ => break
            },
            _ => break
        };
        tokens.pop();
        parsed_tokens.push(Operator(operator.clone()));

        // parse primary RHS expression
        let mut rhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);

        // parse all the RHS operators until their precedence is
        // bigger than the current one
        loop {
            let binary_rhs = match tokens.last().map(|i| {i.clone()}) {
                Some(Operator(ref op)) => match settings.operator_precedence.get(op).map(|i| {*i}) {
                    Some(pr) if pr > precedence => {
                        parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, pr, &rhs)
                    },
                    None => return error("unknown operator found"),
                    _ => break
                },
                _ => break
            };

            rhs = binary_rhs;
        }

        // merge LHS and RHS
        result = BinaryExpr(operator, box result, box rhs);
    }

    Good(result, parsed_tokens)
}
//> ch-1 parser-parse-binary-expr
//< if-parser

fn parse_conditional_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
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

    Good(ConditionalExpr{cond_expr: box cond_expr, then_expr: box then_expr, else_expr: box else_expr}, parsed_tokens)
}
//> if-parser
//< for-parser

fn parse_loop_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    tokens.pop();
    let mut parsed_tokens = vec![For];
    let var_name = expect_token!(
        [Ident(name), Ident(name.clone()), name] <= tokens,
        parsed_tokens, "expected identifier after for");

    expect_token!(
        [Operator(op), Operator(op.clone()), {
            if op.as_str() != "=" {
                return error("expected '=' after for")
            }
        }] <= tokens,
        parsed_tokens, "expected '=' after for");

    let start_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [Comma, Comma, ()] <= tokens,
        parsed_tokens, "expected ',' after for start value");

    let end_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    let step_expr = expect_token!(
        [Comma, Comma, parse_try!(parse_expr, tokens, settings, parsed_tokens)]
        else {LiteralExpr(1.0)}
        <= tokens, parsed_tokens);

    expect_token!(
        [In, In, ()] <= tokens,
        parsed_tokens, "expected 'in' after for");

    let body_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    Good(LoopExpr{var_name: var_name, start_expr: box start_expr, end_expr: box end_expr, step_expr: box step_expr, body_expr: box body_expr}, parsed_tokens)
}
//> ch-4 for-parser

fn parse_var_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    tokens.pop();
    let mut parsed_tokens = vec![Var];
    let mut vars = Vec::new();

    loop {
        let var_name = expect_token!(
            [Ident(name), Ident(name.clone()), name] <= tokens,
            parsed_tokens, "expected identifier list after var");

        let init_expr = expect_token!(
            [Operator(op), Operator(op.clone()), {
                if op.as_str() != "=" {
                    return error("expected '=' in variable initialization")
                }
                parse_try!(parse_expr, tokens, settings, parsed_tokens)
            }]
            else {LiteralExpr(0.0)}
            <= tokens, parsed_tokens);

        vars.push((var_name, init_expr));

        expect_token!(
            [Comma, Comma, ()]
            else {break}
            <= tokens, parsed_tokens);
    }

    expect_token!(
        [In, In, ()] <= tokens,
        parsed_tokens, "expected 'in' after var");

    let body_expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    Good(VarExpr{vars: vars, body_expr: box body_expr}, parsed_tokens)
}
//< unary-parse-expr

fn parse_unary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!(
        [Operator(name), Operator(name.clone()), name] <= tokens,
        parsed_tokens, "unary operator expected");

    let operand = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);

    Good(UnaryExpr(name, box operand), parsed_tokens)
}
//> unary-parse-expr
