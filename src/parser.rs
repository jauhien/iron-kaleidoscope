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
    Call(String, Vec<Expression>)
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
            Def => parse_function(&mut rest, &mut ast, settings),
            Extern => parse_extern(&mut rest, &mut ast, settings),
            Delimiter => {rest.pop(); continue},
            _ => parse_expression(&mut rest, &mut ast, settings)
        };
        match result {
            Ok(true) => (),
            Ok(false) => break,
            Err(message) => return Err(message)
        };
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

fn parse_function(tokens : &mut Vec<Token>, parsed_tree : &mut Vec<ASTNode>, settings : &ParserSettings) -> Result<bool, String> {
    tokens.pop();

    let (prototype, mut proto_tokens) = match parse_prototype(tokens, settings) {
        Good(proto, pr_tok) => (proto, pr_tok),
        NotComplete => {
            tokens.push(Def);
            return Ok(false)
        },
        Bad(message) => return Err(message)
    };

    let body = match parse_expr(tokens, settings) {
        Good(expr, _) => expr,
        NotComplete => {
            proto_tokens.reverse();
            tokens.extend(proto_tokens.into_iter());
            tokens.push(Def);
            return Ok(false)
        },
        Bad(message) => return Err(message)
    };

    let node = FunctionNode(Function{prototype: prototype, body: body});
    parsed_tree.push(node);

    Ok(true)
}

fn parse_extern(tokens : &mut Vec<Token>, parsed_tree : &mut Vec<ASTNode>, settings : &ParserSettings) -> Result<bool, String> {
    tokens.pop();

    let prototype = match parse_prototype(tokens, settings) {
        Good(proto, _) => proto,
        NotComplete => {
            tokens.push(Extern);
            return Ok(false)
        },
        Bad(message) => return Err(message)
    };

    let node = ExternNode(prototype);
    parsed_tree.push(node);

    Ok(true)
}

fn parse_expression(tokens : &mut Vec<Token>, parsed_tree : &mut Vec<ASTNode>, settings : &ParserSettings) -> Result<bool, String> {
    let expression = match parse_expr(tokens, settings) {
        Good(expr, _) => expr,
        NotComplete => return Ok(false),
        Bad(message) => return Err(message)
    };

    let prototype = Prototype{name: "".to_string(), args: vec![]};
    let lambda = Function{prototype: prototype, body: expression};

    let node = FunctionNode(lambda);
    parsed_tree.push(node);

    Ok(true)
}

#[allow(unused_variable)]
fn parse_prototype(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Prototype> {
    let name = match tokens.pop() {
        Some(Ident(name)) => name,
        None => return NotComplete,
        _ => return error("expected function name in prototype")
    };

    match tokens.pop() {
        Some(OpeningParenthesis) => (),
        None => {
            tokens.push(Ident(name));
            return NotComplete
        },
        _ => return error("expected '(' in prototype")
    }

    let mut args = Vec::new();
    let mut proto_tokens = vec![Ident(name.clone()), OpeningParenthesis];

    loop {
        match tokens.pop() {
            Some(Ident(arg)) => {
                args.push(arg.clone());
                proto_tokens.push(Ident(arg))
            },
            Some(Comma) => {
                proto_tokens.push(Comma);
                continue
            },
            Some(ClosingParenthesis) => {
                proto_tokens.push(ClosingParenthesis);
                break
            }
            None => {
                proto_tokens.reverse();
                tokens.extend(proto_tokens.into_iter());
                return NotComplete
            },
            _ => return error("expected ')' in prototype")
        };
    }

    let prototype = Prototype{name: name, args: args};
    Good(prototype, proto_tokens)
}

fn parse_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let (lhs, mut parsed_tokens) = match parse_primary_expr(tokens, settings) {
        Good(lhs_expr, lhs_toks) => (lhs_expr, lhs_toks),
        NotComplete => return NotComplete,
        Bad(message) => return Bad(message)
    };

    match parse_binary_expr(0, &lhs, tokens, settings) {
        Good(expr, expr_toks) => {
            parsed_tokens.extend(expr_toks.into_iter());
            Good(expr, parsed_tokens)
        },
        NotComplete => {
            parsed_tokens.reverse();
            tokens.extend(parsed_tokens.into_iter());
            NotComplete
        },
        Bad(message) => Bad(message)
    }
}

fn parse_primary_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    match tokens.last() {
        Some(&Ident(_)) => parse_ident_expr(tokens, settings),
        Some(&Number(_)) => parse_literal_expr(tokens, settings),
        Some(&OpeningParenthesis) => parse_parenthesis_expr(tokens, settings),
        None => return NotComplete,
        _ => error("unknow token when expecting an expression")
    }
}

fn parse_ident_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let name = match tokens.pop() {
        Some(Ident(nm)) => nm,
        None => return NotComplete,
        _ => return error("identificator expected")
    };

    let mut ident_tokens = vec![Ident(name.clone())];

    match tokens.last() {
        Some(&OpeningParenthesis) => {
            tokens.pop();
            ident_tokens.push(OpeningParenthesis);
        }
        _ => return Good(Variable(name), ident_tokens)
    }

    let mut args = Vec::new();

    loop {
        match tokens.last().map(|i| {i.clone()}) {
            Some(ClosingParenthesis) => {
                tokens.pop();
                ident_tokens.push(ClosingParenthesis);
                break
            },
            Some(Comma) => {
                tokens.pop();
                ident_tokens.push(Comma);
                continue
            },
            _ => {
                let (arg, arg_tokens) = match parse_expr(tokens, settings) {
                    Good(arg_expr, arg_toks) => (arg_expr, arg_toks),
                    NotComplete => {
                        ident_tokens.reverse();
                        tokens.extend(ident_tokens.into_iter());
                        return NotComplete
                    },
                    Bad(message) => return Bad(message)
                };

                args.push(arg);
                ident_tokens.extend(arg_tokens.into_iter());
            }
        }
    }

    let expr = Call(name, args);
    Good(expr, ident_tokens)
}

#[allow(unused_variable)]
fn parse_literal_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    let value = match tokens.pop() {
        Some(Number(val)) => val,
        None => return NotComplete,
        _ => return error("literal expected")
    };

    Good(Literal(value), vec![Number(value)])
}

fn parse_parenthesis_expr(tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
    tokens.pop();
    match parse_expr(tokens, settings) {
        Good(expr, expr_toks) => {
            let mut expr_tokens = vec![OpeningParenthesis];
            expr_tokens.extend(expr_toks.into_iter());
            match tokens.pop() {
                Some(ClosingParenthesis) => {
                    expr_tokens.push(ClosingParenthesis);
                    Good(expr, expr_tokens)
                }
                None => {
                    expr_tokens.reverse();
                    tokens.extend(expr_tokens.into_iter());
                    NotComplete
                }
                _ => error("expression expected")
            }
        }
        NotComplete => {
            tokens.push(OpeningParenthesis);
            NotComplete
        }
        Bad(message) => Bad(message)
    }
}

fn parse_binary_expr(expr_precedence : i32, lhs : &Expression, tokens : &mut Vec<Token>, settings : &ParserSettings) -> PartParsingResult<Expression> {
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

        let (primary_rhs, primary_rhs_tokens) = match parse_primary_expr(tokens, settings) {
            Good(expr, toks) => (expr, toks),
            NotComplete => {
                parsed_tokens.reverse();
                tokens.extend(parsed_tokens.into_iter());
                return NotComplete
            },
            Bad(message) => return Bad(message)
        };

        rhs = primary_rhs;
        parsed_tokens.extend(primary_rhs_tokens.into_iter());

        loop {
            let binary_rhs_result = match tokens.last().map(|i| {i.clone()}) {
                Some(Operator(ref op)) => match settings.operator_precedence.find(op) {
                    Some(pr) if *pr > precedence => {
                        parse_binary_expr(*pr, &rhs, tokens, settings)
                    },
                    None => return error("unknown operator found"),
                    _ => break
                },
                _ => break
            };

            let (binary_rhs, binary_rhs_tokens) = match binary_rhs_result {
                Good(expr, toks) => (expr, toks),
                NotComplete => {
                    parsed_tokens.reverse();
                    tokens.extend(parsed_tokens.into_iter());
                    return NotComplete
                },
                Bad(message) => return Bad(message)
            };

            rhs = binary_rhs;
            parsed_tokens.extend(binary_rhs_tokens.into_iter());
        }

        result = Binary(operator, box result, box rhs);
    }

    Good(result, parsed_tokens)
}
