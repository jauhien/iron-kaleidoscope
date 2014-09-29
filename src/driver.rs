use std::io;

use builder::*;
use lexer::*;
use parser::*;

#[deriving(PartialEq, Clone, Show)]
pub enum Stage {
    Tokens,
    AST,
    IR,
    Exec
}

pub fn main_loop(stage: Stage) {
    let parser_settings = default_parser_settings();
    let mut context = Context::new("main");

    'main: loop {
        print!(">");
        let mut input = io::stdin().read_line().ok().expect("Failed to read line");
        if input.as_slice() == ".quit\n" {
            break;
        }

        let mut ast = Vec::new();
        let mut prev = Vec::new();
        loop {
            let tokens = tokenize(input.as_slice());
            if stage == Tokens {
                println!("{}", tokens);
                continue 'main
            }

            prev.extend(tokens.into_iter());

            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.extend(parsed_ast.into_iter());
                    if rest.is_empty() {
                        break
                    } else {
                        prev = rest;
                    }
                },
                Err(message) => {
                    println!("Error occured: {}", message);
                    continue 'main
                }
            }
            print!(".");
            input = io::stdin().read_line().ok().expect("Failed to read line");
        }

        if stage == AST {
            println!("{}", ast);
            continue
        }

        match ast.codegen(&mut context) {
            Ok((value, runnable)) => if runnable && stage == Exec {
                println!("=> {}", run(value, &context))
            } else {
                dump_value(value)
            },
            Err(message) => println!("Error occured: {}", message)
        }
    }

    if stage == IR || stage == Exec {
        context.dump();
    }
}
