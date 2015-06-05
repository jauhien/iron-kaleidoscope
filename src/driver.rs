use std::io;

//use builder::*;
use lexer::*;
use parser::*;

pub use self::Stage::{
    Tokens,
    AST,
    IR,
    Exec
};

#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
    Tokens,
    AST,
    IR,
    Exec
}

pub fn main_loop(stage: Stage) {
    let mut parser_settings = default_parser_settings();
    //let mut context = Context::new("main");

    'main: loop {
        println!(">");
        let mut input = String::new();
        io::stdin().read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }

        // the constructed AST
        let mut ast = Vec::new();
        // tokens left from the previous lines
        let mut prev = Vec::new();
        loop {
            let tokens = tokenize(input.as_str());
            if stage == Tokens {
                println!("{:?}", tokens);
                continue 'main
            }

            prev.extend(tokens.into_iter());

            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &mut parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.extend(parsed_ast.into_iter());
                    if rest.is_empty() {
                        // we have parsed a full expression
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
            println!(".");
            input.clear();
            io::stdin().read_line(&mut input).ok().expect("Failed to read line");
        }

        if stage == AST {
            println!("{:?}", ast);
            continue
        }

        /*match ast.codegen(&mut context) {
            Ok((value, runnable)) => if runnable && stage == Exec {
                println!("=> {}", run(value, &context))
            } else {
                dump_value(value)
            },
            Err(message) => println!("Error occured: {}", message)
        }*/
    }

    /*if stage == IR || stage == Exec {
        context.dump();
    }*/
}
