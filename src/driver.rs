//< ch-0
use std::io;
use std::io::Write;
//> ch-0

use iron_llvm::core::value::Value;
use iron_llvm::target;

use builder;
use builder::IRBuilder;

use jitter;
//< ch-0
use lexer::*;
//> ch-0
use parser::*;

//< ch-0
pub use self::Stage::{
//> ch-0
    Exec,
    IR,
    AST,
//< ch-0
    Tokens
};
//> ch-0

//< ch-0 parser-stage
#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
//> ch-0 parser-stage
    Exec,
    IR,
//< parser-stage
    AST,
//< ch-0
    Tokens
}
//> parser-stage

//< parser-driver
pub fn main_loop(stage: Stage) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();
//> ch-0
    let mut parser_settings = default_parser_settings();
//> parser-driver
    jitter::init();
    let mut mcjitter = jitter::MCJITter::new("main");
    let mut builder_context = builder::Context::new();

    if stage == Exec {
        target::initilalize_native_target();
        target::initilalize_native_asm_printer();
    }
//< ch-0 parser-driver

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }

//> ch-0
        // the constructed AST
        let mut ast = Vec::new();
        // tokens left from the previous lines
        let mut prev = Vec::new();
//< ch-0
        loop {
            let tokens = tokenize(input.as_str());
            if stage == Tokens {
                println!("{:?}", tokens);
                continue 'main
            }
//> ch-0
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
            print!(". ");
            stdout.flush().unwrap();
            input.clear();
            stdin.read_line(&mut input).ok().expect("Failed to read line");
//< ch-0
        }
//> ch-0

        if stage == AST {
            println!("{:?}", ast);
            continue
        }
//> parser-driver

        match ast.codegen(&mut builder_context, &mut mcjitter) {
            Ok((value, runnable)) =>
                if runnable && stage == Exec {
                    mcjitter.close_current_module();
                    println!("=> {}", mcjitter.run_function(value));
                } else {
                    value.dump();
                },
            Err(message) => println!("Error occured: {}", message)
        }
//< ch-0 parser-driver
    }
//> ch-0 parser-driver

    if stage == IR {
        mcjitter.get_current_module().dump();
    } else if stage == Exec {
        mcjitter.dump();
        mcjitter.get_current_module().dump();
    }
//< ch-0 parser-driver
}
//> ch-0 parser-driver
