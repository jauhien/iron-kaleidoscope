//< ch-0 ch-1
use std::io;
use std::io::Write;
//> ch-0 ch-1

use iron_llvm::core::value::Value;
use iron_llvm::target;

use builder;
use builder::{IRBuilder, ModuleProvider};
use jitter;
use jitter::JITter;
//< ch-0 ch-1
use lexer::*;
//> ch-0
use parser::*;

//< ch-0
pub use self::Stage::{
//> ch-0 ch-1
    Exec,
    IR,
//< ch-1
    AST,
//< ch-0
    Tokens
};

//< parser-stage
#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
//> ch-0 ch-1 parser-stage
    Exec,
    IR,
//< ch-1 parser-stage
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
//> ch-1 parser-driver

    let mut ir_container : Box<JITter> = if stage == Exec {
        target::initilalize_native_target();
        target::initilalize_native_asm_printer();
        jitter::init();
        Box::new(
            jitter::MCJITter::new("main")
                )
    } else {
        Box::new(
            builder::SimpleModuleProvider::new("main")
                )
    };

    let mut builder_context = builder::Context::new();

//< ch-0 ch-1 parser-driver

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
//> ch-1 parser-driver

        match ast.codegen(&mut builder_context, ir_container.get_module_provider()) {
            Ok((value, runnable)) =>
                if runnable && stage == Exec {
                    println!("=> {}", ir_container.run_function(value));
                } else {
                    value.dump();
                },
            Err(message) => println!("Error occured: {}", message)
        }
//< ch-0 ch-1 parser-driver
    }
//> ch-0 ch-1 parser-driver

    if stage == IR || stage == Exec {
        ir_container.dump();
    }
//< ch-0 ch-1 parser-driver
}
//> ch-0 ch-1 parser-driver
