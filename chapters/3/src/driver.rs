use std::io;
use std::io::Write;

use iron_llvm::core::value::Value;
use iron_llvm::target;

use builder;
use builder::{IRBuilder, ModuleProvider};
use jitter;
use jitter::JITter;
use lexer::*;
use parser::*;

pub use self::Stage::{
    Exec,
    IR,
    AST,
    Tokens
};

#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
    Exec,
    IR,
    AST,
    Tokens
}

pub fn main_loop(stage: Stage) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();
    let mut parser_settings = default_parser_settings();
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


    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
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
            print!(". ");
            stdout.flush().unwrap();
            input.clear();
            stdin.read_line(&mut input).ok().expect("Failed to read line");
        }

        if stage == AST {
            println!("{:?}", ast);
            continue
        }

        match ast.codegen(&mut builder_context,
                          ir_container.get_module_provider()) {
            Ok((value, runnable)) =>
                if runnable && stage == Exec {
                    println!("=> {}", ir_container.run_function(value));
                } else {
                    value.dump();
                },
            Err(message) => println!("Error occured: {}", message)
        }
    }

    if stage == IR || stage == Exec {
        ir_container.dump();
    }
}
