//< ch-0 ch-1 ch-2 ch-3
use std::io;
use std::io::Write;
//> ch-0 ch-1

use iron_llvm::core::value::Value;
//> ch-2
use iron_llvm::target;
//< ch-2

use builder;
use builder::{IRBuilder, ModuleProvider};
//> ch-2
use jitter;
use jitter::JITter;
//< ch-0 ch-1 ch-2
use lexer::*;
//> ch-0
use parser::*;
//> ch-1 ch-2 ch-3
use filer;
//< ch-1 ch-2 ch-3

//< ch-0
pub use self::Stage::{
//> ch-0 ch-1 ch-2
    Exec,
//< ch-2
    IR,
//< ch-1
    AST,
//< ch-0
    Tokens
};

//< parser-stage
#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
//> ch-0 ch-1  ch-2 parser-stage
    Exec,
//< ch-2
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
//> ch-1 ch-2 ch-3 parser-driver
/*
//< ch-2
    let mut ir_container = builder::SimpleModuleProvider::new("main");
//> ch-2
*/
//< ch-3
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

//< ch-2
    let mut builder_context = builder::Context::new();

//> ch-2 ch-3
    match filer::load_stdlib(&mut parser_settings, &mut builder_context, ir_container.get_module_provider()) {
        Ok((value, runnable)) => if runnable && stage == Exec {
            ir_container.run_function(value);
        },
        Err(err) => print!("Error occured during stdlib loading: {}\n", err)
    };
//< ch-2 ch-3
//< ch-0 ch-1 parser-driver

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
//> ch-0 ch-1 ch-2 ch-3 parser-driver
        } else if &input[0..5] == ".load" {
            let mut path = input[6..].to_string();
            match path.pop() {
                Some(_) => (),
                None => {
                    print!("Error occured during loading: empty path\n");
                    continue;
                }
            };
            match filer::load_ks(path, &mut parser_settings, &mut builder_context, ir_container.get_module_provider()) {
                Ok((value, runnable)) => if runnable && stage == Exec {
                    ir_container.run_function(value);
                },
                Err(err) => print!("Error occured during loading: {}\n", err)
            };
            continue;
        } else if &input[0..5] == ".dump" {
            let mut path = input[6..].to_string();
            match path.pop() {
                Some(_) => (),
                None => {
                    print!("Error occured during dumping: empty path\n");
                    continue;
                }
            };
            match filer::dump_bitcode(&path, ir_container.get_module_provider()) {
                Ok(_) => (),
                Err(_) => print!("Error occured during dumping\n")
            };
            continue;
        } else if input.as_str() == ".help\n" {
            print!("Enter Kaleidoscope expressions or special commands.\n");
            print!("Special commands are:\n");
            print!(".quit -- quit\n");
            print!(".load <path> -- load .ks file\n");
            print!(".dump <path> -- dump bitcode of currently open module\n");
            print!(".help -- show this help message\n");
            continue;
//< ch-0 ch-1 ch-2 ch-3 parser-driver
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

        match ast.codegen(&mut builder_context,
//> ch-2
                          ir_container.get_module_provider()
//> ch-3
/*
//< ch-2
                          &mut ir_container) {
            Ok((value, _)) => value.dump(),
//> ch-2
*/
//< ch-3
/*j*/                     ) {
            Ok((value, runnable)) =>
                if runnable && stage == Exec {
                    println!("=> {}", ir_container.run_function(value));
                } else {
                    value.dump();
                },
//< ch-2
            Err(message) => println!("Error occured: {}", message)
        }
//< ch-0 ch-1 parser-driver
    }
//> ch-0 ch-1 parser-driver

    if stage == IR
//> ch-2
/*jw*/  || stage == Exec
//< ch-2
/*jw*/ {
        ir_container.dump();
    }
//< ch-0 ch-1 parser-driver
}
//> ch-0 ch-1 ch-2 ch-3 parser-driver
