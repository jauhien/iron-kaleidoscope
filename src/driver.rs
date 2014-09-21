use std::io;

use ir_builder::*;
use lexer::*;
use parser::*;

pub fn main_loop() {
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
            prev.push_all_move(tokens);
            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.push_all_move(parsed_ast);
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

        match ast.codegen(&mut context) {
            Ok(value) => dump_value(value),
            Err(message) => println!("Error occured: {}", message)
        }
    }

    context.dump();
}
