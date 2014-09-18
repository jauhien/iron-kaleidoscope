use std::io;

use lexer::*;
use parser::*;

pub fn main_loop() {
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
            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &default_parser_settings());
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

        println!("ast: {}", ast);
    }
}
