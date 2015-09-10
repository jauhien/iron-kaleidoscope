use std::io;
use std::io::Write;
use lexer::*;
pub use self::Stage::{
    Tokens
};

#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
    Tokens
}

pub fn main_loop(stage: Stage) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    'main: loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).ok().expect("Failed to read line");
        if input.as_str() == ".quit\n" {
            break;
        }

        loop {
            let tokens = tokenize(input.as_str());
            if stage == Tokens {
                println!("{:?}", tokens);
                continue 'main
            }
        }
    }
}
