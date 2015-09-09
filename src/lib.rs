#![feature(box_syntax)]
//< ch-0
#![feature(convert)]
#![feature(plugin)]
#![plugin(regex_macros)]

extern crate regex;
//> ch-0

extern crate iron_llvm;
extern crate llvm_sys;

pub mod builder;
//< ch-0
pub mod driver;
//> ch-0
pub mod jitter;
//< ch-0
pub mod lexer;
//> ch-0
pub mod parser;
