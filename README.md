# [LLVM tutorial](http://llvm.org/docs/tutorial) in [the Rust language](http://www.rust-lang.org/).

I have just started working on the text, for the full tutorial have a look at the first link.
The code for tutorial is fully implemented (it means, it has all the features described in the original
tutorial and corresponds to the state of code for the [Chapter 7](http://llvm.org/docs/tutorial/LangImpl7.html)).

## Table of Contents

* [Introduction](#introduction)
  * [Basic variant of the Kaleidoscope language](#basic-variant-of-the-kaleidoscope-language)
  * [The lexer](#the-lexer)
  * [The driver](#the-driver)
* [Parser and AST implementation](#parser-and-ast-implementation)
* [LLVM IR code generation](#llvm-ir-code-generation)
* [JIT and optimizer support](#jit-and-optimizer-support)
* [Extending Kaleidoscope: control flow](#extending-kaleidoscope-control-flow)
* [Extending Kaleidoscope: user-defined operators](#extending-kaleidoscope-user-defined-operators)
* [Extending Kaleidoscope: mutable variables](#extending-kaleidoscope-mutable-variables)


## Introduction

This tutorial shows how to implement a simple programming language using LLVM and Rust.
Its first goal is to show how to use LLVM to create a simple REPL, so some knowledge of Rust is assumed.
To be honest, author himself is a very beginner both in Rust and LLVM, so any feedback is highly
appreciated.

In the tutorial only pieces of code are shown. To create a fully working program for a given chapter, you'll need
to do an additional work (look at it as an exercise). The code in the repository corresponds to the state of
your program at the end of the last chapter and serves as a starting point for further experiments. If you want to
have a code that corresponds to the different chapters, you can try to look at the repository history, as I have created
code in parallel with reading of the original tutorial. But as Rust evolves quickly, the early versions of the code
probably would be not compilable by the latest Rust compiler.

To experiment with the code in this repo you need:

* the latest Rust compiler

* Cargo Rust package manager

* LLVM (I have used ver. 3.5)

* Python 2 interpreter

To build the code just clone the repo and execute

```
cargo build
```

Then you will find an executable named `iron-kaleidoscope` in the `target` directory.

### Basic variant of the Kaleidoscope language

In this tutorial we will use a simple functional language named Kaleidoscope. In this chapter its basic variant will be presented.
New features will be added in the next chapters step by step.

The language has only one type: 64-bit floating point numbers (f64 in the Rust terminology).

The first variant of the language is very limited and even not Turing complete. It includes only
function defenitions (or declarations) and function invocations together with some simple arithmetic operators.
Examples follow.

Arithmetic expression:

```
1 + 2 * (3 - 4);
```

Function definition:

```
def plus(x, y)
    x + y
```

Function invocation:

```
plus(1 2)
```

Extern function declaration and invocation:

```
extern sin(x);

sin(1)
```

Every statement except of definitions and declarations in Kaleidoscope is an expression and has the corresponding value. Quite
similar to Rust. Function body is just an expresion, its value is returned. No explicit return operator is used.

To show the end of an expression or definition (declaration) we use ';' character. ',' character in function prototypes/calls
is equivalent to the space character. Comments are started by '#' and last until the end of the line.

Names of variables start with an alphabetical character and contain any number of alphanumerical characters. Reserved words at the
moment include `def` and `extern`. Any non-alphanumerical non-whitespace character different from '(', ')', ';' and ',' is treated as an operator.

A number literal is a nonempty sequence of decimal digits, possibly containing a decimal point character.

### The lexer

To create a REPL we will need:

* the lexer

* the parser

* the IR builder

* the JIT compiler

Let's start from the first one. To implement it we'll use a regular expressions. We have next types of tokens:

* def and extern keywords

* identifier

* number literal

* semicolon

* opening and closing parenthesis

* comma

* operator

The corresponding enumeration looks like this:

```rust
#[deriving(PartialEq, Clone, Show)]
pub enum Token {
    Def,
    Extern,
    Delimiter, //';' character
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident(String),
    Number(f64),
    Operator(String)
}
```

Our parser function will accept a string with input characters and produce a vector of tokens. It will look like this:

```rust
pub fn tokenize(input: &str) -> Vec<Token> {
    // regex for commentaries (start with #, end with the line end)
    let comment_re = regex!(r"(?m)#.*\n");
    // remove commentaries from the input stream
    let preprocessed = comment_re.replace_all(input, "\n");

    let mut result = Vec::new();

    // regex for token (just union of straightforward regexes for different token types)
    // operators are parsed the same way as identifier and separated later
    let token_re = regex!(r"(?P<ident>\p{Alphabetic}\w*)|(?P<number>\d+\.?\d*)|(?P<delimiter>;)|(?P<oppar>\()|(?P<clpar>\))|(?P<comma>,)|(?P<operator>\S)");
    for cap in token_re.captures_iter(preprocessed.as_slice()) {

        let token = if !cap.name("ident").is_empty() {
            match cap.name("ident") {
                "def" => Def,
                "extern" => Extern,
                ident => Ident(ident.to_string())
            }
        } else if !cap.name("number").is_empty() {
            match from_str::<f64>(cap.name("number")) {
                Some(number) => Number(number),
                None => panic!("Lexer failed trying to parse number")
            }
        } else if !cap.name("delimiter").is_empty() {
            Delimiter
        } else if !cap.name("oppar").is_empty() {
            OpeningParenthesis
        } else if !cap.name("clpar").is_empty() {
            ClosingParenthesis
        } else if !cap.name("comma").is_empty() {
            Comma
        } else {
            Operator(cap.name("operator").to_string())
        };

        result.push(token)
    }

    result
}
```

Quite simple function. About regex in Rust you can read here: http://doc.rust-lang.org/regex/

Some comments: we create regex with different groups matching to different types of tokens.
Then we match it on input string and iterate over captures,
looking what token we have matched. Identifiers are matched in the same regex with keywords, as they have the same microsyntax.
They are separated later with the additional match.

### The driver

## Parser and AST implementation

## LLVM IR code generation

## JIT and optimizer support

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
