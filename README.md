# [LLVM tutorial](http://llvm.org/docs/tutorial) in [the Rust language](http://www.rust-lang.org/).

I have just started working on the text, for the full tutorial have a look at the first link.
The code for tutorial is fully implemented (it means, it has all the features described in the original
tutorial and corresponds to the state of code for the [Chapter 7](http://llvm.org/docs/tutorial/LangImpl7.html)).

Code was tested on amd64, on x86 I have a trouble with it: it segfaults somewhere in the middle of ExecutionEngine initialization.

## Table of Contents

* [Introduction](#introduction)
  * [Basic variant of the Kaleidoscope language](#basic-variant-of-the-kaleidoscope-language)
  * [The project structure](#the-project-structure)
  * [The lexer](#the-lexer)
* [Parser and AST implementation](#ast-and-parser-implementation)
  * [The grammar](#the-grammar)
  * [The Abstract Syntax Tree (AST)](#the-abstract-syntax-tree-ast)
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

### The project structure

To create a REPL we will need (corresponding source files are shown in parenthesis):

* the lexer ([lexer.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/lexer.rs))

* the parser ([parser.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/parser.rs))

* the IR builder ([builder.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/builder.rs))

* the JIT compiler ([builder.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/builder.rs))

* the driver ([driver.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/driver.rs))

We'll use [Cargo](http://doc.crates.io/) as a build system for this project. All sources will live in the `src` directory.
Project will have two crates: library and binary. All the real functionality will be implemented in the library, and the binary will just
parse command line arguments and invoke the driver.

[Cargo.toml file](https://github.com/jauhien/iron-kaleidoscope/blob/master/Cargo.toml) is quite straight forward.
The [docopt](https://github.com/docopt/docopt.rs) is added because we'll need to parse simple command libe arguments.

Rust at the moment does not have full LLVM bindings, so we'll need to add some functions. That addition is handled by a combination
of [scripts called during compilation](http://doc.crates.io/build-script.html) and some rust code that can be found in `src/missing_llvm_bindings` directory.

### The lexer

To implement the lexer we'll use a regular expressions. We have next types of tokens (and corresponding regexes given in the notation used by the Rust regex library):

* def and extern keywords (`def|extern`)

* identifier (`\p{Alphabetic}\w*`)

* number literal (`\d+\.?\d*`)

* semicolon (`;`)

* opening and closing parenthesis (`\(|\)`)

* comma (`,`)

* operator (`\S`)

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

To experiment with this lexer you can create a simple main function that just reads lines from the input one by one and shows recognized tokens.

## AST and parser implementation

In this chapter we will build a parser for our Kaleidoscope language.
First we need to define its grammar and how will we represent the parsing results.
Then we can use [Recursive Descent Parsing](http://en.wikipedia.org/wiki/Recursive_descent_parser)
and [Operator-Precedence Parsing](http://en.wikipedia.org/wiki/Operator-precedence_parser)
to produce [the Abstract Syntax Tree](http://en.wikipedia.org/wiki/Abstract_syntax_tree)
from the stream of tokens recognized by the lexer.

### The grammar

This grammar description uses the dialect of Extended Backus-Naur Form (EBNF) described
in the [Rust reference](http://doc.rust-lang.org/reference.html#notation). Identifiers
that start with the lowercase name non-terminals, when identifiers that start
with the uppercase name terminals and correspond to the names in the `Token` enum defined
in the lexer.

```{.ebnf .notation}
program          : [[statement | expression] Delimiter ? ]*;
statement        : [declaration | definition];
declaration      : Extern prototype;
definition       : Def prototype expression;
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
expression       : [primary_expr | primary_expr Op expression];
primary_expr     : [Ident | Literal | call_expr | parenthesis_expr];
call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
```

### The Abstract Syntax Tree (AST)

Now we'll create data types corresponding to every item in the Kaleidoscope grammar.

```{.ebnf .notation}
program          : [[statement | expression] Delimiter ? ]*;
```

Program is a sequence of statements and expressions. To make life easier in the future we will
close every expression in an anonymous function (we'll use this during JIT compilation). So, we have
two types of items in the program after such a closure: declarations and definitions. Declarations are
just function prototypes, when definitions are function prototypes combined with a function body.

The data type corresponding to our programm will be:

```rust
Vec<ASTNode>
```

where `ASTNode` is defined as

```rust
#[deriving(PartialEq, Clone, Show)]
pub enum ASTNode {
    ExternNode(Prototype),
    FunctionNode(Function)
}
```

`ExternNode` corresponds to the	`declaration` item in the grammar and `FunctionNode` corresponds to
the `definition` item.

We define `Prototype` and `Function` now according to the grammar:

```{.ebnf .notation}
definition       : Def prototype expression;
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
```

```rust
#[deriving(PartialEq, Clone, Show)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>
}

#[deriving(PartialEq, Clone, Show)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Expression
}
```

Functions are typed only by the number of arguments, as the only type we have
in the Kaleidoscope language is `f64` number.

The only thing to define now is the data type corresponding to the `expression` item.
This one is the most complicated and difficult to pasre, as it includes binary expressions
with operator precedence.

```{.ebnf .notation}
expression       : [primary_expr | primary_expr Op expression];
primary_expr     : [Ident | Literal | call_expr | parenthesis_expr];
call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
```

`Expression` data type will be an `enum` with entries corresponding to every
possible expression type.

```rust
#[deriving(PartialEq, Clone, Show)]
pub enum Expression {
    LiteralExpr(f64),
    VariableExpr(String),
    BinaryExpr(String, Box<Expression>, Box<Expression>),
    CallExpr(String, Vec<Expression>)
}
```

`LiteralExpr` is a number (`Literal` token). `VariableExpr` is a variable name (`Ident` token).
So far we have only one type of variables: function parameters. `BinaryExpr` has information about
operator name and subexpressions. And `CallExpr` fully corresponds to its definition in the grammar.

We did not need a representation for the `parenthesis_expr` item, as the precedence of
evaluation is encoded in the tree formed by `BinaryExpr` itself, so parenthesis are
used only during parsing.

We can proceed with parsing now, as we know both our input format (the sequence of tokens) and the
AST we want to have as the result of parsing.

## LLVM IR code generation

## JIT and optimizer support

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
