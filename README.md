# Status

Everything works. State corresponds to the Chapter 7 of the original tutorial
(i.e. mutable variables implemented).

[TODO list](https://trello.com/b/9EZR5Hzz/iron-kaleidoscope).

# [LLVM tutorial](http://llvm.org/docs/tutorial) in [the Rust language](http://www.rust-lang.org/).

This tutorial is a work in progress and at the moment I'm working on getting it fully working with
the latest Rust and on improvinvg the way it uses LLVM.

## Table of Contents

* [Chapter 0. Introduction](#chapter-0-introduction)
  * [Basic variant of the Kaleidoscope language](#basic-variant-of-the-kaleidoscope-language)
  * [The project structure](#the-project-structure)
  * [The lexer](#the-lexer)
* [Chapter 1. Parser and AST implementation](#chapter-1-ast-and-parser-implementation)
  * [The grammar](#the-grammar)
  * [The Abstract Syntax Tree (AST)](#the-abstract-syntax-tree-ast)
  * [Parser implementation: introduction](#parser-implementation-introduction)
  * [Top level parse function](#top-level-parse-function)
  * [Helper macros for work with tokens](#helper-macros-for-work-with-tokens)
  * [Parsing of statements and top level expressions](#parsing-of-statements-and-top-level-expressions)
  * [Parsing of primary expressions](#parsing-of-primary-expressions)
  * [Parsing of binary expressions](#parsing-of-binary-expressions)
  * [The driver](#the-driver)
* [Chapter 2. LLVM IR code generation](#chapter-2-llvm-ir-code-generation)
  * [Code generation setup](#code-generation-setup)
  * [Top level code generation](#top-level-code-generation)
  * [Expression code generation](#expression-code-generation)
* [Chapter 3. Optimizer and JIT support](#chapter-3-optimizer-and-jit-support)
  * [LLVM Optimization passes](#llvm-optimization-passes)
  * [MCJIT based JIT-compiler](#mcjit-based-jit-compiler)
  * [Changes in the driver and 'built-in' functions](#changes-in-the-driver-and-built-in-functions)
* [Extending Kaleidoscope: control flow](#extending-kaleidoscope-control-flow)
* [Extending Kaleidoscope: user-defined operators](#extending-kaleidoscope-user-defined-operators)
* [Extending Kaleidoscope: mutable variables](#extending-kaleidoscope-mutable-variables)


## Chapter 0. Introduction

This tutorial shows how to implement a simple programming language using LLVM and Rust.
Its first goal is to show how to use LLVM to create a simple REPL, so some knowledge of Rust is assumed.
To be honest, author himself is a very beginner both in Rust and LLVM, so any feedback is highly
appreciated.

The code in the repository corresponds to the state of
your program at the end of the last chapter and serves as a starting point for further experiments.

If you want to look at code that corresponds to a given chapter, see `chapters` directory. Link to relevant code
is attached to every chapter (work in progress).

To experiment with the code in this repo you need:

* the latest Rust compiler

* Cargo Rust package manager

* LLVM (I have used ver. 3.6)

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

* the JIT compiler ([jitter.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/jitter.rs))

* the driver ([driver.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/driver.rs))

We'll use [Cargo](http://doc.crates.io/) as a build system for this project. All sources will live in the `src` directory.
Project will have two crates: library and binary. All real functionality will be implemented in the library, and the binary will just
parse command line arguments and invoke the driver.

[Cargo.toml file](https://github.com/jauhien/iron-kaleidoscope/blob/master/Cargo.toml) is quite straightforward.

### The lexer

To implement the lexer we'll use regular expressions. We have the next types of tokens (and corresponding regexes given in the notation used by the Rust regex library):

* def and extern keywords (`def|extern`)

* identifier (`\p{Alphabetic}\w*`)

* number literal (`\d+\.?\d*`)

* semicolon (`;`)

* opening and closing parenthesis (`\(|\)`)

* comma (`,`)

* operator (`\S`)

The corresponding enumeration looks like this:

```rust
#[derive(PartialEq, Clone, Debug)]
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

Note, that to use enumeration members without scopes as we later do, you need to
add some uses at the beginning of your module (it is needed since changing
enums to be scoped in Rust):

```rust
pub use self::Token::{
    Def,
    Extern,
    Delimiter,
    OpeningParenthesis,
    ClosingParenthesis,
    Comma,
    Ident,
    Number,
    Operator
};
```

We do not mention those uses explicitly in the following.

Our parser function will accept a string with input characters and produce a vector of tokens. It will look like this:

```rust
pub fn tokenize(input: &str) -> Vec<Token> {
    // regex for commentaries (start with #, end with the line end)
    let comment_re = regex!(r"(?m)#.*\n");
    // remove commentaries from the input stream
    let preprocessed = comment_re.replace_all(input, "\n");

    let mut result = Vec::new();

    // regex for token, just union of straightforward regexes for different token types
    // operators are parsed the same way as identifier and separated later
    let token_re = regex!(concat!(
        r"(?P<ident>\p{Alphabetic}\w*)|",
        r"(?P<number>\d+\.?\d*)|",
        r"(?P<delimiter>;)|",
        r"(?P<oppar>\()|",
        r"(?P<clpar>\))|",
        r"(?P<comma>,)|",
        r"(?P<operator>\S)"));

    for cap in token_re.captures_iter(preprocessed.as_str()) {
        let token = if cap.name("ident").is_some() {
            match cap.name("ident").unwrap() {
                "def" => Def,
                "extern" => Extern,
                ident => Ident(ident.to_string())
            }
        } else if cap.name("number").is_some() {
            match cap.name("number").unwrap().parse() {
                Ok(number) => Number(number),
                Err(_) => panic!("Lexer failed trying to parse number")
            }
        } else if cap.name("delimiter").is_some() {
            Delimiter
        } else if cap.name("oppar").is_some() {
            OpeningParenthesis
        } else if cap.name("clpar").is_some() {
            ClosingParenthesis
        } else if cap.name("comma").is_some() {
            Comma
        } else {
            Operator(cap.name("operator").unwrap().to_string())
        };

        result.push(token)
    }

    result
}
```

Quite simple function. About regex in Rust you can read [here](http://doc.rust-lang.org/regex/).

Some comments: we create regex with different groups matching to different types of tokens.
Then we match it on the input string and iterate over captures,
looking what token we have matched. Identifiers are matched in the same regex with keywords, as they have the same microsyntax.
They are separated later with the additional match.

To experiment with this lexer you can create a simple main function that reads lines from the input one by one and shows the recognized tokens.
[Full code for this chapter](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/0) is available, but is
a little bit more complex then needed as it is autogenerated from the full code.

## Chapter 1. AST and parser implementation

In this chapter we will build a parser for the Kaleidoscope language.
First we need to define its grammar and how to represent the parsing results.
Then we can use [Recursive Descent Parsing](http://en.wikipedia.org/wiki/Recursive_descent_parser)
and [Operator-Precedence Parsing](http://en.wikipedia.org/wiki/Operator-precedence_parser)
to produce [the Abstract Syntax Tree](http://en.wikipedia.org/wiki/Abstract_syntax_tree)
from the stream of tokens recognized by the lexer.

### The grammar

This grammar description uses the dialect of Extended Backus-Naur Form (EBNF) described
in the [Rust reference](http://doc.rust-lang.org/reference.html#notation). Identifiers
that start with the lowercase name non-terminals. Identifiers that start
with the uppercase name terminals and correspond to the names in the `Token` enum defined
in the lexer.

```{.ebnf .notation}
program          : [[statement | expression] Delimiter ? ]*;
statement        : [declaration | definition];
declaration      : Extern prototype;
definition       : Def prototype expression;
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
expression       : [primary_expr (Op primary_expr)*];
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
close every expression in an anonymous function (we'll use this during JIT compilation). So, there are
two types of items in the program after such a closure: declarations and definitions. Declarations are
just function prototypes, when definitions are function prototypes combined with a function body.

The data type corresponding to the programm will be:

```rust
Vec<ASTNode>
```

where `ASTNode` is defined as

```rust
#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode {
    ExternNode(Prototype),
    FunctionNode(Function)
}
```

`ExternNode` corresponds to the	`declaration` item in the grammar and `FunctionNode` corresponds to
the `definition` item.

We define `Prototype` and `Function` according to the grammar:

```{.ebnf .notation}
definition       : Def prototype expression;
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
```

```rust
#[derive(PartialEq, Clone, Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Expression
}

#[derive(PartialEq, Clone, Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>
}
```

Functions are typed only by the number of arguments, as the onliest type
in the Kaleidoscope language is an `f64` number.

The only thing left to define is the data type that corresponds to the `expression` item.
This one is the most complicated and difficult to parse, as it includes binary expressions
with operator precedence.

```{.ebnf .notation}
expression       : [primary_expr (Op primary_expr)*];
primary_expr     : [Ident | Literal | call_expr | parenthesis_expr];
call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
```

`Expression` data type will be an `enum` with entries corresponding to every
possible expression type:

```rust
#[derive(PartialEq, Clone, Debug)]
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
evaluation is encoded in the tree formed by `BinaryExpr`, so parenthesis are
used only during parsing. Also, note that `Expression` definition not fully corresponds to the grammar
(grammar has a sequence of primary expressions devided by operators, when here we have a binary tree
of binary expressions), we will speak about it later in the section about binary expressions parsing.

Now we can proceed with parsing, as both our input format (the sequence of tokens) and the
AST we want to have as the result of parsing are known.

### Parser implementation: introduction

Before starting parser implementation we should think about one general question: how will REPL receive the input and
how should it work with it. Basically, REPL should allow user to type statements line by line, parsing
every line as it is entered. If the line contains not finished statement, REPL should consume line by line until
it has something finished that can be interpreted (either declaration/definition or free expression).

As an input we can accept two variables: already parsed AST and tokens that we still need to parse:

```rust
tokens : &[Token], parsed_tree : &[ASTNode]
```

As a result we will have again pair of a parsed AST and tokens that were not parsed because they form nothing finished.
Also we need some kind of error handling. It will be achieved by the usage of `Result` with an error message:

```rust
pub type ParsingResult = Result<(Vec<ASTNode>, Vec<Token>), String>;
```

The function prototype for the parsing function looks like this:

```rust
pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &mut ParserSettings) -> ParsingResult
pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &mut ParserSettings) -> ParsingResult;
```

At the moment `ParserSettings` can be just an empty `enum`, in the nearest future we will use them for handling
binary expressions (they will contain information about operator precedence). They are mutable because later on
we may want to add some dynamically defined constructions to the language that will need additional information
to be stored in the parser settings.

### Top level parse function

The majority of the parsing will be done by the recursive descent parser. This kind of parsers is easy for understanding
and implementation. Every production rule in the grammar has a corresponding function, these functions call
each other according to the production rules.

We will need to handle input tokens efficiently, being able to pick them one by one, or return back to the input vector,
this can be easily achieved if we reverse this vector. Adding and removing elements at the end of a vector is quite
efficient.

Helper parsing functions will accept unparsed tokens as their input.
They will have three possible results:

* AST node was parsed, pair of parsed piece of AST and consumed tokens that correspond to it should be returned
* input token sequence in not complete, no tokens from the input should be consumed
* an error happend, error message should be returned

Corresponding result data type looks like this:

```rust
enum PartParsingResult<T> {
    Good(T, Vec<Token>),
    NotComplete,
    Bad(String)
}
```

We will need a helper function for error generation:

```rust
fn error<T>(message : &str) -> PartParsingResult<T> {
    Bad(message.to_string())
}
```

This function and data type are generic as we will need to return objects of different types depending on what we are parsing (prototype, expression, etc.)
Top level parsing functions will return `ASTNode` which can be directly inserted into `Vec<ASTNode>` that represents the AST.

We can implement first production rules in the topmost parsing function now:

```{.ebnf .notation}
program          : [[statement | expression] Delimiter ? ]*;
statement        : [declaration | definition];
declaration      : Extern prototype;
definition       : Def prototype expression;
```

As one can see from this piece of grammar, we have 3 top level items.
Parser will decide which one it handles at the moment based on the
token it sees:

* `Def` token means we have definition
* `Extern` token means declaration
* `Delimiter` token can be just ignored
* any other token can be interpreted as the beginning of an expression (as there is no other possibility)

With these points in mind we can implement the `parse` function this way:

```rust
pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &mut ParserSettings) -> ParsingResult
{
    let mut rest = tokens.to_vec();
    // we read tokens from the end of the vector
    // using it as a stack
    rest.reverse();

    // we will add new AST nodes to already parsed ones
    let mut ast = parsed_tree.to_vec();

    loop {
        // look at the current token and determine what to parse
        // based on its value
        let cur_token =
            match rest.last() {
                Some(token) => token.clone(),
                None => break
            };
        let result = match cur_token {
            Def => parse_function(&mut rest, settings),
            Extern => parse_extern(&mut rest, settings),
            Delimiter => {rest.pop(); continue},
            _ => parse_expression(&mut rest, settings)
        };
        match result {
            Good(ast_node, _) => ast.push(ast_node),
            NotComplete => break,
            Bad(message) => return Err(message)
        }
    }

    // unparsed tokens
    rest.reverse();
    Ok((ast, rest))
}
```

### Helper macros for work with tokens

As was mentioned before we can have as input both complete and non-complete language sentences.
We need to consume all the tokens that correspond to the complete sentences and leave the
rest untouched. To make life easier we will use macros to work with tokens and parsing results.

We will maintain a list of tokens that correspond to the sentence being parsed now in every
parsing functions. If input tokens are exhausted before we have parsed the whole item, we will insert them
back. If parsing is successful, we will return both parsed item and tokens that correspond to it.
If parsing has failed, that we will inform caller about this failure.

First macros we need will handle calling of other parsing functions. The parsing function signature looks like this:

```rust
fn parse_binary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings, ...) -> PartParsingResult<T>;
```

where `...` means additional parameters.

The calling macro looks like this:

```rust
macro_rules! parse_try(
    ($function:ident, $tokens:ident, $settings:ident, $parsed_tokens:ident) => (
        parse_try!($function, $tokens, $settings, $parsed_tokens,)
    );

    ($function:ident, $tokens:ident, $settings:ident, $parsed_tokens:ident, $($arg:expr),*) => (
        match $function($tokens, $settings, $($arg),*) {
            Good(ast, toks) => {
                $parsed_tokens.extend(toks.into_iter());
                ast
            },
            NotComplete => {
                $parsed_tokens.reverse();
                $tokens.extend($parsed_tokens.into_iter());
                return NotComplete;
            },
            Bad(message) => return Bad(message)
        }
    )
);
```

It declares two variants: with and without additional parameters. The first one calls the second one with zero additional
parameters.

The macro calls the parsing function and looks at the results. If results are good, we extend already parsed tokens with
those returned from the parsing function and have the parsed AST as value of the macro. If called function
returned `NotComplete`, we insert parsed tokens back into the input and also return `NotComplete`. Note, that our temporary
parsed_tokens are stored in the direct way, when the input was stored in the reversed way, so we need to reverse it
before inserting back. If called function failed, we also return a failure.

Next macro we need is a macro that works directly with input tokens. It will look at the current token and try to match it.
We need two variants. One tries to match with different provided alternatives, if no one matches, it failes with error. Other
also tries to match with different alternatives, but if no one is matched, it just executes the action given as a parameter. In the
last case no tokens from the input should be consumed by the macro itself.

```rust
macro_rules! expect_token (
    ([ $($token:pat, $value:expr, $result:stmt);+ ] <= $tokens:ident, $parsed_tokens:ident, $error:expr) => (
        match $tokens.pop() {
            $(
                Some($token) => {
                    $parsed_tokens.push($value);
                    $result
                },
             )+
             None => {
                 $parsed_tokens.reverse();
                 $tokens.extend($parsed_tokens.into_iter());
                 return NotComplete;
             },
            _ => return error($error)
        }
    );

    ([ $($token:pat, $value:expr, $result:stmt);+ ] else $not_matched:block <= $tokens:ident, $parsed_tokens:ident) => (
        match $tokens.last().map(|i| {i.clone()}) {
            $(
                Some($token) => {
                    $tokens.pop();
                    $parsed_tokens.push($value);
                    $result
                },
             )+
            _ => {$not_matched}
        }
    )
);
```

This macro automatically handles inserting tokens into the parsed tokens vector and returning of `NotComplete` (together with
inserting of tokens back into the input vector) or error in the appropriate cases.

### Parsing of statements and top level expressions

We have two kinds of statements in the Kaleidoscope language: function
declarations and function definitions:

```{.ebnf .notation}
statement        : [declaration | definition];
declaration      : Extern prototype;
definition       : Def prototype expression;
```

Let's start from the easier one: function declarations. The function is really straightforward:

```rust
fn parse_extern(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    // eat Extern token
    tokens.pop();
    let mut parsed_tokens = vec![Extern];
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
    Good(ExternNode(prototype), parsed_tokens)
}
```

We eat `Extern` token and parse the function prototype. That's all.

Function definition is not very complicated also:

```rust
fn parse_function(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    // eat Def token
    tokens.pop();
    let mut parsed_tokens = vec!(Def);
    let prototype = parse_try!(parse_prototype, tokens, settings, parsed_tokens);
    let body = parse_try!(parse_expr, tokens, settings, parsed_tokens);
    Good(FunctionNode(Function{prototype: prototype, body: body}), parsed_tokens)
}
```

Again, we eat `Def` token, we parse prototype and function body. That's all.

So far we just called another parsing functions and matched some tokens (like `Def` or `Extern`).
That's time for some real parsing now.

```{.ebnf .notation}
prototype        : Ident OpeningParenthesis [Ident Comma ?]* ClosingParenthesis;
```

```rust
fn parse_prototype(tokens : &mut Vec<Token>, _settings : &mut ParserSettings) -> PartParsingResult<Prototype> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!([
            Ident(name), Ident(name.clone()), name
        ] <= tokens, parsed_tokens, "expected function name in prototype");

    expect_token!(
        [OpeningParenthesis, OpeningParenthesis, ()] <= tokens,
        parsed_tokens, "expected '(' in prototype");

    let mut args = Vec::new();
    loop {
        expect_token!([
            Ident(arg), Ident(arg.clone()), args.push(arg.clone());
            Comma, Comma, continue;
            ClosingParenthesis, ClosingParenthesis, break
        ] <= tokens, parsed_tokens, "expected ')' in prototype");
    }

    Good(Prototype{name: name, args: args}, parsed_tokens)
}
```

Function prototype starts with the function name. Then opening parenthesis goes.
After opening parenthesis we have a list of function parameters (`Ident` tokens).
Note, that we ignore `Comma` tokens, so they are equivalent to whitspaces.
If we find a closing parenthesis, prototype
parsing is done. If we find any other token we emit an error.

The only top level item still left are top level expressions. To make
further work with them easier, we close them in an anonymous function.

```rust
fn parse_expression(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<ASTNode> {
    let mut parsed_tokens = Vec::new();
    let expression = parse_try!(parse_expr, tokens, settings, parsed_tokens);
    let prototype = Prototype{name: "".to_string(), args: vec![]};
    let lambda = Function{prototype: prototype, body: expression};
    Good(FunctionNode(lambda), parsed_tokens)
}
```

Now, when we have code that handles parsing of all top level items, we can proceed
with parsing of the main part of Kaleidoscope language -- expressions.

### Parsing of primary expressions

We will start from easier topic: parsing of primary expressions. Then we will use primary
expressions parsing functions to parse operands of binary operators.

```{.ebnf .notation}
primary_expr     : [Ident | Literal | call_expr | parenthesis_expr];
call_expr        : Ident OpeningParenthesis [expression Comma ?]* ClosingParenthesis;
parenthesis_expr : OpeningParenthesis expression ClosingParenthesis;
```

```rust
fn parse_primary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    match tokens.last() {
        Some(&Ident(_)) => parse_ident_expr(tokens, settings),
        Some(&Number(_)) => parse_literal_expr(tokens, settings),
        Some(&OpeningParenthesis) => parse_parenthesis_expr(tokens, settings),
        None => return NotComplete,
        _ => error("unknow token when expecting an expression")
    }
}
```

To start parsing of a primary expression we just look at the next token and
decide which kind of expression we are working with. Such a look ahead one or more tokens
is a common idea in parsers, we already have seen it before in the top level parse function.

We start with parsing of identifier and call expressions. We use the same parsing function
for them, as they both start from the `Ident` token.

```rust
fn parse_ident_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let name = expect_token!(
        [Ident(name), Ident(name.clone()), name] <= tokens,
        parsed_tokens, "identificator expected");

    expect_token!(
        [OpeningParenthesis, OpeningParenthesis, ()]
        else {return Good(VariableExpr(name), parsed_tokens)}
        <= tokens, parsed_tokens);

    let mut args = Vec::new();
    loop {
        expect_token!(
            [ClosingParenthesis, ClosingParenthesis, break;
             Comma, Comma, continue]
            else {
                args.push(parse_try!(parse_expr, tokens, settings, parsed_tokens));
            }
            <= tokens, parsed_tokens);
    }

    Good(CallExpr(name, args), parsed_tokens)
}
```

First, we parse the name (it will be the name of a variable or function to call).
Then we look at the next token. If it is an opening parenthesis, then we have a function call.
If it is any other token, then we have a variable referenced and we can return
a `VariableExpr` already.

Next (as we have a call expression) we parse a list of arguments to the function. It is
done the same way as in prototype. The only difference is that arguments are not identifiers,
but expressions now.

Parsing of literal expressions is very straightforward:

```rust
fn parse_literal_expr(tokens : &mut Vec<Token>, _settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();

    let value = expect_token!(
        [Number(val), Number(val), val] <= tokens,
        parsed_tokens, "literal expected");

    Good(LiteralExpr(value), parsed_tokens)
}
```

Parenthesis expressions are also easy to parse:

```rust
fn parse_parenthesis_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    // eat the opening parenthesis
    tokens.pop();
    let mut parsed_tokens = vec![OpeningParenthesis];

    let expr = parse_try!(parse_expr, tokens, settings, parsed_tokens);

    expect_token!(
        [ClosingParenthesis, ClosingParenthesis, ()] <= tokens,
        parsed_tokens, "')' expected");

    Good(expr, parsed_tokens)
}
```

Now, when we can parse primary expressions, it is the time for more complicated ones.

### Parsing of binary expressions

Our grammar for expressions looked like this:

```{.ebnf .notation}
expression       : [primary_expr (Op primary_expr)*];
```

The problem with this grammar is that it really does not reveal the semantics of binary expressions.
It is the correct generative grammar. Note however, that we want not just check expressions syntactic correctness,
but have some structure that can be used for code generation (binary tree in this case) that
includes information about operators precedence, so we can not use this grammar for parsing.

Basically speaking, we could take an information about operator precedence and reformulate our grammar,
so it reveals semantic correctly and try to use it to create a parser similar to that we already
have for other types of items. But we will use an [operator precedence parser](http://en.wikipedia.org/wiki/Operator-precedence_parser)
instead as it is much faster for such kind of things. Real compilers also frequently use it for parsing of
expressions. If they are not Lisp language compilers, of course.

We will keep information about operator precedence in a map. That's where our `settings` structure is needed:

```rust
pub struct ParserSettings {
    operator_precedence: HashMap<String, i32>
}
```

Let's create a function that fills this map with some operators:

```rust
pub fn default_parser_settings() -> ParserSettings {
    let mut operator_precedence = HashMap::new();
    operator_precedence.insert("=".to_string(), 2);
    operator_precedence.insert("<".to_string(), 10);
    operator_precedence.insert("+".to_string(), 20);
    operator_precedence.insert("-".to_string(), 20);
    operator_precedence.insert("*".to_string(), 40);

    ParserSettings{operator_precedence: operator_precedence}
}
```

A binary expression is a primary expression followed by zero or more `(operator, primary expression)` pairs.
The expression parsing function looks like this:

```rust
fn parse_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();
    let lhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);
    let expr = parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, 0, &lhs);
    Good(expr, parsed_tokens)
}
```

`parse_binary_expr` will return LHS if there are no `(operator, primary expression)` pairs or parse the whole expression.

To parse a binary expression we will use the following algorithm. Its input is:

* LHS of an expression

* input tokens

* the minimal allowed operator precedence

We will build the resulting value starting from LHS. We will eat
operators with the precedence bigger than the minimal allowed
precedence one by one, constructing the RHS value. RHS construction we
will start from the primary expression. To group operators
correctly, we will call `parse_binary_expr` on the
operators with the precedence bigger than the precedence of the
current operator to make it parse the whole RHS. Then we will
construct the resulting value and use it as the new LHS. We will
continue until the current token is not an operator, or it is an
operator with the precedence lesser than the minimal allowed one.

```rust
fn parse_binary_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings, expr_precedence : i32, lhs : &Expression) -> PartParsingResult<Expression> {
    // start with LHS value
    let mut result = lhs.clone();
    let mut parsed_tokens = Vec::new();

    loop {
        // continue until the current token is not an operator
        // or it is an operator with precedence lesser than expr_precedence
        let (operator, precedence) = match tokens.last() {
            Some(&Operator(ref op)) => match settings.operator_precedence.get(op) {
                Some(pr) if *pr >= expr_precedence => (op.clone(), *pr),
                None => return error("unknown operator found"),
                _ => break
            },
            _ => break
        };
        tokens.pop();
        parsed_tokens.push(Operator(operator.clone()));

        // parse primary RHS expression
        let mut rhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);

        // parse all the RHS operators until their precedence is
        // bigger than the current one
        loop {
            let binary_rhs = match tokens.last().map(|i| {i.clone()}) {
                Some(Operator(ref op)) => match settings.operator_precedence.get(op).map(|i| {*i}) {
                    Some(pr) if pr > precedence => {
                        parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, pr, &rhs)
                    },
                    None => return error("unknown operator found"),
                    _ => break
                },
                _ => break
            };

            rhs = binary_rhs;
        }

        // merge LHS and RHS
        result = BinaryExpr(operator, box result, box rhs);
    }

    Good(result, parsed_tokens)
}
```

That's how parsing of binary expressions looks like.

*TODO:* add an example.

### The driver

It is time to construct a working program from already defined
functions. Our simple REPL will read input line by line. It will write
the parsed AST back to the user when it has some finished
expression(s). Until this it will ask user to write additional lines
to the current expression. On error it will display an error message.

Additionally we will add possibility to call only the lexer. Let's
start from command line options parsing. We will use
[docopt](https://github.com/docopt/docopt.rs) library for this:

```rust
docopt!(Args, "
Usage: iron_kaleidoscope [(-l | -p | -i)]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -i  Run only IR builder and show its output.
");

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Tokens
    } else {
        AST
    };

    main_loop(stage);
}
```

The `Stage` enum is defined in the driver:

```rust
#[derive(PartialEq, Clone, Debug)]
pub enum Stage {
    AST,
    Tokens
}
```

Driver itself looks like this:

```rust
pub fn main_loop(stage: Stage) {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();
    let mut parser_settings = default_parser_settings();

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
    }
}
```

[Full code for this chapter](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/1).

## Chapter 2. LLVM IR code generation

Before it we have used nothing from LLVM libraries. The described
lexer and parser have nothing LLVM specific. It is a time to start a
real LLVM tutorial now.

To represent intermediate results of code generation and to make it
possible to easily do different transformations on the generated code,
LLVM uses a special representation: [LLVM intermediate
representation](http://llvm.org/docs/LangRef.html).

IR is a universal representation used in every component of LLVM. It
has three different forms: an in-memory compiler IR, an on-disk
bitcode representation and a human-readable assembly language
representation.

We want to generate IR from the AST that we have built in the previous
chapter. It is surprisingly simple, as we will see in a moment.

### Code generation setup

LLVM has two interfaces: C++ interface and a stable C interface. Apart
from it LLVM has a number of bindings, usually based on the C interface.

Before or during reading this chapter of tutorial you can read an
[appropriate section in the LLVM Programmer's
Manual](http://llvm.org/docs/ProgrammersManual.html#the-core-llvm-class-hierarchy-reference).

We'll use [my Rust LLVM bindings](https://github.com/jauhien/iron-llvm) in this
tutorial (which serves as first documentation for them). These bindings
are work in progress, but I have plans to fully cover LLVM C API and as much as possible
of usefull features available only in C++ API. `iron-llvm` aims to be safe and rust idiomatic,
it is built on top of [existing full unsafe bindings](https://bitbucket.org/tari/llvm-sys.rs/).

Using it is as simple as adding

```
[dependencies]
llvm-sys = "*"

[dependencies.iron_llvm]
git = "https://github.com/jauhien/iron-llvm.git"
```

to [Cargo.toml file](https://github.com/jauhien/iron-kaleidoscope/blob/master/Cargo.toml) and
importing necessary crates

```rust
extern crate iron_llvm;
extern crate llvm_sys;
```

`iron-llvm` is still not published on [crates.io](https://crates.io/), this is why we use `github`
dependency. Also after I rework what is exports (mainly basic LLVM types), explicit use of
`llvm-sys` will be not necessary.

To generate IR we'll need these objects:

* [Context](http://llvm.org/docs/doxygen/html/classllvm_1_1LLVMContext.html)
* [Module](http://llvm.org/docs/doxygen/html/classllvm_1_1Module.html) where generated IR will be stored
* [IR builder](http://llvm.org/docs/doxygen/html/classllvm_1_1IRBuilder.html) that will do real generation work

Module is a compilation unit of LLVM IR. It contains
[functions](http://llvm.org/docs/doxygen/html/classllvm_1_1Function.html) and other high-level items.
Functions are usual functions well-know from every programming language. They have some type,
defined by parameters and return value.
Functions are built up from [basic blocks](http://llvm.org/docs/doxygen/html/classllvm_1_1BasicBlock.html).
Basic block is an instruction sequence that has no control flow instructions inside. It ends with
[terminator instruction](http://llvm.org/docs/doxygen/html/classllvm_1_1TerminatorInst.html) that
passes control to other basic blocks.

Also we'll need a map of named values (function parameters in our first version) and a reference to
double type.

We'll pack context, IR builder, named values map and double type reference in a simple struct:

```rust
use std::collections::HashMap;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

pub struct Context {
    context: core::Context,
    builder: core::Builder,
    named_values: HashMap<String, LLVMValueRef>,
    ty: RealTypeRef
}

impl Context {
    pub fn new() -> Context {

        let context = core::Context::get_global();
        let builder = core::Builder::new();
        let named_values = HashMap::new();
        let ty = RealTypeRef::get_double();

        Context { context: context,
                  builder: builder,
                  named_values: named_values,
                  ty: ty
        }
    }
}
```

For code genertion we will use one module. But we'll create for it
a trait `ModuleProvider` as it will simplify adding a JIT-compiler later (not showing uses this time):

```rust
pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
}

pub struct SimpleModuleProvider {
    module: core::Module
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        let module = core::Module::new(name);

        SimpleModuleProvider {
            module: module
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }
}
```

Functions defined in this trait do the following:

* `dump` dumps all the module content to the standard output
* `get_module` gives reference to the module
* `get_function` gives generated function by name (if there is any),
    second element of pair tells whether this is prototype (function without body, containing no basic blocks) or definition.

Now when we have everything prepared, there is a time to add code generation functions to every AST element. The
result of these functions will be [`LLVMValueRef`](http://llvm.org/docs/doxygen/html/classllvm_1_1Value.html). This type represents a
[Static Single Assignment value](http://en.wikipedia.org/wiki/Static_single_assignment_form). Near everything you have in IR is
an SSA value. Every such value has its [type](http://llvm.org/docs/doxygen/html/classllvm_1_1Type.html). In our simple language
there will be only double typed values and functions.

AST elements will implement a trait for code generation:

```rust
pub type IRBuildingResult = Result<(LLVMValueRef, bool), String>;

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult;
}
```

We return a pair here with second element showing if the value we generated was an anonymous function.
This will be useful later when we'll want to execute such functions (as you probably remember
we've packed top-level expressions into anonymous functions).

### Top level code generation

Let's implement `IRBuilder` trait for top level data structures. For
`ParsingResults`, AST tree and `ASTNode` it is really trivial:

```rust
impl IRBuilder for parser::ParsingResult {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context, module_provider),
            &Err(ref message) => Err(message.clone())
        }
    }
}

impl IRBuilder for Vec<parser::ASTNode> {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context, module_provider)));
        }

        result
    }
}

impl IRBuilder for parser::ASTNode {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {
            &parser::ExternNode(ref prototype) => prototype.codegen(context, module_provider),
            &parser::FunctionNode(ref function) => function.codegen(context, module_provider)
        }
    }
}
```

We just call `codegen` function for underlying elements and return its results.
For AST tree (in our case, rather AST list) we return value of the
last AST node.

Prototypes and functions are more complicated, as we need to do some
real work there including handling of named function parameters.

Code generation for prototypes looks like this:

```rust
impl IRBuilder for parser::Prototype {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        // check if declaration with this name was already done
        let function = match module_provider.get_function(&self.name) {
            Some((prev_definition, redefinition)) => {
                // we do not allow to redeclare functions with
                // other signatures
                if prev_definition.count_params() as usize != self.args.len() {
                    return error("redefinition of function with different number of args")
                }

                // we do not allow to redefine/redeclare already
                // defined functions (those that have the body)
                if redefinition {
                    return error("redefinition of function");
                }

                prev_definition
            },
            None => {
                // function type is defined by number and types of
                // the arguments
                let mut param_types = iter::repeat(context.ty.to_ref()).take(self.args.len()).collect::<Vec<_>>();
                let fty = FunctionTypeRef::get(&context.ty, param_types.as_mut_slice(), false);
                FunctionRef::new(&mut module_provider.get_module(), &self.name, &fty)
            }
        };

        // set correct parameters names
        for (param, arg) in function.params_iter().zip(&self.args) {
            param.set_name(arg);
        }

        Ok((function.to_ref(), false))
    }
}
```

First we look if a function was already declared. If it was declared
with the same signature but was not defined, we allow redeclaration
(it is useful for e.g. forward function declarations). If the function
was not declared previously, we create a new function with the type we
need. Function type is determined by types and number of arguments. In
our case function type effectively is defined only by number of
arguments, as all of them have the same `f64` type. At the end we
iterate through the parameters setting correct names for them.

Function code generation looks like this:

```rust
impl IRBuilder for parser::Function {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        // we have no global variables, so we can clear all the
        // previously defined named values as they come from other functions
        context.named_values.clear();

        let (function, _) = try!(self.prototype.codegen(context, module_provider));
        let mut function = unsafe {FunctionRef::from_ref(function)};

        // basic block that will contain generated instructions
        let mut bb = function.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);

        // set function parameters
        for (param, arg) in function.params_iter().zip(&self.prototype.args) {
            context.named_values.insert(arg.clone(), param.to_ref());
        }

        // emit function body
        // if error occured, remove the function, so user can
        // redefine it
        let body = match self.body.codegen(context, module_provider) {
            Ok((value, _)) => value,
            Err(message) => {
                unsafe {LLVMDeleteFunction(function.to_ref())};
                return Err(message);
            }
        };

        // the last instruction should be return
        context.builder.build_ret(&body);

        function.verify(LLVMAbortProcessAction);

        // clear local variables
        context.named_values.clear();
        Ok((function.to_ref(), self.prototype.name.as_str() == ""))
    }
}
```

First we call `codegen` for prototype that returns [function
reference](http://llvm.org/docs/ProgrammersManual.html#c-function).
After it we insert a [basic
block](http://llvm.org/docs/ProgrammersManual.html#the-basicblock-class)
into our function. A basic block is a container for a sequence of
instructions. It contains a linear instructions sequence without branching
and should end with a [terminator
instruction](http://llvm.org/docs/ProgrammersManual.html#terminatorinst).
When the entry basic block is appended we position the builder on it,
so all the following instructions will be generated into this basic
block. Then we add function parameters to the `named_values` map.
After it we call `codegen` function for a body. If it fails, we
completely remove the function that we are working with, so user can
try to redefine it. Then we add a return instruction at the end of the
function, clear local variables and return the generated value.

That's all with code generation for functions. We can proceed with
expressions code generation now, as builder is setted up and has a place
where it can emit instructions. Also we have local variables added to
the `named_values` map, so they can be used from inside function
bodies. Also, remember, that we have closed top level expressions into
anonymous functions, we detect this by checking prototype name.

### Expression code generation

As everything apart from function declarations/definitions is
expression in kaleidoscope, we will have a big match on an
`Expression` AST node in branches of which we will generate IR for
appropriate language constructions. Here is the full function for reference,
comments will follow:

```rust
impl IRBuilder for parser::Expression {
    fn codegen(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRBuildingResult {
        match self {


            &parser::LiteralExpr(ref value) => {
                Ok((RealConstRef::get(&context.ty, *value).to_ref(), false))
            },


            &parser::VariableExpr(ref name) => {
                match context.named_values.get(name) {
                    Some(value) => {
                        Ok((*value, false))
                    },
                    None => error("unknown variable name")
                }
            },


            &parser::BinaryExpr(ref name, ref lhs, ref rhs) => {
                let (lhs_value, _) = try!(lhs.codegen(context, module_provider));
                let (rhs_value, _) = try!(rhs.codegen(context, module_provider));

                match name.as_str() {
                    "+" => Ok((context.builder.build_fadd(lhs_value,
                                                          rhs_value,
                                                          "addtmp"),
                               false)),
                    "-" => Ok((context.builder.build_fsub(lhs_value,
                                                          rhs_value,
                                                          "subtmp"),
                               false)),
                    "*" => Ok((context.builder.build_fmul(lhs_value,
                                                          rhs_value,
                                                          "multmp"),
                               false)),
                    "<" => {
                        let cmp = context.builder.build_fcmp(LLVMRealOLT,
                                                             lhs_value,
                                                             rhs_value,
                                                             "cmptmp");

                        // convert boolean to double 0.0 or 1.0
                        Ok((context.builder.build_ui_to_fp(cmp,
                                                           context.ty.to_ref(),
                                                           "booltmp"),
                            false))
                    },
                    _ => error("invalid binary operator")
                }
            },


            &parser::CallExpr(ref name, ref args) => {
                let (function, _) = match module_provider.get_function(name) {
                    Some(function) => function,
                    None => return error("unknown function referenced")
                };

                if function.count_params() as usize != args.len() {
                    return error("incorrect number of arguments passed")
                }

                let mut args_value = Vec::new();
                for arg in args.iter() {
                    let (arg_value, _) = try!(arg.codegen(context, module_provider));
                    args_value.push(arg_value);
                }

                Ok((context.builder.build_call(function.to_ref(),
                                               args_value.as_mut_slice(),
                                               "calltmp"),
                    false))
            }
        }
    }
}
```

Let's have a look at match branches one by one.

For `Literal` expression we just return a real constant with the
appropriate value:

```rust
            &parser::LiteralExpr(ref value) => {
                Ok((RealConstRef::get(&context.ty, *value).to_ref(), false))
            },
```

For variables we look in the `named_values` map and if there is such a
variable there (a value that corresponds to the function argument),
we return the value or emit an error otherwise:

```rust
            &parser::VariableExpr(ref name) => {
                match context.named_values.get(name) {
                    Some(value) => {
                        Ok((*value, false))
                    },
                    None => error("unknown variable name")
                }
            },
```

For binary expressions we do some real instructions generation. Remember,
that LLVM uses
[SSA](http://en.wikipedia.org/wiki/Static_single_assignment_form), so
value, instruction and variable are the same. They are identified by a
value reference that code generation functions return. Names that we
give to those functions are just hints to LLVM and will be changed to
be different for every instruction. We give them to make reading of
generated IR easier.

LLVM IR is a typed language, so when we receive a boolean result of
comparison, we need to convert it to double (as Kaleidoscope has only
double values).

First we generate values for LHS and RHS. Then we generate an
instruction based on the value of operator. For comparison we do
additional type conversion as mentioned.

```rust
            &parser::BinaryExpr(ref name, ref lhs, ref rhs) => {
                let (lhs_value, _) = try!(lhs.codegen(context, module_provider));
                let (rhs_value, _) = try!(rhs.codegen(context, module_provider));

                match name.as_str() {
                    "+" => Ok((context.builder.build_fadd(lhs_value,
                                                          rhs_value,
                                                          "addtmp"),
                               false)),
                    "-" => Ok((context.builder.build_fsub(lhs_value,
                                                          rhs_value,
                                                          "subtmp"),
                               false)),
                    "*" => Ok((context.builder.build_fmul(lhs_value,
                                                          rhs_value,
                                                          "multmp"),
                               false)),
                    "<" => {
                        let cmp = context.builder.build_fcmp(LLVMRealOLT,
                                                             lhs_value,
                                                             rhs_value,
                                                             "cmptmp");

                        // convert boolean to double 0.0 or 1.0
                        Ok((context.builder.build_ui_to_fp(cmp,
                                                           context.ty.to_ref(),
                                                           "booltmp"),
                            false))
                    },
                    _ => error("invalid binary operator")
                }
            },
```

For call code generation we do function name lookup in the LLVM
Module's symbol table first. Then we compare the number of arguments in
call AST node and in the function defined in the LLVM Module. After it
we generate a value for every argument and create the arguments
vector.

```rust
            &parser::CallExpr(ref name, ref args) => {
                let (function, _) = match module_provider.get_function(name) {
                    Some(function) => function,
                    None => return error("unknown function referenced")
                };

                if function.count_params() as usize != args.len() {
                    return error("incorrect number of arguments passed")
                }

                let mut args_value = Vec::new();
                for arg in args.iter() {
                    let (arg_value, _) = try!(arg.codegen(context, module_provider));
                    args_value.push(arg_value);
                }

                Ok((context.builder.build_call(function.to_ref(),
                                               args_value.as_mut_slice(),
                                               "calltmp"),
                    false))
            }
```

That's all for code generation. You can easily add new operators to
Kaleidoscope with this implementation. For a list of instructions look
in [the LLVM language reference](http://llvm.org/docs/LangRef.html).

Full code for this chapter (including changes in driver, where we dump
values as they are generate and the whole module before exiting) is
available [here](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/2).

We can experiment with LLVM IR building now:

```
> 2+2

define double @0() {
entry:
  ret double 4.000000e+00
}
```

We didn't add any optimization, but LLVM already knows, that it can
fold constants. Also, note how top level expressions are enclosed in
anonymous functions.

```
> def f(a) a*a + a*a

define double @f(double %a) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double %a, %a
  %addtmp = fadd double %multmp, %multmp1
  ret double %addtmp
}
```

But more complicated cases are not handled. We will add optimization
in the next chapter. Here you also can see how did LLVM use our names hints.

```
> extern cos(x)

declare double @cos(double)

> cos(1)

define double @1() {
entry:
  %calltmp = call double @cos(double 1.000000e+00)
  ret double %calltmp
}
```

LLVM can generate a call to extern function. We will see that we
really can call standard functions this way later.

```
> .quit
; ModuleID = 'main'

define double @0() {
entry:
  ret double 4.000000e+00
}

define double @f(double %a) {
entry:
  %multmp = fmul double %a, %a
  %multmp1 = fmul double %a, %a
  %addtmp = fadd double %multmp, %multmp1
  ret double %addtmp
}

declare double @cos(double)

define double @1() {
entry:
  %calltmp = call double @cos(double 1.000000e+00)
  ret double %calltmp
}
```

On exit our REPL dumps all the produced LLVM IR.

## Chapter 3. Optimizer and JIT support

That's time to run our generated code. But before it we'll optimize it a little bit.

### LLVM Optimization Passes

Even without adding any optimization passes IRBuilder does some obvious
optimization that it can handle based on the local analysis. One of examples is constant folding:

```
> def text(x) 1+2+x;

define double @text(double %x) {
entry:
  %addtmp = fadd double 3.000000e+00, %x
  ret double %addtmp
}
```

Without ability of IRBuilder to do simple optimizations this IR would look like

```
define double @test(double %x) {
entry:
  %addtmp = fadd double 2.000000e+00, 1.000000e+00
  %addtmp1 = fadd double %addtmp, %x
  ret double %addtmp1
}
```

This is very usefull feature as you do not need to handle constant folding yourself
in the frontend.

However IRBuilder is unable do any optimizations that demand more then local analysis:

```
> def test(x) (1+2+x)*(x+(1+2));

define double @test(double %x) {
entry:
  %addtmp = fadd double 3.000000e+00, %x
  %addtmp1 = fadd double %x, 3.000000e+00
  %multmp = fmul double %addtmp, %addtmp1
  ret double %multmp
}
```

Here we would like to have RHS and LHS of multiplication to be computed
only once.

LLVM provides a general framework for optimization -- LLVM optimization passes.
Pass is analysis or transformation applied to IR. LLVM has two different pass scopes
(and two pass managers) -- function passes and whole module passes.
Function passmanager runs passes that operate on a single function at a time
(global optimization passes), module pass manager runs passes that have the whole module
as their input (interprocedural optimization passes). For more information
see [howto](http://llvm.org/docs/WritingAnLLVMPass.html) and [list of implemented
passes](http://llvm.org/docs/Passes.html).

We'll use function pass manager to run some optimizations on our functions
when they are completely generated. We'll create pass manager together with module
and apply its passes when function generation is complete.

Function pass manager initialization is straightforward:

```rust
pub fn new_module(name: &str) -> (core::Module, core::FunctionPassManager) {
    let module = core::Module::new(name);
    let mut function_passmanager = core::FunctionPassManager::new(&module);
    function_passmanager.add_basic_alias_analysis_pass();
    function_passmanager.add_instruction_combining_pass();
    function_passmanager.add_reassociate_pass();
    function_passmanager.add_GVN_pass();
    function_passmanager.add_CFG_simplification_pass();
    function_passmanager.initialize();

    (module, function_passmanager)
}
```

We create module and function pass manager associated with it. Then we
add a series of passes. The first one is analysis the other four are
transformation. These passes should reasonably cleanup and reorganize
generated IR.

We'll store function pass manager togerther with module in our `SimpleModuleProvider`
(also we'll change the `ModuleProvider` trait):

```rust
pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager;
}

pub struct SimpleModuleProvider {
    function_passmanager: core::FunctionPassManager,
    module: core::Module
}

impl SimpleModuleProvider {
    pub fn new(name: &str) -> SimpleModuleProvider {
        let (module, function_passmanager) = new_module(name);

        SimpleModuleProvider {
            function_passmanager: function_passmanager,
            module: module
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }

    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }
}
```

Now we run our passes on every created function before return it:

```rust
        // emit function body
        // if error occured, remove the function, so user can
        // redefine it
        let body = match self.body.codegen(context, module_provider) {
            Ok((value, _)) => value,
            Err(message) => {
                unsafe {LLVMDeleteFunction(function.to_ref())};
                return Err(message);
            }
        };

        // the last instruction should be return
        context.builder.build_ret(&body);

        function.verify(LLVMAbortProcessAction);
        module_provider.get_pass_manager().run(&mut function);

        // clear local variables
        context.named_values.clear();
        Ok((function.to_ref(), self.prototype.name.as_str() == ""))
    }
}
```

We can try to run our example again and see if optimization helps:

```
> def test(x) (1+2+x)*(x+(1+2));

define double @test(double %x) {
entry:
  %addtmp = fadd double %x, 3.000000e+00
  %multmp = fmul double %addtmp, %addtmp
  ret double %multmp
}
```

Nice, it works and does what we'd expected.

You can experiment with different passes using `opt` command line tool.

### MCJIT based JIT-compiler

Now as we have reasonable IR generated it is time to compile and run it.
As we are implementing REPL, we'll use JIT-compiler for compilation.
There are two JIT-compiler infrastructures in LLVM:
[MCJIT](http://llvm.org/docs/MCJITDesignAndImplementation.html)
and [ORC](http://llvm.cc/t/llvmdev-new-jit-api-orcjit/219).
The second one is newer and nicer, but it lacks C bindings and Rust bindings so far
(I'm working on fixing it though). In this chapter we will use
MCJIT as a base for our JIT-compiler.

First of all we'll think about what do we need. That's quite simple: we want to
generate IR for every entered entity and run top-level expressions (anonymous functions).

Let's define a simple trait for JIT compiler (note, that our compiler will own all
the modules that we will have):

```rust
pub trait JITter : builder::ModuleProvider {
    // TODO: fix https://github.com/rust-lang/rust/issues/5665
    fn get_module_provider(&mut self) -> &mut builder::ModuleProvider;

    fn run_function(&mut self, f: LLVMValueRef) -> f64;
}
```

For IR generation we need a module, so our jit-compiler will be a module provider
(the `get_module_provider` method was added because of an annoying `rustc` bug:
you cannot coerce to supertraits). Also we want to run some of generated functions,
the method `run_function` will do this.

The main object that we'll need is an
[execution engine](http://llvm.org/docs/doxygen/html/classllvm_1_1ExecutionEngine.html).
It has all the methods for code generation/function running and can use
both interpreter and jit-compiler. When it compiles module, it should be frozen and no longer touched.
We'll need a collection of modules and corresponding execution engines.
We will compile and close current module when we encounter a top-level expression.

So the plan is simple. Create a module. Fill it with IR. When user enters
an expression, close the module. Create execution engine. Compile it and evalute the
expression, showing result to user. Remember module and execution engine and repeat this
in a loop.

Everything is fine and looks simple. But there is one problem:
intermodular symbol resolution. By default LLVM resolves symbols only in
current module (plus loaded libraries). If we will try to call function
defined in some of previous modules, it will fail to do so. We will
solve this issue with custom names resolver.
Also we need to add some code that will generate prototypes for functions
defined in other modules, so they can be called.

Let's define a container for execution engines and frozen modules first:

```rust
struct ModulesContainer {
    execution_engines: Vec<ExecutionEngine>,
    modules: Vec<FrozenModule>
}
```

Now we will define a method for looking for function addresses in already
compiled modules (if nothing found we are returning zero):

```rust
impl ModulesContainer {
    fn get_function_address(&self, name: &str) -> u64 {
        for ee in &self.execution_engines {
            let addr = ee.get_function_address(name);
            if addr != 0 {
                return addr;
            }
        }

        0
    }
}
```

JIT-compiler itself will contain current open module, associated function pass manager
and container with already compiled stuff:

```rust
pub struct MCJITter {
    module_name: String,
    current_module: core::Module,
    function_passmanager: core::FunctionPassManager,

    container: Rc<RefCell<ModulesContainer>>
}
```

There is one tricky moment with the container: we make it be internally mutable reference counted
pointer. We need to do this because we need to provide reference to the container
to our symbol resolver, which itself will be owned by execution engine. This leads to
problems with borrow checker that can be solved in the shown way. If you are not familiar with
mentioned concepts, you can read
[appropriate chapter](https://doc.rust-lang.org/nightly/book/choosing-your-guarantees.html)
in the Rust book or consult with [detailed](https://doc.rust-lang.org/nightly/std/rc/index.html)
[documentation](https://doc.rust-lang.org/nightly/std/cell/index.html).

Now we are going to implement `MCJITter` internal methods. Constructor is trivial:

```rust
    pub fn new(name: &str) -> MCJITter {
        let (current_module, function_passmanager) = builder::new_module(name);

        MCJITter {
            module_name: String::from(name),
            current_module: current_module,
            function_passmanager: function_passmanager,

            container: Rc::new(RefCell::new(ModulesContainer {
                execution_engines: vec![],
                modules: vec![]
            }))
        }
    }
```

The method for closing current module is where the magic of execution engine creation happens:

```rust
    fn close_current_module(& mut self) {
        let (new_module, new_function_passmanager) = builder::new_module(&self.module_name);
        self.function_passmanager = new_function_passmanager;
        let current_module = std::mem::replace(&mut self.current_module, new_module);

        let container = self.container.clone();
        let memory_manager = BindingSectionMemoryManagerBuilder::new()
            .set_get_symbol_address(move |mut parent_mm, name| {
                let addr = parent_mm.get_symbol_address(name);
                if addr != 0 {
                    return addr;
                }

                container.borrow().get_function_address(name)
            })
            .create();

        let (execution_engine, module) = match MCJITBuilder::new()
            .set_mcjit_memory_manager(Box::new(memory_manager))
            .create(current_module) {
                Ok((ee, module)) => (ee, module),
                Err(msg) => panic!(msg)
            };

        self.container.borrow_mut().execution_engines.push(execution_engine);
        self.container.borrow_mut().modules.push(module);
    }
```

We create new module and pass manager first. Note the use of `std::mem::replace`,
so we can move only from one record field.

```rust
        let (new_module, new_function_passmanager) = builder::new_module(&self.module_name);
        self.function_passmanager = new_function_passmanager;
        let current_module = std::mem::replace(&mut self.current_module, new_module);
```

Then we create a custom memory manager for our execution engine. The symbol resolution
is just a closure that owns a reference to our modules container. It asks default memory
manager first, than it looks in already compiled modules:

```rust
        let container = self.container.clone();
        let memory_manager = BindingSectionMemoryManagerBuilder::new()
            .set_get_symbol_address(move |mut parent_mm, name| {
                let addr = parent_mm.get_symbol_address(name);
                if addr != 0 {
                    return addr;
                }

                container.borrow().get_function_address(name)
            })
            .create();
```

Now we can create the execution engine for the current module:

```rust
        let (execution_engine, module) = match MCJITBuilder::new()
            .set_mcjit_memory_manager(Box::new(memory_manager))
            .create(current_module) {
                Ok((ee, module)) => (ee, module),
                Err(msg) => panic!(msg)
            };
```

`create` method returns execution engine and a frozen module.

Finally we update our modules and execution engines container:

```rust
        self.container.borrow_mut().execution_engines.push(execution_engine);
        self.container.borrow_mut().modules.push(module);
```

Now, as we have a method for execution engine creation on module closing
and know how will our module organization look like, we can proceed with
implementation of `ModuleProvider` and `JITter` traits.

`dump`, `get_module` and `get_pass_manager` are really simple:

```rust
    fn dump(&self) {
        for module in self.container.borrow().modules.iter() {
            module.get().dump()
        }
        self.current_module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.current_module
    }
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.function_passmanager
    }
```

Note, that we dump already compiled modules first and then our currently open module.


`get_function` is a little bit more complicated as we need to look in already compiled modules first.
If we find already declared/defined function in one of the old modules, we look
for a prototype in the current module. If we find not a prototype, but a declaration, we have
some error, as it should not be possible to redefine functions across modules. If prototype
exists, we can return it. Our symbol resolution code will handle linking correctly.

If no prototype in the current module is available, we need to create and return one.
To create it we need the function type. Note, that `get_type` on function returns
type `() -> F`, where `F` is our real function type, so we need to call `get_return_type`
on the returned value. Also these methods are not especially friendly in my current bindings,
so we need to change reference type by hand (I hope to fix this).

If no function in previous modules is found, we look in the current one as we did in previous
chapter (in `SimpleModuleProvider`).

Code for `get_function` follows:

```rust
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        for module in &self.container.borrow().modules {
            let funct = match module.get().get_function_by_name(name) {
                Some(f) => {
                    f
                },
                None => continue
            };

            let proto = match self.current_module.get_function_by_name(name) {
                Some(f) => {
                    if f.count_basic_blocks() != 0 {
                        panic!("redefinition of function across modules".to_string())
                    }
                    f
                },
                None => {
                    // TODO: fix iron-llvm get_type
                    let fty = unsafe { FunctionTypeRef::from_ref(funct.get_type().to_ref()) };
                    let fty = unsafe { FunctionTypeRef::from_ref(fty.get_return_type().to_ref()) };
                    FunctionRef::new(&mut self.current_module, name, &fty)
                }
            };

            return Some((proto, funct.count_basic_blocks() > 0))
        }

        match self.current_module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }
```

Implementation of the `JITter` trait is straightforward.
For `run_function` we close the current module
and run the function using the lates execution
engine (as the function lives in the just
closed module). That's all. Run function method of execution engine
compiles code automatically. Already written pieces of code
ensures that we have all the necessary prototypes and correct
symbol resolution.

```rust
impl JITter for MCJITter {
    fn get_module_provider(&mut self) -> &mut builder::ModuleProvider {
        self
    }

    fn run_function(&mut self, f: LLVMValueRef) -> f64 {
        self.close_current_module();
        let f = unsafe {FunctionRef::from_ref(f)};
        let mut args = vec![];
        let res = self.container.borrow()
            .execution_engines.last().expect("MCJITter went crazy")
            .run_function(&f, args.as_mut_slice());
        let ty = RealTypeRef::get_double();
        res.to_float(&ty)
    }
}
```

### Changes in the driver and 'built-in' functions

We will make our driver work both with `SimpleModuleProvider` (when only generating IR)
and with `MCJITter` (when having REPL with jit-compiling). To make it easier,
we will implement `JITter` trait for `SimpleModuleProvider`:

```rust
impl JITter for builder::SimpleModuleProvider {
    fn get_module_provider(&mut self) -> &mut builder::ModuleProvider {
        self
    }

    fn run_function(&mut self, _f: LLVMValueRef) -> f64 {
        panic!("not implemented (and will not be)")
    }
}
```

Now we will use an appropriate `ir-container` in the driver and depending on the
stage and type of expression that user has entered we will dump IR or compile and execute it.
Also we need to initialize native target for JIT-compiler.

```rust
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
```

You can see also call of the `init` function that will be explained in a moment, just ignore
it for a while.

Now we modify the main function appropriately, so it calls the main loop
for the `Exec` stage by default.

```rust
#![feature(plugin)]
#![plugin(docopt_macros)]

extern crate rustc_serialize;
extern crate docopt;

extern crate iron_kaleidoscope;

use iron_kaleidoscope::driver::{main_loop,
                                Exec,
                                IR,
                                AST,
                                Tokens
};

docopt!(Args, "
Usage: iron_kaleidoscope [(-l | -p | -i)]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -i  Run only IR builder and show its output.
");

fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Tokens
    } else if args.flag_p {
        AST
    } else if args.flag_i {
        IR
    } else {
        Exec
    };

    main_loop(stage);
}
```

Let's see how does it work now:

```
> 2+2
=> 4
> def a(x) x*x+x*x

define double @a(double %x) {
entry:
  %multmp = fmul double %x, %x
  %addtmp = fadd double %multmp, %multmp
  ret double %addtmp
}

> a(4)
=> 32
> a(3)
=> 18
> extern sin(x)

declare double @sin(double)

> sin(1)
=> 0.8414709848078965
> sin(0)
=> 0
> .quit
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

define double @0() #0 {
entry:
  ret double 4.000000e+00
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

define double @a(double %x) #0 {
entry:
  %multmp = fmul double %x, %x
  %addtmp = fadd double %multmp, %multmp
  ret double %addtmp
}

define double @0() #0 {
entry:
  %calltmp = call double @a(double 4.000000e+00)
  ret double %calltmp
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

define double @0() #0 {
entry:
  %calltmp = call double @a(double 3.000000e+00)
  ret double %calltmp
}

declare double @a(double) #0

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

declare double @sin(double) #0

define double @0() #0 {
entry:
  ret double 0x3FEAED548F090CEE
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

define double @0() #0 {
entry:
  ret double 0.000000e+00
}

declare double @sin(double) #0

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
```

Everything works as great as we expected. Additionaly you can see that we are able
to use functions from libraries such as `sin`. What more, we can define our own
'built-in' functions. To this aim we just create normal rust functions and register
their addresses in LLVM with `add_symbol` function:

```rust
pub extern fn printd(x: f64) -> f64 {
    println!("> {} <", x);
    x
}

pub extern fn putchard(x: f64) -> f64 {
    print!("{}", x as u8 as char);
    x
}

pub fn init() {
    unsafe {
        add_symbol("printd", printd as *const ());
        add_symbol("putchard", putchard as *const ());
    }
}
```

That's all, we are able to call rust functions now:

```
> extern printd(x)

declare double @printd(double)

> printd(10)
> 10 <
=> 10
> extern putchard(x)

declare double @putchard(double)

> putchard(120)
x=> 120
> .quit
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

declare double @printd(double) #0

define double @0() #0 {
entry:
  %calltmp = call double @printd(double 1.000000e+01)
  ret double %calltmp
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

declare double @putchard(double) #0

define double @0() #0 {
entry:
  %calltmp = call double @putchard(double 1.200000e+02)
  ret double %calltmp
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
```

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
