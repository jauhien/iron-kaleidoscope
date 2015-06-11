# UPDATE: Make lexer and parser compile and work with the latest nightly Rust. Other stuff is disabled.

# [LLVM tutorial](http://llvm.org/docs/tutorial) in [the Rust language](http://www.rust-lang.org/).

This tutorial is a work in progress and at the moment I'm working on getting it fully working with
the latest Rust and on improvinvg the way it uses LLVM.

## Table of Contents

* [Introduction](#introduction)
  * [Basic variant of the Kaleidoscope language](#basic-variant-of-the-kaleidoscope-language)
  * [The project structure](#the-project-structure)
  * [The lexer](#the-lexer)
* [Parser and AST implementation](#ast-and-parser-implementation)
  * [The grammar](#the-grammar)
  * [The Abstract Syntax Tree (AST)](#the-abstract-syntax-tree-ast)
  * [Parser implementation: introduction](#parser-implementation-introduction)
  * [Top level parse function](#top-level-parse-function)
  * [Helper macros for work with tokens](#helper-macros-for-work-with-tokens)
  * [Parsing of statements and top level expressions](#parsing-of-statements-and-top-level-expressions)
  * [Parsing of primary expressions](#parsing-of-primary-expressions)
  * [Parsing of binary expressions](#parsing-of-binary-expressions)
  * [The driver](#the-driver)
* [LLVM IR code generation](#llvm-ir-code-generation)
  * [Rust LLVM bindings and code generation setup](#rust-llvm-bindings-and-code-generation-setup)
  * [Top level code generation](#top-level-code-generation)
  * [Expression code generation](#expression-code-generation)
  * [Changes in the driver](#changes-in-the-driver)
* [JIT and optimizer support](#jit-and-optimizer-support)
  * [Missing LLVM bindings](#missing-llvm-bindings)
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

* the JIT compiler ([builder.rs](https://github.com/jauhien/iron-kaleidoscope/blob/master/src/builder.rs))

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
    let token_re = regex!(r"(?P<ident>\p{Alphabetic}\w*)|(?P<number>\d+\.?\d*)|(?P<delimiter>;)|(?P<oppar>\()|(?P<clpar>\))|(?P<comma>,)|(?P<operator>\S)");
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

## AST and parser implementation

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
#[deriving(PartialEq, Clone, Show)]
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

These functions are generic as we will need to return different data types depending on what we are parsing (prototype, expression, etc.)
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
pub fn parse(tokens : &[Token], parsed_tree : &[ASTNode], settings : &mut ParserSettings) -> ParsingResult {
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
)
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
)
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
#[allow(unused_variables)]
fn parse_prototype(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Prototype> {
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
Note, that we ignore `Comma` tokens, so they are equivalent to whitspaces (like
in old good Clojure language). If we find a closing parenthesis, prototype
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
#[allow(unused_variables)]
fn parse_literal_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
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

A binary expression is a primary expression followed by zero or more (operator, primary expression) pairs.
The expression parsing function looks like this:

```rust
fn parse_expr(tokens : &mut Vec<Token>, settings : &mut ParserSettings) -> PartParsingResult<Expression> {
    let mut parsed_tokens = Vec::new();
    let lhs = parse_try!(parse_primary_expr, tokens, settings, parsed_tokens);
    let expr = parse_try!(parse_binary_expr, tokens, settings, parsed_tokens, 0, &lhs);
    Good(expr, parsed_tokens)
}
```

`parse_binary_expr` will return LHS if there are no (operator, primary expression) pairs or parse the whole expression.

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
Usage: iron_kaleidoscope [(-l | -p )]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
")

#[cfg(not(test))]
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
#[deriving(PartialEq, Clone, Show)]
pub enum Stage {
    Tokens,
    AST
}
```

Driver itself looks like this:

```rust
pub fn main_loop(stage: Stage) {
    let mut parser_settings = default_parser_settings();

    'main: loop {
        print!(">");
        let mut input = io::stdin().read_line().ok().expect("Failed to read line");
        if input.as_slice() == ".quit\n" {
            break;
        }

        // the constructed AST
        let mut ast = Vec::new();
        // tokens left from the previous lines
        let mut prev = Vec::new();
        loop {
            let tokens = tokenize(input.as_slice());
            if stage == Tokens {
                println!("{}", tokens);
                continue 'main
            }

            prev.extend(tokens.into_iter());

            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &mut parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.extend(parsed_ast.into_iter());
                    if rest.is_empty() {
                        // we have parsed the full expression
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

        if stage == AST {
            println!("{}", ast);
            continue
        }
    }
}
```

## LLVM IR code generation

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

### Rust LLVM bindings and code generation setup

LLVM has two interfaces: C++ interface and a stable C interface. Apart
from it LLVM has a number of bindings, usually based on the C interface.

Before or during reading of this chapter of tutorial you can read an
[appropriate section in the LLVM Programmer's
Manual](http://llvm.org/docs/ProgrammersManual.html#the-core-llvm-class-hierarchy-reference).

As you probably know, Rust compiler uses LLVM. So it should not
surprise you, that is has LLVM bindings somewhere deeply inside its
code. You can find them in the `src/librustc_llvm` directory of the
Rust compiler sources. Those bindings (based on the stable C
interface) are not full and also are unsafe.
There are some safe wrappers on top of them, but these wrappers are
very Rust compiler specific, so we will use the available unsafe
bindings.

To start using bindings we need to add an appropriate use in the beginning of the
module:

```rust
use rustc::lib::llvm;
```

and link with the necessary crate (in the root module):

```rust
extern crate rustc;
```

Now we need to add code generation functions to every AST element. The
result of these function will be `llvm::ValueRef`:

```rust
pub type IRBuildingResult = Result<llvm::ValueRef, String>;

pub trait IRBuilder {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult;
}

fn error(message : &str) -> IRBuildingResult {
    Err(message.to_string())
}
```

`Context` is a structure with additional context data needed for the
code generation:

```rust
pub struct Context {
    context: llvm::ContextRef,
    module: llvm::ModuleRef,
    builder: llvm::BuilderRef,
    named_values: HashMap<String, llvm::ValueRef>
}
```

`context` is an LLVM context used during code generation. The `module`
contains all the generated code. It is a top level structure similar to the
Rust crate. `builder` is a helper object that emits IR instructions,
it keeps track of the current place to insert instructions and
provides an interface to code generation. `named_values` will be used
to track defined values (at the moment we have only named function parameters).

Before doing any real job we need to initialize the context. Also we add a
`dump` function to display the results of the code generation:

```rust
impl Context {
    pub fn new(module_name : &str) -> Context {
        unsafe {
            let context = llvm::LLVMContextCreate();
            let module = llvm::LLVMModuleCreateWithNameInContext(module_name.to_c_str().as_ptr(), context);
            let builder = llvm::LLVMCreateBuilderInContext(context);
            let named_values = HashMap::new();

            Context { context: context,
                      module: module,
                      builder: builder,
                      named_values: named_values }
        }
    }

    pub fn dump(&self) {
        unsafe {
            llvm::LLVMDumpModule(self.module);
        }
    }
}
```

In addition, we will need to do some cleaning up after code generation is
finished and we no longer need our context:

```rust
impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            llvm::LLVMDisposeBuilder(self.builder);
            llvm::LLVMDisposeModule(self.module);
            llvm::LLVMContextDispose(self.context);
        }
    }
}
```

### Top level code generation

Let's implement `IRBuilder` trait for top level data structures. For
`ParsingResults`, AST tree and `ASTNode` it is really trivial:

```rust
impl IRBuilder for ParsingResult {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &Ok((ref ast, _)) => ast.codegen(context),
            &Err(ref message) => Err(message.clone())
        }
    }
}

impl IRBuilder for Vec<ASTNode> {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        let mut result = error("empty AST");
        for node in self.iter() {
            result = Ok(try!(node.codegen(context)));
        }

        result
    }
}

impl IRBuilder for ASTNode {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        match self {
            &ExternNode(ref prototype) => prototype.codegen(context),
            &FunctionNode(ref function) => function.codegen(context)
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
impl IRBuilder for Prototype {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            // check if declaration with this name was already done
            let prev_definition = llvm::LLVMGetNamedFunction(context.module, self.name.to_c_str().as_ptr());

            let function =
                if !prev_definition.is_null() {
                    // we do not allow to redeclare functions with
                    // other signatures
                    if llvm::LLVMCountParams(prev_definition) as uint != self.args.len() {
                        return error("redefinition of function with different number of args")
                    }
                    // we do not allow to redefine/redeclare already
                    // defined functions (those that have the body)
                    if llvm::LLVMCountBasicBlocks(prev_definition) != 0 {
                        return error("redefinition of function");
                    }

                    prev_definition

                } else {
                    // function type if defined by number and types of
                    // the arguments
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    let param_types = Vec::from_elem(self.args.len(), ty);
                    let fty = llvm::LLVMFunctionType(ty, param_types.as_ptr(), param_types.len() as c_uint, false as c_uint);

                    llvm::LLVMAddFunction(context.module,
                                          self.name.to_c_str().as_ptr(),
                                          fty)
                };

            // set correct parameters names
            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.args.iter() {
                llvm::LLVMSetValueName(param, arg.to_c_str().as_ptr());
                param = llvm::LLVMGetNextParam(param);
            }

            Ok((function, false))
        }
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
impl IRBuilder for Function {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        // we have no global variables, so we can clear all the
        // previously defined named values as they come from other functions
        context.named_values.clear();

        let function = try!(self.prototype.codegen(context));

        unsafe {
            // basic block that will contain generated instructions
            let basic_block = llvm::LLVMAppendBasicBlockInContext(context.context,
                                                                  function,
                                                                  "entry".to_c_str().as_ptr());
            llvm::LLVMPositionBuilderAtEnd(context.builder, basic_block);

            // set function parameters
            let mut param = llvm::LLVMGetFirstParam(function);
            for arg in self.prototype.args.iter() {
                context.named_values.insert(arg.clone(), param);
                param = llvm::LLVMGetNextParam(param);
            }

            // emit function body
            // if error uccured, remove the function, so user can
            // redefine it
            let body = match self.body.codegen(context) {
                Ok((value, _)) => value,
                Err(message) => {
                    llvm::LLVMDeleteFunction(function);
                    return Err(message);
                }
            };

            // the last instruction should be return
            llvm::LLVMBuildRet(context.builder, body);
        }

        // clear local variables
        context.named_values.clear();
        Ok(function)
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
anonymous functions, so they need no additional logic.

### Expression code generation

As everything apart from function declarations/definitions is
expression in kaleidoscope, we will have a big match on an
`Expression` AST node in branches of which we will generate IR for
appropriate language constructions. Here is the full function for reference,
comments will follow:

```rust
impl IRBuilder for Expression {
    fn codegen(&self, context: &mut Context) -> IRBuildingResult {
        unsafe {
            match self {
                &Literal(ref value) => {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    Ok(llvm::LLVMConstReal(ty, *value))
                },
                &Variable(ref name) => {
                    match context.named_values.find(name) {
                        Some(value) => Ok(*value),
                        None => error("unknown variable name")
                    }
                },
                &Binary(ref name, ref lhs, ref rhs) => {
                    let lhs_value = try!(lhs.codegen(context));
                    let rhs_value = try!(rhs.codegen(context));

                    match name.as_slice() {
                        "+" => Ok(llvm::LLVMBuildFAdd(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "addtmp".to_c_str().as_ptr())),
                        "-" => Ok(llvm::LLVMBuildFSub(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "subtmp".to_c_str().as_ptr())),
                        "*" => Ok(llvm::LLVMBuildFMul(context.builder,
                                                    lhs_value,
                                                    rhs_value,
                                                    "multmp".to_c_str().as_ptr())),
                        "<" => {
                            let cmp = llvm::LLVMBuildFCmp(context.builder,
                                                          llvm::RealOLT as c_uint,
                                                          lhs_value,
                                                          rhs_value,
                                                          "cmptmp".to_c_str().as_ptr());
                            let ty = llvm::LLVMDoubleTypeInContext(context.context);
                            // convert boolean to double 0.0 or 1.0
                            Ok(llvm::LLVMBuildUIToFP(context.builder,
                                                      cmp,
                                                      ty,
                                                      "booltmp".to_c_str().as_ptr()))
                        },
                        _ => error("invalid binary operator")
                    }
                },
                &Call(ref name, ref args) => {
                    let function = llvm::LLVMGetNamedFunction(context.module, name.to_c_str().as_ptr());
                    if function.is_null() {
                        return error("unknown function referenced")
                    }
                    if llvm::LLVMCountParams(function) as uint != args.len() {
                        return error("incorrect number of arguments passed")
                    }
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        let arg_value = try!(arg.codegen(context));
                        args_value.push(arg_value);
                    }
                    Ok(llvm::LLVMBuildCall(context.builder,
                                            function,
                                            args_value.as_ptr(),
                                            args_value.len() as c_uint,
                                            "calltmp".to_c_str().as_ptr()))
                }
            }
        }
    }
}
```

Let's have a look at match branches one by one.

For `Literal` expression we just return a real constant with the
appropriate value:

```rust
                &Literal(ref value) => {
                    let ty = llvm::LLVMDoubleTypeInContext(context.context);
                    Ok(llvm::LLVMConstReal(ty, *value))
                }
```

For variables we look in the `named_values` map and if there is such a
variable there (a value that corresponds to the function argument),
we return the value or emit an error otherwise:

```rust
                &Variable(ref name) => {
                    match context.named_values.find(name) {
                        Some(value) => Ok(*value),
                        None => error("unknown variable name")
                    }
                }
```

For binary expressions we do some real instructions generation. Note,
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
                &Binary(ref name, ref lhs, ref rhs) => {
                    let lhs_value = try!(lhs.codegen(context));
                    let rhs_value = try!(rhs.codegen(context));

                    match name.as_slice() {
                        "+" => Ok(llvm::LLVMBuildFAdd(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "addtmp".to_c_str().as_ptr())),
                        "-" => Ok(llvm::LLVMBuildFSub(context.builder,
                                                       lhs_value,
                                                       rhs_value,
                                                       "subtmp".to_c_str().as_ptr())),
                        "*" => Ok(llvm::LLVMBuildFMul(context.builder,
                                                    lhs_value,
                                                    rhs_value,
                                                    "multmp".to_c_str().as_ptr())),
                        "<" => {
                            let cmp = llvm::LLVMBuildFCmp(context.builder,
                                                          llvm::RealOLT as c_uint,
                                                          lhs_value,
                                                          rhs_value,
                                                          "cmptmp".to_c_str().as_ptr());
                            let ty = llvm::LLVMDoubleTypeInContext(context.context);
                            // convert boolean to double 0.0 or 1.0
                            Ok(llvm::LLVMBuildUIToFP(context.builder,
                                                      cmp,
                                                      ty,
                                                      "booltmp".to_c_str().as_ptr()))
                        },
                        _ => error("invalid binary operator")
                    }
                }
```

For call code generation we do function name lookup in the LLVM
Module's symbol table first. Then we compare the number of arguments in
call AST node and in the function defined in the LLVM Module. After it
we generate a value for every argument and create the arguments
vector. Note, how we can pass a pointer to a vector to external code.

```rust
                &Call(ref name, ref args) => {
                    let function = llvm::LLVMGetNamedFunction(context.module, name.to_c_str().as_ptr());
                    if function.is_null() {
                        return error("unknown function referenced")
                    }
                    if llvm::LLVMCountParams(function) as uint != args.len() {
                        return error("incorrect number of arguments passed")
                    }
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        let arg_value = try!(arg.codegen(context));
                        args_value.push(arg_value);
                    }
                    Ok(llvm::LLVMBuildCall(context.builder,
                                            function,
                                            args_value.as_ptr(),
                                            args_value.len() as c_uint,
                                            "calltmp".to_c_str().as_ptr()))
                }
```

That's all for code generation. You can easily add new operators to
Kaleidoscope with this implementation. For a list of instructions look
in [the LLVM language reference](http://llvm.org/docs/LangRef.html).

### Changes in the driver

As we can generate LLVM IR now, it would be interesting to have a look
at it. To do so, we will change our driver slightly. First, we will
add new command line option:

```rust
docopt!(Args, "
Usage: iron_kaleidoscope [(-l | -p | -i)]

Options:
    -l  Run only lexer and show its output.
    -p  Run only parser and show its output.
    -i  Run only IR builder and show its output.
")
```

Now we'll extend `Stage` enum and handle command line option in the
`main` function:

```rust
#[deriving(PartialEq, Clone, Show)]
pub enum Stage {
    Tokens,
    AST,
    IR
}
```

```rust
#[cfg(not(test))]
fn main() {
    let args: Args = Args::docopt().decode().unwrap_or_else(|e| e.exit());

    let stage = if args.flag_l {
        Tokens
    } else if args.flag_p {
        AST
    } else {
        IR
    };

    main_loop(stage);
}
```

Then we can add this code to the driver after checking for `AST`
stage:

```rust
        match ast.codegen(&mut context) {
            Ok(value) => {
                dump_value(value)
            },
            Err(message) => println!("Error occured: {}", message)
        }
```

Also we can dump the whole module before exiting, so driver will look
like this now:

```rust
pub fn main_loop(stage: Stage) {
    let mut parser_settings = default_parser_settings();

    'main: loop {
        print!(">");
        let mut input = io::stdin().read_line().ok().expect("Failed to read line");
        if input.as_slice() == ".quit\n" {
            break;
        }

        // the constructed AST
        let mut ast = Vec::new();
        // tokens left from the previous lines
        let mut prev = Vec::new();
        loop {
            let tokens = tokenize(input.as_slice());
            if stage == Tokens {
                println!("{}", tokens);
                continue 'main
            }

            prev.extend(tokens.into_iter());

            let parsing_result = parse(prev.as_slice(), ast.as_slice(), &mut parser_settings);
            match parsing_result {
                Ok((parsed_ast, rest)) => {
                    ast.extend(parsed_ast.into_iter());
                    if rest.is_empty() {
                        // we have parsed the full expression
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

        if stage == AST {
            println!("{}", ast);
            continue
        }

        match ast.codegen(&mut context) {
            Ok(value) => {
                dump_value(value)
            },
            Err(message) => println!("Error occured: {}", message)
        }
    }

    if stage == IR {
        context.dump();
    }
}
```

We can experiment with LLVM IR building now:

```
>2+2

define double @0() {
entry:
  ret double 4.000000e+00
}
```

We didn't add any optimization, but LLVM already knows, that it can
fold constants. Also, note how top level expressions are enclosed in
anonymous functions.

```
>def f(a) a*a + a*a

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
>extern cos(x)

declare double @cos(double)

>cos(1)

define double @1() {
entry:
  %calltmp = call double @cos(double 1.000000e+00)
  ret double %calltmp
}
```

LLVM can generate a call to extern function. We will see that we
really can call standard functions this way later.

```
>.quit
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

## JIT and optimizer support

It is time to add JIT compilation and optimization to our REPL and see
real evaluation of some kaleidoscope expressions.

### Missing LLVM bindings

So far we were lucky as there were the necessary bindings in the Rust
itself. Now we are starting to deal with things that were not needed
for Rust authors: JIT compilation, so we will need to add some
bindings ourselves. A good starting point is reading of [The Rust
Foreign Function Interface
Guide](http://doc.rust-lang.org/guide-ffi.html) to understand how
FFI in Rust works.

Now, when we understand it, we can look at Rust bindings (found in
`src/librustc_llvm` in the Rust sources) and find out how they work.
We see bindings in the `lib.rs` file and one auto generated file (`llvmdeps.rs`) for
correct linking with `LLVM`. We will do the same (and even use near the
same script for generation of the file necessary for linking).

Before it we need to know how to call a custom script during
compilation of cargo based project. Here [Build Script
Support](http://doc.crates.io/build-script.html) helps us.

Additionally we will need initialization code for LLVM JIT (it uses
macros defined in C headers, so it should be written in C):

```c
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>

void llvm_initialize_native_target() {
	LLVMLinkInJIT();
	LLVMInitializeNativeTarget();
}
```

We will compile it to the static library and link with our Rust code.
To compile it we will use a plain Makefile:

```makefile
CC ?= gcc
AR ?= ar

LLVM_CFLAGS := $(shell llvm-config --cflags)
CFLAGS += $(LLVM_CFLAGS)

all: libllvm_initialization.a

llvm_initialization.o: src/missing_llvm_bindings/llvm_initialization.c
	@$(CC) -c $(CFLAGS) $< -o $(OUT_DIR)/$@

libllvm_initialization.a: llvm_initialization.o
	@cd $(OUT_DIR) && $(AR) crus $@ $<
```

To properly link with LLVM we want to generate something similar to

```rust
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[link(name = "llvm_initialization")]
#[link(name = "LLVMJIT")]
#[link(name = "LLVMExecutionEngine")]
#[link(name = "LLVMCodeGen")]
#[link(name = "LLVMScalarOpts")]
#[link(name = "LLVMInstCombine")]
#[link(name = "LLVMTransformUtils")]
#[link(name = "LLVMipa")]
#[link(name = "LLVMAnalysis")]
#[link(name = "LLVMTarget")]
#[link(name = "LLVMMC")]
#[link(name = "LLVMCore")]
#[link(name = "LLVMSupport")]
#[link(name = "z")]
#[link(name = "pthread")]
#[link(name = "ffi")]
#[link(name = "curses")]
#[link(name = "dl")]
#[link(name = "m")]
#[link(name = "stdc++")]
extern {}
```

Here we have LLVM and system libraries mentioned plus one library
(`llvm_initialization`) created by ourselves.

To generate it we will use as a prototype `src/etc/mklldeps.py` from
the Rust project. Here it is its modified version:

```python
# Copyright 2014 Jauhien Piatlicki <jauhien@gentoo.org>
#
# Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
# file at the top-level directory of this distribution and at
# http://rust-lang.org/COPYRIGHT.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
# <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
# option. This file may not be copied, modified, or distributed
# except according to those terms.

import os
import sys
import subprocess
import itertools
from os import path

f = open(sys.argv[1], 'wb')

out_dir = sys.argv[2]

components = sys.argv[3].split(' ')
components = [i for i in components if i]  # ignore extra whitespaces

f.write("""// Copyright 2014 Jauhien Piatlicki <jauhien@gentoo.org>
//
// Copyright 2013-2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// WARNING: THIS IS A GENERATED FILE, DO NOT MODIFY
//          take a look at src/etc/mklldeps.py if you're interested
""")

def run(args):
    proc = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()

    if err:
        print("failed to run llconfig: args = `{}`".format(args))
        print(err)
        sys.exit(1)
    return out

for llconfig in sys.argv[4:]:
    f.write("\n")

    out = run([llconfig, '--host-target'])
    arch, os = out.split('-', 1)
    arch = 'x86' if arch == 'i686' or arch == 'i386' else arch

    native_target = "INVALID";

    if 'x86' in arch:
        native_target = 'X86'
    elif 'arm' in arch:
        native_target = 'ARM'
    elif 'mips' in arch:
        native_target = 'MIPS'

    if 'darwin' in os:
        os = 'macos'
    elif 'linux' in os:
        os = 'linux'
    elif 'freebsd' in os:
        os = 'freebsd'
    elif 'dragonfly' in os:
        os = 'dragonfly'
    elif 'android' in os:
        os = 'android'
    elif 'win' in os or 'mingw' in os:
        os = 'windows'
    cfg = [
        "target_arch = \"" + arch + "\"",
        "target_os = \"" + os + "\"",
    ]

    f.write("#[cfg(all(" + ', '.join(cfg) + "))]\n")

    version = run([llconfig, '--version']).strip()

    # llvm_initialization static library
    f.write("#[link(name = \"llvm_initialization\")]\n")

    # LLVM libs
    if version < '3.5':
      args = [llconfig, '--libs']
    else:
      args = [llconfig, '--libs', '--system-libs']
    args.extend(components)
    out = run(args)
    for lib in out.strip().replace("\n", ' ').split(' '):
        lib = lib.strip()[2:] # chop of the leading '-l'
        f.write("#[link(name = \"" + lib + "\"")
        f.write(")]\n")

    # llvm-config before 3.5 didn't have a system-libs flag
    if version < '3.5':
      if os == 'win32':
        f.write("#[link(name = \"imagehlp\")]")

    # LLVM ldflags
    out = run([llconfig, '--ldflags'])
    ldflags = ""
    for lib in out.strip().split(' '):
        if lib[:2] == "-l":
            f.write("#[link(name = \"" + lib[2:] + "\")]\n")
        else:
            ldflags += lib

    # C++ runtime library
    out = run([llconfig, '--cxxflags'])
    if 'stdlib=libc++' in out:
        f.write("#[link(name = \"c++\")]\n")
    else:
        f.write("#[link(name = \"stdc++\")]\n")

    # Attach everything to an extern block
    f.write("extern {}\n")
```

It is quite straightforward: it determines a list of LLVM libraries
using `llconfig` tool and makes our auto generated file force linkage
with them. Also it adds our `llvm_initialization` library.

Build script itself looks like this:

```rust
use std::io::Command;
use std::os;

fn main() {
    let out_dir = os::getenv("OUT_DIR").unwrap();

    Command::new("python2").arg("src/etc/mklldeps.py")
        .arg("src/missing_llvm_bindings/llvmdeps.rs")
        .arg(format!("{}", out_dir))
        .arg("jit")
        .arg("llvm-config")
        .status()
        .unwrap();

    Command::new("make")
        .arg(format!("OUT_DIR={}", out_dir))
        .status()
        .unwrap();

    println!("cargo:rustc-flags=-L {} ", out_dir);
}
```

It generates file with information about what libraries link to,
generates a static initialization library and instructs cargo to add
the directory with this library to the link path.

To call this build script we need to add `build = "build.rs"` line to
`Cargo.toml` file.

LLVM bindings themselves will be also quite straightforward:

```rust
#![allow(non_camel_case_types)]

use libc::{c_char, c_uint};

use rustc::lib::llvm::{ExecutionEngineRef, ModuleRef, TargetDataRef, TypeRef, ValueRef};

pub enum GenericValue_opaque {}
pub type GenericValueRef = *mut GenericValue_opaque;

extern {
    pub fn llvm_initialize_native_target();

    pub fn LLVMCreateExecutionEngineForModule(OutEE: *mut ExecutionEngineRef,
                                              M: ModuleRef,
                                              OutError: *mut *const c_char);

    pub fn LLVMDisposeExecutionEngine(EE: ExecutionEngineRef);

    pub fn LLVMGetExecutionEngineTargetData(EE: ExecutionEngineRef)
                                            -> TargetDataRef;

    pub fn LLVMCopyStringRepOfTargetData(TD: TargetDataRef)
                                         -> *const c_char;

    pub fn LLVMDisposeMessage(Message: *const c_char);

    pub fn LLVMRunFunction(EE: ExecutionEngineRef,
                           F: ValueRef,
                           NumArgs: c_uint,
                           Args: *const GenericValueRef)
                           -> GenericValueRef;

    pub fn LLVMGenericValueToFloat(TyRef: TypeRef,
                                   GenVal: GenericValueRef)
                                   -> f64;
}

mod llvmdeps;
```

`mod llvmdeps` is our auto generated file that forces the necessary
linking. Functions described in these bindings we will need for JIT
compilation and optimization. Their description can be found in the C
LLVM interface.

Now, when we have all necessary bindings, we can proceed with JIT compilation.

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
