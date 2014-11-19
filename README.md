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
  * [Parser implementation: introduction](#parser-implementation-introduction)
  * [Top level parse function](#top-level-parse-function)
  * [Helper macros for work with tokens](#helper-macros-for-work-with-tokens)
  * [Parsing of statements and top level expressions](#parsing-of-statements-and-top-level-expressions)
  * [Parsing of primary expressions](#parsing-of-primary-expressions)
  * [Parsing of binary expressions](#parsing-of-binary-expressions)
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
Project will have two crates: library and binary. All real functionality will be implemented in the library, and the binary will just
parse command line arguments and invoke the driver.

[Cargo.toml file](https://github.com/jauhien/iron-kaleidoscope/blob/master/Cargo.toml) is quite straightforward.
The [docopt](https://github.com/docopt/docopt.rs) is added because we'll need to parse simple command line arguments.

Rust at the moment does not have full LLVM bindings, so we'll need to add some functions. That addition is handled by a combination
of [scripts called during compilation](http://doc.crates.io/build-script.html) and some rust code that can be found in `src/missing_llvm_bindings` directory.

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

## LLVM IR code generation

## JIT and optimizer support

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
