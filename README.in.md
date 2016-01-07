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
* [Chapter 4. Extending Kaleidoscope: control flow](#chapter-4-extending-kaleidoscope-control-flow)
  * [If/Then/Else](#ifthenelse)
    * [Lexer and parser changes for if/then/else](#lexer-and-parser-changes-for-ifthenelse)
    * [IR generation for if/then/else](#ir-generation-for-ifthenelse)
  * ['For' loop](#for-loop)
    * [Lexer and parser changes for the 'for' loop](#lexer-and-parser-changes-for-the-for-loop)
    * [IR generation for the 'for' loop](#ir-generation-for-the-for-loop)
* [Chapter 5. Extending Kaleidoscope: user-defined operators](#chapter-5-extending-kaleidoscope-user-defined-operators)
  * [User-defined binary operators](#user-defined-binary-operators)
  * [User-defined unary operators](#user-defined-unary-operators)
  * [Painting the Mandelbrot set](#painting-the-mandelbrot-set)
* [Chapter 6. Extending Kaleidoscope: mutable variables](#chapter-6-extending-kaleidoscope-mutable-variables)
  * [Mutable variables in Kaleidoscope](#mutable-variables-in-kaleidoscope)
  * [Adjusting variables for mutation](#adjusting-variables-for-mutation)
  * [Assignmnet operator](#assignmnet-operator)
  * [User-defined local variables](#user-defined-local-variables)

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
similar to Rust. Function body is just an expression, its value is returned. No explicit return operator is used.

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
<<<src/lexer.rs:lexer-tokens>>>
```

Note, that to use enumeration members without scopes as we later do, you need to
add some uses at the beginning of your module (it is needed since changing
enums to be scoped in Rust):

```rust
<<<src/lexer.rs:lexer-tokens-use>>>
```

We do not mention those uses explicitly in the following.

Our parser function will accept a string with input characters and produce a vector of tokens. It will look like this:

```rust
<<<src/lexer.rs:lexer-tokenize>>>
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
<<<grammar.ebnf:parser-grammar>>>
```

### The Abstract Syntax Tree (AST)

Now we'll create data types corresponding to every item in the Kaleidoscope grammar.

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-program>>>
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
<<<src/parser.rs:parser-astnode>>>
```

`ExternNode` corresponds to the	`declaration` item in the grammar and `FunctionNode` corresponds to
the `definition` item.

We define `Prototype` and `Function` according to the grammar:

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-defproto>>>
```

```rust
<<<src/parser.rs:parser-defproto>>>
```

Functions are typed only by the number of arguments, as the onliest type
in the Kaleidoscope language is an `f64` number.

The only thing left to define is the data type that corresponds to the `expression` item.
This one is the most complicated and difficult to parse, as it includes binary expressions
with operator precedence.

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-expr>>>
```

`Expression` data type will be an `enum` with entries corresponding to every
possible expression type:

```rust
<<<src/parser.rs:parser-expr>>>
```

`LiteralExpr` is a number (`Number` token). `VariableExpr` is a variable name (`Ident` token).
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
<<<src/parser.rs:parser-result>>>
```

The function prototype for the parsing function looks like this:

```rust
<<<src/parser.rs:parser-parse-sign>>>
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
<<<src/parser.rs:parser-part-result>>>
```

We will need a helper function for error generation:

```rust
<<<src/parser.rs:parser-error>>>
```

This function and data type are generic as we will need to return objects of different types depending on what we are parsing (prototype, expression, etc.)
Top level parsing functions will return `ASTNode` which can be directly inserted into `Vec<ASTNode>` that represents the AST.

We can implement first production rules in the topmost parsing function now:

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-top>>>
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
<<<src/parser.rs:parser-parse>>>
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
<<<src/parser.rs:parser-parse-try>>>
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
<<<src/parser.rs:parser-expect-token>>>
```

This macro automatically handles inserting tokens into the parsed tokens vector and returning of `NotComplete` (together with
inserting of tokens back into the input vector) or error in the appropriate cases.

### Parsing of statements and top level expressions

We have two kinds of statements in the Kaleidoscope language: function
declarations and function definitions:

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-statements>>>
```

Let's start from the easier one: function declarations. The function is really straightforward:

```rust
<<<src/parser.rs:parser-parse-extern>>>
```

We eat `Extern` token and parse the function prototype. That's all.

Function definition is not very complicated also:

```rust
<<<src/parser.rs:parser-parse-function>>>
```

Again, we eat `Def` token, we parse prototype and function body. That's all.

So far we just called another parsing functions and matched some tokens (like `Def` or `Extern`).
That's time for some real parsing now.

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-proto>>>
```

```rust
<<<src/parser.rs:parser-parse-prototype>>>
```

Function prototype starts with the function name. Then opening parenthesis goes.
After opening parenthesis we have a list of function parameters (`Ident` tokens).
Note, that we ignore `Comma` tokens, so they are equivalent to whitspaces.
If we find a closing parenthesis, prototype
parsing is done. If we find any other token we emit an error.

The only top level item still left are top level expressions. To make
further work with them easier, we close them in an anonymous function.

```rust
<<<src/parser.rs:parser-parse-expression>>>
```

Now, when we have code that handles parsing of all top level items, we can proceed
with parsing of the main part of Kaleidoscope language -- expressions.

### Parsing of primary expressions

We will start from easier topic: parsing of primary expressions. Then we will use primary
expressions parsing functions to parse operands of binary operators.

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-primary>>>
```

```rust
<<<src/parser.rs:parser-parse-primary-expr>>>
```

To start parsing of a primary expression we just look at the next token and
decide which kind of expression we are working with. Such a look ahead one or more tokens
is a common idea in parsers, we already have seen it before in the top level parse function.

We start with parsing of identifier and call expressions. We use the same parsing function
for them, as they both start from the `Ident` token.

```rust
<<<src/parser.rs:parser-parse-ident-expr>>>
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
<<<src/parser.rs:parser-parse-literal-expr>>>
```

Parenthesis expressions are also easy to parse:

```rust
<<<src/parser.rs:parser-parse-parenthesis-expr>>>
```

Now, when we can parse primary expressions, it is the time for more complicated ones.

### Parsing of binary expressions

Our grammar for expressions looked like this:

```{.ebnf .notation}
<<<grammar.ebnf:parser-grammar-binary>>>
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
<<<src/parser.rs:parser-settings>>>
```

Let's create a function that fills this map with some operators:

```rust
<<<src/parser.rs:parser-default-settings>>>
```

A binary expression is a primary expression followed by zero or more `(operator, primary expression)` pairs.
The expression parsing function looks like this:

```rust
<<<src/parser.rs:parser-parse-expr>>>
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
<<<src/parser.rs:parser-parse-binary-expr>>>
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
<<<src/main.rs:parser-main>>>
```

The `Stage` enum is defined in the driver:

```rust
<<<src/driver.rs:parser-stage>>>
```

Driver itself looks like this:

```rust
<<<src/driver.rs:parser-driver>>>
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
<<<src/lib.rs:ir-import>>>
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
<<<src/builder.rs:ir-context>>>
```

For code genertion we will use one module. But we'll create for it
a trait `ModuleProvider` as it will simplify adding a JIT-compiler later (not showing uses this time):

```rust
<<<src/builder.rs:ir-module-provider>>>
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
<<<src/builder.rs:ir-builder-trait>>>
```

We return a pair here with second element showing if the value we generated was an anonymous function.
This will be useful later when we'll want to execute such functions (as you probably remember
we've packed top-level expressions into anonymous functions).

### Top level code generation

Let's implement `IRBuilder` trait for top level data structures. For
`ParsingResults`, AST tree and `ASTNode` it is really trivial:

```rust
<<<src/builder.rs:ir-top-level>>>
```

We just call `codegen` function for underlying elements and return its results.
For AST tree (in our case, rather AST list) we return value of the
last AST node.

Prototypes and functions are more complicated, as we need to do some
real work there including handling of named function parameters.

Code generation for prototypes looks like this:

```rust
<<<src/builder.rs:ir-prototype>>>
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
<<<src/builder.rs:ir-function>>>
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
<<<src/builder.rs:ir-expression>>>
```

Let's have a look at match branches one by one.

For `Literal` expression we just return a real constant with the
appropriate value:

```rust
<<<src/builder.rs:ir-literal>>>
```

For variables we look in the `named_values` map and if there is such a
variable there (a value that corresponds to the function argument),
we return the value or emit an error otherwise:

```rust
<<<src/builder.rs:ir-variable>>>
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
<<<src/builder.rs:ir-binary>>>
```

For call code generation we do function name lookup in the LLVM
Module's symbol table first. Then we compare the number of arguments in
call AST node and in the function defined in the LLVM Module. After it
we generate a value for every argument and create the arguments
vector.

```rust
<<<src/builder.rs:ir-call>>>
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
<<<src/builder.rs:jit-fpm>>>
```

We create module and function pass manager associated with it. Then we
add a series of passes. The first one is analysis the other four are
transformation. These passes should reasonably cleanup and reorganize
generated IR.

We'll store function pass manager togerther with module in our `SimpleModuleProvider`
(also we'll change the `ModuleProvider` trait):

```rust
<<<src/builder.rs:jit-mp>>>
```

Now we run our passes on every created function before return it:

```rust
<<<src/builder.rs:jit-run-passes>>>
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
<<<src/jitter.rs:jit-jitter>>>
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
<<<src/jitter.rs:jit-mc>>>
```

Now we will define a method for looking for function addresses in already
compiled modules (if nothing found we are returning zero):

```rust
<<<src/jitter.rs:jit-mc-address>>>
```

JIT-compiler itself will contain current open module, associated function pass manager
and container with already compiled stuff:

```rust
<<<src/jitter.rs:jit-mcjitter>>>
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
<<<src/jitter.rs:jit-mcjitter-ctor>>>
```

The method for closing current module is where the magic of execution engine creation happens:

```rust
<<<src/jitter.rs:jit-mcjitter-close-module>>>
```

We create new module and pass manager first. Note the use of `std::mem::replace`,
so we can move only from one record field.

```rust
<<<src/jitter.rs:jit-mcjitter-new-module>>>
```

Then we create a custom memory manager for our execution engine. The symbol resolution
is just a closure that owns a reference to our modules container. It asks default memory
manager first, than it looks in already compiled modules:

```rust
<<<src/jitter.rs:jit-mcjitter-mm>>>
```

Now we can create the execution engine for the current module:

```rust
<<<src/jitter.rs:jit-mcjitter-ee>>>
```

`create` method returns execution engine and a frozen module.

Finally we update our modules and execution engines container:

```rust
<<<src/jitter.rs:jit-mcjitter-container-update>>>
```

Now, as we have a method for execution engine creation on module closing
and know how will our module organization look like, we can proceed with
implementation of `ModuleProvider` and `JITter` traits.

`dump`, `get_module` and `get_pass_manager` are really simple:

```rust
<<<src/jitter.rs:jit-mcjitter-mp-simple>>>
```

Note, that we dump already compiled modules first and then our currently open module.


`get_function` is a little bit more complicated as we need to look in already compiled modules first.
If we find already declared/defined function in one of the old modules, we look
for a prototype in the current module. If we find not a prototype, but a definition (and
we have one definition already), we have
some error, as it should not be possible to redefine functions across modules. If prototype
exists, we can return it. Our symbol resolution code will handle linking correctly.

If no prototype in the current module is available, we need to create one.
To create it we need the function type. Note, that `get_type` on function returns
type `() -> F`, where `F` is our real function type, so we need to call `get_return_type`
on the returned value. Also these methods are not especially friendly in my current bindings,
so we need to change reference type by hand (I hope to fix this).

If we've found or created
only prototypes (in old module in the current one) we continue loop, as we want to find the
definition if it exists.

If no function in previous modules is found, we look in the current one as we did in previous
chapter (in `SimpleModuleProvider`). It also will handle the case when only prototypes were
found, as one will be created in the current module.

Code for `get_function` follows:

```rust
<<<src/jitter.rs:jit-mcjitter-mp-gf>>>
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
<<<src/jitter.rs:jit-mcjitter-jitter>>>
```

### Changes in the driver and 'built-in' functions

We will make our driver work both with `SimpleModuleProvider` (when only generating IR)
and with `MCJITter` (when having REPL with jit-compiling). To make it easier,
we will implement `JITter` trait for `SimpleModuleProvider`:

```rust
<<<src/jitter.rs:jit-smp>>>
```

Now we will use an appropriate `ir-container` in the driver and depending on the
stage and type of expression that user has entered we will dump IR or compile and execute it.
Also we need to initialize native target for JIT-compiler.

```rust
<<<src/driver.rs:ch-3>>>
```

You can see also call of the `init` function that will be explained in a moment, just ignore
it for a while.

Now we modify the main function appropriately, so it calls the main loop
for the `Exec` stage by default.

```rust
<<<src/main.rs:ch-3>>>
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
<<<src/jitter.rs:jit-builtin>>>
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

Full code for this chapter is available
[here](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/3).

## Chapter 4. Extending Kaleidoscope: control flow

We have created a general framework for simple REPL based language implementation.
It's time to add some Turing completeness to our language now.

### If/Then/Else

Let's see how easy it is to extend our implementation. The first thing that
we'll add is conditional branching. If/then/else construct will be
an expression (like everything in the Kaleidoscope language) with value
corresponding to the taken branch. We will consider `0.0` as `false` and
any other value as `true`.

What we finally want to have is something like

```
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2);
```

We will evaluate only one branch (this is important, as we can have side effects in our code).

#### Lexer and parser changes for /if/then/else

Let's start from formal grammar definition (only the relevant part of the grammar is shown):

```{.ebnf .notation}
<<<grammar.ebnf:if-grammar>>>
```

where `If`, `Then`, `Else` are new tokens that we're going to add to the lexer:

```rust
<<<src/lexer.rs:if-lexer>>>
```

Lexer extension is completely staightforward, parser is not much more complicated:

```rust
<<<src/parser.rs:if-parser>>>
```

First we extend our AST. Then we extend primary expression parsing
with the call to conditional expression
parsing if we see the `If` token. In this newly added function
we parse the condition, look for `Then` token, parse 'then' branch, look for
`Else` token and parse 'else' branch.

That's all, we have AST for if/then/else generated.

#### IR generation for if/then/else

Parser changes introduced no new concepts. Now the interesting part of implementation starts.
Control flow for our if/then/else construct will look something like this:

```
        A
        ^
       / \
      /   \
     B     C
     \     /
      \   /
       \ /
        V
        D
```

Execution goes in the top-down direction. Depending on the value calculated in the
basic block `A` we execute either basic block `B` or `C`. In the basic block `D` we assign
value to the result of the if/then/else expression. This value is one of values calculated in
`B` or `C`.

We have code in [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form) form.
To construct a single variable that can be assigned one or other value we need to use the
phi-operation (read the wikipedia article if you do not know what is it). Our case is really
simple as we know exactly how the control flows and what value corresponds to which incoming branch.
Because of this we're going to build the phi-operation by hand here. In the future when we'll define
mutable variables, we will use some kind of available LLVM magic to generate SSA automatically
as the case of mutable variables is more complicated.

So far as the result of IR code generation for if/then/else we want to have something that looks like this:

```
<=== Kaleidoscope input ===>

extern foo();
extern bar();
def baz(x) if x then foo() else bar();


<=========== IR ===========>

declare double @foo()

declare double @bar()

define double @baz(double %x) {
entry:
  %ifcond = fcmp one double %x, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:       ; preds = %entry
  %calltmp = call double @foo()
  br label %ifcont

else:       ; preds = %entry
  %calltmp1 = call double @bar()
  br label %ifcont

ifcont:     ; preds = %else, %then
  %iftmp = phi double [ %calltmp, %then ], [ %calltmp1, %else ]
  ret double %iftmp
}

```

The IR does this:

* **entry**: evaluate the condition and compare it with zero. Branch to
             appropriate basic block depending on the evaluated value.
* **then**: evaluate `then` branch and branch to `ifcont`.
* **else**: evaluate `else` branch and branch to `ifcont`.
* **ifcont**: build the phi operation that assigns resulting value depending
              on the branch from which it received control. Return the result.

You see what do we want, let's add the necessary part to our IR builder:

```rust
<<<src/builder.rs:if-builder>>>
```

Quite straightforward implementation of the described algorithm. Let's look at it line by line.

```rust
<<<src/builder.rs:if-builder-cond>>>
```

Here we evaluate condition and compare it with zero.

```rust
<<<src/builder.rs:if-builder-br>>>
```

Generate a bunch of basic blocks and conditionally branch to `then` or `else` one.

```rust
<<<src/builder.rs:if-builder-then-else>>>
```

Position at the branch block, evaluate its value and branch to the `merge` basic block.
Note, that we remember the end block of `then` and `else` branches as it can be different
from their starting blocks.

```rust
<<<src/builder.rs:if-builder-merge>>>
```

Build phi operation and add incoming values to it. Return its value as the result.
Unsafe block is here due to slight inconsistency of my bindings (they are going to be fixed).

That's all. Let's try if our Fibonacci function works:

```
> def fib(x)
.   if x < 3 then
.     1
.   else
.     fib(x-1)+fib(x-2);

define double @fib(double %x) {
entry:
  %cmptmp = fcmp olt double %x, 3.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  %subtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp1 = fsub double %x, 2.000000e+00
  %calltmp2 = call double @fib(double %subtmp1)
  %addtmp = fadd double %calltmp, %calltmp2
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %ifphi = phi double [ 1.000000e+00, %then ], [ %addtmp, %else ]
  ret double %ifphi
}

> fib(15)
=> 610
> .quit
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

define double @fib(double %x) #0 {
entry:
  %cmptmp = fcmp olt double %x, 3.000000e+00
  %booltmp = uitofp i1 %cmptmp to double
  %ifcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  %subtmp = fsub double %x, 1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp1 = fsub double %x, 2.000000e+00
  %calltmp2 = call double @fib(double %subtmp1)
  %addtmp = fadd double %calltmp, %calltmp2
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %ifphi = phi double [ 1.000000e+00, %then ], [ %addtmp, %else ]
  ret double %ifphi
}

define double @0() #0 {
entry:
  %calltmp = call double @fib(double 1.500000e+01)
  ret double %calltmp
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
```

### 'For' loop

Let's add a loop construct to our language. This will allow us to e.g
output a line from stars (it was possible already with recursion,
but some people think loops are easier):

```
extern putchard(char)
def printstar(n)
  for i = 1, i < n, 1.0 in
    putchard(42);  # ascii 42 = '*'

# print 100 '*' characters
printstar(100);
```

Our loop defines new variable (`i` in this case) that iterates
from starting value (1) while condition (`i < n`) is true. The variable
is incremented by an optional step (`1.0`). If it is not supplied, it defaults to 1.
We return zero as the result of the whole loop. When we'll add mutable variables there
will be a possibility to write code that computes some values using loops.

#### Lexer and parser changes for the 'for' loop

Let's start from the grammar again:

```{.ebnf .notation}
<<<grammar.ebnf:for-grammar>>>
```

We are going to add `For` and `In` tokens to our parser. `Op=` means operator token
with value `=`.

```rust
<<<src/lexer.rs:for-lexer>>>
```

Parser is also quite simple with one thing to note about handling of the optional
part of the grammar:

```rust
<<<src/parser.rs:for-parser>>>
```

If we do not encounter `Comma` that should go before the optional step value,
we return its default value (`1.0`) and continue parsing of the `for` loop expression.
Other parsing code is completely straightforward and well-known from the previous
chapters. Due to already implemented macros it corresponds to our grammar closely and
contains nearly no boilerplate.

#### IR generation for the 'for' loop

IR generated for our short loop example without optimizations should look like

```
declare double @putchard(double)

define double @printstar(double %n) {
entry:
  ; initial value = 1.0 (inlined into phi)
  br label %loop

loop:       ; preds = %loop, %entry
  %i = phi double [ 1.000000e+00, %entry ], [ %nextvar, %loop ]
  ; body
  %calltmp = call double @putchard(double 4.200000e+01)
  ; increment
  %nextvar = fadd double %i, 1.000000e+00

  ; termination test
  %cmptmp = fcmp ult double %i, %n
  %booltmp = uitofp i1 %cmptmp to double
  %loopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:      ; preds = %loop
  ; loop always returns 0.0
  ret double 0.000000e+00
}
```

All the concepts that we see here are familiar (phi expression, branches, basic blocks etc).

We will generate similar code.

```rust
<<<src/builder.rs:for-builder>>>
```

This code should contain nothing new (familiar phi, branches and other machinery) apart
from manipulations with `named_values` table. We create local loop variable and add it to
the table. If there existed any variable with the same name, we hide it and remember the old value.
On exit from loop we restore this all value back. From other interesting things, note, that we
are adding incoming values to our phi node as soon as we have them. Also note, that we
add the value for branch incoming from the end block of the loop body (similar as for
conditional expression loop body can have a number of basic blocks).

Let's see how do our loops work:

```
> extern putchard(char)

declare double @putchard(double)

> def printstar(n)
.   for i = 1, i < n, 1.0 in
.     putchard(42);

define double @printstar(double %n) {
entry:
  br label %preloop

preloop:                                          ; preds = %loop, %entry
  %i = phi double [ 1.000000e+00, %entry ], [ %nextvar, %loop ]
  %cmptmp = fcmp olt double %i, %n
  %booltmp = uitofp i1 %cmptmp to double
  %loopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %preloop
  ret double 0.000000e+00

loop:                                             ; preds = %preloop
  %calltmp = call double @putchard(double 4.200000e+01)
  %nextvar = fadd double %i, 1.000000e+00
  br label %preloop
}

> printstar(100);
***************************************************************************************************=> 0
> .quit
; ModuleID = 'main'
target datalayout = "e-m:e-p:32:32-f64:32:64-f80:32-n8:16:32-S128"

declare double @putchard(double) #0

define double @printstar(double %n) #0 {
entry:
  br label %preloop

preloop:                                          ; preds = %loop, %entry
  %i = phi double [ 1.000000e+00, %entry ], [ %nextvar, %loop ]
  %cmptmp = fcmp olt double %i, %n
  %booltmp = uitofp i1 %cmptmp to double
  %loopcond = fcmp one double %booltmp, 0.000000e+00
  br i1 %loopcond, label %loop, label %afterloop

afterloop:                                        ; preds = %preloop
  ret double 0.000000e+00

loop:                                             ; preds = %preloop
  %calltmp = call double @putchard(double 4.200000e+01)
  %nextvar = fadd double %i, 1.000000e+00
  br label %preloop
}

define double @0() #0 {
entry:
  %calltmp = call double @printstar(double 1.000000e+02)
  ret double %calltmp
}

attributes #0 = { "no-frame-pointer-elim"="false" }
; ModuleID = 'main'
```

Full code for this chapter is as always
[available](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/4).

## Chapter 5. Extending Kaleidoscope: user-defined operators

Our language is quite full at this moment, but we still lack some really important things.
E.g. we have no division, logical negation, operation sequencing etc.
We could implement necessary operators at the compiler level, but it is boring
and shows us no new ideas. We choose to add the possibility to implement user-defined
binary and unary operators instead and then implement all the necessary stuff
in the Kaleidoscope language itself. At the end of this chapter we will be able
to render the [Mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set)
with some simple Kaleidoscope functions.

We're going to implement a mechanism for user-defined operators that is more general than
e.g. one found in C++. We will be able to define completely new operators with their
precedences. This is possible because we have easy to extend hand-written parser. One
of the key features that makes this extension easy is using of operator precedence parsing
for binary expressions. This allows us to dynamically change grammar during execution.

Here are some examples of the features (user-defined unary and binary operators) we're going
to implement:

```
# Logical unary not.
def unary!(v)
  if v then
    0
  else
    1;

# Define > with the same precedence as <.
def binary> 10 (LHS RHS)
  RHS < LHS;

# Binary "logical or", (note that it does not "short circuit")
def binary| 5 (LHS RHS)
  if LHS then
    1
  else if RHS then
    1
  else
    0;

# Define = with slightly lower precedence than relationals.
def binary= 9 (LHS RHS)
  !(LHS < RHS | LHS > RHS);
```

You see that at the end of this chapter we'll be able to implement a fair
amount of things that can be considered significant part of the language itself.

### User-defined binary operators

We need to change our grammar for prototypes to reflect the possibility to
have binary operators definitions:

```{.ebnf .notation}
<<<grammar.ebnf:binary-grammar>>>
```

Note that we do not change grammar for expressions.
Anyway we do not use it to parse expressions (as you remember that grammar
correctly generates binary expressions from the point of view of syntax,
but doesn't express their semantics).

Implementation as usually starts with changing the lexer. We only add the `Binary`
keyword there, so I'll not even show this change as it is completely trivial.
Also note, that we do not need to change lexer to tokenize user-defined operator
usage, as it already treats any unknown ASCII character as an operator.

Changes in the parser are much more interesting.
`BinaryExpr` node of AST already contains string that represents operator
as the corresponding ASCII character, so here we need no changes.

Where the changes start is function definition. We want to be able to
parse definitions starting from something like `def binary| 5`.
Let's extend our function AST nodes with additional information
that shows that this given function is an operator. To this aim let's
add a function type field to the prototype:

```rust
<<<src/parser.rs:binary-proto>>>
```

For normal functions we hold no additional information, for binary
operators we store operator's name and precedence.

In a moment we'll change code for prototype parsing, but let's see
first how we'll change code for function parsing if prototype
parsing already works:

```rust
<<<src/parser.rs:ops-parse-func>>>
```

The only thing we've added here is a match on the function type.
If we are dealing with a binary operator, we add it to the operator
precedence table. Because we use operator precedence parsing for binary
expressions that's all we need to be able to dynamically change grammar.
Note, that we change the operator precedence table before
function body is parsed. In this way we allow recursion in binary
operators.

Now the changes for prototype parsing come:

```rust
<<<src/parser.rs:binary-parse-proto>>>
```

Here we literally implement changes in our grammar and make
parsing code return function type in addition to its name.
Precedence value is optional and defaults to 30 if not supplied.
Also we ensure that it is in the `[1..100]` interval.
We name our function `binary@` where `@` is the operator character.
That's ok as LLVM allows any characters in function names.

At the end we check that exactly two arguments were declared for
binary operators. That's all with parsing, let's switch to IR generation
which is even simpler.

```rust
<<<src/builder.rs:binary-builder>>>
```

We just change here the default branch of the match on the operator name the
way that it doesn't generate error, but looks for binary operator function
declaration. If it finds one, it generates call, otherwise it returns error.

That's all with binary operators. We can define our own items
that play with other parts of expression just like language native ones.

### User-defined unary operators

We had a framework necessary for user-defined binary operators already implemented
in previous chapters. For unary operators we need to add some more pieces. But let's start
from the grammar:

```{.ebnf .notation}
<<<grammar.ebnf:unary-grammar>>>
```

We add new type of prototype and new primary expression.

Implementing starts from the lexer where we add `Unary` token.

Then we add new AST node to the parser:

```rust
<<<src/parser.rs:unary-ast>>>
```

and new function type:

```rust
<<<src/parser.rs:unary-ftype>>>
```

Function parsing stays the same. Prototype parsing changes according to the grammar:

```rust
<<<src/parser.rs:unary-parse-proto>>>
```

We name functions for unary operators `unary@` similar to the binary case
and ensure that they accept exactly one argument.

Then we add parsing of unary expressions:

```rust
<<<src/parser.rs:unary-parse-expr>>>
```

IR building for unary expressions is also simple (just a call
to appropriate function):

```rust
<<<src/builder.rs:unary-builder>>>
```

### Painting the Mandelbrot set

So we have a really powerful language now. Let's start to program great things with it.

First a bunch of primitive operations:

```
> def unary!(v)
.   if v then
.     0
.   else
.     1;

define double @"unary!"(double %v) {
entry:
  %ifcond = fcmp ueq double %v, 0.000000e+00
  %. = select i1 %ifcond, double 1.000000e+00, double 0.000000e+00
  ret double %.
}

> def unary-(v)
.   0-v;

define double @unary-(double %v) {
entry:
  %subtmp = fsub double 0.000000e+00, %v
  ret double %subtmp
}

> def binary> 10 (LHS RHS)
.   RHS < LHS;

define double @"binary>"(double %LHS, double %RHS) {
entry:
  %cmptmp = fcmp olt double %RHS, %LHS
  %booltmp = uitofp i1 %cmptmp to double
  ret double %booltmp
}

> def binary| 5 (LHS RHS)
.   if LHS then
.     1
.   else if RHS then
.     1
.   else
.     0;

define double @"binary|"(double %LHS, double %RHS) {
entry:
  %ifcond = fcmp ueq double %LHS, 0.000000e+00
  %ifcond1 = fcmp ueq double %RHS, 0.000000e+00
  %. = select i1 %ifcond1, double 0.000000e+00, double 1.000000e+00
  %ifphi5 = select i1 %ifcond, double %., double 1.000000e+00
  ret double %ifphi5
}

> def binary& 6 (LHS RHS)
.   if !LHS then
.     0
.   else
.     !!RHS;

define double @"binary&"(double %LHS, double %RHS) {
entry:
  %unop = call double @"unary!"(double %LHS)
  %ifcond = fcmp ueq double %unop, 0.000000e+00
  br i1 %ifcond, label %else, label %ifcont

else:                                             ; preds = %entry
  %unop1 = call double @"unary!"(double %RHS)
  %unop2 = call double @"unary!"(double %unop1)
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %ifphi = phi double [ %unop2, %else ], [ 0.000000e+00, %entry ]
  ret double %ifphi
}

> def binary = 9 (LHS RHS)
.   !(LHS < RHS | LHS > RHS);

define double @"binary="(double %LHS, double %RHS) {
entry:
  %cmptmp = fcmp olt double %LHS, %RHS
  %booltmp = uitofp i1 %cmptmp to double
  %binop = call double @"binary>"(double %LHS, double %RHS)
  %binop1 = call double @"binary|"(double %booltmp, double %binop)
  %unop = call double @"unary!"(double %binop1)
  ret double %unop
}

> def binary : 1 (x y) y;

define double @"binary:"(double %x, double %y) {
entry:
  ret double %y
}
```

The last one (`:`) is a simple precedence operator:

```
> extern printd(x);

declare double @printd(double)

> printd(123) : printd(456) : printd(789);
> 123 <
> 456 <
> 789 <
=> 789
```

Now we can define some funny I/O stuff. E.g. this function
prints out a character whose “density” reflects the value passed in:

```
> extern putchard(char)

declare double @putchard(double)

> def printdensity(d)
.   if d > 8 then
.     putchard(32)  # ' '
.   else if d > 4 then
.     putchard(46)  # '.'
.   else if d > 2 then
.     putchard(43)  # '+'
.   else
.     putchard(42); # '*'

define double @printdensity(double %d) {
entry:
  %binop = call double @"binary>"(double %d, double 8.000000e+00)
  %ifcond = fcmp ueq double %binop, 0.000000e+00
  br i1 %ifcond, label %else, label %then

then:                                             ; preds = %entry
  %calltmp = call double @putchard(double 3.200000e+01)
  br label %ifcont

else:                                             ; preds = %entry
  %binop1 = call double @"binary>"(double %d, double 4.000000e+00)
  %ifcond2 = fcmp ueq double %binop1, 0.000000e+00
  br i1 %ifcond2, label %else4, label %then3

ifcont:                                           ; preds = %then9, %else10, %then3, %then
  %ifphi15 = phi double [ %calltmp, %then ], [ %calltmp6, %then3 ], [ %calltmp12, %then9 ], [ %calltmp13, %else10 ]
  ret double %ifphi15

then3:                                            ; preds = %else
  %calltmp6 = call double @putchard(double 4.600000e+01)
  br label %ifcont

else4:                                            ; preds = %else
  %binop7 = call double @"binary>"(double %d, double 2.000000e+00)
  %ifcond8 = fcmp ueq double %binop7, 0.000000e+00
  br i1 %ifcond8, label %else10, label %then9

then9:                                            ; preds = %else4
  %calltmp12 = call double @putchard(double 4.300000e+01)
  br label %ifcont

else10:                                           ; preds = %else4
  %calltmp13 = call double @putchard(double 4.200000e+01)
  br label %ifcont
}

> printdensity(1): printdensity(2): printdensity(3):
. printdensity(4): printdensity(5): printdensity(9):
. putchard(10);
**++. 
=> 10
```

Now we can do something more complex. Let's define a function
that calculates the number of iterations that it takes for a complex orbit to escape,
saturating to 255 (see [Mandelbrot set](https://en.wikipedia.org/wiki/Mandelbrot_set)).

```
> def mandleconverger(real imag iters creal cimag)
.   if iters > 255 | (real*real + imag*imag > 4) then
.     iters
.   else
.     mandleconverger(real*real - imag*imag + creal,
.                     2*real*imag + cimag,
.                     iters+1, creal, cimag);

define double @mandleconverger(double %real, double %imag, double %iters, double %creal, double %cimag) {
entry:
  %binop = call double @"binary>"(double %iters, double 2.550000e+02)
  %multmp = fmul double %real, %real
  %multmp1 = fmul double %imag, %imag
  %addtmp = fadd double %multmp, %multmp1
  %binop2 = call double @"binary>"(double %addtmp, double 4.000000e+00)
  %binop3 = call double @"binary|"(double %binop, double %binop2)
  %ifcond = fcmp ueq double %binop3, 0.000000e+00
  br i1 %ifcond, label %else, label %ifcont

else:                                             ; preds = %entry
  %subtmp = fsub double %multmp, %multmp1
  %addtmp6 = fadd double %subtmp, %creal
  %multmp7 = fmul double %real, 2.000000e+00
  %multmp8 = fmul double %multmp7, %imag
  %addtmp9 = fadd double %multmp8, %cimag
  %addtmp10 = fadd double %iters, 1.000000e+00
  %calltmp = call double @mandleconverger(double %addtmp6, double %addtmp9, double %addtmp10, double %creal, double %cimag)
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %ifphi = phi double [ %calltmp, %else ], [ %iters, %entry ]
  ret double %ifphi
}

> def mandleconverge(real imag)
.   mandleconverger(real, imag, 0, real, imag);

define double @mandleconverge(double %real, double %imag) {
entry:
  %calltmp = call double @mandleconverger(double %real, double %imag, double 0.000000e+00, double %real, double %imag)
  ret double %calltmp
}
```

Yes, we work with complex numbers using our simple language. Now we are ready to paint some
nice pictures.

```
> def mandelhelp(xmin xmax xstep   ymin ymax ystep)
.   for y = ymin, y < ymax, ystep in (
.     (for x = xmin, x < xmax, xstep in
.        printdensity(mandleconverge(x,y)))
.     : putchard(10)
.   )

define double @mandelhelp(double %xmin, double %xmax, double %xstep, double %ymin, double %ymax, double %ystep) {
entry:
  br label %preloop

preloop:                                          ; preds = %afterloop5, %entry
  %y = phi double [ %ymin, %entry ], [ %nextvar9, %afterloop5 ]
  %cmptmp = fcmp olt double %y, %ymax
  br i1 %cmptmp, label %preloop1, label %afterloop

afterloop:                                        ; preds = %preloop
  ret double 0.000000e+00

preloop1:                                         ; preds = %preloop, %loop6
  %x = phi double [ %nextvar, %loop6 ], [ %xmin, %preloop ]
  %cmptmp2 = fcmp olt double %x, %xmax
  br i1 %cmptmp2, label %loop6, label %afterloop5

afterloop5:                                       ; preds = %preloop1
  %calltmp8 = call double @putchard(double 1.000000e+01)
  %binop = call double @"binary:"(double 0.000000e+00, double %calltmp8)
  %nextvar9 = fadd double %ystep, %y
  br label %preloop

loop6:                                            ; preds = %preloop1
  %calltmp = call double @mandleconverge(double %x, double %y)
  %calltmp7 = call double @printdensity(double %calltmp)
  %nextvar = fadd double %xstep, %x
  br label %preloop1
}

> def mandel(realstart imagstart realmag imagmag)
.   mandelhelp(realstart, realstart+realmag*78, realmag,
.              imagstart, imagstart+imagmag*40, imagmag);

define double @mandel(double %realstart, double %imagstart, double %realmag, double %imagmag) {
entry:
  %multmp = fmul double %realmag, 7.800000e+01
  %addtmp = fadd double %realstart, %multmp
  %multmp1 = fmul double %imagmag, 4.000000e+01
  %addtmp2 = fadd double %imagstart, %multmp1
  %calltmp = call double @mandelhelp(double %realstart, double %addtmp, double %realmag, double %imagstart, double %addtmp2, double %imagmag)
  ret double %calltmp
}

> mandel(-2.3, -1.3, 0.05, 0.07);
******************************************************************************
******************************************************************************
****************************************++++++********************************
************************************+++++...++++++****************************
*********************************++++++++.. ...+++++**************************
*******************************++++++++++..   ..+++++*************************
******************************++++++++++.     ..++++++************************
****************************+++++++++....      ..++++++***********************
**************************++++++++.......      .....++++**********************
*************************++++++++.   .            ... .++*********************
***********************++++++++...                     ++*********************
*********************+++++++++....                    .+++********************
******************+++..+++++....                      ..+++*******************
**************++++++. ..........                        +++*******************
***********++++++++..        ..                         .++*******************
*********++++++++++...                                 .++++******************
********++++++++++..                                   .++++******************
*******++++++.....                                    ..++++******************
*******+........                                     ...++++******************
*******+... ....                                     ...++++******************
*******+++++......                                    ..++++******************
*******++++++++++...                                   .++++******************
*********++++++++++...                                  ++++******************
**********+++++++++..        ..                        ..++*******************
*************++++++.. ..........                        +++*******************
******************+++...+++.....                      ..+++*******************
*********************+++++++++....                    ..++********************
***********************++++++++...                     +++********************
*************************+++++++..   .            ... .++*********************
**************************++++++++.......      ......+++**********************
****************************+++++++++....      ..++++++***********************
*****************************++++++++++..     ..++++++************************
*******************************++++++++++..  ...+++++*************************
*********************************++++++++.. ...+++++**************************
***********************************++++++....+++++****************************
***************************************++++++++*******************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
=> 0
> mandel(-2, -1, 0.02, 0.04);
******************************************************************++++++++++++
****************************************************************++++++++++++++
*************************************************************+++++++++++++++++
***********************************************************+++++++++++++++++++
********************************************************++++++++++++++++++++++
******************************************************++++++++++++++++++++++..
***************************************************+++++++++++++++++++++......
*************************************************++++++++++++++++++++.........
***********************************************+++++++++++++++++++...       ..
********************************************++++++++++++++++++++......        
******************************************++++++++++++++++++++.......         
***************************************+++++++++++++++++++++..........        
************************************++++++++++++++++++++++...........         
********************************++++++++++++++++++++++++.........             
***************************++++++++...........+++++..............             
*********************++++++++++++....  .........................              
***************+++++++++++++++++....   .........   ............               
***********+++++++++++++++++++++.....                   ......                
********+++++++++++++++++++++++.......                                        
******+++++++++++++++++++++++++........                                       
****+++++++++++++++++++++++++.......                                          
***+++++++++++++++++++++++.........                                           
**++++++++++++++++...........                                                 
*++++++++++++................                                                 
*++++....................                                                     
                                                                              
*++++....................                                                     
*++++++++++++................                                                 
**++++++++++++++++...........                                                 
***+++++++++++++++++++++++.........                                           
****+++++++++++++++++++++++++.......                                          
******+++++++++++++++++++++++++........                                       
********+++++++++++++++++++++++.......                                        
***********+++++++++++++++++++++.....                   ......                
***************+++++++++++++++++....   .........   ............               
*********************++++++++++++....  .........................              
***************************++++++++...........+++++..............             
********************************++++++++++++++++++++++++.........             
************************************++++++++++++++++++++++...........         
***************************************+++++++++++++++++++++..........        
=> 0
> mandel(-0.9, -1.4, 0.02, 0.03);
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
******************************************************************************
****************************+++++++++++++++++*********************************
***********************+++++++++++...++++++++++++*****************************
********************+++++++++++++.. . .++++++++++++++*************************
*****************++++++++++++++++... ......++++++++++++***********************
**************+++++++++++++++++++...   .......+++++++++++*********************
************++++++++++++++++++++....    .... ..++++++++++++*******************
**********++++++++++++++++++++++......       ...++++++++++++******************
********+++++++++++++++++++++++.......     .....++++++++++++++****************
******++++++++++++++++++++++++.......      .....+++++++++++++++***************
****+++++++++++++++++++++++++.... .         .....+++++++++++++++**************
**+++++++++++++++++++++++++....                ...++++++++++++++++************
*+++++++++++++++++++++++.......                ....++++++++++++++++***********
+++++++++++++++++++++..........                .....++++++++++++++++**********
++++++++++++++++++.............                .......+++++++++++++++*********
+++++++++++++++................                ............++++++++++*********
+++++++++++++.................                  .................+++++********
+++++++++++...       ....                            ..........  .+++++*******
++++++++++.....                                       ........  ...+++++******
++++++++......                                                   ..++++++*****
+++++++........                                                   ..+++++*****
+++++..........                                                   ..++++++****
++++..........                                                  ....++++++****
++..........                                                    ....+++++++***
..........                                                     ......+++++++**
..........                                                      .....+++++++**
..........                                                       .....++++++**
.........                                                            .+++++++*
........                                                             .+++++++*
 ......                                                             ...+++++++
   .                                                              ....++++++++
                                                                   ...++++++++
                                                                    ..++++++++
=> 0
```

So we see that Kaleidoscope has grown to a real and powerful language.

As usually you can experiment with
[the full code for this chapter](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/5).

## Chapter 6. Extending Kaleidoscope: mutable variables

At the moment our Kaleidoscope is a kind of a functional programming language. We've learnt quite a lot
building it, but because of Kaleidoscope having no mutable variables and being functional we had
no problems with building LLVM IR directly
in [SSA form](http://en.wikipedia.org/wiki/Static_single_assignment_form).

Now it's time to go to the land of imperative pain and start generating SSA form for expressions with
mutable variables. To understand why it's not so simple, you can look in
[the original guide](http://llvm.org/docs/tutorial/LangImpl7.html#why-is-this-a-hard-problem)
or read any book about compilers (e.g. "Engineering a Compiler" by Keith Cooper and Linda Torczon).

The good news are that LLVM has pretty good and easy to use mechanism to generate SSA form.
All LLVM IR must be in the SSA form, but this applies only to register values.
Values in memory are not a subject of this rule. Futher, you have analysis/transform passes
in LLVM, that can transform IR operating on memory values to IR in SSA form operating on register
values.

So what we need to implement mutable variables is:

 1. store variables in memory (using stack allocation in our case)
 2. encode usage of variables as load/store operations
 3. run analysis/transform passes that will generate SSA form for us.

To learn more about memory in LLVM see appropriate part of
[the original tutorial](http://llvm.org/docs/tutorial/LangImpl7.html#memory-in-llvm).

### Mutable variables in Kaleidoscope

At the moment we have only these kinds of variables: function arguments and loop induction variables.
We'll add the possibility to mutate them and also we'll allow user to define new variables
that also will be mutable.

In order to mutate variables we'll add new `=` operator and in order to define new ones we'll
add new syntactic construction:

```{.ebnf .notation}
<<<grammar.ebnf:mutable-grammar-var>>>
```

Here is a small example of what we'll be able to do with these new possibilities:

```
# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

# Recursive fib, we could do this before.
def fib(x)
  if (x < 3) then
    1
  else
    fib(x-1)+fib(x-2);

# Iterative fib.
def fibi(x)
  var a = 1, b = 1, c in
  (for i = 3, i < x in
     c = a + b :
     a = b :
     b = c) :
  b;

# Call it.
fibi(10);
```

### Adjusting variables for mutation

As a first step we'll refactor our code generation so the variables are presented as
memory locations and then are converted to register values.

At the moment our `named_values` map from the `Context` holds values themselves, we'll
change code working with it so it uses memory locations instead.

First we'll need to create memory allocas:

```rust
<<<src/builder.rs:mutable-alloca>>>
```

This code creates a new builder, positions it at the beginning of the function and builds
an alloca for one variable.

We have two types of variables now: function parameters and loop variables. Let's create alloca's
for them when they are defined:

```rust
<<<src/builder.rs:mutable-param-alloca>>>
```

This code replaces old one in functions codegeneration. We create an alloca, store parameter value
to it and insert memory location for variable in context.

In a loop expression we just create an alloca and store value in it in place of explicit phi node
generation:

```rust
<<<src/builder.rs:mutable-loop-alloca>>>
```

Now we are going to change variables usage. There are two places where we need to do so. First
the loop expression (again, no manual phi node manipulation now):

```rust
<<<src/builder.rs:mutable-loop-load>>>
```

We load current value here, calculate the next one and store it.

The other place we change is the variable expression:

```rust
<<<src/builder.rs:mutable-variable>>>
```

Let's see what IR will be generated now:

```
> def fib(x)
.   if (x < 3) then
.     1
.   else
.     fib(x-1)+fib(x-2);

define double @fib(double %x) {
entry:
  %x1 = alloca double
  store double %x, double* %x1
  %cmptmp = fcmp olt double %x, 3.000000e+00
  br i1 %cmptmp, label %ifcont, label %else

else:                                             ; preds = %entry
  %subtmp = fadd double %x, -1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp5 = fadd double %x, -2.000000e+00
  %calltmp6 = call double @fib(double %subtmp5)
  %addtmp = fadd double %calltmp, %calltmp6
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %ifphi = phi double [ %addtmp, %else ], [ 1.000000e+00, %entry ]
  ret double %ifphi
}
```

We are ready to generate SSA form now. It is surprisingly easy, just add one pass:

```rust
<<<src/builder.rs:mutable-pass>>>
```

Kaleidoscope REPL starts to generate what we want:

```
> def fib(x)
.   if (x < 3) then
.     1
.   else
.     fib(x-1)+fib(x-2);

define double @fib(double %x) {
entry:
  %cmptmp = fcmp olt double %x, 3.000000e+00
  br i1 %cmptmp, label %ifcont, label %else

else:                                             ; preds = %entry
  %subtmp = fadd double %x, -1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp5 = fadd double %x, -2.000000e+00
  %calltmp6 = call double @fib(double %subtmp5)
  %addtmp = fadd double %calltmp, %calltmp6
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %ifphi = phi double [ %addtmp, %else ], [ 1.000000e+00, %entry ]
  ret double %ifphi
}
```

It is interesting to see how did this IR look when we generated phi nodes by hand:

```
> def fib(x)
.   if (x < 3) then
.     1
.   else
.     fib(x-1)+fib(x-2);

define double @fib(double %x) {
entry:
  %cmptmp = fcmp olt double %x, 3.000000e+00
  br i1 %cmptmp, label %ifcont, label %else

else:                                             ; preds = %entry
  %subtmp = fadd double %x, -1.000000e+00
  %calltmp = call double @fib(double %subtmp)
  %subtmp1 = fadd double %x, -2.000000e+00
  %calltmp2 = call double @fib(double %subtmp1)
  %addtmp = fadd double %calltmp, %calltmp2
  br label %ifcont

ifcont:                                           ; preds = %entry, %else
  %ifphi = phi double [ %addtmp, %else ], [ 1.000000e+00, %entry ]
  ret double %ifphi
}
```

Ok, it looks the same apart from automatically generated names. Now we can implement our assignment
operator.

### Assignmnet operator

Implementing assignment operator is completely straightforward. First we add it into
the table of predefined operators:

```rust
<<<src/parser.rs:mutable-parser-default-settings>>>
```

Then we implement codegen for it:

```rust
<<<src/builder.rs:mutable-assignment-codegen>>>
```

We codegen differently comparing to other binary operators. First we
check that destination is a variable. Then we generate RHS and store it in the variable
if it is found.

We can run something like this now:

```
# Function to print a double.
extern printd(x);

# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;

def test(x)
  printd(x) :
  x = 4 :
  printd(x);

test(123);
```

This will first print `123` and then `4` showing that our assignment operator really works.

### User-defined local variables

Introduction of local variables starts like every change in syntax from the lexer
(grammar was already defined above):

```rust
<<<src/lexer.rs:mutable-var-lexer>>>
```

We just add new keyword `var` here.

Than we change parser:

```rust
<<<src/parser.rs:mutable-var-parser>>>
```

Here we add new AST entry, namely var expression. It consists of the vector of binding/value pairs
and the body expression. Than we dispatch on `Var` literal in the primary expression parsing
function. In the var expression parsing function we straightforwadly parse list
of bindings (if no value provided, we set it to 0). Finally we parse body expression.

Builder changes follow:

```rust
<<<src/builder.rs:mutable-var-builder>>>
```

We save old bindings, generate new ones and create allocas for them, insert them into context
and than generate code for the body expression. At the end we restore old bindings back.

That's all we needed to add properly scoped mutable local variables. LLVM allowed us
to avoid dirty our hands with "iterated dominance frontier" and to have our code concise and easy.

[The full code for this chapter](https://github.com/jauhien/iron-kaleidoscope/tree/master/chapters/6)
is available. This chapter finishes the main part of the tutorial about writing REPL using LLVM.

Next parts will cover different topics (like debug information, different JITs etc.), but the
main work is done.
