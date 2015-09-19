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

## Extending Kaleidoscope: control flow

## Extending Kaleidoscope: user-defined operators

## Extending Kaleidoscope: mutable variables
