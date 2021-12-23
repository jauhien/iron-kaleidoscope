An attempt to create safe Rust LLVM bindings ([rust-lang/rfcs#644](https://github.com/rust-lang/rfcs/issues/644)).

Primarily will be used in my [LLVM Kaleidoscope tutorial for Rust](https://github.com/jauhien/iron-kaleidoscope).

It is the very beginning of work on safe bindings. Pull requests are welcome.

For initial documentation see
[tutorial](https://github.com/jauhien/iron-kaleidoscope#chapter-2-llvm-ir-code-generation),
[LLVM programmer's manual](http://llvm.org/docs/ProgrammersManual.html)
and [LLVM doxygen docs](http://llvm.org/docs/doxygen/html/index.html).

To use `iron-llvm` you need to install LLVM. Note, that if you use binary distribution, you need
to have the dev version (with includes). `llvm-config` should be in `PATH`.

TODO list (random points to do in the nearest time):

* proper documentation
* refactor boilerplate in memory manager bindings
* full ExecutionEngine API
* ORC
* change all methods to use static dispatch where appropriate (as in GenericValue)
