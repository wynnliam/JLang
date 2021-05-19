# JLang
A Lisp-like language that runs on the Java Virtual Machine.
This compiler is written in OCaml. The goal of this language
is to integrate it into the [OpenJDK ASM Toolset](https://github.com/openjdk/asmtools).

## What are the features of JLang?
(Current version: JVar)

It is called "JVar" because it supports rudimentary integer variables.

JLang is a very minimal language. Currently it supports:
* Integer variables
* Integer addition and subtraction
* Reading integers from the command-line


## How does this compiler work?
This compiler uses the [nanopass approach](https://github.com/IUCompilerCourse/Essentials-of-Compilation).
We take our input language, JLang, and incrementally transform it into
other languages until we get Jasm. Each of these incremental transformations
is called a "nanopass". Sometimes a nanopass will transform JLang into other
versions of JLang. Other times they will transform it into new languages.
Each nanopass will also run a test to ensure the output is syntactically
correct; and they will run an interpreter to ensure they give the same result
as the input program.

## How do I use this compiler?

### Compiling
Make sure you have [make](https://linux.die.net/man/1/make) and
[OCaml](https://ocaml.org/) installed on your machine. Then to compile,
do:

`$ make`

This will compile the source code and, if there are no errors, will produce
an executable `driver` in the project directory.

### Running
When `driver` is on your machine, you can run it with:

`./driver -d 2 ./tests/*.j`

This will run the driver on each JLang program in the `tests` directory.
The `-d 2` flag will ask the driver to print out the code at each step.
