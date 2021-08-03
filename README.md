# JLang
A Lisp-like language that runs on the Java Virtual Machine.
This compiler is written in OCaml. The goal of this language
is to integrate it into the [OpenJDK ASM Toolset](https://github.com/openjdk/asmtools).

## What are the features of JLang?
(Current version: JLoop; Previous version is [JIf](./docs/READMEJIf.md))

In addition to integer variables, sequences, rudimentary I/O, comparisons and if statements,
"JLoop" supports the following:

* Simple while loops

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

The output file this creates will captialize the first letter of the
file. So if I ran:

`./driver -d 2 ./tests/print_test.j`

The result will be:

`./tests/Print_test.jasm`

To assemble and run the resulting jasm, do:

`cd ./tests`

`./run.sh [CAPITALIZED NAME OF JASM FILE].jasm`

### Note about version
By default, we assume that the version of Java Byte Code you wish to compile to is
52.0. If you want a different version you need to do two things: recompile `read_int.java`
with whatever version of `javac` you have, and then rebuild the compiler with the version
you want.

1. Open `JasmIf.ml`
2. Find the variables `version_major` and `version_minor`. They are near the bottom of the file.
3. Change them to the numbers you want.
4. Run `Make`

Now your compiler will produce the version you want. **Assuming your local installation of JDK is the same** now do the following:
1. Run navigate to the tests folder:

```
$ cd tests/
```

2. Recompile `read_int.java`:

```
$ javac read_int.java
```

Now your toolchain should be set to the correct version!

## JLang Semantics
I will now describe JLang's features and semantics.
JLang has only one type: 64 bit integers. A JLang program is a single program
that evaluates to a single 64 bit integer. An expression can be one
of the following:

(From JVar)
* A single integer: `42`. Its value is the integer itself.
* A Variable `v`. The value of the expression is the value of `v`.
* `(read)`, reads a single integer. The value of this expression is the integer read in.
* `(+ exp1 exp2)`, evaluates two expressions and adds them. The value is the sum of
  the two expressions.
* `(- exp)`, Negates an expression. The resulting integer is the value of this expression.
* `(let v exp1 exp2)`, evaluates `exp1`, assigns it to `v`, and then evaluates `exp2` with
  `v = exp1`, The value of this kind of expression is the result of evaluating `exp2`.
* `(:= v exp)`, evaluates `exp` and assigns it to `v`. The value of the expression is
  the new value of `v`.
* `(print exp)`, Prints the value of `exp` to the commandline. The value of the expression
  is 0.
* `(seq exp1 exp2 ... expN)`, evaluates each sub expression in order. The value of this expression
  will be the last expression evaluated: `expN`.

(From JIf)
* A simple comparison: `(< x y)` equals 1 if the comparison is true, and 0 if it is false
* An if statement: `(if exp0 exp1 exp2)` evaluates `exp0`. If it is not 0 then the result is `exp1`, and if
  it is 0 then the result is `exp2`.

(From JLoop)
* A simple loop: `(while exp1 exp2)` Evaluates `exp1`, if it is not 0, it will evaluate `exp2`, then it will repeat
this process until `exp1` is 0. The value of a while expression is 0.
