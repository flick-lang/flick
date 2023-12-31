# The Flick Programming Language

We wrote Flick to explore compiler design. Keep reading to see how the compiler works.

## Table of Contents

<!-- TOC -->
* [How the compiler works](#how-the-compiler-works)
  * [An overview of Flick](#an-overview-of-flick)
  * [Step 1: Lexing/tokenization](#step-1-lexingtokenization)
  * [Step 2: Parsing](#step-2-parsing)
  * [Step 3: Compilation](#step-3-compilation)
* [Installing the compiler](#installing-the-compiler)
* [Using the compiler](#using-the-compiler)
<!-- TOC -->

## How the compiler works

### An overview of Flick

Flick's syntax is based on Rust syntax. Here's
a simple program:

```text
fn main() {
    i64 i = 0
    while i < 10 {
        print(i)
        i += 1
    }
}
```

### Step 1: Lexing/tokenization

_Key question: how do we go from source code to tokens?_

<!-- TODO: link to docs / give one-sentence overview? -->

### Step 2: Parsing

_Key question: how do we go from tokens to an abstract syntax tree?_

<!-- TODO: link to docs / give one-sentence overview? -->

### Step 3: Compilation

_Key question: how do we go from an abstract syntax tree to LLVM code?_

<!-- TODO: link to docs / give one-sentence overview? -->

## Installing the compiler

Assuming Rust is installed, you can install the Flick compiler with

```shell
cargo install --git "https://github.com/flick-lang/flick.git"
```

**Before** that, though, you will also need to install LLVM 17. For example, you could use brew:

```shell
brew install llvm@17
```

You will also need to tell llvm-sys where LLVM 17 is located by adding the following line to `~/.zshrc` (or an equivalent):
```shell
export LLVM_SYS_170_PREFIX=$(brew --prefix llvm)
```

## Using the compiler

You can compile Flick programs with `flick <SOURCE_PATH>`. For example, 

```shell
flick examples/factorial.fl
```