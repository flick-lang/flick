# The Flick Programming Language

We wrote Flick to explore compiler design. Keep reading to see how the compiler works.

Table of contents:
<!-- TOC -->
* [The Flick Programming Language](#the-flick-programming-language)
  * [Installing the compiler](#installing-the-compiler)
  * [Using the compiler](#using-the-compiler)
<!-- TOC -->

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