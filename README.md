# The Flick Programming Language

We wrote Flick to explore compiler design. Keep reading to learn how our compiler works, or
try writing Flick code in our [sandbox](https://github.com/flick-lang/flick-sandbox).

<!-- TODO: change the link above to be https://flick-lang.com -->

## Table of Contents

<!-- TOC -->

* [How the compiler works](#how-the-compiler-works)
    * [An overview of Flick](#an-overview-of-flick)
    * [Step 1: Lexing/tokenization](#step-1-lexingtokenization)
    * [Step 2: Parsing](#step-2-parsing)
    * [Step 3: Compilation](#step-3-compilation)
* [Installing the compiler](#installing-the-compiler)
    * [With Nix (preferred)](#with-nix-preferred)
    * [With Homebrew](#with-homebrew)
* [Using the compiler](#using-the-compiler)

<!-- TOC -->

## How the compiler works

### An overview of Flick

Flick's syntax is based on Rust syntax. Here's
a simple program:

```text
pub fn main() {
    i64 i = 0
    while i < 10 {
        print(i)
        i += 1
    }
}
```

You can find other sample programs in the [examples/][a] folder.

[a]: https://github.com/flick-lang/flick/tree/main/examples

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

### With Nix (preferred)

We recommend installing Flick with [Nix](https://nixos.org) (with flakes enabled). Nix installs the right version of
LLVM for you and sets up the paths and environment variables llvm-sys needs, so there is nothing else to configure.
You can install the Flick compiler with

```shell
nix profile add github:flick-lang/flick
```

or, to try it without installing anything, run

```shell
nix run github:flick-lang/flick -- examples/factorial.fl
```

### With Homebrew

Assuming Rust is installed, you can install the Flick compiler with

```shell
cargo install --git "https://github.com/flick-lang/flick.git"
```

**Before** that, though, you will also need to install LLVM 23.1. For example, you could use brew:

```shell
brew install llvm
# Once LLVM 24 is out, this becomes `brew install llvm@23`
```

You will also need to tell llvm-sys where LLVM is located by adding the following line to `~/.zshrc` (or an
equivalent). First, run `brew info llvm` to make sure the installed version is 23.1. Then, add

```shell
export LLVM_SYS_231_PREFIX=$(brew --prefix llvm)
# Once LLVM 24 is out, this becomes `$(brew --prefix llvm@23)`
```

_(Note: you should replace 231 in the environment variable with whatever version of LLVM is installed; 17.2, for
example, corresponds to the environment variable `LLVM_SYS_172_PREFIX`. The version needs to match the `llvm-sys`
version in `Cargo.toml`.)_

Either way, Flick links executables by calling `cc`, so you will need a C compiler on your `PATH`.

## Using the compiler

You can compile Flick programs with `flick <SOURCE_PATH>`. For example,

```shell
flick examples/factorial.fl
```

By default, `flick` produces an executable. You can pass `--emit` with `obj`, `asm`, `llvm-ir` or `llvm-bc` to produce
an object file, assembly, LLVM IR or LLVM bitcode instead, and `-o` to choose where the output goes.

You can also pass `--static` to link an executable with no runtime dependencies, which runs on any Linux machine with
the same CPU architecture. _(Note: macOS does not support static executables.)_

Run `flick --help` to see all the options.
