## Roadmap to working web demo!!!

- [x] Support for calling void functions
    - [ ] should we let non void functions also be called w value ignored
- [x] Implement if statements so that our language is Turing-complete
- [ ] Pointers / arrays
- [ ] Make a playground for the programming language (with python backend first?)
    - https://ace.c9.io is what rust uses
- [ ] Deploy the docs and attach link to README.md
- [ ] Give an overview of how the compiler works in README
- [ ] Document how to use Flick itself
- [x] Add tests for typer

## Roadmap to a better langauge

- [ ] Implement importing of other files
- [ ] Implement standard library as a different file so that it can be imported
    - [ ] stdout
    - [ ] strings (array of char; implemented as obj in std library)

## Other

### Quick Todos

- [ ] Implement a simple BigInteger function to ensure that the int_value given fits inside the width of the int type provided in typer
- [ ] Make `Typer` take the program in its `new()`
- [ ] Make all module and crate level docstrings start with `//!` instead of `///` and
- [ ] Move `types.rs` into `typing` module and fix the ignored example in `typing/mod.rs` docstring
- [ ] Document functions in Typer
- [ ] Maybe define flick build and flick run subcommands (mimicking cargo)
- [ ] Once typer is done, remove redundant checks in compiler and mark them as `unreachable`
- [x] Add tests to parser for spacing and newlines in body and program
- [x] Don't call skip newlines twice in parse_body and parse_program

### Longer Todos

- [ ] Make a proper website for flick-lang
- [ ] Test suite like Tsoding's where we can test many .fl files
- [ ] Embed LLD linker so not calling clang
- [ ] Think about what an identifier is... is it always a variable name? because callexpr is separate...
- [ ] Be looser with types during typing: coercion, i64 = i32 + i32
- [ ] Pointers
- [ ] Nice compiler errors
    - Idea: to show all the errors at once, we can collect them as we go.
    - But how can we keep compiling if we hit an error? Well, we just propagate it up to the statement-level, and then
      skip to the next statement. (Fatal errors would cause us to stop.)
- [ ] Rethink what structs/files/struct properties/functions in flick should be public vs pub(crate) vs private
- [ ] Remove all clones / think about slices / lifetimes / AsRef?
- [ ] Maybe remove all * imports in all source files
- [ ] Block PRs to main without passing tests and being warning free (though we'd have to install LLVM on GitHub)
- [x] Be consistent with what's pub, pub(crate), etc.
- [x] Rename lexer/ to lexing/; rename parser/ to parsing/; rename compiler/ to compilation/. This way, we avoid
  lexer/lexer.rs, and so we can delete `#[allow(clippy::module_inception)]`
- [x] Add cargo doc to the list of things that git checks before commit
- [x] Move compiler/, lexer/, and parser/ into a lib
- [x] ScopeManager and Type are both structs that belong across all modules of the compiler. Maybe they should be moved
  out of their respective module into the root 'src' folder (or somewhere else)?
- [x] Take all lexer tokens, parsing trees, etc. as references instead of owning
- [x] Either use () in structs or use { } in struct properties

### Maybe one day...

- [ ] Write a syntax highlighting extension for code editors
- [ ] Support importing other flick source files (by compiling multiple programs in LLVM)
