## Roadmap to working demo!!!

- [ ] Make a playground for the programming language (with python backend first?)
    - https://ace.c9.io is what rust uses
- [ ] Deploy the docs and attach link to README.md
- [ ] Give an overview of how the compiler works in README
- [ ] Document how to use Flick itself
- [ ] stdout
- [ ] strings (array of char; implemented as obj in std library)

## Other

### Quick Todos

- [ ] Make `Typer` take the program in its `new()`
- [ ] Make all module and crate level docstrings start with `//!` instead of `///` and
- [ ] Move `types.rs` into `typing` module and fix the ignored example in `typing/mod.rs` docstring
- [ ] Document functions in Typer
- [ ] Maybe define flick build and flick run subcommands (mimicking cargo)
- [ ] Don't allow pull requests to merge without passing tests, getting approval, and being warning free

### Longer Todos

- [ ] Pointers
- [ ] Nice exceptions
- [ ] Rethink what structs/files/struct properties/functions in flick should be public vs pub(crate) vs private
- [ ] Remove all clones / think about slices / lifetimes / AsRef?
- [ ] Maybe remove all * imports in all source files
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
