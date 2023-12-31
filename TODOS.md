### Important Todos

- [ ] Give an overview of how the compiler works in README
- [ ] Document how to use Flick itself
- [x] Move compiler/, lexer/, and parser/ into a lib
- [ ] Maybe define flick build and flick run subcommands (mimicking cargo)
- [x] Be consistent with what's pub, pub(crate), etc.
- [x] Rename lexer/ to lexing/; rename parser/ to parsing/; rename compiler/ to compilation/. This way, we avoid lexer/lexer.rs, and so we can delete `#[allow(clippy::module_inception)]`
- [ ] I think parse_return_statement() processes a semicolon but do we allow semicolons everywhere? Just try making a test that has semicolons mixed in with newlines
- [ ] Add cargo doc to the list of things that GitHub checks before commit
- [ ] Don't allow pull requests to merge without passing tests, getting approval, and being warning free
- [ ] Deploy the docs and attach link to README.md
- [ ] Make a playground for the programming language (with python backend first?)
   - https://ace.c9.io is what rust uses 
- [ ] make a playground with wasm

### Maybe one day...
- [ ] Allow 🇸🇪 as a synonym for something (e.g. return)
- [ ] Write a syntax highlight≥ing extension for code editors
- [ ] Support importing files (by compiling multiple programs in LLVM)