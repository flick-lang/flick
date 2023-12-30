### Important To-dos

- Give an overview of how the compiler works in README
- Document how to use Flick itself
- Move compiler/, lexer/, and parser/ into a lib
- Maybe define flick build and flick run subcommands (mimicking cargo)
- Be consistent with what's pub, pub(crate), etc. (check `cargo doc --open`)
- Rename lexer/ to lexing/; rename parser/ to parsing/; rename compiler/ to compilation/. This way, we avoid lexer/lexer.rs, and so we can delete `#[allow(clippy::module_inception)]`
- I think parse_return_statement() processes a semicolon but do we allow semicolons everywhere? Just try making a test that has semicolons mixed in with newlines

### Maybe one day...
- Allow 🇸🇪 as a synonym for something (e.g. return)
- Write a syntax highlighting extension for code editors

### Done