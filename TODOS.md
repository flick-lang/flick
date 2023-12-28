### Important To-dos

- Document how to use Flick itself
- Move compiler/, lexer/, and parser/ into a lib
- Maybe define flick build and flick run subcommands (mimicking cargo)
- Be consistent with what's pub, pub(crate), etc. (check `cargo doc --open`)
- Rename lexer/ to lexing/; rename parser/ to parsing/; rename compiler/ to compiling/. This way, we avoid lexer/lexer.rs, and so we can delete `#[allow(clippy::module_inception)]`

### Maybe one day...
- Allow 🇸🇪 as a synonym for something (e.g. return)
- Write a syntax highlighting extension for code editors

### Done