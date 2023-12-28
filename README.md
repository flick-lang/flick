# The Flick Programming Language

## Compiling

### Installing LLVM 17

Flick compilation depends on LLVM 17, which must be installed separately.
For example, on macOS:

1. Use brew to install LLVM 17.

```zsh
brew install llvm@17
```

2. Then, set the `LLVM_SYS_170_PREFIX` variable so our dependency (llvm-sys)
works properly.

```zsh
# add the following line to ~/.zshrc
export LLVM_SYS_170_PREFIX=$(brew --prefix llvm)
```

## License

Flick uses the [MIT](LICENSE) license.
