## Setting up a development environment

In order to make sure you never accidentally commit code that doesn't
compile and pass tests, you should install our git hooks:

```shell
git config core.hooksPath hooks  # run from repo root
```

## Library-only development

If you are only using the library (and not using the main.rs frontend to the library), make sure to disable the `binary` feature. You can do this by setting `default-features = false` when adding Flick as a dependency, or by passing `--no-default-features` when using a cargo subcommand.
