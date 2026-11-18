## Setting up a development environment

You can run `nix develop` to get a shell with Rust, LLVM, and the tooling the build needs.

In order to make sure you never accidentally commit code that doesn't
compile and pass tests, you should install our git hooks:

```shell
git config core.hooksPath hooks  # run from repo root
```

## Library-only development

If you are only using the library (and not using the main.rs frontend to the library), make sure to disable the `binary`
feature. You can do this by setting `default-features = false` when adding Flick as a dependency, or by passing
`--no-default-features` when using a cargo subcommand.

## CI checks

Run the same checks as GitHub Actions with:

```shell
nix flake check --no-update-lock-file --print-build-logs
```

This builds the package and runs its tests and doctests (with and without the `binary` feature),
formatting, and compiler example checks.
CI runs on Linux x86_64 and macOS ARM64 for every PR and push to `main`, and can
be run manually from the Actions tab. Check definitions live in `nix/check-*.nix`; the package build is
also exposed as a check.

## Auto-merge setup

After the workflow has run, configure the `main` branch ruleset or branch
protection to require the status checks `checks (ubuntu-latest)` and
`checks (macos-latest)`. Enable **Allow auto-merge** in the repository settings.
Maintainers can then enable auto-merge on individual PRs, including dependency
updates; the workflow does not enable it automatically.
