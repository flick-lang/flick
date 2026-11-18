{
  runCommand,
  rustfmt,
  flick,
}:
runCommand "flick-formatting-check" { nativeBuildInputs = [ rustfmt ]; } ''
  cd ${flick.src}
  rustfmt --edition 2024 --check src/lib.rs src/main.rs
  touch "$out"
''
