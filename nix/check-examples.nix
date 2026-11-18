{
  lib,
  runCommandCC,
  flick,
}:
let
  sources = lib.fileset.toSource {
    root = ../examples;
    fileset = lib.fileset.fileFilter (file: file.hasExt "fl") ../examples;
  };
in
runCommandCC "flick-example-check" { nativeBuildInputs = [ flick ]; } ''
  cp -r ${sources} examples
  chmod -R u+w examples
  cd examples
  for source in *.fl; do
    flick "$source"
  done
  for source in errors/*.fl; do
    if flick "$source"; then
      echo "Expected compilation to fail: $source" >&2
      exit 1
    fi
  done
  ./hello_world > "$out"
  grep -qx 'Hello world' "$out"

  for kind in obj asm llvm-ir llvm-bc; do
    flick --emit "$kind" -o "hello_world.$kind" hello_world.fl
    test -s "hello_world.$kind"
  done
''
