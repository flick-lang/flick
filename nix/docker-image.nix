{
  dockerTools,
  buildEnv,
  runCommand,
  removeReferencesTo,
  writeShellScriptBin,
  bashNonInteractive,
  musl,
  binutils-unwrapped,
  flick,
}:
let
  flickStaticLLVM = flick.override { staticLLVM = true; };

  # Only `ld`; its self-references would pull in all of binutils
  ld = runCommand "ld" { nativeBuildInputs = [ removeReferencesTo ]; } ''
    install -Dm755 ${binutils-unwrapped}/bin/ld.bfd $out/bin/ld
    remove-references-to -t ${binutils-unwrapped} $out/bin/ld
  '';

  # Minimal musl `cc` for flick
  cc = writeShellScriptBin "cc" ''
    exec ${ld}/bin/ld -dynamic-linker ${musl}/lib/libc.so \
      ${musl}/lib/crt1.o ${musl}/lib/crti.o "$@" -L${musl}/lib -lc ${musl}/lib/crtn.o
  '';
in
dockerTools.buildImage {
  name = "flick";
  tag = "latest";
  copyToRoot = buildEnv {
    name = "flick-image-root";
    paths = [
      flickStaticLLVM
      cc
      bashNonInteractive
    ];
    pathsToLink = [ "/bin" ];
  };
  extraCommands = "mkdir -m 1777 tmp workspace";
  config = {
    User = "1000:1000";
    WorkingDir = "/workspace";
    Env = [ "PATH=/bin" ];
  };
}
