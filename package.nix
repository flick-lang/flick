{
  lib,
  pkgs,
  stdenv,
  rustPlatform,
  # Follow the host platform by default; allow embedding LLVM independently.
  staticLLVM ? stdenv.hostPlatform.isStatic,
}:

let
  root = ./.;
  cargoToml = lib.importTOML (root + "/Cargo.toml");

  # llvm-sys versions encode LLVM's major.minor, e.g. "231" is LLVM 23.1
  llvmSysVersion = lib.versions.major cargoToml.dependencies.llvm-sys;
  llvmMajor = lib.substring 0 (lib.stringLength llvmSysVersion - 1) llvmSysVersion;
  llvmPackages = pkgs."llvmPackages_${llvmMajor}";
in
rustPlatform.buildRustPackage {
  pname = cargoToml.package.name;
  inherit (cargoToml.package) version;

  src = lib.fileset.toSource {
    inherit root;
    fileset = lib.fileset.unions [
      (root + "/Cargo.toml")
      (root + "/Cargo.lock")
      (root + "/README.md") # crate docs, via include_str! in lib.rs
      (root + "/src")
    ];
  };
  cargoLock.lockFile = root + "/Cargo.lock";

  buildFeatures = [
    # Force the linking mode instead of silently falling back with prefer.
    (if staticLLVM then "llvm-sys/force-static" else "llvm-sys/force-dynamic")
    "llvm-sys/strict-versioning"
  ];

  # Static LLVM's `llvm-config --system-libs` asks for -lxml2 at link time
  buildInputs = [ llvmPackages.libllvm ] ++ lib.optionals staticLLVM [ pkgs.libxml2 ];
  env."LLVM_SYS_${llvmSysVersion}_PREFIX" = "${llvmPackages.libllvm.dev}";

  meta.mainProgram = "flick";
}
