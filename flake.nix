{
  description = "The Flick Programming Language";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:

    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      forEachSupportedSystem =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          in
          f pkgs
        );
    in
    {
      overlays.default = final: prev: {
        flick = final.callPackage ./nix/flick.nix { };
      };

      packages = forEachSupportedSystem (
        {
          pkgs,
          ...
        }:
        {
          inherit (pkgs) flick;
          default = pkgs.flick;
        }
      );

      devShells = forEachSupportedSystem (pkgs: {
        default = pkgs.mkShell {
          inputsFrom = [ pkgs.flick ];

          packages = with pkgs; [
            clippy
            rustfmt
            rust-analyzer
          ];

          env = {
            # Required by rust-analyzer
            RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
          };
        };
      });

      checks = forEachSupportedSystem (pkgs: {
        examples = pkgs.runCommandCC "flick-example-check" { nativeBuildInputs = [ pkgs.flick ]; } ''
          cp -r --no-preserve=mode ${./examples} examples && cd examples
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
          grep -q 'Hello' "$out"

          for kind in obj asm llvm-ir llvm-bc; do
            flick --emit "$kind" hello_world.fl
          done
        '';
      });
    };
}
