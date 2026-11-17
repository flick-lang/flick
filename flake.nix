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
    };
}
