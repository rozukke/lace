{
  description = "A dev environment for Rust.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  nixConfig = {
    bash-prompt-prefix = "(rust-dev) ";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        
        # Specify Rust version here
        _rustToolchain = (pkgs.rust-bin.stable.latest.default.override {
          extensions = ["rust-src" "cargo" "rustc"];
        });

        _rustPlatform = pkgs.makeRustPlatform {
          rustc = _rustToolchain;
          cargo = _rustToolchain;
          rust-src = _rustToolchain;
        };

        manifest = (pkgs.lib.importTOML ./Cargo.toml).package;

      in {
        packages = {
          mimi = _rustPlatform.buildRustPackage {
            pname = manifest.name;
            inherit (manifest) version;

            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;

            preCheck = ''
              export RUST_BACKTRACE=1 
            '';
          };

          # Default target for nix commands
          default = self.packages.${system}.mimi;

        };

        devShells.default = pkgs.mkShell {
          name = "rust-dev";
          buildInputs = with pkgs; [
            _rustToolchain
            rust-analyzer
          ];

          RUST_SRC_PATH = "${_rustToolchain}/lib/rustlib/src/rust/library";
          CARGO_TERM_COLOR = "always";
          RUST_BACKTRACE = "full";

        };
      }
    );
}
