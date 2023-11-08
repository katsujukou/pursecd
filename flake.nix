{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    ocaml-overlay.url = "github:nix-ocaml/nix-overlays";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, flake-utils, ocaml-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ocaml-overlay.overlays.default ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_1;
        cbor = ocamlPackages.buildDunePackage {
          pname = "cbor";
          version = "0.5";

          src = pkgs.fetchFromGitHub {
            owner = "ygrek";
            repo = "ocaml-cbor";
            rev = "480a743fd5a03237bc488a7aec72e102894edee9";
            sha256 = "1smm030yh5a3x1mlmhdzdjkjxk9h5syifg0q6q20giakl3891kr5";
          };

          buildInputs = with ocamlPackages; [
            ocplib-endian
          ];
        };

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = (with pkgs.ocaml-ng.ocamlPackages_5_1; [
            ocaml
            dune
            ocaml-lsp
            ocamlformat
            findlib
            utop
          # OPAM packages
            cmdliner
            yojson
            cbor
            ppx_deriving
          ]);

        };
      }
    );
}
