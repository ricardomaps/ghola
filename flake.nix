{
  description = "Window manager for the River compositor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      pkgsFor = system: import nixpkgs { inherit system; };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = ocamlPackages.buildDunePackage {
            pname = "ghola";
            version = "0.1.0";

            src = ./.;

            duneVersion = "3";

            buildInputs = with ocamlPackages; [
              ocaml
              xkbcommon
              wayland
              eio
              eio_main
              base
            ];

            propagatedBuildInputs = with ocamlPackages; [
              eio
              eio_main
              ocaml
              xkbcommon
              wayland
              base
            ];
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.default ];

            buildInputs = with ocamlPackages; [
              ocaml
              dune_3
              ocaml-lsp
            ];
          };
        });
    };
}
