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

            nativeBuildInputs = with ocamlPackages; [
              wayland
            ];

            propagatedBuildInputs = with ocamlPackages; [
              xkbcommon
              wayland
              xdg
              eio
              eio_main
              containers
            ];
          };
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          test = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.default ];
            nativeBuildInputs = with ocamlPackages; [
              ocaml
              findlib
              self.packages.${system}.default
            ];
          };

          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.default ];
            nativeBuildInputs = with ocamlPackages; [
              ocaml
              dune_3
              ocaml-lsp
              ocamlformat
              utop
            ];
          };
        });
    };
}
