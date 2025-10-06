{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      mixedcurve = pkgs.rPackages.buildRPackage {
        name = "mixedcurve";
        src = ./.;
        # TODO: add deps
      };
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [pkgs.bashInteractive];
        buildInputs = with pkgs; [
          mixedcurve
          R
          rPackages.devtools
          rPackages.covr
          rPackages.rmarkdown
          rPackages.viridis
          rPackages.knitr
          rPackages.geoR
          pandoc
          entr
        ];
      };
    });
}
