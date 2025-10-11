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
        buildInputs = [
          pkgs.R
          pkgs.rPackages.viridis
        ];
      };
      propagatedBuildInputs = [pkgs.rPackges.viridis];
      nativeBuildInputs = [pkgs.rPackges.viridis];
      # propagatedBuildInputs = with pkgs.rPackages; [viridis];
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [pkgs.bashInteractive];
        buildInputs = with pkgs; [
          mixedcurve
          R
          rPackages.devtools
          rPackages.covr
          rPackages.rmarkdown
          rPackages.roxygen2
          rPackages.knitr
          rPackages.geoR
          rPackages.viridis
          pandoc
          entr
        ];
      };
    });
}
