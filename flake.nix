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
        buildInputs = [
          pkgs.R
          pkgs.rPackages.viridis
          pkgs.rPackages.lme4
          pkgs.rPackages.nloptr
          pkgs.rPackages.geoR
          pkgs.rPackages.languageserver
          pkgs.rPackages.testthat
          pkgs.rPackages.rmarkdown
          pkgs.rPackages.knitr
          pkgs.rPackages.devtools
          pkgs.nlopt
          colorout
        ];
        meta = {
          buildVignettes = true;
        };
      };
      # https://github.com/jalvesaq/colorout
      colorout = pkgs.rPackages.buildRPackage {
        name = "colorout";
        src = pkgs.fetchFromGitHub {
          owner = "jalvesaq";
          repo = "colorout";
          rev = "64863bb252ea9a6c3aeac10fcba6ce547697d176";
          sha256 = "sha256-QCYR00rC0GB12xgztlJWr7OmNEQsti/1p0gnhqPQv1Y=";
        };
      };

      propagatedBuildInputs = [
        pkgs.rPackges.viridis 
        pkgs.rPackges.devtools
      ];
      nativeBuildInputs = [
        pkgs.rPackges.viridis
        pkgs.rPackges.devtools
      ];
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [pkgs.bashInteractive];
        buildInputs = with pkgs; [
          mixedcurve
          colorout
          R
          rPackages.devtools
          rPackages.covr
          rPackages.rmarkdown
          rPackages.roxygen2
          rPackages.knitr
          rPackages.geoR
          rPackages.viridis
          rPackages.lme4
          rPackages.dplyr
          rPackages.nloptr
          rPackages.languageserver
          rPackages.testthat
          qutebrowser
          chromium
          pandoc
          nlopt
          entr
          texliveFull
        ];
      };
    });
}
