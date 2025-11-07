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

      pythonPkgs = pkgs.python3.pkgs;
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
          pkgs.rPackages.inline
          pkgs.nlopt
          # colorout
        ];
        meta = {
          buildVignettes = true;
        };
      };
      # # https://github.com/jalvesaq/colorout
      # colorout = pkgs.rPackages.buildRPackage {
      #   name = "colorout";
      #   src = pkgs.fetchFromGitHub {
      #     owner = "jalvesaq";
      #     repo = "colorout";
      #     rev = "64863bb252ea9a6c3aeac10fcba6ce547697d176";
      #     sha256 = "sha256-QCYR00rC0GB12xgztlJWr7OmNEQsti/1p0gnhqPQv1Y=";
      #   };
      # };

      # https://github.com/IndrajeetPatil/pairwiseComparisons
      pairwiseComparisons = pkgs.rPackages.buildRPackage {
        name = "pairwiseComparisons";
        src = pkgs.fetchFromGitHub {
          owner = "IndrajeetPatil";
          repo = "pairwiseComparisons";
          rev = "617dbfed08bdeb7ac6cf540a4573f38a47920ac3";
          sha256 = "sha256-mgx1ZF5B/wUMgEXtzTV5EEEaz+QOnCZE9ijQ5QoqRl8=";
        };
      nativeBuildInputs = [
          pkgs.R 
          ];


        buildInputs = [
          pkgs.rPackages.dplyr 
          pkgs.rPackages.insight
          pkgs.rPackages.parameters
          pkgs.rPackages.PMCMRplus
          pkgs.rPackages.purrr
          pkgs.rPackages.rlang
          pkgs.rPackages.statsExpressions
          pkgs.rPackages.WRS2
        ];
      };

      propagatedBuildInputs = [
        pkgs.rPackges.viridis 
        pkgs.rPackges.devtools
        pkgs.rPackages.inline
      ];
      nativeBuildInputs = [
        pkgs.rPackges.viridis
        pkgs.rPackges.devtools
        pkgs.rPackages.inline
      ];
    in {
      devShells.default = pkgs.mkShell {
        packages = [ 
          pkgs.hello
          (pkgs.python3.withPackages (python-pkgs:
            with python-pkgs; [
              pytest
              # select Python packages here
              # numpy232
              # pypkg001
              python-lsp-server
              numpy
              matplotlib
              # matplotlib
              # requests
            ]))
           pkgs.R
           pkgs.rPackages.languageserver
           pkgs.rPackages.inline
           pkgs.julia
           pkgs.octaveFull
           pkgs.lua
        ];
        nativeBuildInputs = [pkgs.bashInteractive];
        buildInputs = with pkgs; [
          mixedcurve
          # colorout
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
          rPackages.inline
          rPackages.ParBayesianOptimization
          rPackages.ggdark
          pairwiseComparisons
          rPackages.lmerTest
          rPackages.multcomp
          # chromium
          # qutebrowser
          # pandoc
          # nlopt
          # entr
          # texliveFull
        ];
      };
    });
}
