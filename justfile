# TARGET := "gmc_1d"
TARGET := "lpk_2d"

open-vignette:
    @xdg-open ./doc/{{TARGET}}.html &
    clear

open-single-vignette:
    # @xdg-open ./vignettes/{{TARGET}}/{{TARGET}}.html &
    @xdg-open ./inst/built_vignettes/{{TARGET}}.html &
    clear

open-all-vignettes:
  for file in ./doc/*.html; do \
      xdg-open "$file" & \
  done
  clear

build-vignettes:
    Rscript -e "devtools::build_vignettes('./')"
    rm -rf ./doc/*.R
    rm -rf ./doc/*.rmd

devbuild:
    nix develop . --command bash -c "Rscript -e 'devtools::build_vignettes()'"


build-all-vignettes:
    for fpath in ./vignettes/*.rmd; do \
        fname=$(basename "$fpath" .rmd); \
        Rscript -e "rmarkdown::render('./vignettes/$fname.rmd', output_format = 'all', output_file = '$fname.html', output_dir = './inst/built_vignettes/')"; \
    done


build-single-vignette:
    Rscript -e "rmarkdown::render('./vignettes/{{TARGET}}.rmd', output_format = 'all', output_file = '{{TARGET}}.html', output_dir = './vignettes/{{TARGET}}/')"
    cp ./vignettes/{{TARGET}}/{{TARGET}}.html ./inst/built_vignettes/{{TARGET}}.html

test-watch:
    trap 'exit' INT; while sleep 0.1; do find tests/testthat R -type f \( -iname '*.r' \) | entr -d sh -c 'clear; Rscript -e "devtools::test()"'; done

build:
    Rscript -e "devtools::build()"

build-docs:
    Rscript -e "roxygen2::roxygenise()"

coverage:
    Rscript -e "covr::package_coverage()"

test:
    Rscript -e "devtools::test()"

r-repl:
    nix develop . --command bash -c "R"


