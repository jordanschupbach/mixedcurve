# TARGET := "gmc_1d"
# TARGET := "nw_2d_w_covariate"
# TARGET := "nw_2d"
TARGET := "glpk_1d_w_covariate"

open-vignette:
    @xdg-open ./doc/{{TARGET}}.html &
    clear

open-single-vignette:
    @xdg-open ./vignettes/{{TARGET}}/{{TARGET}}.html &
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
    for dir in ./vignettes/*/; do \
        dir_name=$(basename "$dir"); \
        echo "Processing directory: $dir"; \
        echo "Checking for file: ./vignettes/$dir_name.rmd"; \
        if [ -f "./vignettes/$dir_name/$dir_name.rmd" ]; then \
            echo "Found $dir_name.rmd in: $dir"; \
            Rscript -e "rmarkdown::render(file.path('./vignettes/$dir_name.rmd'), output_format = 'all', output_file = '$dir_name.html', output_dir = './vignettes/$dir_name')"; \
        else \
            echo "$dir_name.rmd not found in: $dir"; \
        fi; \
        cp ./vignettes/$dir_name/$dir_name.rmd ./vignettes/$dir_name/$dir_name.md; \
    done



build-single-vignette:
    Rscript -e "rmarkdown::render('./vignettes/{{TARGET}}.rmd', output_format = 'all', output_file = '{{TARGET}}.html', output_dir = './vignettes/{{TARGET}}/')"

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


