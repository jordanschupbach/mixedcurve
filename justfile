

build-all-vignettes:
    for dir in ./vignettes/*/; do \
        dir_name=$(basename "$dir"); \
        echo "Processing directory: $dir"; \
        echo "Checking for file: ./vignettes/$dir_name/$dir_name.rmd"; \
        if [ -f "./vignettes/$dir_name/$dir_name.rmd" ]; then \
            echo "Found $dir_name.rmd in: $dir"; \
            Rscript -e "rmarkdown::render(file.path('./vignettes/$dir_name', '$dir_name.rmd'), output_format = 'all', output_file = '$dir_name.html', output_dir = './vignettes/$dir_name')"; \
        else \
            echo "$dir_name.rmd not found in: $dir"; \
        fi; \
    done



build-vignette:
    Rscript -e "rmarkdown::render('./vignettes/nw_1d/nw_1d.rmd', output_format = 'all', output_file = 'nw_1d.html', output_dir = './vignettes/nw_1d/')"

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


