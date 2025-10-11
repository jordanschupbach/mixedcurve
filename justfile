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
