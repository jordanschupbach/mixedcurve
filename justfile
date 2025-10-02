# Justfile for building and testing the R package

build:
    Rscript -e "devtools::build()"

build-docs:
    Rscript -e "roxygen2::roxygenise()"

coverage:
    Rscript -e "covr::package_coverage()"

test:
    Rscript -e "devtools::test()"
