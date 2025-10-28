# mixedcurve

An R package for fitting local polynomial kernel regression, mixed-curve models
and hierarchical mixed-curve models and conducting inference using
Westfall-Young adjusted p-values.

To install the package, use:

```r
devtools::install_github("jordanschupbach/mixedcurve")

# Or to also build vignettes: 
# NOTE: this may take a while to complete and is not recommended for most users.
# devtools::install_github("jordanschupbach/mixedcurve", build_vignettes = TRUE)
```

To open a pre-built version of the package vignettes (see `./inst/built_vignettes/`), use:

```r
vignames <- mixedcurve::list_vignettes()

# open the first vignette
mixedcurve::open_vignette(vignames[1])
```

