vignames <- mixedcurve::list_vignettes()
mixedcurve::open_vignette(vignames[1])

# for (vig in vignames) {
#   mixedcurve::open_vignette(vig)
# }

file_path <- paste0(system.file("", package = "mixedcurve"), "/inst/vignettes/")
file_path
