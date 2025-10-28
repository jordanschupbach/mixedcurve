#' Generate a square layout matrix for plotting multiple curves
#'
#' This function generates a layout matrix that arranges a specified
#' number of curves in a nearly square grid for plotting purposes.
#'
#' @param ncurves A positive integer representing the number of curves
#'                to be plotted.
#'
#' @return A matrix representing the layout for plotting the curves.
#'
#' @export
#'
#' @examples
#' ncurves <- 10
#' layout_matrix <- gen_square_layout(ncurves)
#' layout(layout_matrix)
#' for (i in 1:ncurves) {
#'   plot(runif(10), runif(10), main = paste("Curve", i))
#' }
gen_square_layout <- function(ncurves) {
  if (ncurves <= 0) stop("ncurves must be a positive integer.")
  ncols <- ceiling(sqrt(ncurves))
  nrows <- ceiling(ncurves / ncols)
  if (nrows > ncols) {
    nrows <- ceiling(ncurves / ncols)
  } else {
    nrows <- ncols
    ncols <- ceiling(ncurves / nrows)
  }
  layout_matrix <- matrix(1:ncurves, nrow = nrows, ncol = ncols, byrow = TRUE)
  layout_matrix
}


plot_pval_regions <- function(queries, wy_pvals,
                              pthresh = 0.05, ylims = par("usr")[3:4]) {
  in_region <- which(wy_pvals < pthresh)
  if (length(in_region) > 0) {
    # Get contiguous regions
    regions <- split(in_region, cumsum(c(1, diff(in_region) != 1)))
    for (region in regions) {
      if (length(region) > 0) {
        x_coords <- c(
          queries[region[1]] - diff(queries)[1] / 2,
          queries[region][-length(region)],
          queries[tail(region, 1)] + diff(queries)[1] / 2,
          queries[tail(region, 1)] + diff(queries)[1] / 2,
          queries[region[1]] - diff(queries)[1] / 2
        )
        y_coords <- c(
          ylims[1], rep(ylims[1], length(region)),
          ylims[2], ylims[2]
        )
        polygon(x_coords, y_coords, col = rgb(0, 0, 1, 0.2), border = NA)
      }
    }
  }
}
