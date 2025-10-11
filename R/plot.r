#' Generate a square layout matrix for plotting multiple curves
#'
#' This function generates a layout matrix that arranges a specified number of curves
#' in a nearly square grid for plotting purposes.
#'
#' @param ncurves A positive integer representing the number of curves to be plotted.
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
