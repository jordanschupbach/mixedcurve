# -*- origami-fold-style: triple-braces -*-

# {{{ License

# Copyright (C) <2025>  <Jordan Schupbach>
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.

# }}} License

# {{{ Header

# Name: frame.r
# Author: Jordan Schupbach
# Description: Simple data structure for a point process

# }}} Header

# {{{ pp()
#' pp
#'
#' A general datastructure for ppps
#' @param points - a pointset data.frame() object
#' @param win - a list of endpoints giving window
#' @param covs - covariates associated to the pointset
#' @author Jordan Schupbach
#' @export
#' @examples
#' pp1 <- mixedcurve::pp(
#'   points = data.frame(x = runif(100, 0, 1)),
#'   win = list(c(0, 1)),
#'   covs = list()
#' )
#' plot(pp1, tick_col = "red")
#'
#' pp2 <- mixedcurve::pp(
#'   points = data.frame(x = runif(100, 0, 1), y = runif(100, 0, 1)),
#'   win = list(c(0, 1)),
#'   covs = list()
#' )
#' plot(pp2)

pp <- function(points, win, covs) {
  if (!is.data.frame(points)) {
    stop("points must be a data.frame")
  }
  if (!is.list(win)) {
    stop("win must be a list")
  }
  if (!is.list(covs)) {
    stop("win must be a list")
  }
  structure(list(points = points, win = win, covs = covs), class = "pp")
}

# }}} pp()

# {{{ plot.pp()
#' plot.pp
#'
#' plot a pp object
#' @param pp - the pp to plot
#' @param main - main title of plot
#' @param tick_col - color of tick marks
#' @param ... - parms passed to base plot()
#' @author Jordan Schupbach
#' @export
#' @examples
#' print("Hello World")
plot.pp <- function(pp, main = "Density of X",
                    tick_col = "black", ...) {
  dims <- ncol(pp$points)
  if (dims == 1) {
    plot(density(pp$points[, 1]), main = main, ...)
    rug(pp$points[, 1], col = tick_col)
  } else if (dims == 2) {
    minx <- min(pp$points[, 1])
    maxx <- max(pp$points[, 1])
    miny <- min(pp$points[, 2])
    maxy <- max(pp$points[, 2])
    plot(pp$points, xlim = c(minx, maxx),
         ylim = c(miny, maxy), ...)
  }
}

# }}} plot.pp()

# {{{ as.data.frame.pp()
#' as.data.frame.pp
#'
#' coerce a pp object into a dataframe (loses all but point info)
#' @param pp - pp object
#' @author Jordan Schupbach
#' @export
#' @examples
#' pp1 <- mixedcurve::pp(
#'     points = data.frame(x = runif(100, 0, 1)),
#'     win = list(c(0, 1)),
#'     covs = list()
#' )
#' df1 <- mixedcurve::as.data.frame(pp)
as.data.frame.pp <- function(pp) {
  as.data.frame(pp$points)
}
# }}} as.data.frame.pp()

# {{{ is.pp()

# TODO: write docs
as_pp <- function(x) {
  structure(list(points = x$points,
                 win = x$win,
                 covs = x$covs),
            class = "pp")
}

# }}} is.pp()

# {{{ is.pp()

# TODO: write docs
is_pp <- function(x) {
  class(x) == "pp"
}

# }}} is.pp()

# {{{ rppp_r()

#' Generate an inhomogeneous poisson point proccess
#'
#' Uses method of (TODO: source method for ihppp)
#' @param fun the intensity function
#' @param dom a domain for the intensity function
#' @param lmax estimated max for the domain
#' @author Jordan Schupbach
#' @export
#' @examples
#' lmax <- mixedcurve::estimate_max(mixedcurve::bart_simpson, c(0,6), 10000)
#' system.time(x <- mixedcurve::rppp_r(mixedcurve::bart_simpson, c(0,6), lmax))
rppp_r <- function(lambda, dom, lmax) {
  fmax <- lmax
  lambda_ret <- function(x, fmax) {
    lambda(x) / fmax
  }
  lambda_ret_p <- function(x) {
    lambda_ret(x, fmax)
  }
  vlambda_ret <- Vectorize(lambda_ret_p)
  unifx <- runif(rpois(1, fmax), dom[1], dom[2])
  retained <- as.logical(rbinom(length(unifx), 1, vlambda_ret(unifx)))
  x <- unifx[retained]
  x <- x[!is.na(x)]
  x
}
# }}} rppp_r()

# {{{ rppp_f()
#' Generate an inhomogeneous poisson point proccess
#'
#' Uses method of (TODO: source method for ihppp)
#' @param fun - a fun object whose function is the intensity
#' @author Jordan Schupbach
#' @export
#' @examples
#' lmax <- mixedcurve::estimate_max(mixedcurve::bart_simpson, c(0,6), 10000)
#' x <- mixedcurve::rppp_f(mixedcurve::fun(mixedcurve::bart_simpson, c(0,6))
rppp_f <- function(fun) {
  lambda <- fun$f
  dom <- fun$dom
  # TODO: Error handle dims of dom
  lmax <- tmc::emax(fun)
  fmax <- lmax
  lambda_ret <- function(x, fmax) {
    lambda(x) / fmax
  }
  lambda_ret_p <- function(x) {
    lambda_ret(x, fmax)
  }
  vlambda_ret <- Vectorize(lambda_ret_p)
  unifx <- runif(rpois(1, fmax), dom[[1]][1], dom[[1]][2])
  #TODO: diagnose NA warnings
  retained <- as.logical(rbinom(length(unifx), 1,
                                vlambda_ret(unifx)))
  x <- unifx[retained]
  x <- x[!is.na(x)]
  x
}
# }}} rppp_f()

# {{{ seq_centroids
#' seq_centroids
#'
#' Creates a fixed-width sequence and returns centroids in the sequence
#' @param from - left endpoint
#' @param to - right endpoint
#' @param length.out - length of returned centroids
#' @author Jordan Schupbach
#' @export
#' @examples
#' mixedcurve::seq_centroids(0, 1, 100)
seq_centroids <- function(from, to, length.out) {
  xseq <- seq(from = from, to = to, length.out = length.out + 1)
  half_width <- diff(xseq)[1] / 2
  xseq[1:(length(xseq) - 1)] + half_width
}
# }}} seq_centroids

# {{{ get_bt_counts

#' get_bt_counts
#'
#' Gets centroids and counts for a fine-pixel approximation
#' @param frame - a pp object to construct fine-pixel counts
#' @param cells - number of cells in fine-pixel approx
#' @author Jordan Schupbach
#' @export
#' @examples
#' pp1 <- mixedcurve::pp(data.frame(x = runif(100, 0, 1)),
#'                       list(c(0, 1)), list())
#' mixedcurve::get_bt_counts(pp1)
get_bt_counts <- function(frame, cells = 100) {
  if (length(frame$win) == 1) {
    left <- frame$win[[1]][1]
    right <- frame$win[[1]][2]
    xseq <- seq(left, right, length.out = (cells + 1))
    cts <- hist(frame$points[, 1][frame$points[, 1] > left &&
                                    frame$points[, 1] < right],
                breaks = xseq, plot = FALSE)$counts
    centroids <- seq_centroids(left, right, cells)
    return(list(counts = cts, centroids = centroids))
  } else {
    stop("function only implemented for dimension 1")
  }
}

# }}} get_bt_counts

# Local Variables:
# eval: (origami-mode t)
# End:
