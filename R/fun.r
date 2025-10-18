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

# {{{ Description
# Name: fun.r
# Author: Jordan Schupbach
# Description: A function class
# }}} Description

# {{{ fun()

#' fun
#'
#' A function class
#' @param lambda - a function object
#' @param dom - domain of the function

#' @author Jordan Schupbach
#' @export
#' @examples
#' myfun <- fun((function(x) sqrt(x)),
#'   dom = c(0, 1000)
#' )
#' plot.fun(myfun)
#' @export
fun <- function(lambda, dom) {
  if (!is.function(lambda)) stop("lambda must be a function")
  # TODO: get number of args and generalize domain
  structure(list(f = lambda, domain = dom), class = "fun")
}

# }}} fun()

# {{{ plot.fun()
plot.fun <- function(lambda, dom = NULL, evals = 1000,
                     add = FALSE, ...) {
  if (is.null(dom)) {
    dom <- lambda$dom
  }
  est_max <- emax(lambda)
  est_min <- emin(lambda)
  xseq <- seq(lambda$dom[1], lambda$dom[2], evals)
  if (!add) {
    plot(xseq, lambda$f(xseq), type = "l", ylim = c(est_min, est_max), ...)
  } else {
    lines(xseq, lambda$f(xseq), type = "l", ...)
  }
}
# }}} plot.fun()

# {{{ emin() # TODO: use random sampling
emin <- function(fun, dom = NULL, n = 10000) {
  if (is.null(dom)) {
    dom <- fun$dom
  }
  xseq <- seq(dom[1], dom[2], length.out = n)
  yseq <- fun[["f"]](xseq)
  fmin <- min(yseq)
  return(fmin)
}
# }}} emin()

# {{{ emax() # TODO: use random sampling
emax <- function(fun, dom = NULL, n = 10000) {
  if (is.null(dom) && (class(fun) == "fun")) {
    dom <- fun$dom
  }
  if (length(dom) > 1 && (class(dom) == "list")) {
    stop("Implemented for 1d funs only")
  }
  # TODO: generalize to more dims
  xseq <- seq(dom[[1]][1], dom[[1]][2], length.out = n)
  yseq <- fun[["f"]](xseq)
  fmin <- max(yseq)
  fmin
}
# }}} emax()

# Local Variables:
# eval: (origami-mode t)
# End:
