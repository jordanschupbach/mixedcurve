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

# {{{ Shift and rotate
#' Shift and rotate a function
#'
#' This function takes a 1D function and returns a new function that is shifted and rotated in a multi-dimensional space.
#' The rotation is done around the center of the unit cube in the specified dimension.
#'
#' @param func A function that takes a numeric input and returns a numeric output.
#' @param dom A numeric vector of length 2 specifying the domain of the function (default is c(0, 1)).
#' @param dim An integer specifying the dimension of the space (default is 2).
#' @return A new function that takes a numeric matrix as input and returns a numeric vector as output.
#'
#' @export
#'
#' @examples
#' # Define a simple 1D function
#' simple_func <- function(x) { sin(2 * pi * x) }
#'
#' # Create a shifted and rotated version of the function in 2D
#' shifted_rotated_func <- mixedcurveshift_and_rotate(simple_func, dim = 2)
#'
#' # Evaluate the new function on a grid of points in 2D
#' neval <- 100
#' grid_points <- as.matrix(expand.grid(seq(0, 1, length.out = neval),
#'                                      seq(0, 1, length.out = neval)))
#' values <- shifted_rotated_func(grid_points)
#'
#' # Plot the results
#' image(matrix(values, nrow = neval, ncol = neval), main = "Shifted and Rotated Function in 2D")
shift_and_rotate <- function(func, dom = c(0, 1), dim = 2) {
  return(function(x, ...) {
    if (is.matrix(x)) {
      return(apply(x, 1, 
                   function(tx) {
                     # Call func with the distance and any extra arguments passed to this function
                     func(mixedcurve::distance(tx, rep(0.5, dim)) * 1 / (mixedcurve::distance(rep(0, dim), rep(1, dim)) / 2), ...)
                   }))
    } else {
      stop("Input x must be a matrix.")
    }
  })
}
# }}} Shift and rotate

# {{{ mdoppler

mdoppler_1d <- function(x, alpha = 20, beta = 0.25) {
  sin(alpha / (x + beta))
}

#' Modified doppler function (generalized to any dim)
#'
#' A modification of the doppler function (see Donoho and Johnstone 1994)
#' generalized to any dimension. The generalization involves rotating the 1d
#' function around the center of the unit cube.
#
#' @param x A numeric matrix of points in [0, 1]^d or a numeric vector in [0, 1]
#' @param alpha A numeric value controlling the frequency of oscillations (default is 20)
#' @param beta A numeric value controlling the shift of the function (default is 0.25)
#' @return A numeric vector of the same length as the number of rows in x (if x is a matrix)
#' or the same length as x (if x is a vector)
#' @export
#'
#' @examples
#' # 2d example
#' neval <- 100
#' image(matrix(mixedcurve::mdoppler(as.matrix(expand.grid(seq(0, 1, length.out = neval),
#'                                seq(0, 1, length.out = neval)))),
#'        nrow = neval, ncol = neval))
#'
#' # 1d example
#' neval <- 1000
#' xseq <- seq(0, 1, length.out = neval)
#' plot(xseq, mixedcurve::mdoppler(xseq), type = 'l', xlab ='x', ylab = 'mdoppler(x)', main = 'Modified Doppler Function (1d)')
mdoppler <- function(x, alpha = 20, beta = 0.25) { # add dom
  if(is.matrix(x)) {
    return(shift_and_rotate(mdoppler_1d, dim = ncol(x))(x))
  } else if(is.vector(x) & !is.list(x)) {
    return(mdoppler_1d(x, alpha, beta))
  }
  else {
    stop("x must be a matrix or vector")
  }
}

### #' Modified doppler function (generalized to any dim)
### #'
### #' A modification of the doppler function (see Donoho and Johnstone 1994)
### #' generalized to any dimension. The generalization involves rotating the 1d
### #' function around the center of the unit cube.
### #
### #' @param x A numeric matrix of points in [0, 1]^d or a numeric vector in [0, 1]
### #' @param alpha A numeric value controlling the frequency of oscillations (default is 20)
### #' @param beta A numeric value controlling the shift of the function (default is 0.25)
### #' @return A numeric vector of the same length as the number of rows in x (if x is a matrix)
### #' or the same length as x (if x is a vector)
### #' @export
### #'
### #' @examples
### #' # 2d example
### #' neval <- 100
### #' image(matrix(mixedcurve::mdoppler(as.matrix(expand.grid(seq(0, 1, length.out = neval),
### #'                                seq(0, 1, length.out = neval)))),
### #'        nrow = neval, ncol = neval))
### #'
### #' # 1d example
### #' neval <- 1000
### #' xseq <- seq(0, 1, length.out = neval)
### #' plot(xseq, mixedcurve::mdoppler(xseq), type = 'l', xlab ='x', ylab = 'mdoppler(x)', main = 'Modified Doppler Function (1d)')
### mdoppler <- function(x, alpha = 20, beta = 0.25) {
###   if (is.matrix(x)) {
###     dim <- ncol(x)
###     return(apply(x, 1, 
###           function(tx) {
###             sin(alpha/(mixedcurve::distance(tx, rep(0.5, dim)) + beta)) * 
###               (mixedcurve::distance(rep(0, dim), rep(1, dim)) / 2)
###           }))
###   } else if(is.vector(x) & !is.list(x)) {
###     return(apply(as.matrix(x), 1, 
###           function(tx) {
###             sin(alpha/(tx + beta))
###           }))
###   }
###   else {
###     stop("x must be a matrix or vector")
###   }
### }

# }}} mdoppler (multi-d)

# {{{ Cuevas Functions

# {{{ m1

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m1(x, 1), type = 'l', xlab ='t', ylab = 'm1(t,i)', main = 'Cuevas: m1(t,i)')
#' lines(x, mixedcurve::m1(x, 2), col = 2)
#' lines(x, mixedcurve::m1(x, 3), col = 3)
#' legend("topright", legend = c("m1(t,1)", "m1(t,2)", "m1(t,3)"), col = 1:3, lty = 1)
m1_1d <- function(t, i) {
  t * (1 - t)
}

m1 <- function(t, i) {
  if (is.matrix(t)) {
    return(shift_and_rotate(m1_1d, dim = ncol(t))(t, i))  # Pass i through ...
  } else if (is.vector(t) & !is.list(t)) {
    return(m1_1d(t, i))
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m1

# {{{ m2

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m2(x, 1), type = 'l', xlab ='t', ylab = 'm2(t,i)', main = 'Cuevas: m2(t,i)')
#' lines(x, mixedcurve::m2(x, 2), col = 2)
#' lines(x, mixedcurve::m2(x, 3), col = 3)
#' legend("topright", legend = c("m2(t,1)", "m2(t,2)", "m2(t,3)"), col = 1:3, lty = 1)
m2_1d <- function(t, i) {
  t^i * (1 - t)^(6 - i)
}

m2 <- function(t, i) {
  if (is.matrix(t)) {
    return(shift_and_rotate(m2_1d, dim = ncol(t))(t, i))  # Pass i through ...
  } else if (is.vector(t) & !is.list(t)) {
    return(m2_1d(t, i))
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m2

# {{{ m3

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m3(x, 1), type = 'l', xlab ='t', ylab = 'm3(t,i)', main = 'Cuevas: m3(t,i)')
#' lines(x, mixedcurve::m3(x, 2), col = 2)
#' lines(x, mixedcurve::m3(x, 3), col = 3)
#' legend("topright", legend = c("m3(t,1)", "m3(t,2)", "m3(t,3)"), col = 1:3, lty = 1)
m3_1d <- function(t, i) {
  t^(i / 5) * (1 - t)^(6 - i / 5)
}

m3 <- function(t, i) {
  if (is.matrix(t)) {
    return(shift_and_rotate(m3_1d, dim = ncol(t))(t, i))  # Pass i through ...
  } else if (is.vector(t) & !is.list(t)) {
    return(m3_1d(t, i))
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m3

# {{{ m4

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m4(x, 1), type = 'l', xlab ='t', ylab = 'm4(t,i)', main = 'Cuevas: m4(t,i)')
#' lines(x, mixedcurve::m4(x, 2), col = 2)
#' lines(x, mixedcurve::m4(x, 3), col = 3)
#' legend("topright", legend = c("m4(t,1)", "m4(t,2)", "m4(t,3)"), col = 1:3, lty = 1)
m4_1d <- function(t, i) {
  rep(1 + i / 50, length(t))
}

m4 <- function(t, i) {
  if (is.matrix(t)) {
    return(shift_and_rotate(m4_1d, dim = ncol(t))(t, i))
  } else if (is.vector(t) & !is.list(t)) {
    return(m4_1d(t, i))
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m4

# {{{ m5

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m5(x, 1), type = 'l', xlab ='t', ylab = 'm5(t,i)', main = 'Cuevas: m5(t,i)', ylim = c(0, 0.1))
#' lines(x, mixedcurve::m5(x, 2), col = 2)
#' lines(x, mixedcurve::m5(x, 3), col = 3)
#' legend("topright", legend = c("m5(t,1)", "m5(t,2)", "m5(t,3)"), col = 1:3, lty = 1)
m5_1d <- function(t, i) {
  (i - 1) * 0.01 * dbeta(t, 6, 6)
}

m5 <- function(t, i) {
  if (is.matrix(t)) {
    return(shift_and_rotate(m5_1d, dim = ncol(t))(t, i))
  } else if (is.vector(t) & !is.list(t)) {
    return(m5_1d(t, i))
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m5

# }}} Functions

# {{{ generate fanova data

gen_1d_fanova_data <- function(f = tmc::m3, bounds = c(0, 1), n = 10, ngrp = 3, nx = 200,
                               balanced = FALSE, pgrp = sample, pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
                               sigma = 0.05, systematic = FALSE, px = runif, pxargs = list(min = 0, max = 1),
                               white_noise = TRUE, cov_scale = 0.05, gpn = 1000) {
  ntotal <- n * ngrp
  if (balanced) {
    grps <- rep(1:ngrp, each = n)
  } else {
    grps <- do.call(pgrp, pgrpargs)
  }
  ylist <- list()
  xlist <- list()
  grplist <- list()
  idlist <- list()
  if (!white_noise) {
    delta_i <- geoR::grf(gpn, nx = 1, grid = "reg",
                         xlims = c(bounds[1], bounds[2]),
                         cov.model = "gaussian",
                         cov.pars = c(sigma, cov_scale),
                         nugget = 0.0,
                         nsim = ntotal, messages = FALSE)[[2]]
  }
  for (i in 1:ntotal) {
    if (systematic) {
      xlist[[i]] <- seq(bounds[1], bounds[2], length.out = nx)
    } else {
      xlist[[i]] <- do.call(px, c(list(n = nx), pxargs))
    }
    grplist[[i]] <- rep(grps[i], each = nx)
    ylist[[i]] <- f(xlist[[i]], grps[i])
    if (white_noise) {
      ylist[[i]] <- ylist[[i]] + rnorm(nx, 0, sigma)
    } else {
      grid_points <- seq(bounds[1], bounds[2], length.out = gpn)
      idx <- sapply(xlist[[i]], function(xi) {
        which.min(abs(grid_points - xi))
      })
      ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
    }
    idlist[[i]] <- rep(i, nx)
  }
  data.frame(x = unlist(xlist),
             y = unlist(ylist),
             grp = as.factor(unlist(grplist)),
             id = as.factor(unlist(idlist)))
}
# }}} generate fanova data

# Local Variables:
# eval: (origami-mode t)
# End:
