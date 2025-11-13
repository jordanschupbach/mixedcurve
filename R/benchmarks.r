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
#' This function takes a 1D function and returns a new function that
#' is shifted and rotated in a multi-dimensional space.
#'
#' The rotation is done around the center of the unit cube in
#' the specified dimension.
#'
#' @param func A function that takes a numeric input and
#'             returns a numeric output.
#' @param dom A numeric vector of length 2 specifying the
#'            domain of the function (default is c(0, 1)).
#' @param dim An integer specifying the dimension of the space (default is 2).
#' @return A new function that takes a numeric matrix as input and returns
#'         a numeric vector as output.
#'
#' @export
#'
#' @examples
#' # Define a simple 1D function
#' simple_func <- function(x) { sin(2 * pi * x) }
#'
#' # Create a shifted and rotated version of the function in 2D
#' shifted_rotated_func <- mixedcurve::shift_and_rotate(simple_func, dim = 2)
#'
#' # Evaluate the new function on a grid of points in 2D
#' neval <- 100
#' grid_points <- as.matrix(expand.grid(seq(0, 1, length.out = neval),
#'                                      seq(0, 1, length.out = neval)))
#' values <- shifted_rotated_func(grid_points)
#'
#' # Plot the results
#' image(matrix(values, nrow = neval, ncol = neval),
#'       main = "Shifted and Rotated Function in 2D")
shift_and_rotate <- function(func, dom = c(0, 1), dim = 2) {
  return(function(x, ...) {
    if (is.matrix(x)) {
      apply(x, 1,
            function(tx) {
              # Call func with the distance and any extra arguments
              # passed to this function
              func(mixedcurve::distance(tx, rep(0.5, dim)) * 1 /
                     (mixedcurve::distance(rep(0, dim), rep(1, dim)) / 2), ...)
            })
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
#' @param alpha A numeric value controlling the frequency
#'              of oscillations (default is 20)
#' @param beta A numeric value controlling the shift of
#'             the function (default is 0.25)
#' @return A numeric vector of the same length as the number
#'         of rows in x (if x is a matrix)
#' or the same length as x (if x is a vector)
#' @export
#'
#' @examples
#' # 2d example
#' neval <- 100
#' image(matrix(mixedcurve::mdoppler(
#'   as.matrix(expand.grid(seq(0, 1, length.out = neval),
#'                         seq(0, 1, length.out = neval)))),
#'        nrow = neval, ncol = neval))
#'
#' # 1d example
#' neval <- 1000
#' xseq <- seq(0, 1, length.out = neval)
#' plot(xseq, mixedcurve::mdoppler(xseq), type = 'l', xlab ='x',
#'      ylab = 'mdoppler(x)', main = 'Modified Doppler Function (1d)')
mdoppler <- function(x, i = 0, alpha = 20, beta = 0.25) { # add dom
  if (is.matrix(x)) {
    shift_and_rotate(mdoppler_1d, dim = ncol(x))(x)
  } else if (is.vector(x) && !is.list(x)) {
    return(mdoppler_1d(x, alpha, beta))
  } else {
    stop("x must be a matrix or vector")
  }
}

# }}} mdoppler (multi-d)

# {{{ Cuevas Functions

# {{{ m1

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m1(x, 1), type = "l", xlab = "t",
#'      ylab = "m1(t, i)", main = "Cuevas: m1(t, i)")
#' lines(x, mixedcurve::m1(x, 2), col = 2)
#' lines(x, mixedcurve::m1(x, 3), col = 3)
#' legend("topright", legend = c("m1(t, 1)", "m1(t, 2)", "m1(t, 3)"),
#'        col = 1:3, lty = 1)
m1_1d <- function(t, i) {
  t * (1 - t)
}


#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m3(x, 1), type = "l", xlab = "t",
#'      ylab = "m3(t, i)", main = "Cuevas: m3(t, i)")
#' lines(x, mixedcurve::m3(x, 2), col = 2)
#' lines(x, mixedcurve::m3(x, 3), col = 3)
#' legend("topright", legend = c("m3(t, 1)", "m3(t, 2)", "m3(t, 3)"),
#'        col = 1:3, lty = 1)
m1 <- function(t, i) {
  if (is.matrix(t) && ncol(t) > 1) {
    shift_and_rotate(mixedcurve::m1_1d, dim = ncol(t))(t, i)
  } else if (is.matrix(t) && ncol(t) == 1) {
    mixedcurve::m1_1d(as.vector(t), i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(mixedcurve::m1_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    mixedcurve::m1_1d(t, i)
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
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m2(x, 1), type = "l", xlab = "t",
#'      ylab = "m2(t, i)", main = "Cuevas: m2(t, i)")
#' lines(x, mixedcurve::m2(x, 2), col = 2)
#' lines(x, mixedcurve::m2(x, 3), col = 3)
#' legend("topright", legend = c("m2(t,1)", "m2(t,2)", "m2(t,3)"),
#'        col = 1:3, lty = 1)
m2_1d <- function(t, i) {
  t^i * (1 - t)^(6 - i)
}


#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m3(x, 1), type = "l", xlab = "t",
#'      ylab = "m3(t, i)", main = "Cuevas: m3(t, i)")
#' lines(x, mixedcurve::m3(x, 2), col = 2)
#' lines(x, mixedcurve::m3(x, 3), col = 3)
#' legend("topright", legend = c("m3(t, 1)", "m3(t, 2)", "m3(t, 3)"),
#'        col = 1:3, lty = 1)
m2 <- function(t, i) {
  if (is.matrix(t) && ncol(t) > 1) {
    shift_and_rotate(mixedcurve::m2_1d, dim = ncol(t))(t, i)
  } else if (is.matrix(t) && ncol(t) == 1) {
    mixedcurve::m2_1d(as.vector(t), i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(mixedcurve::m2_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    mixedcurve::m2_1d(t, i)
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
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m3(x, 1), type = "l", xlab = "t",
#'      ylab = "m3(t, i)", main = "Cuevas: m3(t, i)")
#' lines(x, mixedcurve::m3(x, 2), col = 2)
#' lines(x, mixedcurve::m3(x, 3), col = 3)
#' legend("topright", legend = c("m3(t, 1)", "m3(t, 2)", "m3(t, 3)"),
#'        col = 1:3, lty = 1)
m3_1d <- function(t, i) {
  t^(i / 5) * (1 - t)^(6 - i / 5)
}


#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m3(x, 1), type = "l", xlab = "t",
#'      ylab = "m3(t, i)", main = "Cuevas: m3(t, i)")
#' lines(x, mixedcurve::m3(x, 2), col = 2)
#' lines(x, mixedcurve::m3(x, 3), col = 3)
#' legend("topright", legend = c("m3(t, 1)", "m3(t, 2)", "m3(t, 3)"),
#'        col = 1:3, lty = 1)
m3 <- function(t, i) {
  if (is.matrix(t) && ncol(t) > 1) {
    shift_and_rotate(mixedcurve::m3_1d, dim = ncol(t))(t, i)
  } else if (is.matrix(t) && ncol(t) == 1) {
    mixedcurve::m3_1d(as.vector(t), i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(mixedcurve::m3_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    mixedcurve::m3_1d(t, i)
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
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m4(x, 1), type = "l", xlab = "t",
#'      ylab = "m4(t, i)", main = "Cuevas: m4(t, i)")
#' lines(x, mixedcurve::m4(x, 2), col = 2)
#' lines(x, mixedcurve::m4(x, 3), col = 3)
#' legend("topright", legend = c("m4(t, 1)", "m4(t, 2)", "m4(t, 3)"),
#'        col = 1:3, lty = 1)
m4_1d <- function(t, i) {
  rep(1 + i / 50, length(t))
}

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m4(x, 1), type = "l", xlab = "t",
#'      ylab = "m4(t, i)", main = "Cuevas: m4(t, i)")
#' lines(x, mixedcurve::m4(x, 2), col = 2)
#' lines(x, mixedcurve::m4(x, 3), col = 3)
#' legend("topright", legend = c("m4(t, 1)", "m4(t, 2)", "m4(t, 3)"),
m4 <- function(t, i) {
  if (is.matrix(t) && ncol(t) > 1) {
    shift_and_rotate(mixedcurve::m4_1d, dim = ncol(t))(t, i)
  } else if (is.matrix(t) && ncol(t) == 1) {
    mixedcurve::m4_1d(as.vector(t), i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(mixedcurve::m4_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    mixedcurve::m4_1d(t, i)
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
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m5(x, 1), type = "l", xlab = "t",
#'      ylab = "m5(t, i)", main = "Cuevas: m5(t, i)", ylim = c(0, 0.1))
#' lines(x, mixedcurve::m5(x, 2), col = 2)
#' lines(x, mixedcurve::m5(x, 3), col = 3)
#' legend("topright", legend = c("m5(t,1)", "m5(t,2)", "m5(t,3)"),
#'        col = 1:3, lty = 1)
#' m5_1d <- function(t, i) {
#'   (i - 1) * 0.01 * dbeta(t, 6, 6)
#' }
m5_1d <- function(t, i) {
  (i - 1) * 0.01 * dbeta(t, 6, 6)
}

#' Functions m1 to m5 as defined in Cuevas et al. (2004)
#' @param t A numeric vector of points in [0, 1]
#' @param i An integer in {1, 2, 3} (technically any i works)
#' @return A numeric vector of the same length as t
#' @export
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' plot(x, mixedcurve::m5(x, 1), type = "l", xlab = "t",
#'      ylab = "m5(t, i)", main = "Cuevas: m5(t, i)", ylim = c(0, 0.1))
#' lines(x, mixedcurve::m5(x, 2), col = 2)
#' lines(x, mixedcurve::m5(x, 3), col = 3)
#' legend("topright", legend = c("m5(t,1)", "m5(t,2)", "m5(t,3)"),
#'        col = 1:3, lty = 1)
#' m5_1d <- function(t, i) {
#'   (i - 1) * 0.01 * dbeta(t, 6, 6)
#' }
m5 <- function(t, i) {
  if (is.matrix(t) && ncol(t) > 1) {
    shift_and_rotate(mixedcurve::m5_1d, dim = ncol(t))(t, i)
  } else if (is.matrix(t) && ncol(t) == 1) {
    mixedcurve::m5_1d(as.vector(t), i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(mixedcurve::m5_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    mixedcurve::m5_1d(t, i)
  } else {
    stop("t must be a matrix or vector")
  }
}

# }}} m5

# }}} Functions

# {{{ generate fanova data

# NOTE: deprecated
gen_1d_fanova_data <- function(f = tmc::m3, bounds = c(0, 1),
                               n = 10, ngrp = 3, nx = 200,
                               balanced = FALSE, pgrp = sample,
                               pgrpargs = list(x = 1:3,
                                               size = 10,
                                               replace = TRUE),
                               sigma = 0.05, systematic = FALSE,
                               px = runif, pxargs = list(min = 0, max = 1),
                               white_noise = TRUE,
                               cov_scale = 0.05, gpn = 1000) {
  if (balanced) {
    ntotal <- n * ngrp
    grps <- rep(1:ngrp, each = n)
  } else {
    ntotal <- n
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

# {{{ gen_1d_mc_data_wn
# NOTE: this is deprecated
gen_1d_mc_data_wn <- function(m,
                              bounds = c(0, 1),
                              ngroups = 3, ni = 6, nj = 5, nk = 25,
                              noise_tau = 0.008, rep_tau = 0.008,
                              ind_tau = 0.008, systematic = TRUE) {
  n <- ni * nj * nk
  list_m <- lapply(1:ngroups, function(i) {
    function(t) m(t, i)
  })
  grp <- factor(rep(sample(1:ngroups, ni * nj, replace = TRUE), each = nk))
  mu_j <- rnorm(ni * nj, mean = 0, sd = rep_tau)
  mu_i <- rnorm(ni, mean = 0, sd = ind_tau)
  if (systematic) {
    x <- rep(seq(bounds[1], bounds[2], length.out = nk), times = ni * nk)
  } else {
    x <- runif(n, bounds[1], bounds[2])
  }
  ytrue <- rowSums(sapply(1:ngroups, function(i) (grp == i) * list_m[[i]](x)))
  y <- ytrue + rep(mu_i, each = nj * nk) +
    rep(mu_j, each = nk) + rnorm(n, mean = 0.0, sd = noise_tau)
  ind <- factor(rep(1:ni, each = nj * nk))
  rep <- factor(rep(rep(1:nj, each = nk), times = ni))
  id <- factor(rep(1:(ni * nj), each = nk))
  ret <- data.frame(x = x, y = y, ind = ind, grp = grp, rep = rep, id = id)
  list(df = ret, mu_i = mu_i, mu_j = mu_j, ytrue = ytrue)
}

# }}} gen_1d_mc_data_wn

# {{{ gen_1d_mc_data_gp
gen_1d_mc_data_gp <- function(m, ngroups = 3, ni = 6, nj = 5, nk = 25,
                              systematic = TRUE,
                              bounds = c(0, 1),
                              ind_tau = 0.000001,
                              rep_tau = 0.0000001,
                              noise_tau = 0.0000001,
                              gp_n = 1000, gp_cov_pars = c(0.0001, 0.05)) {
  #' n <- ni * nj * nk
  #' list_m <- lapply(1:ngroups, function(i) {
  #'   function(t) m(t, i)
  #' })
  grp <- factor(rep(sample(1:ngroups, ni * nj, replace = TRUE), each = nk))
  delta_i <- geoR::grf(gp_n, nx = 1, grid = "reg",
                       xlims = c(0, 1),
                       cov.model = "gaussian",
                       cov.pars = c(ind_tau, 0.05),
                       nugget = 0.0,
                       nsim = ni, messages = FALSE)[[2]]
  delta_ij <- geoR::grf(gp_n, nx = 1, grid = "reg",
                        xlims = c(0, 1),
                        cov.model = "gaussian",
                        cov.pars = c(rep_tau, 0.05),
                        nugget = 0.0,
                        nsim = ni * nj, messages = FALSE)[[2]]
  delta_ijk <- geoR::grf(gp_n, nx = 1, grid = "reg",
                         xlims = c(0, 1),
                         cov.model = "gaussian",
                         cov.pars = c(noise_tau, 0.05),
                         nugget = 0.0,
                         nsim = ni * nj, messages = FALSE)[[2]]
  grid_points <- seq(0, 1, length.out = gp_n)
  ylist <- list()
  xlist <- list()
  grplist <- list()
  indlist <- list()
  replist <- list()
  ij_idx <- 0
  for (i in 1:ni) {
    for (j in 1:nj) {
      ij_idx <- ij_idx + 1
      if (systematic) {
        tx <- seq(0, 1, length.out = nk)
      } else {
        tx <- runif(nk)
      }
      idx <- sapply(tx, function(xi) {
        which.min(abs(grid_points - xi))
      })
      tdi <- delta_i[idx, i]
      tdij <- delta_ij[idx, ij_idx]
      tdijk <- delta_ijk[idx, ij_idx]
      grp <- sample(1:ngroups, 1)
      ylist[[ij_idx]] <- (grp == 1) * m(tx, 1) +
        (grp == 2) * m(tx, 2) +
        (grp == 3) * m(tx, 3) +
        tdi + tdij + tdijk
      grplist[[ij_idx]] <- rep(grp, nk)
      xlist[[ij_idx]] <- tx
      indlist[[ij_idx]] <- rep(i, nk)
      replist[[ij_idx]] <- rep(j, nk)
    }
  }
  tdf <- data.frame(x = unlist(xlist),
                    y = unlist(ylist),
                    grp = factor(unlist(grplist)),
                    ind = factor(unlist(indlist)),
                    rep = factor(unlist(replist)),
                    id = factor(rep(1:(ni * nj), each = nk)))
  tdf
}
# }}} gen_1d_mc_data_gp

# {{{ gen_fanova_data

#' Generate functional ANOVA data
#'
#' This function generates synthetic functional data for functional ANOVA
#' analysis. It allows for multiple groups, different sampling schemes,
#' and the addition of noise.
#' @param f A function that takes a numeric matrix of points and a group
#          identifier and returns a numeric vector of function values.
#          Default is tmc::m3.
#' @param bounds A numeric vector of length 2 specifying the domain
#               of the function, or a list of such vectors for
#               multi-dimensional data. Default is c(0, 1).
#' @param n An integer specifying the number of samples to generate.
#          If balanced is TRUE, this is the number of samples per group.
#          Default is 10.
#' @param ngrp An integer specifying the number of groups. Default is 3.
#' @param nx An integer specifying the number of points to sample
#           for each function. Default is 200.
#' @param balanced A logical indicating whether to generate a balanced
#                 design (TRUE) or an unbalanced design (FALSE).
#                 Default is FALSE.
#' @param pgrp A function to sample group identifiers for an unbalanced
#             design. Default is sample.
#' @param pgrpargs A list of arguments to pass to pgrp. Default is
#                  list(x = 1:3, size = 10, replace = TRUE).
#' @param sigma A numeric value specifying the standard deviation of
#              the noise to add to the function values. Default is 0.05.
#' @param systematic A logical indicating whether to sample points
#                   systematically (TRUE) or randomly (FALSE).
#                   Default is FALSE.
#' @param px A function to sample points for each function. Default is runif.
#' @param pxargs A list of arguments to pass to px. Default is
#                 list(list(min = 0, max = 1)).
#' @param white_noise A logical indicating whether to add white noise
#                    (TRUE) or spatially correlated noise (FALSE).
#                    Default is TRUE.
#' @param cov_scale A numeric value specifying the scale parameter
#                 for the spatially correlated noise. Default is 0.05.
#' @param gpn An integer specifying the number of grid points
#            to use for generating the spatially correlated noise.
#'            Default is 1000.
#' @return A fundata class object (list) containing:
#'         - df: A data frame with columns for the function values (y),
#'               group identifiers (grp), sample identifiers (id),
#'               and input points (x1, x2, ..., xd).
#'         - coreset: A data frame containing one representative point
#'                     from each sample.
#'         - dims: An integer specifying the number of dimensions.
#'         - nx: An integer specifying the number of points per function.
#'         - systematic: A logical indicating whether the points
#                       were sampled systematically.
#' @export
#'
#' @examples
#' # Generate 2D functional ANOVA data on a 20x20 grid
#' nxy <- 20
#' data2 <- mixedcurve::gen_fanova_data(
#'   f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
#'   n = 10, ngrp = 3, nx = nxy,
#'   balanced = FALSE, pgrp = sample,
#'   pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
#'   sigma = 0.0005, systematic = TRUE,
#'   px = runif,
#'   pxargs = list(
#'     list(min = 0, max = 1),
#'     list(min = 0, max = 1)
#'   ),
#'   white_noise = TRUE,
#'   cov_scale = 0.05, gpn = 1000
#' )
#' mixedcurve::dark_mode()
#' par(mfrow = c(3, 3))
#' mixedcurve::plot.fundata(data2, ncurves = 9)
#'
#'
#' # Generate 2D functional ANOVA data with random sampling
#' nxy <- 500
#' data2 <- mixedcurve::gen_fanova_data(
#'   f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
#'   n = 10, ngrp = 3, nx = nxy,
#'   balanced = FALSE, pgrp = sample,
#'   pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
#'   sigma = 0.0005, systematic = FALSE,
#'   px = runif,
#'   pxargs = list(
#'     list(min = 0, max = 1),
#'     list(min = 0, max = 1)
#'   ),
#'   white_noise = TRUE,
#'   cov_scale = 0.05, gpn = 1000
#' )
#' mixedcurve::dark_mode()
#' mixedcurve::plot.fundata(data2, ncurves = 9)
gen_fanova_data <- function(f = mixedcurve::m3, bounds = c(0, 1),
  n = 10, ngrp = 3, nx = 200,
  balanced = FALSE,
  sigma = 0.05, systematic = FALSE,
  px = runif,
  pxargs = list(list(min = 0, max = 1)),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 100, family = "gaussian",
  pgrp = NULL,
  pgrpargs = NULL
) {
  if (is.null(pgrp) && !is.null(pgrpargs) ||
        (!is.null(pgrp) && is.null(pgrpargs))) {
    stop("Both pgrp and pgrpargs must be provided or both must be NULL.")
  }
  if (is.vector(bounds) && !is.list(bounds)) {
    dims <- 1
    tbounds <- as.matrix(bounds, nrow = 1)
  } else if (is.list(bounds)) {
    dims <- length(bounds)
    tbounds <- do.call(rbind, bounds)
  } else if (is.matrix(bounds)) {
    dims <- nrow(bounds)
    tbounds <- bounds
  } else {
    stop("bounds must be a vector, list, or matrix")
  }
  if (balanced) {
    ntotal <- n * ngrp
  } else {
    ntotal <- n
  }
  if (is.null(pgrp)) {
    pgrp <- sample
    pgrpargs <- list(x = 1:ngrp, size = ntotal, replace = TRUE)
  }
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
    if (dims == 1) {
      delta_i <- geoR::grf(gpn, nx = 1, grid = "reg",
                           xlims = c(bounds[[1]][1], bounds[[1]][2]),
                           cov.model = "gaussian",
                           cov.pars = c(sigma, cov_scale),
                           nugget = 0.0,
                           nsim = ntotal, messages = FALSE)[[2]]
    } else if (dims == 2) {
      if (systematic) {
        delta_i <- geoR::grf(nx = nx, ny = nx, grid = "reg",
                             xlims = c(bounds[[1]][1], bounds[[1]][2]),
                             ylims = c(bounds[[2]][1], bounds[[2]][2]),
                             cov.model = "gaussian",
                             cov.pars = c(sigma, cov_scale),
                             nugget = 0.0,
                             nsim = ntotal, messages = FALSE)[[2]]
      } else {
        delta_i <- geoR::grf(nx = gpn, ny = gpn, grid = "reg",
                             xlims = c(bounds[[1]][1], bounds[[1]][2]),
                             ylims = c(bounds[[2]][1], bounds[[2]][2]),
                             cov.model = "gaussian",
                             cov.pars = c(sigma, cov_scale),
                             nugget = 0.0,
                             nsim = ntotal, messages = FALSE)[[2]]
      }
    } else {
      stop("Currently only 1D and 2D data supported for spatial noise.")
    }
  }
  for (i in 1:ntotal) {
    if (systematic) {
      txlist <- list(seq(tbounds[1, 1], tbounds[1, 2], length.out = nx))
      if (dims > 1) {
        for (d in 2:dims) {
          txlist[[d]] <- seq(tbounds[d, 1], tbounds[d, 2], length.out = nx)
        }
      }
      xlist[[i]] <- as.matrix(expand.grid(txlist))
    } else {
      txlist <- as.matrix(do.call(px, c(list(n = nx), pxargs[[1]])))
      if (dims > 1) {
        for (d in 2:dims) {
          txlist <- cbind(txlist,
                          as.matrix(do.call(px, c(list(n = nx), pxargs[[d]]))))
        }
      }
      xlist[[i]] <- as.matrix(txlist)
    }
    grplist[[i]] <- rep(grps[i], each = nrow(xlist[[i]]))
    ylist[[i]] <- f(xlist[[i]], grps[i])
    if (white_noise) {
      #' tnxy <- nx ^ dims
      ylist[[i]] <- ylist[[i]] + rnorm(length(ylist[[i]]), 0, sigma)
      if (family != "gaussian") {
        if (family == "poisson") {
          ylist[[i]] <- rpois(length(ylist[[i]]), lambda = ylist[[i]])
        } else if (family == "binomial") {
          ylist[[i]] <- rbinom(length(ylist[[i]]), size = 1,
                               prob = ylist[[i]])
        } else {
          stop("Only gaussian, poisson, and binomial families supported.")
        }
      }
    } else {
      if (dims == 1) {
        grid_points <- seq(bounds[[1]][1], bounds[[1]][2], length.out = gpn)
        idx <- sapply(xlist[[i]], function(xi) {
          which.min(abs(grid_points - xi))
        })
      }
      if (dims == 1) {
        ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
      } else if (dims == 2) {
        if (!systematic) {
          grid_points <- expand.grid(seq(bounds[[1]][1],
                                         bounds[[1]][2], length.out = gpn),
                                     seq(bounds[[2]][1],
                                         bounds[[2]][2], length.out = gpn))
          idx <- unlist(sapply(seq_len(nrow(xlist[[i]])), function(j) {
            which.min(sqrt((grid_points[j, 1] - xlist[[i]][j, 1])^2 +
                             (grid_points[j, 1] - xlist[[i]][j, 2])^2))
          }))
          ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
        } else {
          ylist[[i]] <- ylist[[i]] + delta_i[, i]
        }
      }
      if (family != "gaussian") {
        if (family == "poisson") {
          ylist[[i]] <- rpois(length(ylist[[i]]), lambda = ylist[[i]])
        } else if (family == "binomial") {
          ylist[[i]] <- rbinom(length(ylist[[i]]), size = 1,
                               prob = ylist[[i]])
        } else {
          stop("Only gaussian poisson, and binomial families are supported.")
        }
      }
    }
    idlist[[i]] <- rep(i, nrow(xlist[[i]]))
  }
  tx <- do.call(rbind, xlist)
  ret <- data.frame(y = unlist(ylist),
                    grp = as.factor(unlist(grplist)),
                    id = as.factor(unlist(idlist)))
  if (dims == 1) {
    ret$x1 <- as.vector(tx)
  } else {
    for (d in 1:dims) {
      ret[[paste0("x", d)]] <- as.vector(tx[, d])
    }
  }
  if (systematic) {
    nxy <- nx ^ dims
  } else {
    nxy <- nx
  }
  coreset <- ret[(0:(ntotal - 1)) * nxy + 1, ]
  structure(list(df = ret, coreset = coreset,
              curves = list(y = ylist, x = xlist),
              dims = dims, nx = nx, systematic = systematic,
              grps = grps,
              family = family
            ),
            class = "fundata")
}

#' Plot functional ANOVA data
#'
#' This function plots functional ANOVA data generated by gen_fanova_data().
#' It supports both 1D and 2D data, with options for systematic and
#' random sampling.
#' @param fundata A fundata class object generated by gen_fanova_data().
#' @param curves A numeric vector specifying which curves (samples)
#'               to plot.
#' @param color_scale An optional color scale for 2D plots.
#' @return Plots of the specified curves.
#' @export
#'
#' @examples
#' # Generate 1D functional ANOVA data
#' data1 <- mixedcurve::gen_fanova_data(
#'   f = mixedcurve::m2, bounds = c(0, 1
#'   ), n = 5, ngrp = 3, nx = 100,
#'   balanced = TRUE, sigma = 0.01,
#'   systematic = TRUE,
#'   white_noise = TRUE
#' )
#' mixedcurve::dark_mode()
#' par(mfrow = c(2, 3))
#' mixedcurve::plot.fundata(data1, curves = 1:6)
#'
#' # Generate 2D functional ANOVA data
#' nxy <- 30
#' data2 <- mixedcurve::gen_fanova_data(
#'   f = mixedcurve::m2, bounds = list(c(0,
#'   1), c(0, 1)),
#'   n = 5, ngrp = 3, nx = nxy,
#'   balanced = TRUE, sigma = 0.0005,
#'   systematic = TRUE,
#'   white_noise = TRUE
#' )
#' mixedcurve::dark_mode()
#' par(mfrow = c(2, 3))
#' mixedcurve::plot.fundata(data2, curves = 1:6)
plot.fundata <- function(fundata, curves, color_scale = NULL) {
  if (fundata$dims == 1) {
    tdf <- fundata$df
    tcurves <- fundata$curves
    tcoreset <- fundata$coreset
    nxy <- fundata$nx
    if (fundata$systematic) {
      for (i in curves) {
        plot(tcurves$x[[i]], tdf$y[((i - 1) * nxy + 1):(i * nxy)],
          lwd = 2,
          type = "l",
          xlab = "x", ylab = "y",
          main = paste("Sample", i, " (Group", tcoreset$grp[i], ")", sep = ""),
          ylim = range(tdf$y)
        )
      }
    } else {
      for (i in curves) {
        ord <- order(tcurves$x[[i]])
        plot(tcurves$x[[i]][ord], tdf$y[((i - 1) * nxy + 1):(i * nxy)][ord],
          lwd = 2,
          type = "l",
          xlab = "x", ylab = "y",
          main = paste("Sample", i, " (Group", tcoreset$grp[i], ")", sep = ""),
          ylim = range(tdf$y)
        )
      }
    }
  } else if (fundata$dims == 2) {
    if (fundata$systematic) {
      nxy <- fundata$nx
      df2 <- fundata$df
      tcoreset <- fundata$coreset
      tcurves <- fundata$curves
      if (is.null(color_scale)) {
        color_scale <- viridis::viridis(100)
      }
      breaks <- seq(min(df2$y), max(df2$y), length.out = 101)
      for (i in curves) {
        image(matrix(tcurves$y[[i]], nxy, nxy, byrow = TRUE),
          xlab = "x1", ylab = "x2",
          main = paste("Sample ", i, " (Group", tcoreset$grp[i], ")", sep = ""),
          col = color_scale,
          breaks = breaks
        )
      }
    } else {
      tdf <- fundata$df
      tcurves <- fundata$curves
      tcoreset <- fundata$coreset
      nxy <- fundata$nx
      if (is.null(color_scale)) {
        color_scale <- viridis::viridis(100)
      }
      color_values <- cut(tdf$y, breaks = 100, labels = FALSE)
      for (i in curves) {
        plot(tcurves$x[[i]][, 1],
          tcurves$x[[i]][, 2],
          col = color_scale[color_values[((i - 1) * nxy + 1):((i) * nxy)]],
          pch = 20, cex = 2, xlab = "x1",
          ylab = "x2",
          main = paste("Sample", i, " (Group", tcoreset$grp[i], ")", sep = "")
        )
      }
    }
  } else {
    stop("plot.fundata() is implemented for 2D data only (TODO)")
  }
}

# }}} gen_fanova_data

# {{{ create_group_df(n)

create_group_df <- function(n) {
  tdf <- as.matrix(1:n[1], 1)
  for (i in 2:length(n)) {
    tgrp <- rep(1:n[i], each = prod(n[1:(i - 1)]))
    tdf <- cbind(do.call(rbind, replicate(n[i], tdf, simplify = FALSE)), tgrp)
  }
  tdf <- as.data.frame(tdf)
  colnames(tdf) <- paste0("grp", 0:(length(n) - 1))
  tdf[] <- lapply(tdf, as.factor)
  tdf
}

# }}} create_group_df(n)

# {{{ gen_hfanova_data(...)

gen_hfanova_data <- function(f, n, sigmas, bounds, ndim = 1,
                             ngrp = 3,
                             px = NULL, pxargs = NULL,
                             family = "gaussian",
                             white_noise = TRUE) {
    #' {{{ Sample x
    if (is.null(px) || is.null(pxargs)) {
      px <- runif
      if (!is.list(bounds) && ndim != 1) {
        stop("If bounds is a vector, ndim must be 1")
      }
      if (is.vector(bounds) && ndim == 1) {
        tbounds <- list()
        tbounds[[1]] <- bounds
        bounds <- tbounds
      }
      pxargs <- lapply(
        1:ndim,
        function(i) {
          list(min = bounds[[i]][1], max = bounds[[i]][2])
        }
      )
    }
    xdata <- as.data.frame(
      do.call(
        cbind,
        lapply(
          seq_along(pxargs),
          function(i) {
            do.call(px, c(list(n = prod(n)), pxargs[[i]]))
          }
        )
      )
    )
    colnames(xdata) <- paste0("x", seq_len(ncol(xdata)))
    #' }}} Sample x
    #' {{{ Create group df
    df1 <- mixedcurve::create_group_df(n)
    neffects <- unlist(lapply(seq_along(n), function(i) {
      prod(n[length(n):(length(n) - (i - 1))])
    }))
    neffects <- neffects[rev(seq_along(neffects))]
    #' }}} Create group df
    #' {{{ Sample noise
    if (white_noise) {
      noises <- lapply(
        seq_along(sigmas),
        function(i) {
          rnorm(neffects[i], 0, sigmas[i])
        }
      )
      noise_cols <- do.call(
        cbind,
        lapply(seq_along(noises),
               function(i) {
                 rep(noises[[i]], each = prod(n) / length(noises[[i]]))
               })
      )
    } else {
      stop("Only white noise currently implemented")
    }
    epsilon <- apply(noise_cols, 1, sum)
    #' }}} Sample noise
    #' {{{ Sample y
    if (family == "gaussian" ||
          (class(family) == "family" && family$family == "gaussian")) {
      y <- f(as.matrix(xdata), g) + epsilon
    } else if (family == "poisson" ||
                 (class(family) == "family" && family$family == "poisson")) {
      mu <- exp(f(as.matrix(xdata), g) + epsilon)
      y <- rpois(length(mu), mu)
    } else if (family == "binomial" ||
                 (class(family) == "family" && family$family == "binomial")) {
      stop("Binomial family not fully tested")
      mu <- plogis(f(as.matrix(xdata), g) + epsilon)
      y <- rbinom(length(mu), size = 1, prob = mu)
    } else if (family == "Gamma" ||
                 (class(family) == "family" && family$family == "Gamma")) {
      stop("Gamma family not fully tested")
      #' mu <- exp(f(as.matrix(xdata), 1) + epsilon)
      #' shape <- 1 / (sigmas[1]^2)
      #' scale <- mu / shape
      #' y <- rgamma(length(mu), shape = shape, scale = scale)
    } else {
      stop(paste("Family", family, "not implemented"))
    }
    #' }}} Sample y
  colnames(ret) <- c("y", colnames(xdata), colnames(df1), "cov")
  ret$cov <- as.factor(ret$cov)
  ret
}
# }}} gen_hfanova_data(...)



# {{{ gen_hfanova_data(...)

gen_hfanova_data2 <- function(f, n, sigmas, bounds, ndim = 1,
                              ngrp = 3,
                              px = NULL, pxargs = NULL,
                              family = "gaussian",
                              white_noise = TRUE) {
  if (is.null(px) || is.null(pxargs)) {
    px <- runif
    if (!is.list(bounds) && ndim != 1) {
      stop("If bounds is a vector, ndim must be 1")
    }
    if (is.vector(bounds) && ndim == 1) {
      tbounds <- list()
      tbounds[[1]] <- bounds
      bounds <- tbounds
    }
    pxargs <- lapply(
      1:ndim,
      function(i) {
        list(min = bounds[[i]][1], max = bounds[[i]][2])
      }
    )
  }
  neffects <- unlist(lapply(seq_along(n), function(i) {
    prod(n[length(n):(length(n) - (i - 1))])
  }))
  neffects <- neffects[rev(seq_along(neffects))]
  coreset_levels <- mixedcurve::create_group_df(n[-1])
  names(coreset_levels) <- paste0("grp", seq_len(ncol(coreset_levels)))
  noises <- lapply(
    seq_along(sigmas),
    function(i) {
      rnorm(neffects[i], 0, sigmas[i])
    }
  )
  g <- sample(1:ngrp, nrow(coreset_levels), replace = TRUE)
  curvdata <- list()
  for (i in seq_len(nrow(coreset_levels))) {
    print(i)
    x <- do.call(
      cbind,
      lapply(
        seq_len(ndim),
        function(d) {
          do.call(px, c(list(n = n[1]), pxargs[[d]]))
        }
      )
    )
    curvdata[[i]] <- list()
    curvdata[[i]]$x <- x
    curvdata[[i]]$grp0 <- as.factor(1:n[1])
    ncol(coreset_levels)
    for (j in seq_len(ncol(coreset_levels))) {
      curvdata[[i]][[colnames(coreset_levels)[j]]] <-
        rep(coreset_levels[i, j], n[1])
    }
    curvdata[[i]]$cov <- rep(g[i], n[1])
    ytrue <- f(x, g[i])
    y <- ytrue + rep(noises[[2]][coreset_levels[i, 1]], each = n[1])
    if (length(noises) > 2) {
      for (j in 2:(length(noises) - 1)) {
        y <- y + rep(noises[[j]][coreset_levels[i, j]], each = n[1])
      }
    }
    curvdata[[i]]$y <- y
  }
  ret <- do.call(
    rbind,
    lapply(curvdata, function(curv) {
      tdf <- as.data.frame(curv$x)
      colnames(tdf) <- paste0("x", seq_len(ncol(tdf)))
      tdf$y <- curv$y
      tdf$grp0 <- curv$grp0
      for (j in seq_len(ncol(coreset_levels))) {
        tdf[[colnames(coreset_levels)[j]]] <- curv[[colnames(coreset_levels)[j]]]
      }
      tdf$cov <- curv$cov
      tdf
    }))
  ret
}



# }}} gen_hfanova_data(...)




# Local Variables:
# eval: (origami-mode t)
# End:
