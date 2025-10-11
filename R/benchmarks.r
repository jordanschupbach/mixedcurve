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
    shift_and_rotate(m3_1d, dim = ncol(t))(t, i)
  } else if (is.data.frame(t)) {
    shift_and_rotate(m3_1d, dim = ncol(t))(as.matrix(t), i)
  } else if (is.vector(t) && !is.list(t)) {
    m3_1d(t, i)
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
  if(systematic) {
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
  n <- ni * nj * nk
  list_m <- lapply(1:ngroups, function(i) {
    function(t) m(t, i)
  })
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
        tdi + tdij + tdijk #??? rnorm(nk, 0, tau2)
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


gen_fanova_data <- function(f = tmc::m3, bounds = c(0, 1),
                            n = 10, ngrp = 3, nx = 200,
                            balanced = FALSE, pgrp = sample,
                            pgrpargs = list(x = 1:3,
                                            size = 10,
                                            replace = TRUE),
                            sigma = 0.05, systematic = FALSE,
                            px = runif,
                            pxargs = list(list(min = 0, max = 1)),
                            white_noise = TRUE,
                            cov_scale = 0.05, gpn = 1000) {
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
      txlist <- list(seq(tbounds[1, 1], tbounds[1, 2], length.out = nx))
      for (d in 2:dims) {
        txlist[[d]] <- seq(tbounds[d, 1], tbounds[d, 2], length.out = nx)
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
      ylist[[i]] <- ylist[[i]] + rnorm(nx, 0, sigma)
    } else {
      grid_points <- seq(bounds[1], bounds[2], length.out = gpn)
      idx <- sapply(xlist[[i]], function(xi) {
        which.min(abs(grid_points - xi))
      })
      ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
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
  coreset <- ret[(0:(ntotal - 1)) * nx + 1, ]
  structure(list(df = ret, coreset = coreset,
                 dims = dims, nx = nx, systematic = systematic),
            class = "fundata")
}

plot.fundata <- function(fundata, ncurves = 1) {
  if (fundata$dims == 1) {
    stop("plot.fundata() is implemented for 2d data only (TODO)")
  } else if (fundata$dims == 2) {
    if (fundata$systematic) {
      nxy <- fundata$nx
      df2 <- data2$df
      color_scale <- viridis::viridis(100)
      breaks <- seq(min(df2$y), max(df2$y), length.out = 101)
      for (i in 1:ncurves) {
        image(matrix(df2$y[((i - 1) * nxy * nxy + 1):(i * nxy * nxy)],
                     nxy, nxy, byrow = TRUE),
              xlab = "x1", ylab = "x2",
              main = paste("Sample ", i, " (Group",
                           df2$grp[((i - 1) * nxy * nxy + 1)], ")", sep = ""),
              col = color_scale,
              breaks = breaks
        )
      }
    } else {
      df2 <- data2$df
      dfcs2 <- data2$coreset
      nxy <- data2$nx
      layout(mixedcurve::gen_square_layout(ncurves))
      color_scale <- viridis::viridis(100)
      color_values <- cut(df2$y, breaks = 100, labels = FALSE)
      for (i in 1:ncurves) {
        plot(df2$x1[((i - 1) * nxy + 1):((i) * nxy)],
             df2$x2[((i - 1) * nxy + 1):((i) * nxy)],
             col = color_scale[color_values[((i - 1) * nxy + 1):((i) * nxy)]],
             pch = 20, cex = 2, xlab = "x1",
             ylab = "x2", main = paste("Sample", i,
                                       " (Group", df2$grp[((i - 1) * nxy + 1)], ")", sep = "")
             )
      }
    }
  } else {
    stop("plot.fundata() is implemented for 2d data only (TODO)")
  }
}

# }}} gen_fanova_data

# Local Variables:
# eval: (origami-mode t)
# End:
