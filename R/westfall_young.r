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

# {{{ perm fanova

#' Permutation function for functional ANOVA
#'
#' @param dataf - data frame with columns x, y, grp, id
#' @param grp_col - name of the group column
#' @param id_col - name of the subject ID column
#' @return A permuted data frame with the same structure as input
gen_perm_fanova <- function(dataf, grp_col = "grp", id_col = "id") {
  ret <- dataf
  shuffled_ids <- sample(unique(ret[[id_col]]))
  shuffled_grp <- rep(sample(unique(ret[[grp_col]])),
    length.out = length(unique(ret[[id_col]]))
  )
  new_mapping <- setNames(shuffled_grp, shuffled_ids)
  ret[[grp_col]] <- new_mapping[as.character(ret[[id_col]])]
  ret
}

# }}} perm fanova

# {{{ gen_pvals_anova

#' Generate p-values using ANOVA (requires data to be on a grid)
#'
#' @param dataf - data frame with columns x, y, grp, id
#' @param xseq - sequence of x values to test
#' @return A numeric vector of p-values corresponding to each x in xseq
gen_pvals_anova <- function(dataf, xseq) {
  pvals <- numeric(length(xseq))
  for (j in seq_along(xseq)) {
    subdf <- dataf[dataf$x == xseq[j], ]
    lm1 <- lm(y ~ grp, data = subdf)
    lm2 <- lm(y ~ 1, data = subdf)
    pvals[j] <- anova(lm1, lm2)$Pr[2]
  }
  pvals
}

# }}} gen_pvals_anova

# {{{ westfall-young full

#' Westfall-Young full method
#'
#' @param dataf - data frame with columns x, y, grp, id
#' @param xseq - sequence of x values to test
#' @param nperm - number of permutations
#' @param gen_pvals_fun - function to generate p-values,
#'                        takes dataf and xseq as arguments
#' @param gen_perm_fun - function to generate a permutation of
#'                       the data, takes dataf as argument
wy_full <- function(dataf, xseq, nperm, gen_pvals_fun,
                    gen_perm_fun, cl = NULL) {
  if (is.numeric(xseq)) {
    nx <- length(xseq)
  } else {
    stop("xseq must be numeric vector")
  }
  rpvals <- gen_pvals_fun(dataf, xseq)
  srpvals <- sort(rpvals, index.return = TRUE)
  pstars <- matrix(0, nrow = nx, ncol = nperm)
  calc_perm_pvals <- function(tdataf, xseq) {
    perm <- gen_perm_fun(tdataf)
    perm_pvals <- gen_pvals_fun(perm, xseq)
    perm_pvals
  }
  pstars_list <- parallel::parLapply(
    cl, seq_len(nperm),
    function(i) calc_perm_pvals(dataf, xseq)
  )
  pstars <- do.call(cbind, pstars_list)
  pstars <- pstars[srpvals$ix, ]
  qstars <- matrix(0, nrow = nx, ncol = nperm)
  for (j in 1:nx) {
    for (l in 1:nperm) {
      qstars[j, l] <- min(pstars[j:nx, l])
    }
  }
  rj <- numeric(nx)
  for (j in 1:nx) {
    rj[j] <- mean(qstars[j, ] <= srpvals$x[j])
  }
  corrected_pvals <- numeric(nx)
  corrected_pvals[srpvals$ix] <- rj
  corrected_pvals
}
# }}} westfall-young full

# {{{ Westfall-young one-step

#' Westfall-Young (Cox-Lee) one-step method
#'
#' @param dataf - data frame with columns x, y, grp, id
#' @param xseq - sequence of x values to test
#' @param nperm - number of permutations
#' @param gen_pvals_fun - function to generate p-values,
#'                        takes dataf and xseq as arguments
#' @param gen_perm_fun - function to generate a permutation
#'                       of the data, takes dataf as argument
wy_one_step <- function(dataf, xseq, nperm, gen_pvals_fun,
                        gen_perm_fun, cl = NULL) {
  if (is.numeric(xseq)) {
    nx <- length(xseq)
  } else {
    stop("xseq must be numeric vector")
  }
  rpvals <- gen_pvals_fun(dataf, xseq)
  calc_min_pval <- function(dataf, xseq) {
    perm <- gen_perm_fun(dataf)
    perm_pvals <- gen_pvals_fun(perm, xseq)
    min(perm_pvals)
  }
  min_pvals <- unlist(parallel::parLapply(
    cl,
    seq_len(nperm),
    function(i) {
      calc_min_pval(dataf, xseq)
    }
  ))
  corrected_pvals <- numeric(nx)
  for (j in 1:nx) {
    corrected_pvals[j] <- mean(min_pvals <= rpvals[j])
  }
  corrected_pvals
}

# }}} Westfall-young one-step

# Local Variables:
# eval: (origami-mode t)
# End:
