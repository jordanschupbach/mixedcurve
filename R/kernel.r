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

# {{{ gauss_kern
#' Gaussian kernel function
#'
#' This function computes the Gaussian kernel value for a given input.
#' @param x A numeric value or vector for which to compute the Gaussian kernel.
#' @return The Gaussian kernel value(s) corresponding to the input.
#'
#' @examples
#' gauss_kern(0)  # Should return 1
#' gauss_kern(c(-1, 0, 1))  # Should return values for -1, 0, and 1
gauss_kern <- function(x) {
  exp((-1 / 2) * ((x)^2))
}
# }}} gauss_kern

# {{{ k_h
#' Scaled Gaussian kernel function
#'
#' This function computes the scaled Gaussian kernel value for a given input and bandwidth.
#' @param x A numeric value or vector for which to compute the scaled Gaussian kernel.
#' @param h A positive numeric value representing the bandwidth.
#' @return The scaled Gaussian kernel value(s) corresponding to the input and bandwidth.
#'
#' @examples
#' k_h(0, 1)  # Should return 1
#' k_h(c(-1, 0, 1), 0.5)  # Should return scaled values for -1, 0, and 1 with bandwidth 0.5
kern_h <- function(x, h, kern = gauss_kern) {
  kern(x / h) / h
}
# }}} k_h

# {{{ lm_kernel_weights
lm_kernel_weights <- function(dataframe, bwidth, query) {
  design_matrix <- cbind(dataframe$y, model.matrix(~ grp, dataframe))
  colnames(design_matrix) <- c("y", "intercept",
                               paste("grp",
                                     levels(dataframe$grp)[2:length(levels(dataframe$grp))],
                                     sep = ""))
  weights <- sqrt(k_h(dataframe$x - query, bwidth))
  weighted_design_matrix <- sweep(design_matrix, 1, weights, FUN = "*")
  colnames(weighted_design_matrix) <- paste("w_", colnames(design_matrix), sep = "")
  as.data.frame(weighted_design_matrix)
}

# }}} lm_kernel_weights

# Local Variables:
# eval: (origami-mode t)
# End:
