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
#' gauss_kern(0)
gauss_kern <- function(x) {
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  kernel_values <- apply(x, 1, function(row) {
    exp((-1 / 2) * sum(row^2))
  })
  kernel_values
}

# }}} gauss_kern

# {{{ k_h

#' Scaled Gaussian kernel function
#'
#' This function computes the scaled Gaussian kernel value
#' for a given input and bandwidth.
#' @param x A numeric value or vector for which to compute
#'          the scaled Gaussian kernel.
#' @param h A positive numeric value representing the
#'          bandwidth.
#' @return The scaled Gaussian kernel value(s) corresponding
#'         to the input and bandwidth.
#'
#' @examples
#' kern_h(0, 1) # Should return 1
#' kern_h(c(-1, 0, 1), 0.5)
kern_h <- function(x, h, kernel_fun = gauss_kern) {
  kernel_fun(x / h) / h
}

kern_ih <- function(pt, qry, kernel_fun, h) {
  if (is.vector(pt)) {
    pt <- matrix(pt, ncol = 1)
  }
  if (!is.matrix(pt)) {
    stop("pt must be a matrix or vector")
  }
  kern_h(pt - rep(qry, each = nrow(pt)), h = h, kernel_fun = kernel_fun)
}

# }}} k_h

# {{{ lm_kernel_weights

lm_kernel_weights <- function(form, data, bwidth, query,
                              kernel_fun = mixedcurve::gauss_kern) {
  #' form <- y ~ K_h(x)
  #' bwidth <- 0.1
  terms <- mixedcurve::parse_terms(as.formula(form))
  response_term <- terms[terms$type == "response", ]$lhs
  kterm_lhs <- terms[terms$type == "kernel fixed effect", ]$lhs
  kterm_rhs <- terms[terms$type == "kernel fixed effect", ]$rhs
  dim_labels <- unlist(lapply(
    strsplit(kterm_lhs, "\\*")[[1]],
    function(s) trimws(s)
  ))
  if (sum(terms$type == "kernel fixed effect") == 0) {
    stop("No kernel term found in the formula.
Please include a term like K_h(x) or K_h(x | grp).")
  }
  if (sum(terms$type == "kernel fixed effect") > 1) {
    stop("Multiple kernel terms found in the formula.
Please include only one kernel term or a
multi-dimensional kernel term (e.g., K_h(x * y)
or K_h(x * y | grp)).")
  }
  if (length(dim_labels) == 0) {
    stop("No dimension labels found in the kernel term.")
  }
  if (length(dim_labels) != length(query)) {
    print(str(query))
    stop(paste0(
      "Number of dimension labels (", length(dim_labels),
      ") does not match dimension of query point (", length(query), ")."
    ))
  }
  if (!all(dim_labels %in% colnames(data))) {
    stop("Some dimension labels in the kernel term are not found in the data.")
  }
  #' str(data)
  #' response_term
  if (!is.na(kterm_rhs)) {
    dmat <- cbind(
      data[[response_term]],
      model.matrix(as.formula(paste0("~", kterm_rhs)), data)
    )
  } else {
    dmat <- cbind(
      data[[response_term]],
      model.matrix(as.formula(paste0("~ 1")), data)
    )
  }
  if (!is.na(kterm_rhs)) {
    colnames(dmat) <- c(
      "y", "intercept",
      unlist(lapply(
        colnames(dmat)[3:length(colnames(dmat))],
        function(x) {
          if (grepl(":", x)) {
            paste(paste0("", strsplit(x, ":")[[1]]),
              collapse = ":"
            )
          } else {
            paste0("", x)
          }
        }
      ))
    )
  } else {
    colnames(dmat) <- c("y", "intercept")
  }
  dim_data <- do.call(cbind, lapply(dim_labels, function(label) data[[label]]))
  weights <- sqrt(mixedcurve::kern_ih(dim_data, query,
                                      bwidth,
                                      kernel_fun = kernel_fun))
  weights
  #' weighted_design_matrix <- sweep(dmat, 1, weights, FUN = "*")
  #' colnames(weighted_design_matrix) <- paste("w_", colnames(dmat), sep = "")
  #' as.data.frame(weighted_design_matrix)
}

# }}} lm_kernel_weights

# Local Variables:
# eval: (origami-mode t)
# End:
