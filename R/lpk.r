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

# {{{ lpk
#' Local polynomial Kernel regression
#'
#' @param formula A formula object specifying the model to be fitted of
#'                the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on
#'                whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial
#'               (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted
#'                values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
lpk_query <- function(formula,
                      query,
                      data,
                      degree = 0,
                      kernel = mixedcurve::gauss_kern,
                      h) {
  weighted_data <- mixedcurve::lm_kernel_weights(formula,
    data = data,
    bwidth = h, query = query
  )
  lm_fit <- lm(w_y ~ . - 1, data = weighted_data)
  queries <- coef(lm_fit)[1]
  print(length(coef(lm_fit))[1])
  if (length(coef(lm_fit)) > 1) {
    queries <- c(
      queries,
      sapply(
        2:length(coef(lm_fit)),
        function(i) coef(lm_fit)[1] + coef(lm_fit)[i]
      )
    )
  }
  list(
    query = query,
    weights = weighted_data,
    coefs = coef(lm_fit),
    queries = queries
  )
}
lpk <- function(formula,
                queries,
                data,
                degree = 0,
                kernel = mixedcurve::gauss_kern,
                h,
                parallel = TRUE, cl = NULL) {
  if (is.matrix(queries)) {
    tqueries <- split(queries, seq_len(nrow(queries)))
  } else if (is.vector(queries)) {
    tqueries <- as.list(queries)
  } else {
    stop("tqueries must be a matrix or a vector")
  }
  #' datas <- data
  if (parallel) {
    if (is.null(cl)) {
      cl <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
      parallel::clusterEvalQ(cl, {
        library(mixedcurve)
      })
      res <- parallel::parLapply(tqueries, function(q, datas) {
        lpk_query(formula,
          query = q,
          data = datas,
          degree = degree,
          kernel = kernel,
          h = h
        )
      }, cl = cl, datas = data)
      on.exit(parallel::stopCluster(cl))
    }
    res <- parallel::parLapply(tqueries, function(q) {
      lpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    }, cl = cl)
  } else {
    res <- lapply(tqueries, function(q) {
      lpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    })
  }
  lpk_mod <- list(queries = res, formula = formula, bandwidth = h)
  class(lpk_mod) <- "lpkMod"
  lpk_mod
}
get_queries <- function(lpkmod) {
  if (class(lpkmod) == "lpkMod" || class(lpkmod) == "glpkMod") {
    do.call(rbind, lapply(lpkmod$queries, function(x) x$queries))
  } else {
    stop("Object is not of class lpkMod")
  }
}
# }}} lpk

# {{{ glpk

# TODO: redo docs here

#' Generalized Local polynomial Kernel regression query
#'
#' @param formula A formula object specifying the model to be fitted of
#'                the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on
#'                whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial
#'               (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted
#'                values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
glpk_query <- function(formula,
                       query,
                       data,
                       degree = 0,
                       kernel = mixedcurve::gauss_kern,
                       h, family = "gaussian") {
  #' terms <- parse_terms(formula)
  weighted_data <- lm_kernel_weights(formula,
    data = data,
    bwidth = h, query = query
  )
  lm_fit <- glm(w_y ~ . - 1, data = weighted_data, family = family)
  if (family == "gaussian") {
    #' queries <- c(
    #'   coef(lm_fit)[1],
    #'   sapply(2:length(coef(lm_fit)),
    #'          function(i) coef(lm_fit)[1] + coef(lm_fit)[i])
    #' )
  } else if (family == "poisson") {
    #' queries <- exp(c(
    #'   coef(lm_fit)[1],
    #'   sapply(2:length(coef(lm_fit)),
    #'          function(i) coef(lm_fit)[1] + coef(lm_fit)[i])
    #' ))
  } else {
    stop("Currently only 'gaussian' and 'poisson' families are supported")
  }
  list(
    query = query,
    weights = weighted_data, coefs = coef(lm_fit),
    coefs = coef(lm_fit),
    queries = c(
      coef(lm_fit)[1],
      sapply(
        2:length(coef(lm_fit)),
        function(i) coef(lm_fit)[1] + coef(lm_fit)[i]
      )
    )
  )
}


glpk <- function(formula,
                 queries,
                 data,
                 h,
                 degree = 0,
                 kernel = mixedcurve::gauss_kern,
                 family = "gaussian",
                 parallel = TRUE, cl = NULL) {
  if (is.matrix(queries)) {
    tqueries <- split(queries, seq_len(nrow(queries)))
  } else if (is.vector(queries)) {
    tqueries <- as.list(queries)
  } else {
    stop("tqueries must be a matrix or a vector")
  }
  #' datas <- data
  if (parallel) {
    if (is.null(cl)) {
      cl <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
      parallel::clusterEvalQ(cl, {
        library(mixedcurve)
      })
      res <- parallel::parLapply(tqueries, function(q, datas) {
        glpk_query(formula,
          query = q,
          data = datas,
          degree = degree,
          kernel = kernel,
          h = h,
          family = family
        )
      }, cl = cl, datas = data)
      on.exit(parallel::stopCluster(cl))
    }
    res <- parallel::parLapply(tqueries, function(q) {
      glpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h,
        family = family
      )
    }, cl = cl)
  } else {
    res <- lapply(tqueries, function(q) {
      glpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h,
        family = family
      )
    })
  }
  lpk_mod <- list(queries = res, formula = formula, bandwidth = h)
  class(lpk_mod) <- "lpkMod"
  lpk_mod
}


# }}} glpk

# {{{ lpkme

lpkme_query <- function(formula,
                        query,
                        data,
                        degree = 0,
                        kernel = mixedcurve::gauss_kern,
                        h) {
  weighted_data <- lm_kernel_weights(formula,
    data = data,
    bwidth = h, query = query
  )
  lm_fit <- lme4::lmer(w_y ~ . - 1 + (w_intercept | ind / rep),
    data = weighted_data
  )
  list(
    query = query,
    weights = weighted_data, coefs = coef(lm_fit),
    queries = c(
      coef(lm_fit)[1],
      coef(lm_fit)[1] + coef(lm_fit)[2:length(coef(lm_fit))]
    )
  )
}



#' Local polynomial Kernel Mixed-Effect regression
#'
#' @param formula A formula object specifying the model to be fitted of
#'                the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on
#'                whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial
#'               (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate
#'                the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
lpkme <- function(formula,
                  queries,
                  data,
                  h,
                  degree = 0,
                  kernel = mixedcurve::gauss_kern,
                  parallel = FALSE) {

}
# }}} lpkme

# {{{ glpkme
#' Local polynomial Kernel Mixed-Effect regression
#'
#' @param formula A formula object specifying the model to be fitted
#'                of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending
#'                on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial
#'               (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate
#'                the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
glpkme <- function(formula,
                   queries,
                   data,
                   h,
                   degree = 0,
                   kernel = mixedcurve::gauss_kern,
                   family = "poisson",
                   parallel = FALSE) { }
# }}} lpkme

# Local Variables:
# eval: (origami-mode t)
# End:
