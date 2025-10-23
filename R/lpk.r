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
lpk_query <- function(formula,
                       query,
                       data,
                       degree = 0,
                       kernel = mixedcurve::gauss_kern, h) {
  lme_form <- as.formula(mixedcurve::kernel_to_lm_formula(formula))
  w <- as.numeric(mixedcurve::lm_kernel_weights(
    formula,
    data = data,
    bwidth = h,
    query = query
  ))
  data$w <- w # NOTE: forces a copy of data?
  lm1 <- lm(
    lme_form,
    data = data,
    weights = w
  )
  list(
    query = query,
    coefs = coef(lm1)
  )
}


lpk <- function(formula,
                queries,
                data,
                h,
                degree = 0,
                kernel = mixedcurve::gauss_kern,
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

# }}} glpk

# {{{ glpk

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
  lme_form <- as.formula(mixedcurve::kernel_to_lm_formula(formula))
  w <- as.numeric(mixedcurve::lm_kernel_weights(
    formula,
    data = data,
    bwidth = h,
    query = query
  ))
  data$w <- w # NOTE: forces a copy?
  lm1 <- glm(
    lme_form,
    data = data,
    family = family,
    weights = w
  )
  list(
    query = query,
    coefs = coef(lm1)
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
  kernel <- mixedcurve::gauss_kern
  lme4_form <- as.formula(mixedcurve::kernel_to_lme4_formula(formula))
  parse_terms <- mixedcurve::parse_terms(as.formula(formula))
  domain_cols <- parse_terms[parse_terms$type == "kernel fixed effect",]$lhs
  domain_cols <- unlist(strsplit(gsub("\\s+", "", domain_cols), "\\*"))
  domain_cols <- domain_cols[domain_cols != ""]
  weights <- mixedcurve::kern_ih(pt = as.matrix(data[,domain_cols, drop = FALSE]),
                                 qry = query, h = h, kernel_fun = kernel)
  data$w <- weights
  lm_fit <- lme4::lmer(
    lme4_form,
    data = data,
    weights = w
  )
  list(
    query = query,
    coefs = coef(lm_fit),
    fixefs = lme4::fixef(lm_fit),
    ranefs = lme4::ranef(lm_fit)
  )
}

lpkme <- function(formula,
                 queries,
                 data,
                 h,
                 degree = 0,
                 kernel = mixedcurve::gauss_kern,
                 parallel = TRUE, cl = NULL) {
  # TODO: enforce that formula has random effect kernel terms
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
        lpkme_query(
          formula = formula,
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
      lpkme_query(
        formula = formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    }, cl = cl)
  } else {
    res <- lapply(tqueries, function(q) {
      lpkme_query(
        formula = formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    })
  }
  lpkme_mod <- list(queries = res, formula = formula, bandwidth = h)
  class(lpkme_mod) <- "lpkmeMod"
  lpkme_mod
}
# }}} lpkme

# {{{ glpkme

glpkme_query <- function(formula,
                        query,
                        data,
                        degree = 0,
                        kernel = mixedcurve::gauss_kern,
                        h, family = "poisson") {
  # data <- df1
  # df1$rep <- as.factor(rep(rep(1:20, each = 15), times = 15))
  # df1 <- data
  # nrow(df1)
  # formula <- y ~ K_h(x) + (K_h(x) | id/ rep)
  # query <- 0.2
  # h <- 0.20
  # family <- "poisson"
  kernel <- mixedcurve::gauss_kern
  lme4_form <- as.formula(mixedcurve::kernel_to_lme4_formula(formula))
  parse_terms <- mixedcurve::parse_terms(as.formula(formula))
  domain_cols <- parse_terms[parse_terms$type == "kernel fixed effect",]$lhs
  domain_cols <- unlist(strsplit(gsub("\\s+", "", domain_cols), "\\*"))
  domain_cols <- domain_cols[domain_cols != ""]
  weights <- mixedcurve::kern_ih(pt = as.matrix(data[,domain_cols, drop = FALSE]),
                                 qry = query, h = h, kernel_fun = kernel)
  data$w <- weights
  glm_fit <- lme4::glmer(
    lme4_form,
    data = data,
    weights = w,
    family = family
  )
  list(
    query = query,
    coefs = coef(glm_fit),
    fixefs = lme4::fixef(glm_fit),
    ranefs = lme4::ranef(glm_fit)
  )
}

glpkme <- function(formula,
                 queries,
                 data,
                 h,
                 degree = 0,
                 kernel = mixedcurve::gauss_kern,
                 family = "gaussian",
                 parallel = TRUE, cl = NULL) {
  # TODO: enforce that formula has random effect kernel terms
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
        glpkme_query(formula,
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
      glpkme_query(formula,
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
      glpkme_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h,
        family = family
      )
    })
  }
  glpkme_mod <- list(queries = res, formula = formula, bandwidth = h)
  class(glpkme_mod) <- "glpkmeMod"
  glpkme_mod
}
# }}} glpkme

#' # {{{ oldlpk
#' #' Local polynomial Kernel regression
#' #'
#' #' @param formula A formula object specifying the model to be fitted of
#' #'                the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on
#' #'                whether a grouping variable is present.
#' #' @param degree An integer specifying the degree of the local polynomial
#' #'               (default is 0 for Nadaraya-Watson estimator).
#' #' @param queries A numeric vector of points at which to evaluate the fitted
#' #'                values.
#' #' @param data A data frame containing the variables in the formula.
#' #' @param h A positive numeric value representing the bandwidth for the kernel.
#' lpk_query <- function(formula,
#'                       query,
#'                       data,
#'                       degree = 0,
#'                       kernel = mixedcurve::gauss_kern,
#'                       h) {
#'   weighted_data <- mixedcurve::lm_kernel_weights(formula,
#'     data = data,
#'     bwidth = h, query = query
#'   )
#'   lm_fit <- lm(w_y ~ . - 1, data = weighted_data)
#'   queries <- coef(lm_fit)[1]
#'   print(length(coef(lm_fit))[1])
#'   if (length(coef(lm_fit)) > 1) {
#'     queries <- c(
#'       queries,
#'       sapply(
#'         2:length(coef(lm_fit)),
#'         function(i) coef(lm_fit)[1] + coef(lm_fit)[i]
#'       )
#'     )
#'   }
#'   list(
#'     query = query,
#'     weights = weighted_data,
#'     coefs = coef(lm_fit),
#'     queries = queries
#'   )
#' }
#' lpk <- function(formula,
#'                 queries,
#'                 data,
#'                 degree = 0,
#'                 kernel = mixedcurve::gauss_kern,
#'                 h,
#'                 parallel = TRUE, cl = NULL) {
#'   if (is.matrix(queries)) {
#'     tqueries <- split(queries, seq_len(nrow(queries)))
#'   } else if (is.vector(queries)) {
#'     tqueries <- as.list(queries)
#'   } else {
#'     stop("tqueries must be a matrix or a vector")
#'   }
#'   #' datas <- data
#'   if (parallel) {
#'     if (is.null(cl)) {
#'       cl <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
#'       parallel::clusterEvalQ(cl, {
#'         library(mixedcurve)
#'       })
#'       res <- parallel::parLapply(tqueries, function(q, datas) {
#'         lpk_query(formula,
#'           query = q,
#'           data = datas,
#'           degree = degree,
#'           kernel = kernel,
#'           h = h
#'         )
#'       }, cl = cl, datas = data)
#'       on.exit(parallel::stopCluster(cl))
#'     }
#'     res <- parallel::parLapply(tqueries, function(q) {
#'       lpk_query(formula,
#'         query = q,
#'         data = data,
#'         degree = degree,
#'         kernel = kernel,
#'         h = h
#'       )
#'     }, cl = cl)
#'   } else {
#'     res <- lapply(tqueries, function(q) {
#'       lpk_query(formula,
#'         query = q,
#'         data = data,
#'         degree = degree,
#'         kernel = kernel,
#'         h = h
#'       )
#'     })
#'   }
#'   lpk_mod <- list(queries = res, formula = formula, bandwidth = h)
#'   class(lpk_mod) <- "lpkMod"
#'   lpk_mod
#' }
#' get_queries <- function(lpkmod) {
#'   if (class(lpkmod) == "lpkMod" || class(lpkmod) == "glpkMod") {
#'     do.call(rbind, lapply(lpkmod$queries, function(x) x$queries))
#'   } else {
#'     stop("Object is not of class lpkMod")
#'   }
#' }
#' # }}} lpk



# Local Variables:
# eval: (origami-mode t)
# End:
