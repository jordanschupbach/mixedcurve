library(mixedcurve)

source("./R/benchmarks.r")
source("./R/utils.r")
source("./R/formula.r")
source("./R/kernel.r")

# {{{ Df1
df1 <- gen_1d_fanova_data(
  f = mixedcurve::m3, bounds = c(0, 1),
  n = 10, ngrp = 3, nx = 10,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
  sigma = 0.05, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
# Add dummy cols
df1$ngrp <- sample(df1$grp, nrow(df1), replace = TRUE)
df1$nngrp <- sample(df1$grp, nrow(df1), replace = TRUE)
# }}} Df1

# Plot {{{
dark_mode()
plot(df1$x, df1$y, col = df1$grp, pch = 20)
# Plot }}}

# {{{ lpk
#' Local polynomial Kernel regression
#'
#' @param formula A formula object specifying the model to be fitted of the form y ~ K_h(x | grp) or  y ~ K_h(x) depending on whether a grouping variable is present.
#' @param degree An integer specifying the degree of the local polynomial (default is 0 for Nadaraya-Watson estimator).
#' @param queries A numeric vector of points at which to evaluate the fitted values.
#' @param data A data frame containing the variables in the formula.
#' @param h A positive numeric value representing the bandwidth for the kernel.
lpk_query <- function(formula,
                      query,
                      data,
                      degree = 0,
                      kernel = mixedcurve::gauss_kern,
                      h) {
  # if (is.vector(queries)) {
  #   queries <- as.matrix(queries)
  # }
  terms <- parse_terms(formula)
  # query <- queries[i, ]
  weighted_data <- lm_kernel_weights(formula,
    data = data,
    bwidth = h, query = query
  )
  # weighted_data
  lm_fit <- lm(w_y ~ . - 1, data = weighted_data)
  # lm_fit
  list(
    lm = lm_fit, query = query,
    weights = weighted_data, coefs = coef(lm_fit),
    queries = c(
      coef(lm_fit)[1],
      coef(lm_fit)[1] + coef(lm_fit)[2:length(coef(lm_fit))]
    )
  )
}
lpk_query(y ~ K_h(x | grp),
  query = c(0.1),
  data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
  h = 0.2
)$queries


lpk <- function(formula,
                queries,
                data,
                degree = 0,
                kernel = mixedcurve::gauss_kern,
                h,
                parallel = TRUE, cl = NULL) {
  if (parallel) {
    if (is.null(cl)) {
      print("Creating cluster")
      cl <- parallel::makeCluster(parallel::detectCores() - 1)
      queries <- parallel::parLapply(queries, function(q) {
        lpk_query(formula,
          query = q,
          data = data,
          degree = degree,
          kernel = kernel,
          h = h
        )
      })
      on.exit(parallel::stopCluster(cl))
    }
    queries <- parallel::parLapply(queries, function(q) {
      lpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    })
  } else {
    queries <- lapply(queries, function(q) {
      lpk_query(formula,
        query = q,
        data = data,
        degree = degree,
        kernel = kernel,
        h = h
      )
    })
  }
  list(queries = queries)
}

lpk1 <- lpk(y ~ K_h(x | grp), as.matrix(c(0.0, 1.0, length.out = 200)),
  data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
  h = 0.2, parallel = TRUE
)

# str(lpk1)

str(lpk1[[1]])

1:length(lpk1[[1]])
lapply(1:length(lpk1), function(i) lpk1[[i]]$queries)

lpk1$queries[[4]]
do.call(rbind, lapply(1:length(lpk1), function(i) lpk1[[i]]$queries))





# lpk(formula

# weighted_data <- lm_kernel_weights(y ~ K_h(x | grp),
#   data = df1,
#   bwidth = 0.2, query = c(0.0)
# )

# str(weighted_data)
# as.matrix(c(0.0, 1.0, length.out = 200))[1, ]

# lpk(y ~ K_h(x | grp), as.matrix(c(0.0, 1.0, length.out = 200)),
#   data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
#   h = 0.2, parallel = FALSE
# )





# }}} lpk









# Nadaraya-Watson
lpk1 <- lpk(y ~ K_h(x | grp), c(0.0, 1.0, length.out = 200),
  data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
  h = 0.2, parallel = FALSE
)











# gen_1d_curve_data <- function(n = 300,
#                               ngrp = 1,
#                               noise_sd = 0.1,
#                               seed = 123) {
#   set.seed(seed)
#   grp <- sample(1:ngrp, n, replace = TRUE)
#   x <- runif(n)
#   y <- sapply(1:n, function(i) mixedcurve::m1(x[i], grp[i])) + rnorm(n, sd = noise_sd)
#   data.frame(x = x, y = y, grp = as.factor(grp))
# }

df1 <- gen_1d_fanova_data()
gen_1d_fanova_data()
