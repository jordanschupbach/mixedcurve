# Simulate some quadratic Poisson data with group-specific curves
set.seed(300)
# 1. Define the true curves
tf <- function(t, i) {
  # Define the rate function for Poisson data at time t
  exp(3 * exp(mixedcurve::m3(t, i)) - 2.7)
}
n <- 10000
set.seed(1234)
fundata1 <- mixedcurve::gen_fanova_data(
  f = tf,
  bounds = c(0, 1),
  n = 1, # Still sensitive to n?
  nx = n,
  balanced = TRUE,
  ngrp = 3,
  sigma = 0.015,
  family = "poisson"
)
df1 <- fundata1$df
mixedcurve::dark_mode()
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.03),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Modified M3 Poisson data"
)


qseq <- seq(0.0, 1.0, length.out = 200)
cl <- parallel::makeCluster(parallel::detectCores() - 1, type = "PSOCK")
parallel::clusterEvalQ(cl, {
  library(mixedcurve)
})
parallel::clusterExport(cl, varlist = c("df1", "qseq"), envir = environment())
time <- system.time({
  glpk1 <- mixedcurve::glpk(y ~ K_h(x1 | grp),
    queries = qseq,
    data = df1,
    degree = 0,
    kernel = mixedcurve::gauss_kern,
    h = 0.015,
    parallel = FALSE# ,
    # cl = cl
  )
})
time
coefs <- matrix(
  unlist(lapply(
    glpk1[[1]],
    function(elmt) {
      elmt$coef
    }
  )), 200, 3,
  byrow = TRUE
)
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.01),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Modified M3 Poisson data"
)
points(qseq, tf(qseq, 1), col = 1)
points(qseq, tf(qseq, 2), col = 2)
points(qseq, tf(qseq, 3), col = 3)
lines(qseq, coefs[, 1], col = 1, lwd = 2)
lines(qseq, coefs[, 1] + coefs[, 2], col = 2, lwd = 2)
lines(qseq, coefs[, 1] + coefs[, 3], col = 3, lwd = 2)


time <- system.time({
  qseq <- seq(0, 1, length.out = 200)
  coefmat <- matrix(0, nrow = length(qseq), ncol = 3)
  for (i in seq_along(qseq)) {
    weights <- sqrt(mixedcurve::kern_ih(
      pt = df1$x1,
      qry = qseq[i],
      kernel_fun = mixedcurve::gauss_kern,
      h = 0.005
    ))
    df1$w <- weights
    coefmat[i, ] <- as.numeric(
      coef(
        glm(y ~ grp,
          data = df1,
          weights = w, family = "poisson"
        )
      )
    )
  }
})
time
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.01),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Modified M3 Poisson data"
)
points(qseq, tf(qseq, 1), col = 1)
points(qseq, tf(qseq, 2), col = 2)
points(qseq, tf(qseq, 3), col = 3)
lines(qseq, exp(coefmat[, 1]), col = 1, lwd = 2)
lines(qseq, exp(coefmat[, 1] + coefmat[, 2]), col = 2, lwd = 2)
lines(qseq, exp(coefmat[, 1] + coefmat[, 3]), col = 3, lwd = 2)


time <- system.time({
  qseq <- seq(0, 1, length.out = 200)
  coefmat <- matrix(0, nrow = length(qseq), ncol = 3)
  for (i in seq_along(qseq)) {
    coefmat[i, ] <- mixedcurve::glpk_query(
      form = y ~ K_h(x1 | grp),
      query = qseq[i],
      data = df1,
      degree = 0,
      kernel = mixedcurve::gauss_kern,
      h = 0.005
    )$coefs
  }
})
time
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.01),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Modified M3 Poisson data"
)
points(qseq, tf(qseq, 1), col = 1)
points(qseq, tf(qseq, 2), col = 2)
points(qseq, tf(qseq, 3), col = 3)
lines(qseq, coefmat[, 1], col = 1, lwd = 2)
lines(qseq, coefmat[, 1] + coefmat[, 2], col = 2, lwd = 2)
lines(qseq, coefmat[, 1] + coefmat[, 3], col = 3, lwd = 2)


time <- system.time({
  qseq <- seq(0, 1, length.out = 200)
  coefmat <- matrix(0, nrow = length(qseq), ncol = 3)
  coefs <- parallel::parLapply(
    cl, qseq,
    function(q) {
      mixedcurve::glpk_query(
        form = y ~ K_h(x1 | grp),
        query = q,
        data = df1,
        degree = 0,
        kernel = mixedcurve::gauss_kern,
        h = 0.005
      )$coef
    }
  )
  coefs <- matrix(unlist(coefs), ncol = 3, byrow = TRUE)
})
time
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.01),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Modified M3 Poisson data"
)
points(qseq, tf(qseq, 1), col = 1)
points(qseq, tf(qseq, 2), col = 2)
points(qseq, tf(qseq, 3), col = 3)
lines(qseq, coefs[, 1], col = 1, lwd = 2)
lines(qseq, coefs[, 1] + coefs[, 2], col = 2, lwd = 2)
lines(qseq, coefs[, 1] + coefs[, 3], col = 3, lwd = 2)





lines(qseq, exp(coefmat[, 1]), col = 1)
lines(qseq, exp(coefmat[, 1] + coefmat[, 2]), col = 2)
lines(qseq, exp(coefmat[, 1] + coefmat[, 3]), col = 3)




#
