# Example of 1D Nadaraya-Watson kernel regression

n <- 500
fundata1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = c(0, 1),
  n = 1, ngrp = 1, nx = n,
  balanced = TRUE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 50, replace = TRUE),
  sigma = 0.005, systematic = FALSE, px = runif, # NOTE: systematic fails..
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
mixedcurve::dark_mode()
plot(fundata1$df$x1, fundata1$df$y,
  col = adjustcolor(fundata1$df$grp, 0.50),
  pch = 20, ylim = c(0, 0.08),
  ylab = "y", xlab = "x1",
  main = "Local Polynomial Kernel Regression Fits (degree=0)"
)

time_took <- system.time({
  lpk1 <- mixedcurve::lpk(y ~ K_h(x1),
    queries = matrix(seq(0.0, 1.0, length.out = 200), ncol = 1),
    data = fundata1$df, degree = 0,
    kernel = mixedcurve::gauss_kern,
    h = 0.02, parallel = TRUE
  )
})
qs <- mixedcurve::get_queries(lpk1)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(fundata1$df$x1, fundata1$df$y,
  col = adjustcolor(fundata1$df$grp, 0.50),
  pch = 20, ylim = c(0, 0.08),
  ylab = "y", xlab = "x1",
  main = "Local Polynomial Kernel Regression Fits (degree=0)"
)
lines(seq(0.0, 1.0, length.out = 200), qs[, 1], col = 1, lwd = 2)
