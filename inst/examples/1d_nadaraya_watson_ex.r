# 1D nadaraya-watson kernel regression

# 1d case
# library(mixedcurve)
fundata1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = c(0, 1),
  n = 30, ngrp = 3, nx = 200,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
  sigma = 0.005, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
par(mfrow = c(3, 2))
mixedcurve::dark_mode()
mixedcurve::plot.fundata(fundata1, curves = 1:6)

time_took <- system.time({
  lpk1 <- mixedcurve::lpk(y ~ K_h(x1 | grp),
    queries = as.matrix(seq(0.0, 1.0, length.out = 200)),
    data = fundata1$df, degree = 0, kernel = mixedcurve::gauss_kern,
    h = 0.01, parallel = TRUE
  )
})
print(paste("Time took:", round(time_took[3], 2), "secs"))

qs <- mixedcurve::get_queries(lpk1)
dark_mode()
par(mfrow = c(1, 1))
plot(fundata1$df$x1, fundata1$df$y, 
     col = adjustcolor(fundata1$df$grp, 0.20),
     pch = 20, ylim = c(0, 0.08),
     ylab = "y", xlab = "x1",
     main = "Local Polynomial Kernel Regression Fits (degree=0), (i.e. Nadaraya-Watson)"
)
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), qs[, i], col = i, lwd = 2)
}

