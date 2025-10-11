library(mixedcurve)
df1 <- gen_1d_fanova_data(
  f = mixedcurve::m2, bounds = c(0, 1),
  n = 30, ngrp = 3, nx = 200,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
  sigma = 0.02, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
time_took <- system.time({
  lpk1 <- lpk(y ~ K_h(x | grp),
    queries = as.matrix(seq(0.0, 1.0, length.out = 200)),
    data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
    h = 0.01, parallel = TRUE
  )
})
print(paste("Time took:", round(time_took[3], 2), "secs"))
qs <- get_queries(lpk1)
dark_mode()
plot(df1$x, df1$y, col = df1$grp, pch = 20)
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), qs[, i], col = i, lwd = 2)
}




#
