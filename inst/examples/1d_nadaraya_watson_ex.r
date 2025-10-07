library(mixedcurve)
library(parallel)

# source("./R/benchmarks.r")
# source("./R/utils.r")
# source("./R/formula.r")
# source("./R/kernel.r")

# {{{ Df1
df1 <- gen_1d_fanova_data(
  f = mixedcurve::m3, bounds = c(0, 1),
  n = 30, ngrp = 3, nx = 200,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
  sigma = 0.02, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
# Add dummy cols
df1$ngrp <- sample(df1$grp, nrow(df1), replace = TRUE)
df1$nngrp <- sample(df1$grp, nrow(df1), replace = TRUE)
# }}} Df1

# {{{ lpk fit
lpk1 <- lpk(y ~ K_h(x | grp),
  queries = as.matrix(seq(0.0, 1.0, length.out = 200)),
  data = df1, degree = 0, kernel = mixedcurve::gauss_kern,
  h = 0.005, parallel = TRUE
) # , cl = cl)
qs <- do.call(rbind, lapply(lpk1$queries, function(x) x$queries))
plot(df1$x, df1$y, col = df1$grp, pch = 20)
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), qs[, i], col = i, lwd = 2)
}
# }}} lpk fit

#
