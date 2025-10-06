library(mixedcurve)

source("./R/benchmarks.r")
source("./R/utils.r")

dark_mode()
df1 <- gen_1d_fanova_data(
  f = mixedcurve::m3, bounds = c(0, 1),
  n = 10, ngrp = 3, nx = 10,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
  sigma = 0.05, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
plot(df1$x, df1$y, col = df1$grp, pch = 20)

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
