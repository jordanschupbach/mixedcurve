
nxy <- 300
fundata1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.001, systematic = FALSE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 1000
)
par(mfrow = c(3, 2))
mixedcurve::dark_mode()
mixedcurve::plot.fundata(fundata1, curves = 1:6)
lpk2 <- mixedcurve::lpk(y ~ K_h(x1 * x2 | grp),
  queries = as.matrix(expand.grid(
    seq(0.0, 1.0, length.out = 20),
    seq(0.0, 1.0, length.out = 20)
  )),
  data = fundata1$df, degree = 0, kernel = mixedcurve::gauss_kern,
  h = c(0.05, 0.05), parallel = TRUE
)

# move this to plot method?
qrs <- mixedcurve::get_queries(lpk2)
all_values <- c(qrs[, 1], qrs[, 2], qrs[, 3])
breaks <- seq(min(all_values), max(all_values), length.out = 101)
colors <- viridis::viridis(100)
par(mfrow = c(1, 3))
for (i in 1:3) {
  image(matrix(qrs[, i], 20, 20), col = colors, breaks = breaks, axes = FALSE, main = paste("Image", i))
}





