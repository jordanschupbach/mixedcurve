# 1. Define the true curves
tf <- function(t, i) {
  # Define the rate function for Poisson data at time t
  exp(3 * exp(mixedcurve::m3(t, i)) - 2.7)
}
# 2. Generate the data
fundata1 <- mixedcurve::gen_fanova_data(
  f = tf,
  bounds = c(0, 1),
  n = 20, # Still sensitive to n?
  nx = 300,
  ngrp = 3,
  sigma = 0.02,
  family = "poisson"
)
df1 <- fundata1$df
mixedcurve::dark_mode()
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.40),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Functional Poisson data"
)


# Use normal distribution for now...
lpk1 <- mixedcurve::lpk(y ~ K_h(x1 | grp),
  seq(0.0, 1.0, length.out = 200),
  df1,
  degree = 0,
  kernel = mixedcurve::gauss_kern,
  h = 0.01,
  parallel = TRUE
)

qrs <- mixedcurve::get_queries(lpk1)
plot(df1$x1, df1$y,
  col = adjustcolor(df1$grp, 0.40),
  pch = 20, ylim = c(0, 8.5),
  ylab = "y", xlab = "x1",
  main = "Functional Poisson data"
)
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), qrs[, i],
    col = adjustcolor(i, 0.90),
    lwd = 2
  )
}
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), tf(seq(0.0, 1.0, length.out = 200), i),
    col = adjustcolor(i, 0.60),
    lwd = 3, lty = 2
  )
}




# 3. Fit the model
glpk1 <- mixedcurve::glpk(y ~ K_h(x1 | grp),
  seq(0.0, 1.0, length.out = 200),
  tdata1$df,
  degree = 0,
  kernel = mixedcurve::gauss_kern,
  h = 0.01,
  family = "poisson",
  parallel = TRUE, cl = NULL
)
# 4. Plot the true and estimated curves
coefs <- do.call(rbind, lapply(glpk1$queries, function(x) x$coefs))
mixedcurve::dark_mode()
xseq <- seq(0.0, 1.0, length.out = 200)
par(mfrow = c(1, 2))
plot(xseq, tf(xseq, 1),
  type = "l", ylim = c(0, 8.5),
  xlab = "x1", ylab = "y", main = "True curves", lwd = 3
)
lines(xseq, tf(xseq, 2), col = 2, lwd = 3)
lines(xseq, tf(xseq, 3), col = 3, lwd = 3)
plot(tdata1$df$x1, tdata1$df$y + rnorm(length(tdata1$df$y), 0, 0.02),
  col = adjustcolor(tdata1$df$grp, 0.3),
  ylim = c(0, 8.5), main = "Estimated curves",
  xlab = "x1", ylab = "y", pch = 20
)
lines(xseq, exp(coefs[, 1]), lwd = 3)
lines(xseq, exp(coefs[, 1] + coefs[, 2]), col = 2, lwd = 3)
lines(xseq, exp(coefs[, 1] + coefs[, 3]), col = 3, lwd = 3)
