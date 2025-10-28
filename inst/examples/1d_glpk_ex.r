set.seed(300)
n <- 1000
x <- runif(n)
xmat <- cbind(rep(1, n), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
eta_true <- as.vector(xmat %*% betas_true)
y <- rpois(n, exp(eta_true))
df1 <- data.frame(y = y, x = x)

# 3. Plot the data
mixedcurve::dark_mode()
plot(df1$x, df1$y,
  col = adjustcolor("yellow", 0.2),
  pch = 20, ylab = "y", xlab = "x",
  main = "Quadratic Poisson Data"
)
points(df1$x, exp(eta_true),
  col = adjustcolor("red", 1.00), pch = 20
)
legend("topright",
  legend = c("True means", "Observed data"),
  col = c(adjustcolor("red", 0.90), adjustcolor("yellow", 0.90)),
  pch = c(20, 20)
)


# 4. Fit GNW kernel regression model (in parallel)
qseq <- seq(0.0, 1.0, length.out = 200)
glpk1 <- mixedcurve::glpk(y ~ K_h(x),
  queries = qseq,
  data = df1,
  degree = 0,
  kernel = mixedcurve::gauss_kern,
  h = 0.02,
  parallel = TRUE
)

# Extract the fitted curve
qrs <- unlist(lapply(glpk1[[1]], function(elmt) {
  elmt$coefs
}))

# 5. Plot the results
mixedcurve::dark_mode()
plot(df1$x, df1$y,
  col = adjustcolor("yellow", 0.2),
  pch = 20, ylim = c(0, 30),
  ylab = "y", xlab = "x",
  main = "Quadratic Poisson data with GNW fit"
)
points(df1$x, exp(eta_true), col = adjustcolor("blue", 1.00), cex = 0.5)
lines(qseq, qrs, col = adjustcolor("red", 1.00), lwd = 2)
legend("topright",
  legend = c("True means", "Estimated means"),
  col = c(
    adjustcolor("blue", 0.90),
    adjustcolor("red", 0.90)
  ),
  lty = c(1, 1),
  lwd = 2
)
