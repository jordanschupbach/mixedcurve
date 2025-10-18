set.seed(300)
n <- 1000
x <- runif(n)
xhlp <- cbind(1, x, x^2)
betas_true <- c(2, 0.8, -1.2)
eta_true <- xhlp %*% betas_true
y <- rpois(n, exp(eta_true))
z <- log(y + .5)
betas <- lm(z ~ -1 + xhlp)$coef
eta <- xhlp %*% betas
ylogy <- function(y) {
  ifelse(y == 0, rep(0, length(y)), y * log(y))
}
deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
deviance_old <- 1e30
tol <- 1e-6
iteration <- 0
while (((devianceOld - deviance) / devianceOld) > tol) {
  iteration <- iteration + 1
  z <- eta + exp(-eta) * (y - exp(eta))
  w <- c(exp(eta))
  lm_update <- lm(z ~ -1 + xhlp, weight = w)
  eta <- lmUpdate$fitted
  betas <- lmUpdate$coef
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat(
    "iteration", iteration, "Deviance Old",
    devianceOld, "Deviance", deviance, "\n"
  )
}
glmfit <- glm(y ~ -1 + xhlp, family = poisson)
xseq <- seq(0.0, 1.0, length.out = 100)
xmat <- cbind(1, xseq, xseq^2)
eta_hat <- xmat %*% betas
eta_true <- xmat %*% betas_true
mu_hat <- exp(xmat %*% betas)
mu_true <- exp(xmat %*% betas_true)
plot(xseq, muTrue,
  type = "l", lwd = 3, col = 1,
  ylim = range(c(muHat, muTrue, y)), ylab = "y", xlab = "x"
)
points(x, y, pch = 19, col = adjustcolor("black", 0.1))
lines(xseq, muHat, col = 2, lty = 2, lwd = 3)
legend("topright",
  legend = c("True mean", "Estimated mean"),
  col = c(1, 2), lty = c(1, 2), lwd = 2
)
