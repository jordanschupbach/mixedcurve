# https://statomics.github.io/SGA2019/assets/poissonIRWLS-implemented.html
set.seed(300)
xhlp <- cbind(1, rnorm(100), rnorm(100))
betas_true <- c(2, 0.8, 1.2)
eta_true <- xhlp %*% betasTrue
y <- rpois(100, exp(etaTrue))
# Initial estimate
iteration <- 0
betas <- c(log(mean(y)), 0, 0)
plot(betasTrue,
  ylab = expression(beta), ylim = c(0, 4),
  pch = 19, type = "b", main = "True and initial betas"
)
lines(betas, type = "b", lty = 2)
# Calculate current eta
eta <- xhlp %*% betas
iteration <- 0
for (i in 1:3) {
  # start IRLS update step
  iteration <- iteration + 1
  # calculate pseudo data based on current betas
  z <- eta + exp(-eta) * (y - exp(eta))
  # calculate new weights: diagonal elements
  w <- c(exp(eta))
  # update betas
  lm_update <- lm(z ~ -1 + xhlp, weight = w)
  eta <- lmUpdate$fitted
  betas <- lmUpdate$coef
  lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
}

legend("topright",
  legend = c(
    "True betas", "Initial betas",
    "1st update", "2nd update", "3rd update"
  ),
  col = c(1, 1, 2, 3, 4), pch = c(19, NA, 1, 2, 3), lty = c(NA, 2, 2, 2, 2)
)

z <- log(y + .5)
betas <- lm(z ~ -1 + xhlp)$coef
plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
lines(betas, col = 2, type = "b", lty = 2)

# calculate current eta
eta <- xhlp %*% betas
eta
ylogy <- function(y) {
  ifelse(y == 0, rep(0, length(y)), y * log(y))
}
deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
deviance_old <- 1e30
plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
lines(betas, type = "b", lty = 2)
tol <- 1e-6
iteration <- 0
while (((devianceOld - deviance) / devianceOld) > tol) {
  iteration <- iteration + 1
  z <- eta + exp(-eta) * (y - exp(eta))
  w <- c(exp(eta))
  lm_update <- lm(z ~ -1 + xhlp, weight = w)
  eta <- lm_update$fitted
  betas <- lmUpdate$coef
  lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat(
    "iteration", iteration, "Deviance Old",
    deviance_old, "Deviance", deviance, "\n"
  )
}
var_beta <- solve(t(xhlp) %*% diag(w) %*% xhlp)
glmfit <- glm(y ~ -1 + xhlp, family = poisson)
comp <- data.frame(
  glmfit = c(glmfit$deviance, glmfit$coef, summary(glmfit)$coef[, 2]),
  ourFit = c(deviance, betas, sqrt(diag(varBeta)))
)
row.names(comp) <- c(
  "deviance", paste("beta", 1:3, sep = ""),
  paste("se", 1:3, sep = "")
)
