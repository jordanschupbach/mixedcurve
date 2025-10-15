set.seed(300)
n <- 1000
x <- runif(n)
xhlp <- cbind(1, x, x^2)
betasTrue <- c(2, 0.8, -1.2)
etaTrue <- xhlp %*% betasTrue
y <- rpois(n, exp(etaTrue))
z <- log(y + .5)
betas <- lm(z ~ -1 + xhlp)$coef
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# lines(betas, col = 2, type = "b", lty = 2)
#calculate current eta
eta <- xhlp %*% betas
# eta
ylogy <- function(y) {
  return(ifelse(y == 0, rep(0, length(y)), y * log(y)))
}
deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
devianceOld <- 1e30
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# lines(betas, type = "b", lty = 2)
tol <- 1e-6
iteration <- 0
while (((devianceOld - deviance) / devianceOld) > tol) {
  #start IRLS UPDATE STEP
  iteration <- iteration + 1
  #calculate pseudo data based on current betas
  z <- eta + exp(-eta) * (y - exp(eta))
  #calculate new weights: diagonal elements
  w <- c(exp(eta))
  #update betas
  lmUpdate <- lm(z ~ -1 + xhlp, weight = w)
  #eta<-xhlp%*%betas
  eta <- lmUpdate$fitted
  betas <- lmUpdate$coef
  # lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
  # criterion for convergence
  devianceOld <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat("iteration", iteration, "Deviance Old", devianceOld, "Deviance", deviance, "\n")
}
# varBeta <- solve(t(xhlp) %*% diag(w) %*% xhlp)
glmfit <- glm(y ~ -1 + xhlp, family = poisson)
# comp <- data.frame(glmfit = c(glmfit$deviance, glmfit$coef, summary(glmfit)$coef[, 2]),
#                    ourFit = c(deviance, betas, sqrt(diag(varBeta))))
# row.names(comp) <- c("deviance", paste("beta", 1:3, sep = ""), paste("se", 1:3, sep = ""))
# comp
xseq <- seq(0.0, 1.0, length.out = 100)
xmat <- cbind(1, xseq, xseq^2)
etaHat <- xmat %*% betas
etaTrue <- xmat %*% betasTrue
muHat <- exp(xmat %*% betas)
muTrue <- exp(xmat %*% betasTrue)
# cbind(muHat, muTrue)
plot(xseq, muTrue, type = "l", lwd = 3, col = 1,
     ylim = range(c(muHat, muTrue, y)), ylab = "y", xlab = "x")
points(x, y, pch = 19, col = adjustcolor("black", 0.1))
lines(xseq, muHat, col = 2, lty = 2, lwd = 3)
legend("topright", legend = c("True mean", "Estimated mean"), col = c(1, 2), lty = c(1, 2), lwd = 2)



