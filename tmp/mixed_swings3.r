
library(Matrix)
library(lme4)
set.seed(307)
n <- 200
nind <- 20
x <- runif(n * nind)
xhlp <- cbind(rep(1, n * nind), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
bs_true <- rnorm(nind * 3, 0, 0.21)
id <- rep(1:nind, each = n)
z_blocks <- lapply(seq_len(ncol(xhlp)),
                   function(j) lapply(split(xhlp[, j], id), as.matrix))
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xhlp %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
betas <- rep(log(mean(y)), ncol(xhlp))
bs <- matrix(rnorm(nind * ncol(xhlp), 0, 1), nind, ncol(xhlp))
eta <- as.numeric(xhlp %*% betas + zmat %*% bs_true)
str(eta)
id <- rep(1:nind, each = n)
ylogy <- function(y) {
  ifelse(y == 0, rep(0, length(y)), y * log(y))
}
deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
deviance_old <- 1e30
iteration <- 0
tol <- 1e-6
while (((deviance_old - deviance) / deviance_old) > tol) {
  iteration <- iteration + 1
  z <- as.numeric(eta + exp(-eta) * (y - exp(eta)))
  w <- c(exp(as.numeric(eta)))
  lm_update <- lmer(z ~ 1 + xhlp[, 2] + I(xhlp[, 2]^2) +
                      (1 + xhlp[, 2] + I(xhlp[, 2]^2) | id), weight = w)
  betas <- matrix(as.numeric(fixef(lm_update)), ncol(xhlp), 1)
  bs <- as.numeric(t(do.call(cbind, ranef(lm_update)$id)))
  eta <- as.numeric(xhlp %*% betas + zmat %*% bs)
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat("iteration", iteration, "Deviance Old",
      deviance_old, "Deviance", deviance, "\n")
}
lm_update <- glmer(y ~ 1 + xhlp[, 2] + I(xhlp[, 2]^2) +
                     (1 + xhlp[, 2] + I(xhlp[, 2]^2) | id), family = poisson)
mixedcurve::dark_mode()
plot(x, exp(eta), col = id, ylim = range(c(exp(eta), y)))
sortedx <- lapply(split(x, id), function(elmt) sort(elmt, index.return = TRUE))
split_true_eta <- split(eta_true, id)
split_y <- split(y, id)
for (i in seq_along(split_y)) {
  lines(sortedx[[i]]$x, exp(split_true_eta[[i]][sortedx[[i]]$ix]), col = i)
}
for (i in seq_along(split_y)) {
  points(sortedx[[i]]$x, split_y[[i]][sortedx[[i]]$ix],
         col = adjustcolor(i, 0.3), pch = 17)
}
legend("topleft", c("True", "Fitted", "Actual Data"), lty = c(1, NA, NA),
       pch = c(NA, 20, 17), col = c("black", "black"), bty = "n")
