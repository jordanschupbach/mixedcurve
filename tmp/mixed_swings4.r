# NOTE: is this any different from mixed_swings3.r?
# load libraries
library(Matrix)
library(lme4)
set.seed(300)
# simulate data
n <- 200
nind <- 20
x <- runif(n * nind)
xhlp <- cbind(rep(1, n * nind), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
bs_true <- rnorm(nind * 3, 0, 0.21)
id <- rep(1:nind, each = n)
z_blocks <- lapply(
  seq_len(ncol(xhlp)),
  function(j) lapply(split(xhlp[, j], id), as.matrix)
)
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xhlp %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
# initial values for IWLS
betas <- rep(log(mean(y)), ncol(xhlp))
bs <- matrix(rnorm(nind * ncol(xhlp), 0, 1), nind, ncol(xhlp))
eta <- as.numeric(xhlp %*% betas + zmat %*% bs_true)
id <- rep(1:nind, each = n)
ylogy <- function(y) {
  ifelse(y == 0, rep(0, length(y)), y * log(y))
}
deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
deviance_old <- 1e30
iteration <- 0
tol <- 1e-6
# IWLS loop
while (((deviance_old - deviance) / deviance_old) > tol) {
  iteration <- iteration + 1
  z <- as.numeric(eta + exp(-eta) * (y - exp(eta)))
  w <- c(exp(as.numeric(eta)))
  lm_update <- lmer(
    z ~ 1 + xhlp[, 2] +
      I(xhlp[, 2]^2) + (1 + xhlp[, 2] + I(xhlp[, 2]^2) | id),
    weight = w
  )
  betas <- matrix(as.numeric(fixef(lm_update)), ncol(xhlp), 1)
  bs <- as.numeric(t(do.call(cbind, ranef(lm_update)$id)))
  eta <- as.numeric(xhlp %*% betas + zmat %*% bs)
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
}

# plot results
plot(x, exp(eta),
  col = id, ylim = range(c(exp(eta), y)),
  ylab = "Count / lambda = exp(eta)", xlab = "x"
)
sortedx <- lapply(split(x, id), function(elmt) sort(elmt, index.return = TRUE))
split_true_eta <- split(etaTrue, id)
split_y <- split(y, id)
for (i in seq_along(split_y)) {
  lines(sortedx[[i]]$x, exp(split_true_eta[[i]][sortedx[[i]]$ix]), col = i)
}
for (i in seq_along(split_y)) {
  points(sortedx[[i]]$x, split_y[[i]][sortedx[[i]]$ix],
    col = adjustcolor(i, 0.3), pch = 17
  )
}
legend("topleft", c("True", "Fitted", "Actual Data"),
  lty = c(1, NA, NA),
  pch = c(NA, 1, 17), col = c("black", "black"), bty = "n"
)
xseq <- seq(0.0, 1.0, length.out = 200)
xseqmat <- cbind(1, xseq, xseq^2)
lines(xseq, exp(xseqmat %*% betas), lwd = 4)
