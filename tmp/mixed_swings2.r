library(Matrix)
library(lme4)
set.seed(300)
n <- 200
nind <- 80
xhlp <- cbind(rep(1, n * nind), runif(n * nind))
betas_true <- c(2.323, 1.73)
bs_true <- rnorm(nind * 2, 0, 1.21)
length(bs_true)
id <- rep(1:nind, each = n)
z_blocks <- lapply(
  seq_len(ncol(xhlp)),
  function(j) {
    lapply(split(xhlp[, j], id), as.matrix)
  }
)
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xhlp %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
betas <- c(log(mean(y)), log(mean(y)))
bs <- matrix(rnorm(nind * 2, 0, 1), nind, 2)
eta <- xhlp %*% betas + zmat %*% bs_true
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
  lm_update <- lmer(z ~ -1 + xhlp + (-1 + xhlp | id), weight = w)
  betas <- matrix(as.numeric(fixef(lm_update)), 2, 1)
  bs <- as.numeric(t(do.call(cbind, ranef(lm_update)$id)))
  as.numeric(t(do.call(cbind, ranef(lm_update)$id)))
  eta <- xhlp %*% betas + zmat %*% bs
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat(
    "iteration", iteration, "Deviance Old",
    deviance_old, "Deviance", deviance, "\n"
  )
}
lm_update <- glmer(y ~ -1 + xhlp + (-1 + xhlp | id), family = poisson)
summary(lm_update)
cbind(fixef(lm_update), betas, betas_true)
cbind(ranef(lm_update)$id$xhlp, bs, bs_true)
sum((eta - eta_true)^2)
cbind(fixef(lm_update), betas, betas_true)
