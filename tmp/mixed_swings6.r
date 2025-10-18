library(Matrix)
library(lme4)
set.seed(300)
# Gen data
n <- 200
nind <- 80
id <- rep(1:nind, each = n)
xhlp <- cbind(rep(1, n * nind), runif(n * nind))
betas_true <- c(2.323)
bs_true <- rnorm(nind * 1, 0, 0.21)
length(bTrue)
id <- rep(1:nind, each = n)
z_blocks <- lapply(1:1, function(j) lapply(split(xhlp[, j], id), as.matrix))
z_blocks <- lapply(seq_long(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(
  matrix(xhlp[, 1], n * nind, 1) %*%
    betasTrue + zmat %*% bs_true
)
split_eta_true <- split(etaTrue, id)

y <- rpois(n * nind, exp(etaTrue))
# kern funs
gauss_kern <- function(x) {
  exp((-1 / 2) * ((x)^2))
}
k_h <- function(x, h) {
  gauss_kern(x / h) / h
}
k_ih <- function(x, qry, h) {
  k_h(x - qry, h)
}
plot(xhlp[, 2], y, col = adjustcolor(id, 0.2), pch = 20)
for (i in seq_along(split_eta_true)) {
  ord <- order(xhlp[id == i, 2])
  lines(xhlp[id == i, 2][ord], exp(split_eta_true[[i]][ord]), col = i)
}

betas <- c(log(mean(y)), log(mean(y)))
bs <- matrix(rnorm(nind * 2, 0, 1), nind, 2)
eta <- xhlp %*% betas + Z %*% bs_true
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
  betas <- matrix(as.numeric(fixef(lmUpdate)), 2, 1)
  bs <- as.numeric(t(do.call(cbind, ranef(lmUpdate)$id)))
  as.numeric(t(do.call(cbind, ranef(lmUpdate)$id)))
  eta <- xhlp %*% betas + Z %*% bs
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat(
    "iteration", iteration, "Deviance Old",
    deviance_old, "Deviance", deviance, "\n"
  )
}
lm_update <- glmer(y ~ -1 + xhlp + (-1 + xhlp | id), family = poisson)
summary(lmUpdate)
cbind(fixef(lm_update), betas, betasTrue)
cbind(ranef(lm_update)$id$xhlp, bs, bTrue)
sum((eta - etaTrue)^2)
cbind(fixef(lmUpdate), betas, betasTrue)
