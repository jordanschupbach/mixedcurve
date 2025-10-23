library(Matrix)
library(lme4)
library(mixedcurve)
set.seed(300)
n <- 400
nind <- 80
x <- runif(n * nind)
xmat <- cbind(rep(1, n * nind), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
bs_true <- rnorm(nind * 3, 0, 0.21)
id <- rep(1:nind, each = n)
z_blocks <- lapply(
  seq_len(ncol(xmat)),
  function(j) lapply(split(xmat[, j], id), as.matrix)
)
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- Matrix::bdiag(z_blocks)
pop_eta_true <- as.vector(xmat %*% betas_true)
eta_true <- as.vector(xmat %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
tdf <- data.frame(y = y, x = x, id = as.factor(id))
qseq <- seq(0, 1, length.out = 100)
queries <- glpk(
  formula = y ~ K_h(x),
  queries = qseq,
  data = tdf,
  h = 0.02,
  family = "poisson",
  parallel = TRUE
)
qs <- unlist(lapply(queries[[1]], function(elmt) as.numeric(elmt$coefs)))
plot(x, exp(pop_eta_true), col = adjustcolor("blue", 0.3), pch = 16)
lines(qseq, exp(qs), type = "l", col = adjustcolor("red", 0.8), lwd = 3)
