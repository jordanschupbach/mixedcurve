# mixed curve (n-w)

# load libraries
library(Matrix)
library(lme4)

# {{{ generate data

set.seed(300)
# simulate data
n <- 200
nind <- 50
x <- runif(n * nind)
xmat <- cbind(rep(1, n * nind), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
bs_true <- rnorm(nind * 3, 0, 0.21)
id <- rep(1:nind, each = n)
z_blocks <- lapply(seq_len(ncol(xmat)),
                   function(j) lapply(split(xmat[, j], id), as.matrix))
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xmat %*% betas_true + zmat %*% bs_true)
eta_true <- as.vector(xmat %*% betas_true)

x <- runif(n)
xmat <- cbind(rep(1, n), x, x^2)
eta_true <- as.vector(xmat %*% betas_true)
y <- rpois(n, exp(eta_true))
gauss_kern <- function(pt) {
  exp((-1 / 2) * ((pt)^2))
}
k_h <- function(pt, h) {
  gauss_kern(pt / h) / h
}
k_ih <- function(pt, qry, h) {
  k_h(pt - qry, h)
}
xseq <- seq(0, 1, length.out = 1000)
vals <- numeric(length(xseq))
coefmat <- matrix(0, nrow = length(xseq), ncol = 20)
str(coefmat)
cl <- parallel::makeCluster(31)
invisible(parallel::clusterEvalQ(cl, { library(lme4) }))
parallel::clusterExport(cl, c("x", "y", "id", "k_ih", "k_h", "gauss_kern"))
fits <- parallel::parLapply(cl, xseq, function(qry) {
  kernel_weights <- sqrt(k_ih(x, qry, 0.03))
  glm1 <- glmer(y ~ 1 + (1 | id), family = "poisson", weights = kernel_weights)
  list(fixef = as.numeric(fixef(glm1)[1]),
       coefs = as.numeric(coef(glm1)$id[[1]]))
})
parallel::stopCluster(cl)
fixefs <- unlist(lapply(fits, function(elmt) exp(elmt$fixef)))
coefs <- do.call(rbind, lapply(fits, function(elmt) exp(elmt$coefs)))
plot(x, y, col = id)
lines(xseq, fixefs, col = "black", lwd = 3)


plot(x, exp(eta_true))
gauss_kern <- function(pt) {
  exp((-1 / 2) * ((pt)^2))
}
k_h <- function(pt, h) {
  gauss_kern(pt / h) / h
}
k_ih <- function(pt, qry, h) {
  k_h(pt - qry, h)
}
xseq <- seq(0, 1, length.out = 1000)
vals <- numeric(length(xseq))
coefmat <- matrix(0, nrow = length(xseq), ncol = 20)
str(coefmat)
cl <- parallel::makeCluster(31)
invisible(parallel::clusterEvalQ(cl, { library(lme4) }))
parallel::clusterExport(cl, c("x", "y", "id", "k_ih", "k_h", "gauss_kern"))
fits <- parallel::parLapply(cl, xseq, function(qry) {
  kernel_weights <- sqrt(k_ih(x, qry, 0.03))
  glm1 <- glm(y ~ 1, family = "poisson", weights = kernel_weights)
  list(fixef = as.numeric(coef(glm1)[1]))
})





parallel::stopCluster(cl)
fixefs <- unlist(lapply(fits, function(elmt) exp(elmt$fixef)))
lines(xseq, fixefs, col = "red", lwd = 3)
coefs <- do.call(rbind, lapply(fits, function(elmt) exp(elmt$coefs)))
plot(x, y, col = id)
lines(xseq, fixefs, col = "black", lwd = 3)









nrow(xseq_mat)

z_blocks <- lapply(seq_len(ncol(xseq_mat)),
                   function(j) lapply(split(xseq_mat[, j], id), as.matrix))

zseq_blocks <- lapply(seq_along(zseq_blocks[[1]]), function(i)
  do.call(cbind, lapply(zseq_blocks, function(block) block[[i]]))
)
str(zseq_blocks)
zmat_seq <- bdiag(zseq_blocks)
nrow(zmat_seq)
ncol(zmat_seq)
etaseq <- as.vector(xmat %*% betas_true + zmat_seq %*% bs_true)




# }}} generate data

# {{{ Fit model

gauss_kern <- function(pt) {
  exp((-1 / 2) * ((pt)^2))
}
k_h <- function(pt, h) {
  gauss_kern(pt / h) / h
}
k_ih <- function(pt, qry, h) {
  k_h(pt - qry, h)
}
xseq <- seq(0, 1, length.out = 1000)
vals <- numeric(length(xseq))
coefmat <- matrix(0, nrow = length(xseq), ncol = 20)
str(coefmat)
cl <- parallel::makeCluster(31)
invisible(parallel::clusterEvalQ(cl, { library(lme4) }))
parallel::clusterExport(cl, c("x", "y", "id", "k_ih", "k_h", "gauss_kern"))
fits <- parallel::parLapply(cl, xseq, function(qry) {
  kernel_weights <- sqrt(k_ih(x, qry, 0.03))
  glm1 <- glmer(y ~ 1 + (1 | id), family = "poisson", weights = kernel_weights)
  list(fixef = as.numeric(fixef(glm1)[1]),
       coefs = as.numeric(coef(glm1)$id[[1]]))
})
parallel::stopCluster(cl)
fixefs <- unlist(lapply(fits, function(elmt) exp(elmt$fixef)))
coefs <- do.call(rbind, lapply(fits, function(elmt) exp(elmt$coefs)))
plot(x, y, col = id)
lines(xseq, fixefs, col = "black", lwd = 3)
for (i in seq_len(ncol(coefmat))) {
  lines(xseq, coefs[, i], col = adjustcolor(i, 0.8), lwd = 3)
}
lines(xseq, fixefs, col = "black", lwd = 5)
xseq <- seq(0, 1, length.out = 1000)
xseq_mat <- cbind(rep(1, length(xseq)), xseq, xseq^2)
plot(x, y, col = adjustcolor(id, 0.002),
     ylim = range(c(y, exp(eta_true))),
     ylab = "Count / lambda = exp(eta)", xlab = "x")
for (i in 1:nind) {
  xseq_mat %*% (betas_true + matrix(bs_true, nind, 3)[i,])
  lines(xseq, exp(xseq_mat %*% (betas_true + matrix(bs_true, nind, 3)[i,])),
  lty = 2, col = adjustcolor(i, 0.5), lwd = 3)
}
lines(xseq, coefs[, 1], col = adjustcolor(1, 0.8), lwd = 3)
lines(xseq, coefs[, 2], col = adjustcolor(2, 0.8), lwd = 3)
lines(xseq, coefs[, 3], col = adjustcolor(3, 0.8), lwd = 3)
lines(xseq, coefs[, 4], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 5], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 6], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 7], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 8], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 9], col = adjustcolor(4, 0.8), lwd = 3)
lines(xseq, coefs[, 10], col = adjustcolor(4, 0.8), lwd = 3)


# }}} Query a point

#
