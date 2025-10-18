
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
z_blocks <- lapply(seq_len(ncol(xhlp)),
                   function(j) lapply(split(xhlp[, j], id), as.matrix))
z_blocks <- lapply(seq_along(Z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(Z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
z_blocks_mat <- lapply(1:1, function(j) lapply(split(xhlp[, j], id), as.matrix))
z_blocks_mat <- lapply(seq_along(Z_blocks_mat[[1]]), function(i) {
  do.call(cbind, lapply(Z_blocks_mat, function(block) block[[i]]))
})
z_blocks_mat
zmat <- bdiag(Z_blocks_mat)
eta_true <- as.vector(xhlp %*% betasTrue + Z %*% bTrue)
y <- rpois(n * nind, exp(etaTrue))

gauss_kern <- function(x) {
  exp((-1 / 2) * ((x)^2))
}
k_h <- function(x, h) {
  gauss_kern(x / h) / h
}
k_ih <- function(x, qry, h) {
  k_h(x - qry, h)
}
#### query pt
query_pt <- function(qry, bw) {
  kernel_weights <- k_ih(x, qry, bw)
  betas <- matrix(rep(log(mean(y)), 1), 1, 1)
  bs <- matrix(rnorm(nind * 1, 0, 1), nind, 1)
  eta <- xhlp[, 1] %*% betas + zmat %*% bs
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
    wz <- z * kernel_weights
    w <- c(exp(as.numeric(eta)))
    lm_update <- lmer(
      wz ~ -1 + kernel_weights + (-1 + kernel_weights | id), weight = w
    )
    betas <- matrix(as.numeric(fixef(lm_update)), 1, 1)
    bs <- as.numeric(t(do.call(cbind, ranef(lm_update)$id)))
    eta <- as.numeric(xhlp[, 1] %*% betas + zmat %*% bs)
    deviance_old <- deviance
    deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  }
  list(betas = betas, bs = bs, qry = qry)
}
cl <- parallel::makeCluster(31)
parallel::clusterEvalQ(cl, {
  library(Matrix)
  library(lme4)
})
parallel::clusterExport(cl, c("x", "y", "id", "nind", "n", "xhlp", "Z", "Z_mat",
                              "bTrue", "k_h", "k_ih", "gauss_kern", "query_pt"))
xseq <- seq(0.0, 0.98, by = 0.01)
fits <- parallel::parLapply(cl,
  xseq, function(qry) {
    query <- query_pt(qry, 0.5)
    list(betas = query$betas, qry = query$qry)
  }
)
parallel::stopCluster(cl)
ft <- unlist(lapply(fits, function(elmt) {
  elmt$betas
}))
plot(xseq, exp(ft), type = "l", ylim = c(0, 40))

# plot results
plot(x, exp(eta), col = id, ylim = range(c(exp(eta), y)),
     ylab = "Count / lambda = exp(eta)", xlab = "x")
sortedx <- lapply(split(x, id), function(elmt) sort(elmt, index.return = TRUE))
split_true_eta <- split(etaTrue, id)
split_y <- split(y, id)
for (i in seq_along(split_y)) {
  lines(sortedx[[i]]$x, exp(split_true_eta[[i]][sortedx[[i]]$ix]), col = i)
}
for (i in seq_along(split_y)) {
  points(sortedx[[i]]$x, split_y[[i]][sortedx[[i]]$ix],
         col = adjustcolor(i, 0.3), pch = 17)
}
legend("topleft", c("True", "Fitted", "Actual Data"), lty = c(1, NA, NA),
       pch = c(NA, 1, 17), col = c("black", "black"), bty = "n")
