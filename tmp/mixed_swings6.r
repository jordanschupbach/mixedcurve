# load libraries
library(Matrix)
library(lme4)

# {{{ generate data

set.seed(300)
# simulate data
n <- 400
nind <- 50
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
eta_true <- as.vector(xmat %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
tdf <- data.frame(y = y, x = x, id = as.factor(id))
plot(x, y, col = id)
plot(x, exp(eta_true), col = id)

# }}} generate data


glpk_query <- function(formula,
                       query,
                       data,
                       degree = 0,
                       kernel = mixedcurve::gauss_kern,
                       h, family = "gaussian") {
  parse_formula <- mixedcurve::parse_terms(formula)
  print(parse_formula)
  # weights <- sqrt(mixedcurve::k_ih(data$x, query, h))
}








# {{{ Estimation
gauss_kern <- function(pt) {
  exp((-1 / 2) * ((pt)^2))
}
k_h <- function(pt, h) {
  gauss_kern(pt / h) / h
}
k_ih <- function(pt, qry, h) {
  k_h(pt - qry, h)
}
xseq <- seq(0, 1, length.out = 100)
cl <- parallel::makeCluster(31)
invisible(parallel::clusterEvalQ(cl, {
  library(lme4)
}))
parallel::clusterExport(cl, c("x", "y", "id", "k_ih", "k_h", "gauss_kern"))
fits <- parallel::parLapply(cl, xseq, function(qry) {
  kernel_weights <- sqrt(k_ih(x, qry, 0.05))
  glm1 <- glmer(y ~ 1 + (1 | id), family = "poisson", weights = kernel_weights)
  list(
    fixef = as.numeric(fixef(glm1)[1]),
    coefs = as.numeric(coef(glm1)$id[[1]])
  )
})
parallel::stopCluster(cl)
fixefs <- unlist(lapply(fits, function(elmt) exp(elmt$fixef)))
coefs <- do.call(rbind, lapply(fits, function(elmt) exp(elmt$coefs)))
plot(x, exp(eta_true), col = id)
lines(xseq, fixefs, col = "black", lwd = 5)
for (i in 1:nind) {
  lines(xseq, coefs[, i], col = adjustcolor(i, 0.5), lwd = 3)
}
lines(xseq, fixefs, col = "black", lwd = 5)
# }}} Estimation


#
