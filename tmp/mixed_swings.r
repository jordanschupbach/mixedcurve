library(Matrix)
library(lme4)
set.seed(300)
n <- 20
nind <- 1000
xhlp <- cbind(rep(1, n * nind))
betas_true <- c(2.323)
bs_true <- rnorm(nind, 0, 1.21)
zmat <- as.matrix(bdiag(replicate(nind, matrix(1, n, 1), simplify = FALSE)))
eta_true <- xhlp %*% betas_true + zmat %*% bs_true
y <- rpois(n * nind, exp(eta_true))
betas <- c(log(mean(y)))
bs <- matrix(rnorm(nind, 0, 1), nind, 1)
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
  z <- eta + exp(-eta) * (y - exp(eta))
  w <- c(exp(eta))
  lm_update <- lmer(z ~ -1 + xhlp + (-1 + xhlp | id), weight = w)
  betas <- matrix(as.numeric(fixef(lm_update)), 1, 1)
  bs <- matrix(as.numeric(ranef(lm_update)$id$xhlp), nind, 1)
  eta <- xhlp %*% betas + zmat %*% bs
  deviance_old <- deviance
  deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
  cat("iteration", iteration, "Deviance Old",
      deviance_old, "Deviance", deviance, "\n")
}
lm_update <- glmer(y ~ -1 + xhlp + (-1 + xhlp | id), family = poisson)
summary(lm_update)
cbind(fixef(lm_update), betas, betas_true)
cbind(ranef(lm_update)$id$xhlp, bs, bs_true)
sum((eta - eta_true)^2)




library(lme4)
set.seed(300)
n <- 1000
n_i <- 10
x <- runif(n * n_i)
id <- rep(1:n_i, each = n)
df1 <- data.frame(x = x, id = id)
xhlp <- as.matrix(rep(1, n_i * n), nrow = n * n_i)
betas_true <- c(2)
bs_true <- rnorm(n_i, 0, 0.5)
tdf1 <- df1
tdf1$X1 <- 1
z_blocks <- lapply(split(tdf1[, c("X1")], tdf1$id), as.matrix)
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xhlp %*% betas_true +  Z %*% bs_true)
length(etaTrue)
y <- rpois(n * n_i, as.vector(exp(etaTrue)))
df1$y <- y
glmer1 <- glmer(y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | id),
                data = df1, family = poisson)
cbind(betas_True,
  as.numeric(t(as.matrix(cbind(ranef(glmer1)$id[[1]])))),
  abs(abs(betas_True) -
        abs(as.numeric(t(as.matrix(cbind(ranef(glmer1)$id[[1]]))))))
)


#
