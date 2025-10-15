library(lme4)
set.seed(300)
n <- 100
n_i <- 5
x <- runif(n * n_i)
id <- rep(1:n_i, each = n)
df1 <- data.frame(x = x, id = id)
xhlp <- cbind(1, x, x^2)
betasTrue <- c(2, 0.8, -1.2)
betas_iTrue <- rnorm(n_i * 3, 0, 0.2)
tdf1 <- df1
tdf1$X1 <- 1
tdf1$X2 <- tdf1$x
tdf1$X3 <- tdf1$x^2
Z_blocks <- lapply(split(tdf1[, c("X1", "X2", "X3")], tdf1$id), as.matrix)
Z <- bdiag(Z_blocks)
etaTrue <- as.vector(xhlp %*% betasTrue +  Z %*% betas_iTrue)
length(etaTrue)
y <- rpois(n * n_i, as.vector(exp(etaTrue)))
df1$y <- y
glmer1 <- glmer(y ~ 1 + x + I(x^2) + (1 + x + I(x^2) | id), data = df1, family = poisson)
cbind(betas_iTrue
as.vector(t(as.matrix(cbind(ranef(glmer1)$id[[1]], ranef(glmer1)$id[[2]], ranef(glmer1)$id[[3]]))))

# cbind(as.vector(cbind(ranef(glmer1)$id[[1]], ranef(glmer1)$id[[2]], ranef(glmer1)$id[[3]])),
#       betas_iTrue)

# z <- log(y + .5)
# betas <- lm(z ~ -1 + xhlp)$coef
# # plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# # lines(betas, col = 2, type = "b", lty = 2)
# #calculate current eta
# eta <- xhlp %*% betas
# # eta
# ylogy <- function(y) {
#   return(ifelse(y == 0, rep(0, length(y)), y * log(y)))
# }
# deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
# devianceOld <- 1e30
# # plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# # lines(betas, type = "b", lty = 2)
# tol <- 1e-6
# iteration <- 0
# while (((devianceOld - deviance) / devianceOld) > tol) {
#   #start IRLS UPDATE STEP
#   iteration <- iteration + 1
#   #calculate pseudo data based on current betas
#   z <- eta + exp(-eta) * (y - exp(eta))
#   #calculate new weights: diagonal elements
#   w <- c(exp(eta))
#   #update betas
#   lmUpdate <- lm(z ~ -1 + xhlp, weight = w)
#   #eta<-xhlp%*%betas
#   eta <- lmUpdate$fitted
#   betas <- lmUpdate$coef
#   # lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
#   # criterion for convergence
#   devianceOld <- deviance
#   deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
#   cat("iteration", iteration, "Deviance Old", devianceOld, "Deviance", deviance, "\n")
# }
# # varBeta <- solve(t(xhlp) %*% diag(w) %*% xhlp)
# glmfit <- glm(y ~ -1 + xhlp, family = poisson)
# # comp <- data.frame(glmfit = c(glmfit$deviance, glmfit$coef, summary(glmfit)$coef[, 2]),
# #                    ourFit = c(deviance, betas, sqrt(diag(varBeta))))
# # row.names(comp) <- c("deviance", paste("beta", 1:3, sep = ""), paste("se", 1:3, sep = ""))
# # comp
# xseq <- seq(0.0, 1.0, length.out = 100)
# xmat <- cbind(1, xseq, xseq^2)
# etaHat <- xmat %*% betas
# etaTrue <- xmat %*% betasTrue
# muHat <- exp(xmat %*% betas)
# muTrue <- exp(xmat %*% betasTrue)
# # cbind(muHat, muTrue)
# plot(xseq, muTrue, type = "l", lwd = 3, col = 1,
#      ylim = range(c(muHat, muTrue, y)), ylab = "y", xlab = "x")
# points(x, y, pch = 19, col = adjustcolor("black", 0.1))
# lines(xseq, muHat, col = 2, lty = 2, lwd = 3)
# legend("topright", legend = c("True mean", "Estimated mean"), col = c(1, 2), lty = c(1, 2), lwd = 2)



A <- matrix(1:4, nrow=2)
B <- matrix(5:6, nrow=2)

# Compute the Kronecker product
result <- kronecker(A, B)

# Print the result
print(result)


I <- diag(3)  # 3x3 identity matrix

# Define a block matrix
B <- matrix(c(1, 2, 3, 4, 5, 6), nrow=2)

# Compute the Kronecker product
result <- kronecker(I, B)

result

Z <- bdiag(replicate(n_i, as.matrix(xhlp), simplify = FALSE))

Z_blocks <- lapply(split(xhlp, xhlp$id), function(data) { cbind(1, data$x, data$x^2) })
Z <- bdiag(Z_blocks)

str(Z)

tdf1 <- df1
df1$X1 <- 1
df1$X2 <- df1$x
df1$X3 <- df1$x^2
Z_blocks <- lapply(split(df1[, c("X1", "X2", "X3")], df1$id), as.matrix)
Z <- bdiag(Z_blocks)
str(Z_blocks[[1]])
str(Z)

