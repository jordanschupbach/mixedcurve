library(parallel)
# 1. Define the true curves
tf <- function(t, i) { 
  3 * exp(mixedcurve::m3(t, i)) - 2.7
}
tdata1 <- mixedcurve::gen_fanova_data(f = tf, 
                                      bounds = c(0, 1),
                                      n = 20,
                                      nx = 300,
                                      ngrp = 3,
                                      sigma = 0.02,
                                      family = "poisson")
summary(tdata1$df)
# {{{ Kernel functions
gauss_kern <- function(x) {
  return(exp((-1 / 2) * ((x)^2)))
}
k_h <- function(x, h) {
  return(gauss_kern(x / h) / h)
}
k_ih <- function(x, qry, h) {
  return(k_h(x - qry, h))
}
# }}} Kernel functions
# {{{ Query functions
tdf <- tdata1$df
qry <- 0.1
bw <- 0.2
query_pt <- function(tdf, qry, bw, verbose = TRUE) {
  print(paste("Starting Querying point: ", qry, " with bandwidth: ", bw))
  n_I <- length(unique(tdf$id))
  bdf <- tdf
  print(str(tdf))
  lm1 <- lm(y ~ 1 + grp, data = bdf)
  xmat <- model.matrix(lm1)
  # TODO: find a better way to do this
  # betahats <- c(log(mean(bdf$y)),
  #               log(mean(bdf$y[bdf$grp == 1])) - log(mean(bdf$y[bdf$grp == 2])),
  #               log(mean(bdf$y[bdf$grp == 1])) - log(mean(bdf$y[bdf$grp == 3])))
  betahats <- rnorm(ncol(xmat))
  print(betahats)
  eta <- xmat %*% betahats
  deviance <- 2 * sum(ifelse(bdf$y == 0,
                      rep(0, length(bdf$y)),
                      bdf$y * log(bdf$y)) - bdf$y * eta - (bdf$y - exp(eta)))
  print(deviance)
  devianceOld <- 1e30
  tol <- 1e-8
  iteration <- 0
  verbose <- TRUE
  kernel_weights <- k_ih(bdf$x1, qry, bw)
  while (((devianceOld - deviance) / devianceOld) > tol) {
    iteration <- iteration + 1
    z <- eta + exp(-eta) * (bdf$y - exp(eta))
    w <- as.numeric(exp(eta))
    wz <- matrix(z * kernel_weights)
    wx <- xmat[, 1] * kernel_weights
    wgrp <- xmat[, 2] * kernel_weights
    wgrp2 <- xmat[, 3] * kernel_weights
    ttdf <- data.frame(wz = wz, wx = wx, wgrp = wgrp, id = bdf$id)
    tlm <- lm(wz ~ -1 + wx + wgrp + wgrp2, data = ttdf, weight = w)
    tlm2 <- lm(wz ~ -1 + wx, data = ttdf, weight = w)
    fepvals <- suppressMessages(anova(tlm, tlm2)$Pr[2])
    betahats <- as.numeric(coef(tlm))
    eta <- xmat %*% betahats
    devianceOld <- deviance
    deviance <- 2 * sum(ifelse(bdf$y == 0,
                               rep(0, length(bdf$y)),
                               bdf$y * log(bdf$y)) - bdf$y * eta - (bdf$y - exp(eta)))
    if (verbose) {
      cat("iteration", iteration, "Deviance Old", devianceOld, "Deviance New", deviance, "\n")
    }
  }
  ret <- c(coef(tlm))  
  list(fits = ret, beta = betahats, fepvals = fepvals)
  # deviance
}
query_pt(tdata1$df, 0.1, 0.2)


query_function <- function(dframe, qry, bw) {
  tempdf <- dframe
  query <- query_pt(tempdf, qry, bw, FALSE)
  list(beta = query)
}
# }}} Query functions
# {{{ Estimate the model
df <- tdata1$df
xseq <- seq(10, 40, length.out = 100)
bandwidth <- 2
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, list("k_ih", "query_function", "query_pt", "xseq", "df",
                       "bandwidth", "k_h", "gauss_kern"))
clusterEvalQ(cl, library(lme4))
clusterEvalQ(cl, library(dplyr))
results <- parLapply(cl, seq_along(xseq), function(idx) {
  query_function(df, xseq[idx], bandwidth)
})
stopCluster(cl)

queries <- do.call(rbind, lapply(results, function(x) x$beta$fits))
# }}} Estimate the model







# # https://statomics.github.io/SGA2019/assets/poissonIRWLS-implemented.html
# set.seed(300)
# xhlp <- cbind(1, rnorm(100), rnorm(100))
# betasTrue <- c(2, 0.8, 1.2)
# etaTrue <- xhlp %*% betasTrue
# y <- rpois(100, exp(etaTrue))
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# 
# 
# # Initial estimate
# iteration <- 0
# betas <- c(log(mean(y)), 0, 0)
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# lines(betas, type = "b", lty = 2)
# 
# 
# #Calculate current eta
# eta <- xhlp %*% betas
# 
# iteration <- 0
# for (i in 1:3) {
#   # start IRLS update step
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
#   lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
# }
# 
# 
# 
# 
# z <- log(y + .5)
# betas <- lm(z ~ -1 + xhlp)$coef
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# lines(betas, col = 2, type = "b", lty = 2)
# 
# #calculate current eta
# eta <- xhlp %*% betas
# eta
# ylogy <- function(y) {
#   return(ifelse(y == 0, rep(0, length(y)), y * log(y)))
# }
# deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
# devianceOld <- 1e30
# plot(betasTrue, ylab = expression(beta), ylim = c(0, 4), pch = 19, type = "b")
# lines(betas, type = "b", lty = 2)
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
#   lines(betas, type = "b", col = iteration + 1, pch = iteration, lty = 2)
#   # criterion for convergence
#   devianceOld <- deviance
#   deviance <- 2 * sum(ylogy(y) - y * eta - (y - exp(eta)))
#   cat("iteration", iteration, "Deviance Old", devianceOld, "Deviance", deviance, "\n")
# }
# 
# varBeta <- solve(t(xhlp) %*% diag(w) %*% xhlp)
# varBeta
# 
# glmfit <- glm(y ~ -1 + xhlp, family = poisson)
# comp <- data.frame(glmfit = c(glmfit$deviance, glmfit$coef, summary(glmfit)$coef[, 2]),
#                    ourFit = c(deviance, betas, sqrt(diag(varBeta))))
# row.names(comp) <- c("deviance", paste("beta", 1:3, sep = ""), paste("se", 1:3, sep = ""))
# comp


