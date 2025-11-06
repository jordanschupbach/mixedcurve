library(mixedcurve)
nt <- c(100)
ni <- c(10)
set.seed(123)
df1 <- mixedcurve::gen_hfanova_data(
  f = function(t, i) {
    3.2 * mixedcurve::m3(t, i)
  },
  n = c(nt, ni), sigmas = c(0.01, 0.15), bounds = c(0, 1),
  ngrp = 3,
  ndim = 1, px = NULL, pxargs = NULL,
  family = "gaussian", white_noise = TRUE
)
par(mfrow = c(3, 1))
mixedcurve::dark_mode()
plot(df1$x1, df1$y,
  main = "Cuevas M2 function data (1)",
  ylab = "y", xlab = "x", pch = 20, col = adjustcolor(df1$cov, 0.1)
)
subsetdf <- function(dataf, dnames, query, h) {
  if (length(dnames) != length(query)) {
    stop("Length of dnames and query must be the same")
  }
  if (!all(dnames %in% names(dataf))) {
    stop("Some dnames are not found in the dataframe")
  }
  queries <- t(replicate(nrow(dataf), as.numeric(query), simplify = "matrix"))
  distances <- NULL
  if (length(query) > 1) {
    distances <- sqrt(apply(
      matrix(unlist(lapply(seq_len(nrow(dataf)), function(i) {
        dataf[i, dnames] - queries[i, ]
      })), nrow = nrow(dataf), byrow = TRUE)^2,
      1, sum
    ))
  } else {
    distances <- abs(dataf[, dnames] - as.numeric(query))
  }
  dataf[distances <= h, , drop = FALSE]
}
nq <- 101
qseq <- seq(0, 1, length.out = nq)
bandwidth <- 0.04
tstats <- numeric(nrow(as.matrix(qseq)))
qseq_fit <- numeric(nrow(as.matrix(qseq)))
pvals <- numeric(nrow(as.matrix(qseq)))
cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, varlist = c("df1", "subsetdf", "qseq", "bandwidth"))
qs <- parallel::parLapply(seq_len(nrow(as.matrix(qseq))), function(i) {
  df_sub <- subsetdf(df1, c("x1"), qseq[i], h = bandwidth)
  model1 <- lme4::lmer(y ~ cov + (1 | grp1), data = df_sub)
  model0 <- lme4::lmer(y ~ -1 + (1 | grp1), data = df_sub)
  qseq_fit <- lme4::fixef(model1)
  pval <- anova(model0, model1)$`Pr`[2]
  tstat <- sqrt(anova(model0, model1)$Chisq[2])
  list(fit = qseq_fit, pval = pval, tstat = tstat)
}, cl = cl)
parallel::stopCluster(cl)
fits <- matrix(unlist(lapply(qs, function(elmt) elmt$fit)),
  nrow = nq, byrow = TRUE
)
pvals <- unlist(lapply(qs, function(elmt) elmt$pval))
tstats <- unlist(lapply(qs, function(elmt) elmt$tstat))
str(qseq)
lines(qseq, fits[, 1], col = "orange", lwd = 2)
lines(qseq, fits[, 1] + fits[, 2], col = "orange", lwd = 2)
lines(qseq, fits[, 1] + fits[, 3], col = "orange", lwd = 2)
plot(qseq, pvals, type = "l", ylim = c(0, 1))
nb <- 100
boot_tstats <- matrix(NA, nrow = nb - 1, ncol = nq)
system.time({
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  parallel::clusterExport(cl, varlist = c(
    "df1", "subsetdf",
    "qseq", "bandwidth"
  ))
  for (i in seq_len(nrow(as.matrix(qseq)))) {
    df_sub <- subsetdf(df1,
      dnames = c("x1"),
      qseq[i], h = bandwidth
    )
    parallel::clusterExport(cl, varlist = c("df_sub"))
    boot_t <- parallel::parLapply(1:(nb - 1), function(b) {
      df_sub$pcov <- sample(df_sub$cov, replace = TRUE)
      model1 <- lme4::lmer(y ~ pcov + (1 | grp1),
        data = df_sub
      )
      model0 <- lme4::lmer(y ~ 1 + (1 | grp1), data = df_sub)
      anova_res <- anova(model1, model0)
      stat <- anova_res$Chisq[2]
      stat
    }, cl = cl)
    boot_tstats[, i] <- unlist(boot_t)
  }
  parallel::stopCluster(cl)
})
system.time({
  adj_pvals <- mixedcurve::romano_wolf(tstats, boot_tstats)
  # adj_pvals <- mixedcurve::westfall_young(tstats, boot_tstats)
})
plot(qseq, adj_pvals, type = "l")
