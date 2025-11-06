df1 <- mixedcurve::gen_hfanova_data(
  f = mixedcurve::mdoppler,
  bounds = c(0, 1),
  sigmas = c(0.5, 0),
  n = c(4000, 1),
  ngrp = 1
)
bandwidth <- 0.003
nb <- 200
mixedcurve::dark_mode()
par(mfrow = c(2, 1))
plot(df1$x1, df1$y, pch = 20, cex = 1.0)
subsetdf <- function(df, q, h) {
  df[abs(df$x1 - q) <= h, ]
}
nq <- 101
qseq <- seq(0, 1, length.out = nq)
tstats <- numeric(nq)
qseq_fit <- numeric(nq)
pvals <- numeric(nq)
for (i in seq_along(qseq)) {
  df_sub <- subsetdf(df1, qseq[i], h = bandwidth)
  model1 <- lm(y ~ 1, data = df_sub)
  model0 <- lm(y ~ -1, data = df_sub)
  qseq_fit[i] <- predict(model1, newdata = data.frame(x1 = qseq[i]))
  pvals[i] <- anova(model0, model1)$`Pr(>F)`[2]
  tstats[i] <- sqrt(anova(model0, model1)$F[2])
}
lines(qseq, qseq_fit, col = "orange", lwd = 2)
abline(h = 0, col = "red", lty = 2)
boot_tstars <- matrix(NA, nrow = nb - 1, ncol = nq)
boot_pvals <- matrix(NA, nrow = nb - 1, ncol = nq)
set.seed(1234)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, varlist = c("df1", "subsetdf"))
for (i in 1:nq) {
  df_sub <- subsetdf(df1, qseq[i], h = bandwidth)
  parallel::clusterExport(cl, varlist = c("df_sub"))
  bootstraps <- parallel::parLapply(1:(nb - 1), function(b) {
    df_sub$y_boot <- sample(df_sub$y, replace = TRUE) - mean(df_sub$y)
    model1 <- lm(y_boot ~ 1,
      data = df_sub
    )
    model0 <- lm(y_boot ~ -1, data = df_sub)
    anova_res <- anova(model0, model1)
    f_stat <- anova_res$F[2]
    pval <- anova_res$P[2]
    list("fstat" = f_stat, "pval" = pval)
  }, cl = cl)
  boot_tstars[, i] <- unlist(lapply(bootstraps, function(elmt) elmt$fstat))
  boot_pvals[, i] <- unlist(lapply(bootstraps, function(elmt) elmt$pval))
}
parallel::stopCluster(cl)
system.time({
  adj_pvals_rw <- mixedcurve::romano_wolf(tstats, boot_tstars)
  # Doesn't make sense to use WY here since we have p-values from the bootstrap (not permutation)
  # adj_pvals_wy <- mixedcurve::westfall_young(tstats, boot_pvals)
})

# par(mfrow = c(1, 1))
plot(qseq, pvals,
  ylim = c(0, 1),
  type = "b", pch = 20, col = 1,
  ylab = "Romano-Wolf adjusted p-values",
  xlab = "Index"
)
lines(qseq, adj_pvals_rw,
  type = "b", pch = 20, col = 2,
  xlab = "Index"
)
# lines(qseq, adj_pvals_wy,
#   type = "b", pch = 20, col = 3,
#   xlab = "Index"
# )
abline(h = 0.05, col = "red", lty = 2)
lines(
  qseq, p.adjust(pvals,
    method = "BY",
    n = length(pvals)
  ),
  type = "l", col = 4
)
lines(
  qseq, p.adjust(pvals,
    method = "holm",
    n = length(pvals)
  ),
  type = "l", col = 5
)
lines(
  qseq, p.adjust(pvals,
    method = "hochberg",
    n = length(pvals)
  ),
  type = "l", col = 6
)
lines(
  qseq, p.adjust(pvals,
    method = "hommel",
    n = length(pvals)
  ),
  type = "l", col = 7
)
lines(
  qseq, p.adjust(pvals,
    method = "BH",
    n = length(pvals)
  ),
  type = "l", col = 8
)
p.adjust(pvals, method = "holm", n = length(pvals))
