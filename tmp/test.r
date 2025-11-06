romano_wolf <- function(t_stats, boot_t_stats) {
  t_stats <- abs(t_stats)
  boot_t_stats <- abs(boot_t_stats)
  ns <- ncol(boot_t_stats)
  nb <- nrow(boot_t_stats)
  pinit <- corr_padj <- pval <- vector(mode = "numeric", length = ns)
  stepdown_index <- order(t_stats, decreasing = TRUE)
  ro <- order(stepdown_index)
  for (s in 1:ns) {
    if (s == 1) {
      max_stat <- apply(boot_t_stats, 1, max)
      pinit[s] <- pmin(
        1,
        (sum(max_stat >= abs(t_stats[stepdown_index[s]])) + 1) / (nb + 1)
      )
    }
    if (s > 1) {
      boot_t_stat_udp <- boot_t_stats[, -stepdown_index[1:(s - 1)],
        drop = FALSE
      ]
      max_stat <- apply(boot_t_stat_udp, 1, max)
      pinit[s] <- pmin(
        1,
        (sum(max_stat >= abs(t_stats[stepdown_index[s]])) + 1) / (nb + 1)
      )
    }
  }
  for (j in 1:ns) {
    if (j == 1) {
      corr_padj[j] <- pinit[j]
    }
    if (j > 1) {
      corr_padj[j] <- max(pinit[j], corr_padj[j - 1])
    }
  }
  pval <- corr_padj[ro]
  pval
}







df1 <- mixedcurve::gen_hfanova_data(
  f = mixedcurve::m3,
  bounds = c(0, 1),
  sigmas = c(0.01, 0),
  n = c(200, 1),
  ngrp = 1
)

mixedcurve::dark_mode()
par(mfrow = c(2, 1))
plot(df1$x1, df1$y, pch = 20, cex = 1.0)
subsetdf <- function(df, q, h) {
  df[abs(df$x1 - q) <= h, ]
}
nq <- 401
qseq <- seq(0, 1, length.out = nq)
tstats <- numeric(nq)
qseq_fit <- numeric(nq)
pvals <- numeric(nq)
for (i in seq_along(qseq)) {
  print(i)
  df_sub <- subsetdf(df1, qseq[i], h = 0.09)
  model1 <- lm(y ~ poly(x1, 3, raw = TRUE), data = df_sub)
  model0 <- lm(y ~ -1, data = df_sub)
  qseq_fit[i] <- predict(model1, newdata = data.frame(x1 = qseq[i]))
  pvals[i] <- anova(model0, model1)$`Pr(>F)`[2]
  tstats[i] <- sqrt(anova(model0, model1)$F[2])
}
lines(qseq, qseq_fit, col = "orange", lwd = 2)
abline(h = 0, col = "red", lty = 2)
# tstats
boot_t_stats <- matrix(NA, nrow = 999, ncol = nq)
set.seed(1234)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, varlist = c("df1", "subsetdf"))
for (i in 1:nq) {
  df_sub <- subsetdf(df1, qseq[i], h = 0.07)
  parallel::clusterExport(cl, varlist = c("df_sub"))
  boot_t <- parallel::parLapply(1:999, function(b) {
    df_sub$y_boot <- sample(df_sub$y, replace = TRUE) - mean(df_sub$y)
    model1 <- lm(y_boot ~ poly(x1, 3, raw = TRUE),
      data = df_sub
    )
    model0 <- lm(y_boot ~ -1, data = df_sub)
    anova_res <- anova(model0, model1)
    f_stat <- anova_res$F[2]
    f_stat
  }, cl = cl)
  boot_t_stats[, i] <- unlist(boot_t)
}
parallel::stopCluster(cl)
system.time({
  adj_pvals <- romano_wolf(tstats, boot_t_stats)
})
mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05)
# system.time({
#   adj_pvals2 <- romano_wolf_c(tstats, boot_t_stats)
# })
plot(qseq, adj_pvals,
  type = "b", pch = 20, col = "lightblue",
  ylab = "Romano-Wolf adjusted p-values",
  xlab = "Index"
)
abline(h = 0.05, col = "red", lty = 2)
mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05, ylim = c(0, 1.0))

cbind(adj_pvals, adj_pvals2)
