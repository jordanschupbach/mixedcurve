nk <- 400
df1 <- mixedcurve::gen_hfanova_data(
  f = mixedcurve::m3,
  bounds = c(0, 1),
  sigmas = c(0.01, 0),
  n = c(nk, 1),
  ngrp = 2
)
df2 <- mixedcurve::gen_hfanova_data(
  f = mixedcurve::m3,
  bounds = c(0, 1),
  sigmas = c(0.01, 0),
  n = c(nk, 1),
  ngrp = 2
)
df2 <- df2[df2$cov == 2, ]
df2$cov <- rep(3, nrow(df2))
levels(df1$cov) <- c(1, 2, 3)
levels(df2$cov) <- c(1, 2, 3)
df1 <- rbind(df1, df2)
mixedcurve::dark_mode()
par(mfrow = c(2, 1))
plot(df1$x1, df1$y, pch = 20, cex = 1.0, col = df1$cov)
subsetdf <- function(df, q, h) {
  df[abs(df$x1 - q) <= h, ]
}
nq <- 101
qseq <- seq(0, 1, length.out = nq)
tstats <- numeric(nq)
qseq_fe <- matrix(0, nrow = nq, ncol = 3)
pvals <- matrix(0, nrow = nq, ncol = 3)
anovapvals <- numeric(nq)
pairtstats <- matrix(0, nrow = nq, ncol = 3)
for (i in seq_along(qseq)) {
  print(i)
  df_sub <- subsetdf(df1, qseq[i], h = 0.09)
  model1 <- lm(y ~ cov, data = df_sub)
  model0 <- lm(y ~ 1, data = df_sub)
  qseq_fe[i, ] <- coef(summary(model1))[, 1]
  anovapvals[i] <- anova(model0, model1)$`Pr(>F)`[2]
  aovmodel1 <- aov(y ~ cov, data = df_sub)
  pvals[i, ] <- as.numeric(stats::TukeyHSD(aovmodel1, conf.level = .95)$cov[, 4])
  tstats[i] <- sqrt(anova(model0, model1)$F[2])
  pairtstats[i, ] <- summary(multcomp::glht(model1, linfct = multcomp::mcp(cov = "Tukey")),
    test = multcomp::univariate()
  )$test$tstat
}

plot(qseq, pvals[, 1], type = "l")
lines(qseq, pvals[, 2], col = 2)
lines(qseq, pvals[, 3], col = 3)
lines(qseq, qseq_fit, col = "orange", lwd = 2)
abline(h = 0, col = "red", lty = 2)
nb <- 299
boot_t_stats <- matrix(NA, nrow = nb, ncol = nq)
boot_pvals <- matrix(NA, nrow = nb, ncol = nq)
boot_pc1 <- matrix(NA, nrow = nb, ncol = nq)
boot_pc2 <- matrix(NA, nrow = nb, ncol = nq)
boot_pc3 <- matrix(NA, nrow = nb, ncol = nq)
boot_tstats1 <- matrix(NA, nrow = nb, ncol = nq)
boot_tstats2 <- matrix(NA, nrow = nb, ncol = nq)
boot_tstats3 <- matrix(NA, nrow = nb, ncol = nq)
set.seed(1234)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, varlist = c("df1", "subsetdf"))
pval_mats <- list
for (i in 1:nq) {
  df_sub <- subsetdf(df1, qseq[i], h = 0.07)
  parallel::clusterExport(cl, varlist = c("df_sub"))
  boot_t <- parallel::parLapply(1:nb, function(b) {
    df_sub$cov <- sample(df_sub$cov, replace = TRUE)
    model1 <- lm(y ~ cov, data = df_sub)
    model0 <- lm(y ~ 1, data = df_sub)
    qseq_fe <- coef(summary(model1))[, 1]
    anovapval <- anova(model0, model1)$`Pr(>F)`[2]
    fstat <- anova(model0, model1)$F[2]
    aovmodel1 <- aov(y ~ cov, data = df_sub)
    cht <- multcomp::glht(model1, linfct = multcomp::mcp(cov = "Tukey"))
    tstats <- summary(cht, test = multcomp::univariate())$test$tstat
    pairpval <- summary(cht, test = multcomp::univariate())$test$pvalues
    # str(summary(cht, test = multcomp::univariate()))
    # summary(cht, test = adjusted("Shaffer"))
    # summary(cht, test = adjusted("Westfall"))
    # stats::parwise.t.test(df_sub$y, df_sub$cov, p.adjust.method = "none")
    # str(stats::TukeyHSD(aovmodel1, conf.level = .95))
    list(
      fstat = fstat,
      tstats = tstats,
      pval = anovapval,
      pairpval = pairpval,
      qseq_fe = qseq_fe
    )
  }, cl = cl)
  boot_t_stats[, i] <- unlist(lapply(boot_t, function(elmt) elmt$fstat))
  boot_pvals[, i] <- unlist(lapply(boot_t, function(elmt) elmt$pval))
  boot_pc1[, i] <- unlist(lapply(boot_t, function(elmt) elmt$pairpval[1]))
  boot_pc2[, i] <- unlist(lapply(boot_t, function(elmt) elmt$pairpval[2]))
  boot_pc3[, i] <- unlist(lapply(boot_t, function(elmt) elmt$pairpval[3]))
  boot_tstats1[, i] <- unlist(lapply(boot_t, function(elmt) elmt$tstats[1]))
  boot_tstats2[, i] <- unlist(lapply(boot_t, function(elmt) elmt$tstats[2]))
  boot_tstats3[, i] <- unlist(lapply(boot_t, function(elmt) elmt$tstats[3]))
}
parallel::stopCluster(cl)

str(boot_tstats3)

plot(qseq, pval)

# anova_adj_pvals <- mixedcurve::romano_wolf(tstats, boot_t_stats)
plot(qseq, anova_adj_pvals, type = "l")
lines(qseq, pc1_adj_pvals)
lines(qseq, pc2_adj_pvals)
lines(qseq, pc3_adj_pvals)

plot(df1$x1, df1$y, pch = 20, cex = 1.0)
lines(qseq, qseq_fit, col = "orange", lwd = 2)
system.time({
  anova_adj_pvals <- mixedcurve::romano_wolf(tstats, boot_t_stats)
  pc1_adj_pvals <- mixedcurve::romano_wolf(pairtstats[, 1], boot_tstats1)
  pc2_adj_pvals <- mixedcurve::romano_wolf(pairtstats[, 2], boot_tstats2)
  pc3_adj_pvals <- mixedcurve::romano_wolf(pairtstats[, 3], boot_tstats3)
})


abline(h = 0.00, col = "red", lty = 2)
mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05)
plot(qseq, adj_pvals,
  type = "b", pch = 20, col = "lightblue",
  ylab = "Romano-Wolf adjusted p-values",
  xlab = "Index"
)
abline(h = 0.05, col = "red", lty = 2)
mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05, ylim = c(0, 1.0))
lines(qseq, pvals, col = "white")
lines(qseq, p.adjust(pvals, method = "BH"), col = "green")
lines(qseq, p.adjust(pvals, method = "BY"), col = "orange")
lines(qseq, p.adjust(pvals, method = "holm"), col = "yellow")
lines(qseq, p.adjust(pvals, method = "hochberg"), col = "purple")
lines(qseq, p.adjust(pvals, method = "hommel"), col = "pink")


cbind(adj_pvals, adj_pvals2)
