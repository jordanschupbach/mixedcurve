library(mixedcurve)
nxy <- 5000
xseq <- seq(0, 1, length.out = 100)
set.seed(123)
df1 <- mixedcurve::gen_hfanova_data(
  f = mixedcurve::m2, n = c(nxy, 2), sigmas = c(0.02, 0),
  bounds = list(c(0, 1), c(0, 1)), ndim = 2,
  ngrp = 1, px = NULL, pxargs = NULL,
  family = "gaussian", white_noise = TRUE
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
  if (length(queries) > 1) {
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
nq <- 31
qseq <- expand.grid(
  seq(0, 1, length.out = nq),
  seq(0, 1, length.out = nq)
)


nb <- 100
tstats <- numeric(nrow(qseq))
qseq_fit <- numeric(nrow(qseq))
pvals <- numeric(nrow(qseq))
cl <- parallel::makeCluster(parallel::detectCores() - 1)
parallel::clusterExport(cl, varlist = c("df1", "subsetdf", "qseq"))
qs <- parallel::parLapply(seq_len(nrow(qseq)), function(i) {
  df_sub <- subsetdf(df1, c("x1", "x2"), qseq[i, ], h = 0.20)
  model1 <- lm(y ~ poly(x1, 3, raw = TRUE), data = df_sub)
  model0 <- lm(y ~ -1, data = df_sub)
  qseq_fit <- predict(model1,
    newdata = data.frame(
      x1 = qseq[i, 1],
      x2 = qseq[i, 2]
    )
  )
  pval <- anova(model0, model1)$`Pr(>F)`[2]
  tstat <- sqrt(anova(model0, model1)$F[2])
  list(fit = qseq_fit, pval = pval, tstat = tstat)
}, cl = cl)
parallel::stopCluster(cl)
fits <- unlist(lapply(qs, function(elmt) elmt$fit))
pvals <- unlist(lapply(qs, function(elmt) elmt$pval))
tstats <- unlist(lapply(qs, function(elmt) elmt$tstat))
image(matrix(fits, nq, nq))
boot_tstats <- matrix(NA, nrow = nb - 1, ncol = nq * nq)
set.seed(1234)

system.time({
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  parallel::clusterExport(cl, varlist = c("df1", "subsetdf", "qseq"))
  for (i in seq_len(nrow(qseq))) {
    df_sub <- subsetdf(df1,
      dnames = c("x1", "x2"),
      qseq[i, ], h = 0.07
    )
    parallel::clusterExport(cl, varlist = c("df_sub"))
    boot_t <- parallel::parLapply(1:(nb - 1), function(b) {
      df_sub$y_boot <- sample(df_sub$y, replace = TRUE) - mean(df_sub$y)
      model1 <- lm(y_boot ~ poly(x1, 3, raw = TRUE),
        data = df_sub
      )
      model0 <- lm(y_boot ~ -1, data = df_sub)
      anova_res <- anova(model0, model1)
      f_stat <- anova_res$F[2]
      f_stat
    }, cl = cl)
    boot_tstats[, i] <- unlist(boot_t)
  }
  parallel::stopCluster(cl)
})
system.time({
  adj_pvals <- mixedcurve::romano_wolf(tstats, boot_tstats)
})
adj_pvals

par(mfrow = c(2, 1))
image(matrix(fits, nq, nq))
image(matrix(adj_pvals, nq, nq))


# mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05)
# plot(qseq, adj_pvals,
#   type = "b", pch = 20, col = "lightblue",
#   ylab = "Romano-Wolf adjusted p-values",
#   xlab = "Index"
# )
# abline(h = 0.05, col = "red", lty = 2)
# mixedcurve::plot_pval_regions(qseq, adj_pvals, pthresh = 0.05, ylim = c(0, 1.0))
