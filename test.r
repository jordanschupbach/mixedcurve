

# Test box_subset function
# x <- runif(1000)
# box_subset(x, c(0.2, 0.5))
# x <- cbind(runif(1000), runif(1000))
# box_subset(x, list(c(0.2, 0.3), c(0.5, 0.7)))
# x <- cbind(runif(1000), runif(1000), runif(1000))
# box_subset(x, list(c(0.2, 0.3), c(0.4, 0.5), c(0.6, 0.7)))

x1 <- runif(1000)
y <- mixedcurve::m3(x1, 1) + rnorm(length(x1), 0, 0.05)
df1 <- data.frame(
  x1 = x1,
  y = y
)
# mixedcurve::box_kern(0.2 / 0.01) / 0.01
mixedcurve::kern_ih(seq(0.0, 1.0, length.out = 1000), 0.3, kernel_fun = mixedcurve::box_kern, h=0.01)


par(mfrow = c(2, 1))
mixedcurve::dark_mode()
plot(x1, y, pch = 20, cex = 1.5, col = "white")
qseq <- seq(0, 1, length.out = 1000)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "qseq"))
fits <- mixedcurve::lpk(y ~ K_h(x1),
                        alternative_hypothesis = y ~ K_h(x1 | -1),
                        data = df1,
                        queries = qseq,
                        kern = mixedcurve::box_kern,
                        h = 0.005,
                        kthresh = 0, cl = cl)
parallel::stopCluster(cl)
qseq_fit <- unlist(lapply(fits[[1]], function(elmt)  elmt$coefs))
lines(qseq, qseq_fit, col = "cyan", lwd = 2)
raw_pvals <- unlist(lapply(fits[[1]], function(elmt)  elmt$pvals))
plot(qseq, raw_pvals, type = "l")

fits <- lapply(
  seq_along(qseq),
  function(i) {
    weights <- mixedcurve::kern_ih(
      cbind(df1$x1),
      qseq[i],
      mixedcurve::box_kern,
      0.05
    )
    subdf <- df1[weights > 0, ]
    lm_fit1 <- lm(y ~ 1, data = subdf)
    lm_fit2 <- lm(y ~ -1, data = subdf)
    list(coef = as.numeric(coef(lm_fit1)),
         pval = anova(lm_fit2, lm_fit1)$Pr[2])
  })
coefs <- unlist(lapply(fits, function(elmt) elmt$coef))
pvals <- unlist(lapply(fits, function(elmt) elmt$pval))
plot(x1, y, pch = 20, cex = 1.5, col = "white")
lines(qseq, coefs, col = "red", lwd = 2)
plot(qseq, pvals, type = "l")













abline(h = 0, col = "cyan", lty = 3)
pseq <- seq(0, 1, length.out = 51)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "qseq", "pseq"))
wy_pvals <- mixedcurve::wy_full(
  dataf = df1,
  xseq = as.matrix(pseq),
  nperm = 100,
  gen_pvals_fun = function(dataf, xseq) {
    snapped_x <- mixedcurve::snap_to_grid(dataf$x1, pseq)
    pvals <- numeric(length(xseq))
    for (i in seq_along(xseq)) {
      subset_y <- dataf$y[snapped_x == xseq[i]]
      pvals[i] <- t.test(subset_y)$p.value
    }
    pvals
  },
  gen_perm_fun = function(dataf) {
    tdf <- dataf
    tdf$y <- sample(tdf$y)
    tdf
  },
  cl = cl
)
parallel::stopCluster(cl)
mixedcurve::plot_pval_regions(
  queries = pseq,
  wy_pvals = wy_pvals,
  pthresh = 0.05
)
plot(pseq, wy_pvals, type = "l",
     main = "Westfall-Young corrected P-values")
mixedcurve::plot_pval_regions(
  queries = pseq,
  wy_pvals = wy_pvals,
  pthresh = 0.05
)

# {{{ 1d case

summary(lm(y ~ 1, data = df1))
set.seed(1234)
nx <- 1000
x1 <- runif(nx)
y <- mixedcurve::m3(x1, 1) + rnorm(nx, 0, 0.05)
df1 <- data.frame(
  x1 = x1,
  y = y
)
bandwidth <- 0.01
nq <- 100
qseq <- as.matrix(seq(0, 1, length.out = nq))
breaks <- seq(min(y), max(y), length.out = 100)
color_values <- cut(y, breaks = 100, labels = FALSE)
colors <- viridis::viridis(100)
plot(x1, y, col = colors[color_values], pch = 20, cex = 1.5)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "bandwidth", "qseq"))
fits <- parallel::parLapply(
  cl,
  seq_len(nrow(qseq)),
  function(ix, df1, bandwidth, qseq) {
    weights <- mixedcurve::kern_ih(
      cbind(df1$x1),
      qseq[ix, ],
      mixedcurve::gauss_kern,
      bandwidth
    )
    tdf <- df1
    tdf$w <- weights
    tdf$w_y <- weights * tdf$y
    lm_fit1 <- lm(
      y ~ 1,
      data = tdf,
      weights = w
    )
    lm_fit2 <- lm(
      y ~ -1,
      data = tdf,
      weights = w
    )
    list(
      coef = as.numeric(coef(lm_fit1)),
      pvals = anova(lm_fit2, lm_fit1)$Pr[2]
    )
  },
  df1 = df1,
  bandwidth = bandwidth,
  qseq = qseq
)
parallel::stopCluster(cl)
kernel_fits <- unlist(lapply(fits, function(elmt) {
  elmt$coef
}))
kernel_pvals <- unlist(lapply(fits, function(elmt) {
  elmt$pvals
}))
par(mfrow = c(4, 2))
plot(qseq, kernel_fits, type = "l",
     main = "Raw data and kernel fit", ylim = range(y))
points(x1, y, col = colors[color_values], pch = 20, cex = 1.5)
snapped_x <- qseq[apply(as.matrix(df1$x1), 1, function(xi) {
  which.min(abs(qseq - xi))
})]
pwise_pvals <- numeric(length(qseq))
pwise_coefs <- numeric(length(qseq))
for (i in seq_along(qseq)) {
  subset_x <- snapped_x[snapped_x == qseq[i]]
  subset_y <- df1$y[snapped_x == qseq[i]]
  pwise_pvals[i] <- t.test(subset_y)$p.value
  pwise_coefs[i] <- t.test(subset_y)$estimate
}
plot(qseq, pwise_coefs, type = "l", xlim = range(qseq),
     ylim = range(y), main = "Raw data and pointwise Fit")
points(x1, y, col = colors[color_values], pch = 20, cex = 1.5)
plot(qseq, kernel_fits, type = "l", main = "Pointwise kernel-reg Fit")
plot(qseq, pwise_coefs, type = "l", main = "Pointwise Fit")
plot(qseq, kernel_pvals, type = "l", main = "Pointwise kernel-reg P-values")
lines(qseq, stats::ksmooth(qseq, kernel_pvals, bandwidth = 0.1)$y, col = "red")
plot(qseq, pwise_pvals, type = "l", main = "Pointwise t-test P-values")
lines(qseq, stats::ksmooth(qseq, pwise_pvals, bandwidth = 0.1)$y, col = "red")

# {{{ Westfall-Young corrected

# W-Y for pointwise kernel-regression p-values
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "bandwidth", "qseq"))
wy_kern_pvals <- mixedcurve::wy_full(
  data = df1,
  xseq = qseq,
  nperm = 100,
  gen_pvals_fun = function(dataf, xseq) {
    pvals <- numeric(nrow(xseq))
    for (ix in seq_len(nrow(xseq))) {
      weights <-
        mixedcurve::kern_ih(
          cbind(dataf$x1),
          xseq[ix, ],
          mixedcurve::gauss_kern,
          bandwidth
        )
      tdf <- dataf
      tdf$w <- weights
      tdf$w_y <- weights * tdf$y
      lm_fit1 <- lm(
        y ~ 1,
        data = tdf,
        weights = w
      )
      lm_fit2 <- lm(
        y ~ -1,
        data = tdf,
        weights = w
      )
      pvals[ix] <- anova(lm_fit2, lm_fit1)$Pr[2]
    }
    pvals
  },
  gen_perm_fun = function(dataf) {
    tdf <- dataf
    tdf$y <- sample(tdf$y)
    tdf
  },
  cl = cl
)
parallel::stopCluster(cl)
plot(qseq, wy_kern_pvals, type = "l",
     main = "Westfall-Young corrected (kernel regression) P-values")

x <- runif(10000)
xseq <- seq(0, 1, length.out = 101)
snapped_x <- xseq[apply(as.matrix(x), 1, function(xi) {
  which.min(abs(xseq - xi))
})]
sort(unique(snapped_x))

# }}} W-Y kernel regression corrected

# {{{ W-Y for pointwise t-test p-values
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "bandwidth", "qseq"))
ttest_pvals <- mixedcurve::wy_full(data = df1,
                                   xseq = qseq,
                                   nperm = 100,
                                   gen_pvals_fun = function(dataf, xseq) {
                                     snapped_x <- xseq[apply(as.matrix(dataf$x1), 1, function(xi) {
                                       which.min(abs(xseq - xi))
                                     })]
                                     pvals <- numeric(length(xseq))
                                     for (i in seq_along(xseq)) {
                                       subset_x <- snapped_x[snapped_x == xseq[i]]
                                       subset_y <- dataf$y[snapped_x == xseq[i]]
                                       pvals[i] <- t.test(subset_y)$p.value
                                     }
                                     pvals
                                   },
                                   gen_perm_fun = function(dataf) {
                                     tdf <- dataf
                                     tdf$y <- sample(tdf$y)
                                     tdf
                                   },
                                   cl = cl)
parallel::stopCluster(cl)
plot(qseq, ttest_pvals, type = "l",
     main = "Westfall-Young corrected (t-test) P-values")

# }}} Westfall-Young corrected

# }}} 1d case

# {{{ 2d case (kernel)

# Simulate data
x1 <- runif(1000)
x2 <- runif(1000)
y <- mixedcurve::m3(cbind(x1, x2), 1) + rnorm(length(x1), 0, 0.05)
breaks <- seq(min(y), max(y), length.out = 100)
color_values <- cut(y, breaks = 100, labels = FALSE)
colors <- viridis::viridis(100)
par(mfrow = c(4, 1))
plot(x1, x2, col = colors[color_values], pch = 20, cex = 1.5)
summary(y)
df1 <- data.frame(
  x1 = x1,
  x2 = x2,
  y = y
)
bandwidth <- 0.02
nxy <- 40
qseq <- as.matrix(expand.grid(
  seq(0, 1, length.out = nxy),
  seq(0, 1, length.out = nxy)
))
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "bandwidth", "qseq"))
fits <- parallel::parLapply(
  cl,
  seq_len(nrow(qseq)),
  function(ix, df1, bandwidth, qseq) {
    weights <-
      mixedcurve::kern_ih(
        cbind(df1$x1, df1$x2),
        qseq[ix, ],
        mixedcurve::gauss_kern,
        bandwidth
      )
    tdf <- df1
    tdf$w <- weights
    tdf$w_y <- weights * tdf$y
    lm_fit1 <- lm(
      w_y ~ w - 1,
      data = tdf
    )
    lm_fit2 <- lm(
      w_y ~ -1,
      data = tdf
    )
    list(
      coef = as.numeric(coef(lm_fit1)),
      pvals = anova(lm_fit2, lm_fit1)$Pr[2]
    )
  },
  df1 = df1,
  bandwidth = bandwidth,
  qseq = qseq
)
parallel::stopCluster(cl)
qseq_fits <- unlist(lapply(fits, function(elmt) {
  elmt$coef
}))
pvals <- unlist(lapply(fits, function(elmt) {
  # elmt$pvals$Pr[2]
  elmt$pvals
}))
image(matrix(qseq_fits, ncol = nxy))
image(matrix(pvals, ncol = nxy))
# Try westfall young adjustment
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "bandwidth", "qseq"))
pvals <- mixedcurve::wy_one_step(
  df1, qseq, 100,
  gen_pvals_fun = function(dataf, xseq) {
    pvals <- numeric(nrow(xseq))
    for (ix in seq_len(nrow(xseq))) {
      weights <-
        mixedcurve::kern_ih(
          cbind(df1$x1, df1$x2),
          qseq[ix, ],
          mixedcurve::gauss_kern,
          bandwidth
        )
      tdf <- df1
      tdf$w <- weights
      tdf$w_y <- weights * tdf$y
      lm_fit1 <- lm(
        w_y ~ w - 1,
        data = tdf
      )
      lm_fit2 <- lm(
        w_y ~ -1,
        data = tdf
      )
      pvals[ix] <- anova(lm_fit2, lm_fit1)$Pr[2]
    }
    pvals
  },
  gen_perm_fun = function(dataf) {
    tdf <- dataf
    tdf$y <- sample(tdf$y)
    tdf
  },
  cl = cl
)
parallel::stopCluster(cl)

image(matrix(pvals, ncol = nxy))

# }}} 2d case

# {{{ 2d calse (pointwise t-test)
n <- 10000
x1 <- runif(n)
x2 <- runif(n)
y <- mixedcurve::m3(cbind(x1, x2), 1) + rnorm(length(x1), 0, 0.05)
breaks <- seq(min(y), max(y), length.out = 100)
color_values <- cut(y, breaks = 100, labels = FALSE)
colors <- viridis::viridis(100)
par(mfrow = c(3, 1))
plot(x1, x2, col = colors[color_values], pch = 20, cex = 1.5)
x <- cbind(x1, x2)
xgrid <- expand.grid(seq(0.0, 1.0, length.out = 21),
                     seq(0.0, 1.0, length.out = 21))
# Function to find nearest centroid
snap_to_grid <- function(points, grid) {
  apply(points, 1, function(point) {
    distances <- sqrt(rowSums((grid - point)^2))  # Calculate distances
    grid[which.min(distances), ]                   # Get the nearest centroid
  })
}
snapped_x <- mixedcurve::snap_to_grid(x, xgrid)
snapped_x <- do.call(rbind, snapped_x)
pvals <- numeric(nrow(xgrid))
for (i in seq_len(nrow(xgrid))) {
  subset_y <- y[(snapped_x[,1] == xgrid[i, 1]) & (snapped_x[, 2] == xgrid[i, 2])]
  pvals[i] <- t.test(subset_y)$p.value
}
image(matrix(pvals, ncol = 21))
df1 <- data.frame(
  x1 = x1,
  x2 = x2,
  y = y
)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
invisible(parallel::clusterEvalQ(cl, library(mixedcurve)))
parallel::clusterExport(cl, varlist = c("df1", "xgrid", "snap_to_grid", "snapped_x"))
wy_pvals <- mixedcurve::wy_full(
  data = df1,
  xseq = as.matrix(xgrid),
  nperm = 100,
  gen_pvals_fun = function(dataf, xseq) {
    pvals <- numeric(nrow(xseq))
    for (i in seq_len(nrow(xseq))) {
      subset_y <- dataf$y[(snapped_x[,1] == xseq[i, 1]) & (snapped_x[, 2] == xseq[i, 2])]
      pvals[i] <- t.test(subset_y)$p.value
    }
    pvals
  },
  gen_perm_fun = function(dataf) {
    tdf <- dataf
    tdf$y <- sample(tdf$y)
    tdf
  },
  cl = cl
)
parallel::stopCluster(cl)
image(matrix(wy_pvals, ncol = 21))

# }}} 2d calse (pointwise t-test)
