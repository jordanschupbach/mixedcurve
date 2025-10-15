
# 2D FANOVA power analysis
# (An anova test for functional data, Cuevas et al, 2003) extended to 2D

save_figs <- FALSE

# {{{ imports

use_package <- function(package_name, repo = "http://cran.us.r-project.org") {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, repos = repo, quiet = TRUE, ask = FALSE)
    library(package_name, character.only = TRUE)
  }
}
use_package("geoR")
use_package("viridis")
use_package("dplyr")
use_package("parallel")
# use_package("ggplot2")
# use_package("foreach")
# use_package("doParallel")
use_package("lme4")
# use_package("plotly")
# use_package("reshape2")
# use_package("emdbook")

# }}} imports

# {{{ test Functions

dist_2d <- function(xy1, xy2) {
  sqrt((xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2)
}

m1_1d <- function(t, i) {
  t * (1 - t)
}

m1_2d <- function(xy, i) {
  m1_1d(dist_2d(c(0.5, 0.5), xy) * sqrt(2) / 2, i)
}

m2_1d <- function(t, i) {
  t^i * (1 - t)^(6 - i)
}

m2_2d <- function(xy, i) {
  m2_1d(dist_2d(c(0.5, 0.5), xy) * sqrt(2) / 2, i)
}


m3_1d <- function(t, i) {
  t^(i / 5) * (1 - t)^(6 - i / 5)
}

m3_2d <- function(xy, i) {
  m3_1d(dist_2d(c(0.5, 0.5), xy) * sqrt(2) / 2, i)
}

m4_1d <- function(t, i) {
  rep(1 + i / 50, length(t))
}

m4_2d <- function(xy, i) {
  m4_1d(dist_2d(c(0.5, 0.5), xy) * sqrt(2) / 2, i)
}

m <- m2_2d

# }}} test Functions

# {{{ plot test functions

# add_eval_function_line <- function(f, dim = "1d", nx = 20, bounds = c(0, 1), ...) {
#   xseq <- seq(bounds[1], bounds[2], length.out = nx)
#   lines(xseq, unlist(sapply(xseq, f)), ...)
# }

plot_true_functions <- function(f, dim = "1d", nx) {
  if (save_figs) {
    pdf("figs/2d_m1_true_functions.pdf", width = 18, height = 6)
  }
  xseq <- seq(0, 1, length.out = 12)
  xyseq <- expand.grid(x = xseq, y = xseq)
  z1 <- matrix(apply(xyseq, 1, function(xy) m1_2d(xy, 1)), nrow = length(xseq), ncol = length(xseq))
  z2 <- matrix(apply(xyseq, 1, function(xy) m1_2d(xy, 2)), nrow = length(xseq), ncol = length(xseq))
  z3 <- matrix(apply(xyseq, 1, function(xy) m1_2d(xy, 3)), nrow = length(xseq), ncol = length(xseq))
  # Combine into one data frame
  data_long <- rbind(
    melt(z1) %>% mutate(heatmap = "M1-1"),
    melt(z2) %>% mutate(heatmap = "M1-2"),
    melt(z3) %>% mutate(heatmap = "M1-3")
  )
  # Create heatmaps
  ggplot(data_long, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    facet_wrap(~ heatmap) +
    scale_fill_gradient2() +
    labs(x = "X-axis", y = "Y-axis", fill = "Value") +
    theme_minimal()
  if (save_figs) {
    dev.off()
  }
}

# }}} plot test functions

# {{{ Simulate data funs

sim_data <- function(f, n, nseq, sigma) {
  # Simulate a dataset (for m1)
  create_df <- function(mf, n, nseq, i, sigma, group) {
    tseq <- seq(0, 1, length.out = nseq)
    xyseq <- expand.grid(x = tseq, y = tseq)
    do.call("rbind", lapply(1:n, function(ni) {
      data.frame(
        id = ni,
        x = xyseq[, 1],
        y = xyseq[, 2],
        z = apply(xyseq, 1, function(x) mf(x, i)) + rnorm(nrow(xyseq), sd = sigma),
        grp = sample(c(group), length(tseq), replace = TRUE)
      )
    }))
  }
  tseq <- seq(0, 1, length.out = nseq)
  f1 <- create_df(f, n, nseq, 1, sigma, "G1")
  f2 <- create_df(f, n, nseq, 2, sigma, "G2")
  f3 <- create_df(f, n, nseq, 3, sigma, "G3")
  ret <- rbind(f1, f2, f3)
  ret$id <- as.factor(rep(1:(3 * n), each = length(tseq)^2))
  ret$grp <- as.factor(ret$grp)
  ret
}

# }}} Simulate data funs

# {{{ Plot 4 images from each group

plot_imgs_from_each_group <- function(x) {
  if (save_figs) {
    pdf("figs/2d_m1_examples.pdf", width = 12, height = 12)
  }
  z_min <- min(x$z)
  z_max <- max(x$z)
  # Set up layout: first 12 plots, then the color bar
  layout_matrix <- matrix(c(1, 2, 3, 13,
                            4, 5, 6, 13,
                            7, 8, 9, 13,
                            10, 11, 12, 13),
                          nrow = 4, byrow = TRUE)
  layout(layout_matrix)
  # Set margins for images
  par(mar = c(1, 1, 2, 1))
  # Function to plot images
  plot_image <- function(data, id, zlim = c(z_min, z_max), main = NULL) {
    # Create matrix and check if it's valid
    z_matrix <- matrix(data[data$id == id, ]$z, nrow = 12, ncol = 12, byrow = TRUE)
    image(z_matrix, col = viridis(256), axes = FALSE, main = main, zlim = zlim)
  }
  # Plot images for each group
  plot_image(x, 1, main = "Group 1")
  plot_image(x, 11, main = "Group 2")
  plot_image(x, 21, main = "Group 3")
  plot_image(x, 2)
  plot_image(x, 12)
  plot_image(x, 22)
  plot_image(x, 3)
  plot_image(x, 13)
  plot_image(x, 23)
  plot_image(x, 4)
  plot_image(x, 14)
  plot_image(x, 24)
  # Adjust margins for the color bar
  par(mar = c(6, 1, 4, 4))  # Reset margins for color bar
  # Define color scale
  color_scale <- seq(z_min, z_max, length.out = 100)  # Color scale values based on global limits
  image(1, color_scale, matrix(color_scale, nrow = 1), col = viridis(256), axes = FALSE, xlab = "", ylab = "")
  axis(4, at = pretty(color_scale), labels = round(pretty(color_scale), 2))  # Add axis for scale
  title("Height")  # Title for the color scale
  if (save_figs) {
    dev.copy(pdf, "2d_cuevas_m2_examples.pdf", width = 12, height = 12)
  }
}

# }}} Plot 4 images from each group

# {{{ Kernel estimation functions

k_2d <- function(points, qry, h) {
  x_diff <- points[, 1] - qry[1]
  y_diff <- points[, 2] - qry[2]
  squared_dist <- ((x_diff / h)^2 + (y_diff / h)^2)
  k_weights <- exp(-squared_dist) / h
  k_weights
}

query_pt <- function(mydf, qry, bw, verbose = TRUE) {
  tdf <- mydf
  n_rep <- length(unique(tdf$id)) / 2
  weights <- k_2d(cbind(tdf$x, tdf$y), qry, bw)
  wxmat <- Diagonal(x = weights)
  tlmer <- suppressMessages(lmer(z ~ 1 + grp + (1 | id), data = tdf))
  txmat <- getME(tlmer, "X")
  tzmat <- getME(tlmer, "Z")
  # wxmat <- txmat * weights
  wx <- txmat[, 1] * weights
  wgrp <- txmat[, 2] * weights
  wgrp2 <- txmat[, 3] * weights
  wzmat <- wx %*% tzmat
  tlmer1 <- suppressMessages(lmer(z ~ wx + wgrp + wgrp2 - 1 + (wx - 1 | id), data = tdf))
  tlmer2 <- suppressMessages(lmer(z ~ wx - 1 + (wx - 1 | id), data = tdf))
  anovaobj <- suppressMessages(anova(tlmer1, tlmer2))
  lls <- anovaobj$logLik
  pval <- 1 - pchisq(abs(diff(lls)), 1)
  betahat <- fixef(tlmer1)
  gammahat <- ranef(tlmer1)
  coefhat <- coef(tlmer1)
  fits <- c(coefhat$id[, 1][1:n_rep] + betahat[1],
            coefhat$id[, 1][(n_rep + 1):(n_rep * 2)] + betahat[1] + betahat[2])
  return(list(pval = pval, lls = lls, betahat = betahat, gammahat = gammahat, fits = fits))
}

# Define a function to handle each row of xyseq
query_function <- function(qry, dframe, bw) {
  query_pt(dframe, as.numeric(qry), bw, FALSE)
}

# }}} Kernel estimation functions

# {{{ Estimate

estimate_curves <- function(dataf, xyseq) {
  # xyseq <- expand.grid(seq(0, 1, length.out = 12), seq(0, 1, length.out = 12))
  cl <- makeCluster(28)
  # cl <- makeCluster(detectCores() - 1)
  clusterExport(cl, c("query_function", "query_pt", "k_2d", "ndf"))
  clusterEvalQ(cl, library(lme4))
  # clusterEvalQ(cl, library(emdbook))
  clusterEvalQ(cl, library(dplyr))
  system.time({
    results <- parLapply(cl, seq_len(nrow(xyseq)), function(idx) {
      tdf <- dataf
      txyseq <- xyseq
      query_function(txyseq[idx, ], tdf, 0.10) # bw = 0.05
    })
  })
  stopCluster(cl)
  val <- sapply(results, function(res) res$fits)
  raw_pvls <- sapply(results, function(res) res$pval)
  results
}


# system.time(results <- mclapply(seq_len(nrow(xyseq)), function(i) {
#   query_function(xyseq[i, ], dataf, 0.10)
# }, mc.cores = detectCores()))
# val <- sapply(results, function(res) res$fits)
# raw_pvls <- sapply(results, function(res) res$pval)
# ## NOTE: move this out
# # if (save_figs) {
# #   pdf("2d_cuevas_m2_raw_pvals.pdf", width = 12, height = 12)
# # }
# # par(mfrow = c(1, 1))
# # image(matrix(raw_pvls, 12, 12), axes = FALSE, col = viridis::viridis(256))
# # if (save_figs) {
# #   dev.off()
# # }


# }}} Estimate

# {{{ Westfall-Young funs

wy_pvals <- function(dframe, qseq, bw, nperms, raw_pvalues) {
  ni <- length(unique(dframe$id))
  nk <- nrow(dframe[dframe$id == 1, ])
  coreset <- dframe[(0:(ni - 1)) * nk + 1, ]
  sorted_raw_pvals <- sort(raw_pvalues, index.return = TRUE)
  perm_pi <- sorted_raw_pvals$ix
  pstars <- matrix(1.0, nrow = nperms, ncol = nrow(qseq))
  minpstars <- numeric(nperms)
  for (l in 1:nperms) {
    # print(paste("Permutation", l, "of", nperms))
    tldf <- dframe
    tldf$grp <- rep(sample(coreset$grp, replace = FALSE), each = nk)
    # perm <- sample(1:(3 * 10), size = 3 * 10, replace = FALSE)
    # perms <- as.vector(sapply(as.list((perm - 1) * nrow(qseq)),
    #                           function(x) {
    #                             x + seq_len(nrow(qseq))
    #                           }))
    # tldf$grp <- as.factor(tldf$grp[perms])
    # summary(tldf$grp[perms])
    results <- estimate_curves(tldf, qseq)
    # system.time(results <- mclapply(seq_along(qseq), function(qidx) {
    #   query_function(qseq[qidx, ], tldf, 0.05)
    # }, mc.cores = detectCores() - 2))
    pstars[l, ] <- sapply(results, function(res) res$pval)[perm_pi]
    minpstars[l] <- min(pstars[l, ])
  }
  adjusted_pvals <- numeric(nrow(qseq))
  for (i in seq_along(raw_pvalues)) {
    adjusted_pvals[i] <- 1 - sum(raw_pvalues[i] < minpstars) / nperms
  }
  adjusted_pvals
}

# }}} Westfall-Young funs

# {{{ Plot wy sourfaces

plot_wy_surfaces <- function() {
  qseq <- seq(0, 1, length.out = 12)
  system.time({
    adjusted_pvals <- wy_pvals(df, xyseq, 0.05, 50, raw_pvls)
  })
  if (save_figs) {
    pdf("figs/2d_m1_wy_pvals.pdf", width = 12, height = 12)
  }
  par(mar = c(1, 1, 2, 0))
  layout_matrix <- matrix(c(rep(1, 7), 2), nrow = 1, byrow = TRUE)
  layout(layout_matrix)
  image(matrix(adjusted_pvals, 12, 12), axes = FALSE, col = viridis::viridis(256),
        main = "Westfall-Young Adjusted P-Values", zlim = c(0, 1))
  color_scale <- seq(0, 1, length.out = 256)  # Color scale values based on global limits
  par(mar = c(1, 1, 2, 3))
  image(1, color_scale, matrix(color_scale, nrow = 1), col = viridis(256),
        axes = FALSE, xlab = "", ylab = "", zlim = c(0, 1))
  axis(4, at = pretty(color_scale), labels = round(pretty(color_scale), 2))  # Add axis for scale
  if (save_figs) {
    dev.off()
  }
  df <- sim_data(m2, 10, 25, 0.2)
  qseq <- seq(0, 1, length.out = 25)
  system.time(results <- mclapply(seq_along(qseq), function(i) {
    query_function(qseq[i], df, 0.05)
  }, mc.cores =  detectCores() - 4))
  val <- sapply(results, function(res) res$fits)
  raw_pvals <- sapply(results, function(res) res$pval)
  adjusted_pvals <- wy_pvals(df, qseq, 0.05, 50, raw_pvals)
  par(mfrow = c(1, 2))
  plot(qseq, raw_pvals, type = "l", col = "red", ylim = c(0, 1), xlab = "Query Point", ylab = "Adjusted P-Value")
  plot(qseq, adjusted_pvals, type = "l", col = "red", ylim = c(0, 1), xlab = "Query Point", ylab = "Adjusted P-Value")
  # Power Analysis
  fun_seq <- list(m1, m2, m3, m4)
  fun_seq_names <- c("m1", "m2", "m3", "m4") # 5 funs
}

# }}} Plot wy sourfaces


bw <- 0.1
n_i <- 10
n_x <- 12
xseq <- seq(0, 1, length.out = 12)
xyseq <- expand.grid(x = xseq, y = xseq)
m <- m2_2d

ndf <- sim_data(m2_2d, n_i, n_x, 0.0005)
summary(ndf$grp)

results <- estimate_curves(ndf, xyseq)

tfits <- do.call(cbind, lapply(results, function(res) res$fits))

str(tfits)

samps <- sample(1:30, size = 12, replace = FALSE)
par(mfrow = c(3, 4), mar = c(1, 1, 2, 0))
for(s in samps) {
  image(matrix(tfits[,s], 12, 12), main = paste("Curve", s), axes = FALSE, col = viridis(256))
}



