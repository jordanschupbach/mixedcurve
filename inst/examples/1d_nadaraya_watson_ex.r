# 1D nadaraya-watson kernel regression

# 1d case
# library(mixedcurve)
gen_fanova_data <- function(f = mixedcurve::m3, bounds = c(0, 1),
                            n = 10, ngrp = 3, nx = 200,
                            balanced = FALSE, pgrp = sample,
                            pgrpargs = list(x = 1:3,
                                            size = 10,
                                            replace = TRUE),
                            sigma = 0.05, systematic = FALSE,
                            px = runif,
                            pxargs = list(list(min = 0, max = 1)),
                            white_noise = TRUE,
                            cov_scale = 0.05, gpn = 100, family = "gaussian") {
  if (is.vector(bounds) && !is.list(bounds)) {
    dims <- 1
    tbounds <- as.matrix(bounds, nrow = 1)
  } else if (is.list(bounds)) {
    dims <- length(bounds)
    tbounds <- do.call(rbind, bounds)
  } else if (is.matrix(bounds)) {
    dims <- nrow(bounds)
    tbounds <- bounds
  } else {
    stop("bounds must be a vector, list, or matrix")
  }
  if (balanced) {
    ntotal <- n * ngrp
    grps <- rep(1:ngrp, each = n)
  } else {
    ntotal <- n
    grps <- do.call(pgrp, pgrpargs)
  }
  ylist <- list()
  xlist <- list()
  grplist <- list()
  idlist <- list()
  if (!white_noise) {
    if(dims == 1) {
      delta_i <- geoR::grf(gpn, nx = 1, grid = "reg",
                           xlims = c(bounds[[1]][1], bounds[[1]][2]),
                           cov.model = "gaussian",
                           cov.pars = c(sigma, cov_scale),
                           nugget = 0.0,
                           nsim = ntotal, messages = FALSE)[[2]]
    } else if(dims == 2) {
      if(systematic) {
        delta_i <- geoR::grf(nx = nx, ny = nx, grid = "reg",
                             xlims = c(bounds[[1]][1], bounds[[1]][2]),
                             ylims = c(bounds[[2]][1], bounds[[2]][2]),
                             cov.model = "gaussian",
                             cov.pars = c(sigma, cov_scale),
                             nugget = 0.0,
                             nsim = ntotal, messages = FALSE)[[2]]
      } else {
        delta_i <- geoR::grf(nx = gpn, ny = gpn, grid = "reg",
                             xlims = c(bounds[[1]][1], bounds[[1]][2]),
                             ylims = c(bounds[[2]][1], bounds[[2]][2]),
                             cov.model = "gaussian",
                             cov.pars = c(sigma, cov_scale),
                             nugget = 0.0,
                             nsim = ntotal, messages = FALSE)[[2]]
      }
    } else {
      stop("Currently only 1D and 2D data supported for spatial noise.")
    }
  }
  for (i in 1:ntotal) {
    if (systematic) {
      txlist <- list(seq(tbounds[1, 1], tbounds[1, 2], length.out = nx))
      if (dims > 1) {
        for (d in 2:dims) {
          txlist[[d]] <- seq(tbounds[d, 1], tbounds[d, 2], length.out = nx)
        }
      }
      xlist[[i]] <- as.matrix(expand.grid(txlist))
    } else {
      txlist <- as.matrix(do.call(px, c(list(n = nx), pxargs[[1]])))
      if (dims > 1) {
        for (d in 2:dims) {
          txlist <- cbind(txlist,
                          as.matrix(do.call(px, c(list(n = nx), pxargs[[d]]))))
        }
      }
      xlist[[i]] <- as.matrix(txlist)
    }
    grplist[[i]] <- rep(grps[i], each = nrow(xlist[[i]]))
    ylist[[i]] <- f(xlist[[i]], grps[i])
    if (white_noise) {
      tnxy <- nx ^ dims
      ylist[[i]] <- ylist[[i]] + rnorm(length(ylist[[i]]), 0, sigma)
      if(family != "gaussian") {
        if(family == "poisson") {
          ylist[[i]] <- rpois(length(ylist[[i]]), lambda = ylist[[i]])
        } else if(family == "binomial") {
          ylist[[i]] <- rbinom(length(ylist[[i]]), size = 1,
                               prob = ylist[[i]])
        } else {
          stop("Currently only gaussian, poisson, and binomial families are supported.")
        }
      }
    } else {
      if(dims == 1) {
        grid_points <- seq(bounds[[1]][1], bounds[[1]][2], length.out = gpn)
        idx <- sapply(xlist[[i]], function(xi) {
          which.min(abs(grid_points - xi))
        })
      }
      if(dims == 1) {
        ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
      } else if(dims == 2) {
        if(!systematic) {
        grid_points <- expand.grid(seq(bounds[[1]][1], bounds[[1]][2], length.out = gpn),
                                   seq(bounds[[2]][1], bounds[[2]][2], length.out = gpn))
        # NOTE: changed here, may mess up other cases
        idx <- unlist(sapply(1:nrow(xlist[[i]]), function(j) {
          which.min(sqrt((grid_points[j, 1] - xlist[[i]][j, 1])^2 +
                           (grid_points[j,1] - xlist[[i]][j, 2])^2))
        }))
        ylist[[i]] <- ylist[[i]] + delta_i[idx, i]
        } else {
          ylist[[i]] <- ylist[[i]] + delta_i[, i]
        }
      }
      if(family != "gaussian") {
        if(family == "poisson") {
          ylist[[i]] <- rpois(length(ylist[[i]]), lambda = ylist[[i]])
        } else if(family == "binomial") {
          ylist[[i]] <- rbinom(length(ylist[[i]]), size = 1,
                               prob = ylist[[i]])
        } else {
          stop("Currently only gaussian, poisson, and binomial families are supported.")
        }
      }
    }
    idlist[[i]] <- rep(i, nrow(xlist[[i]]))
  }
  tx <- do.call(rbind, xlist)
  ret <- data.frame(y = unlist(ylist),
                    grp = as.factor(unlist(grplist)), id = as.factor(unlist(idlist)))
  if (dims == 1) {
    ret$x1 <- as.vector(tx)
  } else {
    for (d in 1:dims) {
      ret[[paste0("x", d)]] <- as.vector(tx[, d])
    }
  }
  if(systematic) {
    nxy <- nx ^ dims
  } else {
    nxy <- nx
  }
  coreset <- ret[(0:(ntotal - 1)) * nxy + 1, ]
  structure(list(df = ret, coreset = coreset,
                 curves = list(y = ylist, x = xlist),
                 dims = dims, nx = nx, systematic = systematic,
                 grps = grps,
                 family = family
                 ),
            class = "fundata")
}
fundata1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = c(0, 1),
  n = 50, ngrp = 3, nx = 200,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 50, replace = TRUE),
  sigma = 0.005, systematic = FALSE, px = runif,
  pxargs = list(min = 0, max = 1),
  white_noise = TRUE, cov_scale = 0.05, gpn = 1000
)
fundata1$df
par(mfrow = c(3, 2))
mixedcurve::dark_mode()
mixedcurve::plot.fundata(fundata1, curves = 1:6)
# str(fundata1$df)
time_took <- system.time({
  lpk1 <- mixedcurve::lpk(y ~ K_h(x1 | grp),
    queries = as.matrix(seq(0.0, 1.0, length.out = 200)),
    data = fundata1$df, degree = 0, kernel = mixedcurve::gauss_kern,
    h = 0.02, parallel = TRUE
  )
})
print(paste("Time took:", round(time_took[3], 2), "secs"))
qs <- mixedcurve::get_queries(lpk1)
dark_mode()
par(mfrow = c(1, 1))
plot(fundata1$df$x1, fundata1$df$y, 
     col = adjustcolor(fundata1$df$grp, 0.20),
     pch = 20, ylim = c(0, 0.08),
     ylab = "y", xlab = "x1",
     main = "Local Polynomial Kernel Regression Fits (degree=0), (i.e. Nadaraya-Watson)"
)
for (i in 1:3) {
  lines(seq(0.0, 1.0, length.out = 200), qs[, i], col = i, lwd = 2)
}

