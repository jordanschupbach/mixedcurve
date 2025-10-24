---
title: "Generalized Nadaraya-Watson (1d) Kernel Regression"
author: "Jordan Schupbach"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Generalized Nadaraya-Watson (1d) Kernel Regression}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

<!-- {{{ Setup -->
```{r setup}
#| include: false
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```
<!-- }}} Setup -->

## Introduction
This vignette demonstrates how to use the `mixedcurve` package to fit a
generalized Nadaraya-Watson kernel regression model to one-dimensional Poisson
data.

## Example Usage
Let's start by simulating some functional Poisson data with group effects to
fit a generalized Nadaraya-Watson kernel regression model to. We will make some
modifications to the Cuevas et al. M3 curve for this purpose.

<!-- {{{ Simulate data -->

```{R}

library(Matrix)
library(lme4)
set.seed(123)
n <- 300
nind <- 15
x <- runif(n * nind)
xmat <- cbind(rep(1, n * nind), x, x^2)
betas_true <- c(2.323, 1.73, -1.4)
bs_true <- as.numeric(t(as.matrix(cbind(rnorm(nind, 0, 0.81),
                                        rnorm(nind, 0, 0.08),
                                        rnorm(nind, 0, 0.03)))))
id <- rep(1:nind, each = n)
z_blocks <- lapply(seq_len(ncol(xmat)),
                   function(j) lapply(split(xmat[, j], id), as.matrix))
z_blocks <- lapply(seq_along(z_blocks[[1]]), function(i) {
  do.call(cbind, lapply(z_blocks, function(block) block[[i]]))
})
zmat <- bdiag(z_blocks)
eta_true <- as.vector(xmat %*% betas_true + zmat %*% bs_true)
y <- rpois(n * nind, exp(eta_true))
df1 <- data.frame(y = y, x = x, id = as.factor(id))

# png("vignettes/gmc_1d/gmc_1d_poisson_data.png", width = 4800, height = 4800, res = 600)
png("gmc_1d_poisson_data.png", width = 4800, height = 4800, res = 600)
mixedcurve::dark_mode()
plot(x, y, col = adjustcolor(id, 0.2), pch = 20,
  main = "Simulated Quadratic Poisson Functional Data",
)
points(x, exp(eta_true), col = adjustcolor(id, 1.0), pch = 20)
invisible(dev.off())

#
```

<!-- }}} Simulate data -->

![Functional Poisson data](./gmc_1d_poisson_data.png){width=95%}

Now, we can fit the generalized Nadaraya-Watson kernel regression model using
the `lpk` function from the `mixedcurve` package. We will specify the
bandwidth, kernel type, degree, and use the formula `y ~ K_h(x)` to indicate
that we want to fit a generalized local polynomial kernel model across domain
`x`.

<!-- {{{ Fit GNW model -->

```{r, message=FALSE, warning=FALSE}

# 4. Fit GNW kernel regression model (in parallel)
qseq <- seq(0.0, 1.0, length.out = 200)
glpk1 <- mixedcurve::glpkme(y ~ K_h(x) + (K_h(x) | id),
  queries = qseq,
  data = df1,
  degree = 0,
  kernel = mixedcurve::gauss_kern,
  family = "poisson",
  h = 0.02,
  parallel = TRUE
)

qrs <-do.call(rbind, lapply(glpk1[[1]], function(elmt) { as.numeric(unlist(elmt$coefs)) }))

# 5. Plot the results
# png("vignettes/gmc_1d/gmc_1d_poisson_fit.png", width = 4800, height = 4800, res = 600)
png("gmc_1d_poisson_fit.png", width = 4800, height = 4800, res = 600)
mixedcurve::dark_mode()
plot(df1$x, df1$y,
  col = adjustcolor(df1$id, 0.2),
  pch = 20, ylim = c(0, 50),
  ylab = "y", xlab = "x",
  main = "Quadratic Poisson Functional data with GMC fit"
)
points(df1$x, exp(eta_true), col = adjustcolor(df1$id, 1.00), cex = 0.5)
for(i in 1:nind) {
  lines(qseq, exp(qrs[, i]), col = adjustcolor(i, 0.50), lwd = 3)
}
legend("topright",
  legend = c("True means", "Estimated means", "Raw Data"),
  col = "white",
  lty = c(NA, 1, NA),
  pch = c(1, NA, 20),
  lwd = 2
)
invisible(dev.off())

#
```

<!-- }}} Fit GNW model -->

![Functional Poisson data with GNW fit](./gmc_1d_poisson_fit.png){width=95%}


#### TODO: add WY adjusted test



