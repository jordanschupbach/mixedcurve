---
title: "Generalized Nadaraya-Watson (2d) Kernel Regression W Covariate"
author: "Jordan Schupbach"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Generalized Nadaraya-Watson (2d) Kernel Regression W Covariate}
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
generalized Nadaraya-Watson kernel regression model to 2D Poisson
data.

## Example Usage
Let's start by simulating some 2D spatial Poisson data with group effects to
fit a generalized Nadaraya-Watson kernel regression model to. We will make some
modifications to the Cuevas et al. M3 curve for this purpose.

<!-- {{{ Simulate data -->
```{r}

set.seed(123)
# 1. Define the true curves
tf <- function(t, i) {
  # Define the rate function for Poisson data at time t
    exp(3 * exp(mixedcurve::m3(t, i)))
}
n <- 3000
set.seed(123)
fundata1 <- mixedcurve::gen_fanova_data(
  f = tf,
  bounds = list(c(0, 1), c(0, 1)),
  pxargs = list(list(min = 0, max = 1), list(min = 0, max = 1)),
  n = 1, # Still sensitive to n?
  nx = n,
  balanced = TRUE,
  ngrp = 3,
  sigma = 3.020,
  family = "poisson"
)
df1 <- fundata1$df
color_values <- cut(df1$y, breaks = 100, labels = FALSE)
color_scale <- viridis::viridis(100)
# png("./vignettes/gnw_2d_w_covariate/gnw_2d_poisson_data.png", width = 3 * 4800, height = 4800, res = 600)
png("./gnw_2d_poisson_data.png", width = 3 * 4800, height = 4800, res = 600)
mixedcurve::dark_mode()
par(mfrow = c(1, 3))
plot(df1$x1[df1$grp == 1], df1$x2[df1$grp == 1],
  col = color_scale[color_values[df1$grp == 1]],
  pch = 20,
  main = "Modified M3 Poisson Data (Group 1)"
)
plot(df1$x1[df1$grp == 2], df1$x2[df1$grp == 2],
  col = color_scale[color_values[df1$grp == 2]],
  pch = 20,
  main = "Modified M3 Poisson Data (Group 2)"
)
plot(df1$x1[df1$grp == 3], df1$x2[df1$grp == 3],
  col = color_scale[color_values[df1$grp == 3]],
  pch = 20,
  main = "Modified M3 Poisson Data (Group 3)"
)
invisible(dev.off())

#
```
<!-- }}} Simulate data -->

![Functional Poisson data](./gnw_2d_poisson_data.png){width=95%}

Now, we can fit the generalized Nadaraya-Watson kernel regression model using
the `lpk` function from the `mixedcurve` package. We will specify the
bandwidth, kernel type, degree, and use the formula `y ~ K_h(x * y | grp)` to
indicate that we want to fit a generalized local polynomial kernel model across
domain `x * y`, i.e. the two-dimensional plane, conditional on group `grp` as a
covariate.

<!-- {{{ Fit GNW model -->

```{r, message=FALSE, warning=FALSE}
mixedcurve::kernel_to_lm_formula(y ~ K_h(x1 * x2 | grp))

# 4. Fit GNW kernel regression model (in parallel)
nxy <- 20
qseq <- as.matrix(expand.grid(
  seq(0.0, 1.0, length.out = nxy),
  seq(0.0, 1.0, length.out = nxy)
))
glpk1 <- mixedcurve::glpk(y ~ K_h(x1 * x2 | grp),
  queries = qseq,
  data = df1,
  degree = 0,
  kernel = mixedcurve::gauss_kern,
  family = "poisson",
  h = c(0.025),
  parallel = TRUE
)
qs <- matrix(
  unlist(lapply(glpk1[[1]], function(elmt) { elmt$coefs })),
  nxy * nxy, 3, byrow = TRUE
)
qscoefs <- cbind( qs[, 1], qs[, 1] + qs[, 2], qs[, 1] + qs[, 3])
all_values <- c(as.vector(exp(qscoefs)), tf(qseq, 1), tf(qseq, 2), tf(qseq, 3))
breaks <- seq(min(all_values), max(all_values), length.out = 101)
colors <- viridis::viridis(100)
# png("./vignettes/gnw_2d_w_covariate/gnw_2d_poisson_fit.png", width = 3 * 4800, height = 2 * 4800, res = 600)
png("gnw_2d_poisson_fit.png", width = 3 * 4800, height = 2 * 4800, res = 600)
par(mfrow = c(2, 3))
mixedcurve::dark_mode()
image(matrix(tf(qseq, 1), nxy, nxy),
  col = colors, breaks = breaks,
  main = "True M3_1 curve"
)
image(matrix(tf(qseq, 2), nxy, nxy),
  col = colors, breaks = breaks,
  main = "True M3_2 curve"
)
image(matrix(tf(qseq, 3), nxy, nxy),
  col = colors, breaks = breaks,
  main = "True M3_3 curve"
)
image(matrix(exp(qscoefs[, 1]), nxy, nxy),
  col = colors, breaks = breaks,
  main = "GNW fit"
)
image(matrix(exp(qscoefs[, 2]), nxy, nxy),
  col = colors, breaks = breaks,
  main = "GNW fit"
)
image(matrix(exp(qscoefs[, 3]), nxy, nxy),
  col = colors, breaks = breaks,
  main = "GNW fit"
)
dev.off()

#
```
<!-- }}} Fit GNW model -->

![Functional Poisson data](./gnw_2d_poisson_fit.png){width=95%}


#### TODO: add WY adjusted test

