---
title: "Nadaraya-Watson (2d) Kernel Regression w/ Covariate"
author: "Jordan Schupbach"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Nadaraya-Watson (2d) Kernel Regression w/ Covariate}
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
Nadaraya-Watson kernel regression model to one-dimensional data with a single covariate.

## Example Usage
Let's start by simulating some data according to the m2 Cuevas et al. function
modified to two dimensions:

<!-- {{{ Generate data -->
```{r, fig.width=7, fig.height=5, dpi=600, out.width="700px", message=FALSE, warning=FALSE}

library(mixedcurve)
nxy <- 1000
fundata1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
  n = 1, ngrp = 3, nx = nxy, balanced = TRUE,
  systematic = FALSE, sigma = 0.001, px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ), white_noise = TRUE
)
df1 <- fundata1$df
# png("./vignettes/nw_2d_w_covariate/m2_data.png", width = 3 * 4800, height = 4800, res = 600)
# png("m2_data.png", width = 3 * 4800, height = 4800, res = 600)
mixedcurve::dark_mode()
color_scale <- viridis::viridis(100)
color_values <- cut(df1$y, breaks = 100, labels = FALSE)
par(mfrow = c(1, 3))
plot(df1$x1[df1$grp == 1], df1$x2[df1$grp == 1],
     col = color_scale[color_values[df1$grp == 1]],
     pch = 20, xlab = "x1", ylab = "x2",
     main = "Cuevas M2 (2d) function data (Group 1)", cex = 3.0)
plot(df1$x1[df1$grp == 2], df1$x2[df1$grp == 2],
     col = color_scale[color_values[df1$grp == 2]],
     pch = 20, xlab = "x1", ylab = "x2",
     main = "Cuevas M2 (2d) function data (Group 2)", cex = 3.0)
plot(df1$x1[df1$grp == 3], df1$x2[df1$grp == 3],
     col = color_scale[color_values[df1$grp == 3]],
     pch = 20, xlab = "x1", ylab = "x2",
     main = "Cuevas M2 (2d) function data (Group 3)", cex = 3.0)
# invisible(dev.off())

#
```      
<!-- }}} Generate data -->

![Cuevas M2 (2d) function data](./m2_data.png){width=95%}

To fit the Nadaraya-Watson kernel regression model, we can use the `lpk`
function from the `mixedcurve` package. We will specify the bandwidth, kernel
type, and degree and the function `y ~ K_h(x1 * x2 | grp)` to indicate a
two-dimensional kernel regression on the 2d (product) space of `x1 * x2` with a
covariate `grp`, fitting surfaces for each group.

<!-- {{{ Fit NW model -->

```{r, message=FALSE, warning=FALSE}

# Fit Nadaraya-Watson kernel regression model (in parallel)
qseq <- as.matrix(expand.grid(
  seq(0.0, 1.0, length.out = 20),
  seq(0.0, 1.0, length.out = 20))
)
lpk2 <- mixedcurve::lpk(y ~ K_h(x1 * x2 | grp),
  queries = qseq, data = df1, degree = 0,
  kernel = mixedcurve::gauss_kern, h = 0.02, parallel = TRUE
)
coefs <- matrix(unlist(lapply(lpk2[[1]], function (fit) fit$coefs)), ncol = 3, byrow = TRUE)
fits <- cbind(coefs[, 1], coefs[, 1] + coefs[, 2], coefs[, 1] + coefs[, 3])
all_values <- c(fits, mixedcurve::m2(qseq, 1), mixedcurve::m2(qseq, 2), mixedcurve::m2(qseq, 3))
breaks <- seq(min(all_values), max(all_values), length.out = 101)
colors <- viridis::viridis(100)
# png("./vignettes/nw_2d_w_covariate/m2_data_fit.png", width = 3 * 4800, height = 2 * 4800, res = 600)
png("m2_data_fit.png", width = 3 * 4800, height = 2 * 4800, res = 600)
mixedcurve::dark_mode()
par(mfrow = c(2, 3))
for (i in 1:3) {
  image(matrix(mixedcurve::m2(qseq, i), 20, 20),
    col = colors, breaks = breaks,
    axes = FALSE, main = paste("Cuevas M2 True Function (Group", i, ")")
  )
}
for (i in 1:3) {
  image(matrix(fits[, i], 20, 20),
    col = colors, breaks = breaks,
    axes = FALSE, main = paste("Nadaraya-Watson Fits Cuevas M2 Group", i)
  )
}
invisible(dev.off())

#
```

<!-- }}} Fit NW model -->

![Nadaraya-Watson Kernel Regression of M2 data](./m2_data_fit.png){width=95%}
