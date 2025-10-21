# Hello, World! Around The World

This file contains code chunks in multiple languages. The idea is to test literate programming
using nix, markdown, and various language interpreters.

<!-- {{{ R -->
Here's an R code chunk that generates a noisy Doppler function plot:
```{r}
library(mixedcurve)
# R code chunk to define Doppler function and plot noisy data

#' Modified Doppler function
#'
#' This function computes a modified Doppler function.
#'
#' @param x Numeric input.
#' @return Numeric output of the modified Doppler function.
#' @examples
#' mdoppler(0.5)
#' mdoppler(1.0)
#' @export
mdoppler <- function(x) {
  sin(20 / (x + 0.25))
}

n <- 1000
x <- seq(0, 1, length.out = n)
y <- mdoppler(x) + rnorm(n, 0, 0.05)

png("figs/doppler_plot_r.png")
par(bg = "black", col.axis = "white", col.lab = "white", col.main = "white")
plot(x, y, col = adjustcolor("blue", 0.2), pch = 20,
     cex = 2.0, xlab = "X-axis", ylab = "Y-axis",
     main = "Doppler Function with Noise")
dev.off()

#
```

![Dopppler data (R)](figs/doppler_plot_r.png)

<!-- }}} R -->
