library(ParBayesianOptimization)
simpleFunction <- function(x) dnorm(x, 3, 2) * 1.5 + dnorm(x, 7, 1) + dnorm(x, 10, 2)
xmax <- optim(8, simpleFunction, method = "L-BFGS-B", lower = 0, upper = 15, control = list(fnscale = -1))$par
library(ggplot2)
library(ggdark)
mixedcurve::dark_mode()
ggplot(data = data.frame(x = c(0, 15)), aes(x = x)) +
  stat_function(fun = simpleFunction) +
  geom_vline(xintercept = xmax, linetype = "dashed") +
  ggtitle("simpleFunction") +
  dark_theme_gray()
# theme_bw()
xmax

bounds <- list(x = c(0, 15))
initGrid <- data.frame(x = c(0, 5, 10))

FUN <- function(x) list(Score = simpleFunction(x))
set.seed(6)
optObjSimp <- bayesOpt(
  FUN = FUN,
  bounds = bounds,
  initGrid = initGrid,
  iters.n = 20
)
str(optObjSimp)
getBestPars(optObjSimp)
