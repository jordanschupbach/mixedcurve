library(mixedcurve)

gen_1d_curve_data <- function(n = 300, 
                              ngrp = 1, 
                              noise_sd = 0.1, 
                              seed = 123) {
  set.seed(seed)
  grp <- sample(1:ngrp, n, replace = TRUE)
  x <- runif(n)
  y <- sapply(1:n, function(i) mixedcurve::m1(x[i], grp[i])) + rnorm(n, sd = noise_sd)
  data.frame(x = x, y = y, grp = as.factor(grp))
}
df1 <- gen_1d_curve_data()
plot(df1$x, df1$y, col = df1$grp)

