# Some examples of benchmark functions and benchmark datasets

# {{{ mdoppler

# 1d
neval <- 5000
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
plot(xseq, mixedcurve::mdoppler(xseq), type = "l",
     main = "mdoppler", ylab = "f(x)", xlab = "x")

# 2d
neval <- 300
xseq <- seq(0, 1, length.out = neval)
xyseq <- expand.grid(x = xseq, y = xseq)
mixedcurve::dark_mode()
image(matrix(mixedcurve::mdoppler(as.matrix(xyseq)), nrow = neval),
      axes = FALSE, xlab = "x", ylab = "y", main = "mdoppler")

# }}} mdoppler

# {{{ m1

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(xseq, mixedcurve::m1(xseq, 1), type = "l",
     main = "m1", ylab = "f(x)", xlab = "x")
for (i in 2:3){
  lines(xseq, mixedcurve::m1(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
layout(matrix(c(1, 2, 3, 4), ncol = 4),
       widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for (i in 1:3) {
  image(matrix(mixedcurve::m1(as.matrix(xyseq), i), nrow = neval),
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i),
        col = viridis(256))
}
z_values <- c(mixedcurve::m1(as.matrix(xyseq), 1),
              mixedcurve::m1(as.matrix(xyseq), 2),
              mixedcurve::m1(as.matrix(xyseq), 3))
z_range <- range(z_values)
par(mar = c(4, 2, 2, 2))
image(1, seq(z_range[1], z_range[2], length.out = 256),
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1),
      col = viridis(256), axes = FALSE, xlab = "")
axis(2, las = 1, at = pretty(z_range))
title(main = "z")

# }}} 2d

# }}} m1

# {{{ m2

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(xseq, mixedcurve::m2(xseq, 1), type = "l",
     main = "m1", ylab = "f(x)", xlab = "x")
for (i in 2:3){
  lines(xseq, mixedcurve::m2(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
z_values <- c(mixedcurve::m2(as.matrix(xyseq), 1),
              mixedcurve::m2(as.matrix(xyseq), 2),
              mixedcurve::m2(as.matrix(xyseq), 3))
z_range <- range(z_values)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for (i in 1:3) {
  image(matrix(mixedcurve::m2(as.matrix(xyseq), i), nrow = neval),
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i),
        col = viridis(256), zlim = z_range)
}
par(mar = c(4, 2, 2, 2))
image(1, seq(z_range[1], z_range[2], length.out = 256),
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1),
      col = viridis(256), axes = FALSE, xlab = "")
axis(2, las = 1, at = pretty(z_range))
title(main = "z")

# }}} 2d

# }}} m2

# {{{ m3

# {{{ 1d
neval <- 1000
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(xseq, mixedcurve::m3(xseq, 1), type = "l",
     main = "m1", ylab = "f(x)", xlab = "x")
for (i in 2:3){
  lines(xseq, mixedcurve::m3(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

neval <- 100
xseq <- seq(0, 1, length.out = neval)
library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
z_values <- c(mixedcurve::m3(as.matrix(xyseq), 1),
              mixedcurve::m3(as.matrix(xyseq), 2),
              mixedcurve::m3(as.matrix(xyseq), 3))
z_range <- range(z_values)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for (i in 1:3) {
  image(matrix(mixedcurve::m3(as.matrix(xyseq), i), nrow = neval),
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i),
        col = viridis(256), zlim = z_range)
}
par(mar = c(4, 2, 2, 2))
image(1, seq(z_range[1], z_range[2], length.out = 256),
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1),
      col = viridis(256), axes = FALSE, xlab = "")
axis(2, las = 1, at = pretty(z_range))
title(main = "z")

# }}} 2d

# }}} m3

# {{{ m4

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(xseq, mixedcurve::m4(xseq, 1), type = "l",
     main = "m1", ylab = "f(x)", xlab = "x")
for (i in 2:3){
  lines(xseq, mixedcurve::m4(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
z_values <- c(mixedcurve::m4(as.matrix(xyseq), 1),
              mixedcurve::m4(as.matrix(xyseq), 2),
              mixedcurve::m4(as.matrix(xyseq), 3))
z_range <- range(z_values)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for (i in 1:3) {
  image(matrix(mixedcurve::m4(as.matrix(xyseq), i), nrow = neval),
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i),
        col = viridis(256), zlim = z_range)
}
par(mar = c(4, 2, 2, 2))
image(1, seq(z_range[1], z_range[2], length.out = 256),
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1),
      col = viridis(256), axes = FALSE, xlab = "")
axis(2, las = 1, at = pretty(z_range))
title(main = "z")

# }}} 2d

# }}} m4

# {{{ m5

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1, 1))
plot(xseq, mixedcurve::m5(xseq, 1), type = "l",
     main = "m1", ylab = "f(x)", xlab = "x", ylim = c(0, 0.1))
for (i in 2:3){
  lines(xseq, mixedcurve::m5(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
z_values <- c(mixedcurve::m5(as.matrix(xyseq), 1),
              mixedcurve::m5(as.matrix(xyseq), 2),
              mixedcurve::m5(as.matrix(xyseq), 3))
z_range <- range(z_values)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for (i in 1:3) {
  image(matrix(mixedcurve::m5(as.matrix(xyseq), i), nrow = neval),
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i),
        col = viridis(256), zlim = z_range)
}
par(mar = c(4, 2, 2, 2))
image(1, seq(z_range[1], z_range[2], length.out = 256),
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1),
      col = viridis(256)[256:1], axes = FALSE, xlab = "")
axis(2, las = 1, at = pretty(z_range))
title(main = "z")

# }}} 2d

# }}} m5

# {{{ gen_fanova_data & plot

# {{{ 1d

# {{{ 1) gen 1d, systematic, white noise

nxy <- 200
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m3, bounds = list(c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.05, systematic = TRUE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 1000
)
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)

# }}} 1) gen 2d systematic data

# {{{ 2) gen 1d, nonsystematic, white noise

nxy <- 200
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m3, bounds = list(c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.05, systematic = FALSE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 1000
)
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)

# }}} 1) gen 2d systematic data

# {{{ 3) gen 1d, systematic, gaussian process noise
nxy <- 200
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m3, bounds = list(c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.010, systematic = TRUE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = FALSE,
  cov_scale = 0.10, gpn = 1000
)
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)

# }}} 3) gen 2d systematic data

# {{{ 4) gen 1d, nonsystematic, gaussian process noise

nxy <- 200
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m3, bounds = list(c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.010, systematic = FALSE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = FALSE,
  cov_scale = 0.1, gpn = 1000
)
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)

# }}} 4) gen 2d systematic data

# }}} 1d

# {{{ 2d

# {{{ 5) gen 2d systematic, white_noise

nxy <- 20
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m5, bounds = list(c(0, 1), c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.005, systematic = TRUE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 1000
)
data1$df
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)


# delta_i <- geoR::grf(1000, nx = 10, ny = 10, grid = "reg",
#                      xlims = c(0, 1),
#                      ylims = c(0, 1),
#                      cov.model = "gaussian",
#                      cov.pars = c(0.001, 0.25),
#                      nugget = 0.0,
#                      nsim = 10, messages = FALSE)[[2]]
# image(matrix(delta_i[,1], 10, 10))

# }}} 5) gen 2d systematic data

# {{{ 6) gen 2d nonsystematic, white noise

nxy <- 3000
data2 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.01, systematic = FALSE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = TRUE,
  cov_scale = 0.05, gpn = 1000
)
mixedcurve::light_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data2, curves = 1:6)

# }}} 6) gen 2d nonsystematic data

# {{{ 7) gen 2d systematic, gaussian process

nxy <- 20
data1 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m5, bounds = list(c(0, 1), c(0, 1)),
  n = 10, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.00005, systematic = TRUE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = FALSE,
  cov_scale = 0.10, gpn = 1000
)
data1$df
mixedcurve::dark_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data1, curves = 1:6)

# }}} 7) gen 2d systematic, gaussian process

# {{{ 8) gen 2d nonsystematic, gaussian process # BUG: something off here... the colors don't seem right?

nxy <- 5000
data2 <- mixedcurve::gen_fanova_data(
  f = mixedcurve::m2, bounds = list(c(0, 1), c(0, 1)),
  n = 20, ngrp = 3, nx = nxy,
  balanced = FALSE, pgrp = sample,
  pgrpargs = list(x = 1:3, size = 10, replace = TRUE),
  sigma = 0.0010, systematic = FALSE,
  px = runif,
  pxargs = list(
    list(min = 0, max = 1),
    list(min = 0, max = 1)
  ),
  white_noise = FALSE,
  cov_scale = 0.05, gpn = 50
)
mixedcurve::light_mode()
layout(mixedcurve::gen_square_layout(6))
mixedcurve::plot.fundata(data2, curves = 1:6)

# }}} 6) gen 2d nonsystematic data

# }}} 2d

# }}} gen_fanova_data & plot

# {{{ Gen hierarchical fanova data




# }}} Gen hierarchical fanova data
