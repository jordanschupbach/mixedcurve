library(mixedcurve)
library(viridis)

# {{{ mdoppler

# 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
plot(xseq, mixedcurve::mdoppler(xseq), type = "l", main = "mdoppler", ylab = "f(x)", xlab = "x")

# 2d
xyseq <- expand.grid(x = xseq, y = xseq)
mixedcurve::dark_mode()
image(matrix(mixedcurve::mdoppler(as.matrix(xyseq)), nrow = neval), axes = FALSE, xlab = "x", ylab = "y", main = "mdoppler")

# }}} mdoppler

# {{{ m1

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1,1))
plot(xseq, mixedcurve::m1(xseq, 1), type = "l", main = "m1", ylab = "f(x)", xlab = "x")
for(i in 2:3){
  lines(xseq, mixedcurve::m1(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d
library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))  # Reduced width for color scale
par(mar = c(4, 4, 2, 2))  # Bottom, left, top, right
for(i in 1:3) {
  image(matrix(mixedcurve::m1(as.matrix(xyseq), i), nrow = neval), 
        axes = FALSE, xlab = "x", ylab = "y", main = paste0("Grp", i), 
        col = viridis(256))  # Use viridis color palette
}
z_values <- c(mixedcurve::m1(as.matrix(xyseq), 1), 
              mixedcurve::m1(as.matrix(xyseq), 2), 
              mixedcurve::m1(as.matrix(xyseq), 3))
z_range <- range(z_values)  # Determine the range of values
par(mar = c(4, 2, 2, 2)) # Adjust margins for color scale
image(1, seq(z_range[1], z_range[2], length.out = 256),  # Keep the range from low to high
      matrix(seq(z_range[1], z_range[2], length.out = 256), nrow = 1), 
      col = viridis(256)[256:1], axes = FALSE, xlab = "")  # Color bar using viridis
axis(2, las = 1, at = pretty(z_range)) # Add axis on the right of color scale
title(main = "z")
# }}} 2d

# }}} m1

# {{{ m2

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1,1))
plot(xseq, mixedcurve::m2(xseq, 1), type = "l", main = "m1", ylab = "f(x)", xlab = "x")
for(i in 2:3){
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
for(i in 1:3) {
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
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1,1))
plot(xseq, mixedcurve::m3(xseq, 1), type = "l", main = "m1", ylab = "f(x)", xlab = "x")
for(i in 2:3){
  lines(xseq, mixedcurve::m3(xseq, i), col = i)
}
legend("topright", legend = paste0("grp", 1:3), col = 1:3, lty = 1)
# }}} 1d

# {{{ 2d

library(viridis)
xyseq <- expand.grid(x = xseq, y = xseq)
z_values <- c(mixedcurve::m3(as.matrix(xyseq), 1), 
               mixedcurve::m3(as.matrix(xyseq), 2), 
               mixedcurve::m3(as.matrix(xyseq), 3))
z_range <- range(z_values)
layout(matrix(c(1, 2, 3, 4), ncol = 4), widths = c(1, 1, 1, 0.2))
par(mar = c(4, 4, 2, 2))
for(i in 1:3) {
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
par(mfrow = c(1,1))
plot(xseq, mixedcurve::m4(xseq, 1), type = "l", main = "m1", ylab = "f(x)", xlab = "x")
for(i in 2:3){
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
for(i in 1:3) {
  image(matrix(mixedcurve::m4(as.matrix(xyseq), i), nrow = neval), 
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

# }}} m4

# {{{ m5

# {{{ 1d
neval <- 100
xseq <- seq(0, 1, length.out = neval)
mixedcurve::dark_mode()
par(mfrow = c(1,1))
plot(xseq, mixedcurve::m5(xseq, 1), type = "l", main = "m1", ylab = "f(x)", xlab = "x", ylim = c(0, 0.1))
for(i in 2:3){
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
for(i in 1:3) {
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
