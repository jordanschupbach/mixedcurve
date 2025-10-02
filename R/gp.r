# -*- origami-fold-style: triple-braces -*-

# {{{ License
# Copyright (C) <2025>  <Jordan Schupbach>
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
# }}} License

# {{{ Description
# Name: fun.r
# Author: Jordan Schupbach
# Description: A function class
# }}} Description

# {{{ dist_2d

#' Function to calculate the Euclidean distance between two points in 2D space
#' @param xy1 A numeric vector of length 2 representing the coordinates of the first point (x1, y1)
#' @param xy2 A numeric vector of length 2 representing the coordinates of the second point (x2, y2)
#' @return The Euclidean distance between the two points
#' @examples
#' point1 <- c(1, 2)
#' point2 <- c(4, 6)
#' distance <- dist_2d(point1, point2)
dist_2d <- function(xy1, xy2) {
  sqrt((xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2)
}

# }}} dist_2d

# {{{ distance

#' Function to calculate the Euclidean distance between two vectors
#' @param x A numeric vector (or scalar)
#' @param y A numeric vector (or scalar) of the same length as x
distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

# }}} distance

# {{{ grf
grf <- function(n, interval, v, s) {
  xg <- seq(interval[1], interval[2], length.out = n) # Sequence over [0,1] of length n + 1
  cf <- function(d, variance, scale) {  # Gaussian Covariance Function
    variance * exp(-(d / scale)^2)
  }
  Sigma <- cf(as.matrix(dist(c(xg, rev(xg)[-length(xg)][-1]))), v, s) # Torus (1d) over grid
  temp <- numeric(nrow(Sigma))
  temp[floor(nrow(Sigma))] <- 1  # Dirac
  w <- fft(Sigma[1, ]) / (fft(temp) * n) # Compute weights
  z <- fft(rnorm(length(w), interval[1], interval[2])) # Fourier of standard normal
  y <- Re(fft(sqrt(w) * z, inverse = TRUE))[1:(n)] / sqrt(n) # Inverse Fourier over sqrt(w) * z
  list(x = xg, y = y)
}
# }}} grf

# Local Variables:
# eval: (origami-mode t)
# End:
