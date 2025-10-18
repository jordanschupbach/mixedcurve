mixedcurve::mdoppler(1.1)

dist_2d <- function(xy1, xy2) {
  sqrt((xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2)
}

k_2d <- function(points, qry, h) {
  x_diff <- points[, 1] - qry[1]
  y_diff <- points[, 2] - qry[2]
  squared_dist <- ((x_diff / h)^2 + (y_diff / h)^2)
  k_weights <- exp(-squared_dist) / h
  as.numeric(k_weights)
}
