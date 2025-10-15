
dist_2d <- function(xy1, xy2) {
  sqrt((xy1[1] - xy2[1])^2 + (xy1[2] - xy2[2])^2)
}


k_2d <- function(points, qry, h) {
  # print(str(head(points)))
  # Compute the squared differences for x and y
  x_diff <- points[, 1] - qry[1]
  y_diff <- points[, 2] - qry[2]
  # Compute the squared distance for the Gaussian kernel
  squared_dist <- ((x_diff / h)^2 + (y_diff / h)^2)
  # Compute the kernel weights using the Gaussian kernel
  k_weights <- exp(-squared_dist) / h
  as.numeric(k_weights)
}



