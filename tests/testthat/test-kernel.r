test_that("GaussKern 1d", {
  h <- runif(1)
  pt <- runif(1)
  qry <- runif(1)
  x_diff <- pt - qry
  squared_dist <- (x_diff / h)^2
  k_weights <- exp(-(1 / 2) * squared_dist) / h
  expect_equal(
    mixedcurve::kern_h(pt - qry, h, kern = mixedcurve::gauss_kern),
    k_weights
  )
})

test_that("GaussKern 2d", {
  h <- runif(1)
  pt <- c(runif(1), runif(1))
  qry <- c(runif(1), runif(1))
  x_diff <- pt[1] - qry[1]
  y_diff <- pt[2] - qry[2]
  squared_dist <- ((x_diff / h)^2 + (y_diff / h)^2)
  k_weights <- exp(-(1 / 2) * squared_dist) / h
  expect_equal(
    mixedcurve::kern_h(pt - qry, h, kern = mixedcurve::gauss_kern),
    k_weights
  )
})
