test_that("Sqrt(1) = 1", {
  myfun <- mixedcurve::fun((function(x) sqrt(x)), dom = c(0, 1000))
  expect_equal(myfun$f(1), 1)
  expect_equal(myfun$dom[1], 0)
  expect_equal(myfun$dom[2], 1000)
})

test_that("GaussKern 1", {
  expect_equal(mixedcurve::gauss_kern(1), 0.6065307, tolerance = 1e-7)
})
