# Tests for the fun API
# NOTE: is this a class?
test_that("Sqrt(1) = 1", {
  myfun <- mixedcurve::fun((function(x) sqrt(x)),
    dom = c(0, 1000)
  )
  expect_equal(myfun$f(1), 1)
  expect_equal(myfun$dom[1], 0)
  expect_equal(myfun$dom[2], 1000)
})
