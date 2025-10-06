test_that("1d fanova generator", {
  df1 <- gen_1d_fanova_data(
    f = mixedcurve::m3,
    bounds = c(0, 1), n = 10,
    ngrp = 3, nx = 10, balanced = FALSE,
    pgrp = sample,
    pgrpargs = list(x = 1:3, size = 30, replace = TRUE),
    sigma = 0.05, systematic = FALSE, px = runif,
    pxargs = list(min = 0, max = 1),
    white_noise = TRUE, cov_scale = 0.05, gpn = 1000
  )
  expect_equal(ncol(df1), 4)
  expect_equal(colnames(df1), c("x", "y", "grp", "id"))
  expect_equal(nrow(df1), 100)
  expect_equal(length(unique(df1$id)), 10)
  expect_equal(length(unique(df1$grp)), 3)
})
