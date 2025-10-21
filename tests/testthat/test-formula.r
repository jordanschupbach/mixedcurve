test_that(
  "parse_kernfun_1d_0covariate",
  {
    form <- y ~ K_h(x1)
    terms <- mixedcurve::parse_terms(as.formula(form))
    expect_equal(
      terms[terms$type == "response", ]$lhs,
      "y"
    )
    expect_true(is.na(terms[terms$type == "response", ]$rhs))
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$lhs,
      "x1"
    )
    expect_true(is.na(terms[terms$type == "kernel fixed effect", ]$rhs))
  }
)

test_that(
  "parse_kernfun_1d_1covariate",
  {
    form <- y ~ K_h(x1 | grp)
    terms <- mixedcurve::parse_terms(as.formula(form))
    expect_equal(
      terms[terms$type == "response", ]$lhs,
      "y"
    )
    expect_true(is.na(terms[terms$type == "response", ]$rhs))
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$lhs,
      "x1"
    )
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$rhs,
      "grp"
    )
  }
)

test_that(
  "parse_kernfun_1d_1covariate_w_random_effect",
  {
    form <- y ~ K_h(x1 | grp) + (1 | cluster)
    terms <- mixedcurve::parse_terms(as.formula(form))
    terms
    expect_equal(
      terms[terms$type == "response", ]$lhs,
      "y"
    )
    expect_true(is.na(terms[terms$type == "response", ]$rhs))
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$lhs,
      "x1"
    )
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$rhs,
      "grp"
    )
    expect_equal(
      terms[terms$type == "random effect", ]$rhs,
      "cluster"
    )
    expect_equal(
      terms[terms$type == "random effect", ]$lhs,
      "1"
    )
  }
)
test_that(
  "parse_kernfun_1d_1covariate_w_kernel_random_effect",
  {
    form <- y ~ K_h(x1 | grp) + (K_h(x1) | cluster)
    terms <- mixedcurve::parse_terms(as.formula(form))
    terms
    expect_equal(
      terms[terms$type == "response", ]$lhs,
      "y"
    )
    expect_true(is.na(terms[terms$type == "response", ]$rhs))
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$lhs,
      "x1"
    )
    expect_equal(
      terms[terms$type == "kernel fixed effect", ]$rhs,
      "grp"
    )
    expect_equal(
      terms[terms$type == "random effect", ]$rhs,
      "cluster"
    )
    expect_equal(
      terms[terms$type == "random effect", ]$lhs,
      "K_h(x1)"
    )
  }
)
