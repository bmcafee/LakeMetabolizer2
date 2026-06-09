## ---- par.to.sw ----

test_that("par.to.sw.base known value: 100 PAR -> 47.3 W/m2", {
  expect_equal(par.to.sw.base(100), 47.3)
})

test_that("par.to.sw.base is linear in input", {
  expect_equal(par.to.sw.base(200), 2 * par.to.sw.base(100))
  expect_equal(par.to.sw.base(0),   0)
})

test_that("par.to.sw.base handles vector input", {
  par_vals <- c(100, 200, 500)
  expected <- par_vals * 0.473
  expect_equal(par.to.sw.base(par_vals), expected)
})

test_that("par.to.sw wrapper returns data.frame with sw column", {
  df <- data.frame(par = c(100, 500, 1000))
  result <- par.to.sw(df)
  expect_s3_class(result, "data.frame")
  expect_true("sw" %in% names(result))
  expect_false("par" %in% names(result))
  expect_equal(result$sw, par.to.sw.base(c(100, 500, 1000)))
})

## ---- sw.to.par ----

test_that("sw.to.par.base known value: 100 W/m2 -> 211.4 PAR", {
  expect_equal(sw.to.par.base(100), 211.4)
})

test_that("sw.to.par.base is linear in input", {
  expect_equal(sw.to.par.base(200), 2 * sw.to.par.base(100))
  expect_equal(sw.to.par.base(0),   0)
})

test_that("sw.to.par wrapper returns data.frame with par column", {
  df <- data.frame(sw = c(100, 500))
  result <- sw.to.par(df)
  expect_s3_class(result, "data.frame")
  expect_true("par" %in% names(result))
  expect_false("sw" %in% names(result))
  expect_equal(result$par, sw.to.par.base(c(100, 500)))
})

## ---- roundtrip ----

test_that("par -> sw -> par roundtrip is approximate (within 0.1%)", {
  par_in  <- 100
  sw_val  <- par.to.sw.base(par_in)
  par_out <- sw.to.par.base(sw_val)
  # 0.473 * 2.114 ≈ 0.99992, so roundtrip is not exact
  expect_equal(par_out, par_in, tolerance = 1e-3)
})
