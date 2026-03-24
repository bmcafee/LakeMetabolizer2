## ---- k.cole ----

test_that("k.cole.base known value at U10=4 m/s", {
  # k600 = (2.07 + 0.215 * 4^1.7) * 24/100 in m/day
  expected <- (2.07 + 0.215 * 4^1.7) * 24 / 100
  result <- k.cole.base(4)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("k.cole.base returns positive k600 at zero wind (intercept)", {
  result <- k.cole.base(0)
  expected <- 2.07 * 24 / 100
  expect_equal(result, expected, tolerance = 1e-10)
  expect_true(result > 0)
})

test_that("k.cole.base k600 increases with wind speed", {
  expect_true(k.cole.base(5) > k.cole.base(3))
  expect_true(k.cole.base(3) > k.cole.base(1))
})

test_that("k.cole wrapper returns data.frame with datetime and k600 columns", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 01:00"), tz = "UTC"),
    wnd = c(3, 5))
  result <- k.cole(ts)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("datetime", "k600") %in% names(result)))
  expect_equal(result$k600, k.cole.base(ts$wnd))
})

test_that("k.cole errors when wnd column is missing", {
  ts_bad <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wind = 5)
  expect_error(k.cole(ts_bad))
})

## ---- k.crusius ----

test_that("k.crusius.base power method returns numeric vector", {
  result <- k.crusius.base(c(2, 4, 6), method = "power")
  expect_type(result, "double")
  expect_length(result, 3)
  expect_true(all(is.finite(result)))
})

test_that("k.crusius.base constant method: all values below threshold equal same k600", {
  # Below 3.7 m/s threshold → constant 1 cm/h = 0.24 m/day
  result <- k.crusius.base(c(1, 2, 3), method = "constant")
  expect_equal(result, rep(1 * 24 / 100, 3))
})

test_that("k.crusius.base constant method: value at threshold uses second formula", {
  # At exactly 3.7, ifelse(3.7 < 3.7, ...) is FALSE → uses 5.14*3.7 - 17.9
  expected <- (5.14 * 3.7 - 17.9) * 24 / 100
  result <- k.crusius.base(3.7, method = "constant")
  expect_equal(result, expected, tolerance = 1e-10)
  expect_true(result > 1 * 24 / 100)  # higher than the below-threshold value
})

test_that("k.crusius.base bilinear method returns numeric vector", {
  result <- k.crusius.base(c(2, 5), method = "bilinear")
  expect_type(result, "double")
  expect_length(result, 2)
  # Below threshold
  expect_equal(result[1], 0.72 * 2 * 24 / 100, tolerance = 1e-10)
  # Above threshold
  expect_equal(result[2], (4.33 * 5 - 13.3) * 24 / 100, tolerance = 1e-10)
})

test_that("k.crusius.base errors on invalid method", {
  expect_error(k.crusius.base(5, method = "invalid"))
})

test_that("k.crusius wrapper returns data.frame with datetime and k600", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 01:00"), tz = "UTC"),
    wnd = c(3, 5))
  result <- k.crusius(ts, method = "power")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("datetime", "k600") %in% names(result)))
  expect_equal(result$k600, k.crusius.base(ts$wnd, method = "power"))
})

## ---- k.vachon ----

test_that("k.vachon.base known value at U10=3, lake.area=1e6 m2", {
  # At 1e6 m^2, log10(1e6/1e6) = 0, so third term drops out
  # k600 = (2.51 + 1.48*3 + 0) * 24/100
  expected <- (2.51 + 1.48 * 3 + 0.39 * 3 * log10(1e6 / 1e6)) * 24 / 100
  result <- k.vachon.base(3, 1e6)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("k.vachon.base k600 increases with lake area", {
  # log10(area/1e6) > 0 for area > 1e6 → adds to k600
  expect_true(k.vachon.base(3, 1e7) > k.vachon.base(3, 1e6))
  expect_true(k.vachon.base(3, 1e6) > k.vachon.base(3, 1e5))
})

test_that("k.vachon wrapper returns data.frame with datetime and k600", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 01:00"), tz = "UTC"),
    wnd = c(3, 5))
  result <- k.vachon(ts, lake.area = 1e6)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("datetime", "k600") %in% names(result)))
  expect_equal(result$k600, k.vachon.base(ts$wnd, 1e6))
})

test_that("k.vachon errors when wnd column is missing", {
  ts_bad <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wind = 5)
  expect_error(k.vachon(ts_bad, lake.area = 1e6))
})
