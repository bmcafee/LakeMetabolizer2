test_that("wind.scale.base returns correct value: wnd=5 at height=2", {
  # U10 = wnd * (10/wnd.z)^0.15 = 5 * 5^0.15
  expected <- 5 * (10 / 2)^0.15
  result <- wind.scale.base(5, 2)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("wind.scale.base identity: wind at 10m is unchanged", {
  result <- wind.scale.base(5, 10)
  expect_equal(result, 5)
})

test_that("wind.scale.base scales up wind from height below 10m", {
  result <- wind.scale.base(5, 2)
  expect_true(result > 5)
})

test_that("wind.scale.base scales down wind from height above 10m", {
  result <- wind.scale.base(5, 20)
  expect_true(result < 5)
})

test_that("wind.scale.base handles vector input", {
  wnd <- c(3, 5, 7)
  result <- wind.scale.base(wnd, 2)
  expected <- wnd * (10 / 2)^0.15
  expect_equal(result, expected)
})

test_that("wind.scale wrapper returns data.frame with wnd_10 column", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00"),
                           tz = "UTC"),
    wnd = c(3, 5))
  result <- wind.scale(ts, wnd.z = 2)
  expect_s3_class(result, "data.frame")
  expect_true("wnd_10" %in% names(result))
  expect_true("datetime" %in% names(result))
  expect_equal(result$wnd_10, wind.scale.base(ts$wnd, 2))
})

test_that("wind.scale errors when wnd column is missing", {
  ts_bad <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wind_speed = 5)
  expect_error(wind.scale(ts_bad, wnd.z = 2))
})
