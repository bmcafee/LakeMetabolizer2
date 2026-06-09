## ---- has.vars ----

test_that("has.vars returns TRUE for existing columns", {
  df <- data.frame(datetime = Sys.time(), wtr = 20, wnd = 5)
  expect_true(has.vars(df, "wtr"))
  expect_true(has.vars(df, "wnd"))
})

test_that("has.vars returns FALSE for missing columns", {
  df <- data.frame(datetime = Sys.time(), wtr = 20)
  expect_false(has.vars(df, "wnd"))
})

test_that("has.vars is case-insensitive", {
  df <- data.frame(datetime = Sys.time(), WTR = 20)
  expect_true(has.vars(df, "wtr"))
})

test_that("has.vars handles multiple variable names", {
  df <- data.frame(datetime = Sys.time(), wtr = 20, wnd = 5)
  result <- has.vars(df, c("wtr", "wnd", "par"))
  expect_equal(result, c(TRUE, TRUE, FALSE))
})

test_that("has.vars errors when input is not a data.frame", {
  expect_error(has.vars(c(1, 2, 3), "x"))
})

## ---- get.vars ----

test_that("get.vars returns data.frame with datetime and matched columns", {
  df <- data.frame(
    datetime = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"),
    wtr = c(15, 16),
    wnd = c(3, 4))
  result <- get.vars(df, "wtr")
  expect_s3_class(result, "data.frame")
  expect_true("datetime" %in% names(result))
  expect_true("wtr" %in% names(result))
  expect_false("wnd" %in% names(result))
})

test_that("get.vars errors when no variable pattern matches", {
  df <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wtr = 20)
  expect_error(get.vars(df, "nonexistent_var"))
})

test_that("get.vars errors when no datetime column is present", {
  df <- data.frame(wtr = 20, wnd = 5)
  expect_error(get.vars(df, "wtr"))
})

## ---- rmv.vars ----

test_that("rmv.vars removes named column", {
  df <- data.frame(datetime = Sys.time(), wtr = 20, wnd = 5)
  result <- rmv.vars(df, "wnd")
  expect_false("wnd" %in% names(result))
  expect_true("wtr" %in% names(result))
})

test_that("rmv.vars with ignore.missing=TRUE does not error on absent column", {
  df <- data.frame(datetime = Sys.time(), wtr = 20)
  expect_null(rmv.vars(df, "nonexistent", ignore.missing = TRUE))
})

test_that("rmv.vars with ignore.missing=FALSE errors on absent column", {
  df <- data.frame(datetime = Sys.time(), wtr = 20)
  expect_error(rmv.vars(df, "nonexistent", ignore.missing = FALSE))
})

## ---- get.offsets (from rLakeAnalyzer) ----

test_that("get.offsets parses numeric suffix from column name", {
  df <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wtr_1.5 = 20)
  wtr_df <- get.vars(df, "wtr")
  offsets <- get.offsets(wtr_df)
  expect_equal(offsets, 1.5)
})

test_that("get.offsets returns NA for column without numeric suffix", {
  df <- data.frame(
    datetime = as.POSIXct("2020-01-01", tz = "UTC"),
    wtr = 20)
  wtr_df <- get.vars(df, "wtr")
  offsets <- suppressWarnings(get.offsets(wtr_df))
  expect_true(is.na(offsets))
})

## ---- date2doy ----

test_that("date2doy returns 1.5 for Jan 1 at noon UTC", {
  dt <- as.POSIXct("2020-01-01 12:00:00", tz = "UTC")
  result <- LakeMetabolizer:::date2doy(dt)
  expect_equal(result, 1.5)
})

test_that("date2doy returns 1.0 for Jan 1 at midnight UTC", {
  dt <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  result <- LakeMetabolizer:::date2doy(dt)
  expect_equal(result, 1.0)
})

test_that("date2doy handles vector input", {
  dts <- as.POSIXct(c("2019-01-01 00:00:00", "2019-01-01 12:00:00"), tz = "UTC")
  result <- LakeMetabolizer:::date2doy(dts)
  expect_length(result, 2)
  expect_equal(result, c(1.0, 1.5))
})

test_that("date2doy Dec 31 returns ~365 for non-leap year", {
  dt <- as.POSIXct("2019-12-31 00:00:00", tz = "UTC")
  result <- LakeMetabolizer:::date2doy(dt)
  expect_equal(result, 365)
})

## ---- calc.freq ----

test_that("calc.freq returns 144 for 10-minute interval data", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  dt <- start + seq(0, 143 * 600, by = 600)  # 144 obs, 10-min apart
  result <- LakeMetabolizer:::calc.freq(dt)
  expect_equal(result, 144)
})

test_that("calc.freq returns 48 for 30-minute interval data", {
  start <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  dt <- start + seq(0, 47 * 1800, by = 1800)  # 48 obs, 30-min apart
  result <- LakeMetabolizer:::calc.freq(dt)
  expect_equal(result, 48)
})
