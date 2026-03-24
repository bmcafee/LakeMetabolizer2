test_that("sun.rise.set returns correct class and column names", {
  dates <- as.POSIXlt(
    c("2020-08-22", "2020-08-23", "2020-08-24"),
    tz = "America/Chicago")
  r <- sun.rise.set(dates, lat = 41.8781)

  expect_s3_class(r, "data.frame")
  expect_equal(colnames(r)[1], "sunrise")
  expect_equal(colnames(r)[2], "sunset")
  expect_equal(attr(r$sunrise, "tzone"), "America/Chicago")
})

test_that("sun.rise.set sunrise is before sunset at mid-latitudes in summer", {
  date <- as.POSIXct("2020-07-15 00:00:00", tz = "UTC")
  r <- sun.rise.set(date, lat = 45)
  expect_true(r$sunrise < r$sunset)
})

test_that("sun.rise.set returns NA at high latitude polar day", {
  # At lat=89 in summer, sun does not set: omegaInput < -1 → NA
  summer_date <- as.POSIXct("2020-06-21", tz = "UTC")
  r <- sun.rise.set(summer_date, lat = 89)
  expect_true(is.na(r$sunrise))
  expect_true(is.na(r$sunset))
})

test_that("is.day and is.night are complementary boolean vectors", {
  datetimes <- as.POSIXct(
    c("2020-07-15 06:00:00", "2020-07-15 14:00:00", "2020-07-15 22:00:00"),
    tz = "UTC")
  lat <- 45
  day_vals   <- is.day(datetimes, lat)
  night_vals <- is.night(datetimes, lat)

  expect_type(day_vals, "logical")
  expect_type(night_vals, "logical")
  expect_length(day_vals, 3)
  expect_equal(day_vals, !night_vals)
})

test_that("is.day returns TRUE at midday and FALSE at midnight", {
  noon     <- as.POSIXct("2020-07-15 12:00:00", tz = "UTC")
  midnight <- as.POSIXct("2020-07-15 00:00:00", tz = "UTC")
  lat <- 45
  expect_true(is.day(noon, lat))
  expect_false(is.day(midnight, lat))
})
