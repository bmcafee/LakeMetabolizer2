test_that("o2.at.sat.base returns known value at 20C sea level freshwater (garcia-benson)", {
  # Garcia-Benson at 20°C, sea level, freshwater ≈ 9.08 mg/L
  result <- o2.at.sat.base(temp = 20, altitude = 0, salinity = 0,
                            model = "garcia-benson")
  expect_equal(result, 9.08, tolerance = 0.05)
})

test_that("o2.at.sat.base all four models return numeric values at 20C", {
  models <- c("garcia-benson", "garcia", "weiss", "benson")
  for (m in models) {
    result <- o2.at.sat.base(temp = 20, altitude = 0, salinity = 0, model = m)
    expect_type(result, "double")
    expect_true(is.finite(result))
  }
})

test_that("o2.at.sat.base saturation decreases with altitude", {
  sat_low  <- o2.at.sat.base(temp = 20, altitude = 0)
  sat_high <- o2.at.sat.base(temp = 20, altitude = 1000)
  expect_true(sat_high < sat_low)
})

test_that("o2.at.sat.base saturation decreases with salinity", {
  sat_fresh <- o2.at.sat.base(temp = 20, salinity = 0)
  sat_saline <- o2.at.sat.base(temp = 20, salinity = 35)
  expect_true(sat_saline < sat_fresh)
})

test_that("o2.at.sat.base saturation decreases with temperature", {
  sat_cold <- o2.at.sat.base(temp = 5)
  sat_warm <- o2.at.sat.base(temp = 25)
  expect_true(sat_warm < sat_cold)
})

test_that("o2.at.sat.base stops on unrecognized model", {
  expect_error(o2.at.sat.base(temp = 20, model = "unknown_model"))
})

test_that("o2.at.sat wrapper returns data.frame with do.sat column", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00:00", "2020-01-01 01:00:00"),
                           tz = "UTC"),
    wtr = c(10, 15))
  result <- o2.at.sat(ts)
  expect_s3_class(result, "data.frame")
  expect_true("do.sat" %in% names(result))
  expect_true("datetime" %in% names(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$do.sat, o2.at.sat.base(ts$wtr))
})

test_that("benson model warns when salinity is non-zero", {
  expect_warning(o2.at.sat.base(temp = 20, salinity = 10, model = "benson"),
                 regexp = "Benson model does not currently include salinity")
})
