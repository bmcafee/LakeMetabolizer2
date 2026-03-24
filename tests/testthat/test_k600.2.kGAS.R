test_that("k600.2.kGAS.base formula: kGAS = k600 * (Sc/600)^-0.5", {
  k600 <- 2.0
  temp  <- 20
  Sc    <- getSchmidt(temp, "O2")
  expected <- k600 * (Sc / 600)^(-0.5)
  result   <- k600.2.kGAS.base(k600, temp, "O2")
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("k600.2.kGAS.base: kGAS > k600 at warm water where Sc(O2) < 600", {
  # At 20°C, Sc(O2) ≈ 531 < 600, so (Sc/600)^-0.5 > 1
  k600 <- 2.0
  kGAS <- k600.2.kGAS.base(k600, 20, "O2")
  expect_true(kGAS > k600)
})

test_that("k600.2.kGAS.base: kGAS ≈ k600 at temperature where Sc(O2) ≈ 600", {
  # Sc(O2) ≈ 600 when temp ≈ 17.55°C
  k600 <- 2.0
  temp_approx <- 17.55
  Sc <- getSchmidt(temp_approx, "O2")
  kGAS <- k600.2.kGAS.base(k600, temp_approx, "O2")
  # kGAS should be close to k600 (within ~1%)
  expect_equal(kGAS, k600, tolerance = 0.01)
})

test_that("k600.2.kGAS.base: larger gas molecules (higher Sc) yield smaller kGAS", {
  # CO2 has higher Sc than O2 at same temperature, so kGAS should be smaller
  k600 <- 2.0
  kO2  <- k600.2.kGAS.base(k600, 20, "O2")
  kCO2 <- k600.2.kGAS.base(k600, 20, "CO2")
  expect_true(kCO2 < kO2)
})

test_that("k600.2.kGAS.base handles vector inputs", {
  k600  <- c(1, 2, 3)
  temps <- c(10, 15, 20)
  result <- k600.2.kGAS.base(k600, temps, "O2")
  expected <- k600 * (getSchmidt(temps, "O2") / 600)^(-0.5)
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("k600.2.kGAS wrapper returns data.frame with k.gas column", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-01-01 00:00", "2020-01-01 01:00"), tz = "UTC"),
    k600 = c(1.0, 2.0),
    wtr  = c(15, 20))
  result <- k600.2.kGAS(ts, gas = "O2")
  expect_s3_class(result, "data.frame")
  expect_true("k.gas" %in% names(result))
  expect_true("datetime" %in% names(result))
  expect_equal(result$k.gas,
               k600.2.kGAS.base(ts$k600, ts$wtr, "O2"),
               tolerance = 1e-10)
})
