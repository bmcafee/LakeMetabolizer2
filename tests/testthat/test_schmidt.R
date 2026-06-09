test_that("getSchmidt returns known value for O2 at 20C", {
  # Sc = A + B*T + C*T^2 + D*T^3; O2 coefficients: 1568, -86.04, 2.142, -0.0216
  expected <- 1568 + (-86.04) * 20 + 2.142 * 20^2 + (-0.0216) * 20^3
  result <- getSchmidt(20, "O2")
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("getSchmidt returns numeric for all supported gases", {
  gases <- c("He", "O2", "CO2", "CH4", "SF6", "N2O", "Ar", "N2")
  for (g in gases) {
    result <- getSchmidt(20, g)
    expect_type(result, "double")
    expect_true(is.finite(result))
  }
})

test_that("getSchmidt errors on unrecognized gas", {
  expect_error(getSchmidt(20, "Xe"))
})

test_that("getSchmidt warns when temperature is outside 4-35C range", {
  expect_warning(getSchmidt(2, "O2"),  regexp = "temperature out of range")
  expect_warning(getSchmidt(40, "O2"), regexp = "temperature out of range")
})

test_that("getSchmidt Schmidt number decreases as temperature increases (O2)", {
  sc_low  <- getSchmidt(5,  "O2")
  sc_mid  <- getSchmidt(15, "O2")
  sc_high <- getSchmidt(30, "O2")
  expect_true(sc_mid < sc_low)
  expect_true(sc_high < sc_mid)
})

test_that("getSchmidt larger molecules have higher Schmidt numbers than smaller ones at same T", {
  # SF6 (heavy) should have higher Sc than He (light) at same temperature
  sc_He  <- getSchmidt(20, "He")
  sc_SF6 <- getSchmidt(20, "SF6")
  expect_true(sc_SF6 > sc_He)
})

test_that("getSchmidt handles vector temperature input", {
  temps <- c(10, 20, 30)
  result <- getSchmidt(temps, "O2")
  expect_length(result, 3)
  expect_type(result, "double")
})

test_that("getSchmidt handles NA in temperature vector without error", {
  temps <- c(10, NA, 20)
  # Should warn if non-NA values are out of range, but not error
  result <- getSchmidt(temps, "O2")
  expect_length(result, 3)
  expect_true(is.na(result[2]))
})
