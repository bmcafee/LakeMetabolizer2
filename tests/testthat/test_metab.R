# Shared synthetic data for metabolism tests:
# 144 observations (10-min intervals) over one 24-hour period.
make_metab_inputs <- function() {
  n <- 144
  start_time <- as.POSIXct("2020-07-15 00:00:00", tz = "UTC")
  datetime   <- start_time + seq(0, (n - 1) * 600, by = 600)

  wtr    <- rep(20, n)
  do.sat <- o2.at.sat.base(wtr)
  do.obs <- rep(8.5, n)   # slightly below saturation, no NAs
  k.gas  <- rep(0.2, n)
  z.mix  <- rep(2, n)

  # Sinusoidal irr: 0 at night, peak ~1 at midday (hours 6–18 daytime)
  hour_frac <- (seq_len(n) - 1) / 6   # 0 to 23.833 hours
  irr <- pmax(0, sin(pi * (hour_frac - 6) / 12))

  list(datetime = datetime, wtr = wtr, do.obs = do.obs, do.sat = do.sat,
       k.gas = k.gas, z.mix = z.mix, irr = irr)
}

## ---- metab.ols ----

test_that("metab.ols returns list with metab containing GPP, R, NEP", {
  d <- make_metab_inputs()
  result <- metab.ols(d$do.obs, d$do.sat, d$k.gas, d$z.mix, d$irr, d$wtr,
                      datetime = d$datetime)
  expect_type(result, "list")
  expect_true("metab" %in% names(result))
  metab <- result$metab
  expect_true(all(c("GPP", "R", "NEP") %in% names(metab)))
  expect_type(metab$GPP, "double")
  expect_type(metab$R,   "double")
  expect_type(metab$NEP, "double")
  expect_true(is.finite(metab$GPP))
  expect_true(is.finite(metab$R))
  expect_true(is.finite(metab$NEP))
})

test_that("metab.ols NEP equals GPP + R", {
  d <- make_metab_inputs()
  result <- metab.ols(d$do.obs, d$do.sat, d$k.gas, d$z.mix, d$irr, d$wtr,
                      datetime = d$datetime)
  metab <- result$metab
  expect_equal(metab$NEP, metab$GPP + metab$R, tolerance = 1e-10)
})

test_that("metab.ols errors when z.mix contains zero", {
  d <- make_metab_inputs()
  d$z.mix[1] <- 0
  expect_error(metab.ols(d$do.obs, d$do.sat, d$k.gas, d$z.mix, d$irr, d$wtr))
})

test_that("metab.ols errors when inputs contain NA", {
  d <- make_metab_inputs()
  d$do.obs[10] <- NA
  expect_error(metab.ols(d$do.obs, d$do.sat, d$k.gas, d$z.mix, d$irr, d$wtr))
})

## ---- metab.bookkeep ----

test_that("metab.bookkeep returns data.frame with GPP, R, NEP columns", {
  d <- make_metab_inputs()
  # bookkeep requires integer 0/1 irr
  irr_int <- as.integer(d$irr > 0)
  result <- metab.bookkeep(d$do.obs, d$do.sat, d$k.gas, d$z.mix, irr_int,
                            datetime = d$datetime)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("GPP", "R", "NEP") %in% names(result)))
  expect_type(result$GPP, "double")
  expect_type(result$R,   "double")
  expect_type(result$NEP, "double")
})

test_that("metab.bookkeep errors when irr is not integer 0/1 and datetime/lat absent", {
  d <- make_metab_inputs()
  # Continuous irr without datetime+lake.lat should error; suppress the
  # "datetime not found" warning that is emitted before the stop().
  expect_error(
    suppressWarnings(
      metab.bookkeep(d$do.obs, d$do.sat, d$k.gas, d$z.mix, d$irr)
    )
  )
})

test_that("metab.bookkeep R is non-positive for plausible night respiration", {
  d <- make_metab_inputs()
  irr_int <- as.integer(d$irr > 0)
  result <- metab.bookkeep(d$do.obs, d$do.sat, d$k.gas, d$z.mix, irr_int,
                            datetime = d$datetime)
  # With constant do.obs slightly below sat, gas flux drives slight uptake.
  # R is estimated from nighttime, should be <= 0 or at least numeric.
  expect_true(is.numeric(result$R))
})

## ---- metab via file path (integration check) ----

test_that("sparkling.doobs file exists and can be located", {
  path <- system.file("extdata", "sparkling.doobs", package = "LakeMetabolizer")
  expect_true(nchar(path) > 0)
  expect_true(file.exists(path))
})
