test_that("tidy_ghg_analyzer works correctly", {
  data <- tidy_ghg_analyzer(test_file("ch4.xlsx"), gas = "ch4")
  expect_true(!is.null(data))
  column_names <- c("DATE", "TIME", "CO2", "CH4", "date_time")
  expect_identical(column_names, names(data))
})

test_that("convert_time adjusts datetime correctly", {
  data <- data.frame(date_time = c("2024/12/16 12:00:00"))
  result <- convert_time(data, hr = 1, min = 30)

  # lubridate::ymd_hms() parses as UTC, so compare with UTC timezone
  expect_equal(result$real_datetime[1], as.POSIXct("2024-12-16 13:30:00", tz = "UTC"))
})

test_that("calculate_regression returns correct results", {
  # Prepare mock data
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = sample(1:100, 420, replace = TRUE)
  )

  reference_df <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1",
    analyzer = "TEST"
  )
  result <- calculate_regression(data, reference_df, "CH4",
                                 reference_time = "date_time",
                                 site = "site",
                                 analyzer_code = "TEST")

  expect_equal(nrow(result), 1)  # Expecting one result
  expect_true("slope" %in% colnames(result))  # Check if slope is calculated
  expect_true("site" %in% colnames(result))
  expect_true("analyzer" %in% colnames(result))
})

test_that("calculate_regression output includes flag and correlation columns", {
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.01 * seq(0, 419)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST")
  expect_true("flag" %in% names(result))
  expect_true("correlation" %in% names(result))
})

test_that("calculate_regression slope is in ppm/s (elapsed seconds x-axis)", {
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.01 * elapsed
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST")
  expect_equal(result$slope, 0.01, tolerance = 1e-6)
})

test_that("calculate_regression flags clean linear signal as ok", {
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.01 * elapsed
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST")
  expect_equal(result$flag, "ok")
  expect_gt(result$r_square, 0.99)
  expect_gt(result$correlation, 0.99)
})

test_that("calculate_regression flags flat noisy signal as zero", {
  set.seed(42)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = rnorm(420, mean = 5, sd = 0.1)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST")
  expect_equal(result$flag, "zero")
  expect_lt(abs(result$correlation), 0.5)
})

test_that("instrument_precision overrides ok to zero when slope is within noise", {
  elapsed <- seq(0, 419)
  # slope = 0.0001 ppm/s; min detectable = 0.1 ppm / 300 s ≈ 0.000333 ppm/s > 0.0001
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.0001 * elapsed
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result_no_prec <- calculate_regression(data, ref, "CH4",
                                         reference_time = "date_time",
                                         site = "site", analyzer_code = "TEST")
  result_with_prec <- calculate_regression(data, ref, "CH4",
                                           reference_time = "date_time",
                                           site = "site", analyzer_code = "TEST",
                                           instrument_precision = 0.1)
  expect_equal(result_no_prec$flag, "ok")
  expect_equal(result_with_prec$flag, "zero")
})

test_that("calculate_regression exp_tz fits exponential signal and returns positive slope", {
  elapsed <- seq(0, 419)
  # C(t) = 10 + (2 - 10)*exp(-0.01*t); saturating rise from 2 to 10
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 10 + (2 - 10) * exp(-0.01 * elapsed)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "exp_tz")
  expect_false(is.na(result$slope))
  expect_gt(result$slope, 0)
})

test_that("calculate_regression exp_zhao18 fits exponential signal and returns positive slope", {
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 10 + (2 - 10) * exp(-0.01 * elapsed)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "exp_zhao18")
  expect_false(is.na(result$slope))
  expect_gt(result$slope, 0)
})

test_that("calculate_regression returns no_data when window has insufficient rows", {
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 50),
    CH4 = seq(2, 2.05, length.out = 50)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST")
  expect_equal(result$flag, "no_data")
})

test_that("calculate_regression quadratic fit returns initial slope (linear coefficient at t=0)", {
  elapsed <- seq(0, 419)
  # C(t) = 2 + 0.01*t + 0.00005*t²; initial slope (dC/dt at t=0) = 0.01
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.01 * elapsed + 0.00005 * elapsed^2
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "quadratic")
  expect_equal(result$slope, 0.01, tolerance = 1e-4)
  expect_gt(result$r_square, 0.99)
})

test_that("calculate_regression kappamax falls back to linear when b exceeds threshold", {
  elapsed <- seq(0, 29)
  # Fast exponential (b=0.1, rapid saturation); NLS finds implausibly large b
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 30),
    CH4 = 10 + (2 - 10) * exp(-0.1 * elapsed)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result_exp  <- calculate_regression(data, ref, "CH4",
                                      reference_time = "date_time",
                                      site = "site", analyzer_code = "TEST",
                                      fit_type = "exp_tz", window_type = "fixed",
                                      duration_minutes = 1)
  result_kmax <- calculate_regression(data, ref, "CH4",
                                      reference_time = "date_time",
                                      site = "site", analyzer_code = "TEST",
                                      fit_type = "exp_tz", window_type = "fixed",
                                      duration_minutes = 1, kappamax = 0.05)
  expect_false(is.na(result_exp$b_param))
  expect_gt(result_exp$b_param, 0.05)           # b >> kappamax triggers fallback
  expect_false(isTRUE(all.equal(result_exp$slope, result_kmax$slope, tolerance = 0.001)))
})

test_that("calculate_ghg_flux uses 3600 s h-1 giving mmol m-2 h-1", {
  # slope = 1 ppm/s, V = 1 L, T = 25 C, A = 1 m²
  # expected flux = 1 * 1 * 3600 * 0.001 / (0.082057 * 298.15 * 1) ≈ 0.147 mmol m-2 h-1
  df <- data.frame(slope = 1, area = 1, volume = 1, temp = 25)
  result <- calculate_ghg_flux(df)
  expect_equal(result$flux, 3600 * 0.001 / (0.082057 * 298.15), tolerance = 0.001)
  expect_equal(result$unit, "mmol m-2 h-1")
})

test_that("convert_ghg_unit converts correctly", {
  result <- convert_ghg_unit(1, "co2", mass = "mmol")
  expect_equal(as.numeric(result$value), 44010, tolerance = 1)  # 1 mmol co2 = 44.01g/mol * 1000 µg/mg * 1000 = 44010 µg
})

test_that("convert_ghg_unit throws error for invalid GHG", {
  expect_error(convert_ghg_unit(1, "invalid_gas"), "Invalid GHG type")
})
