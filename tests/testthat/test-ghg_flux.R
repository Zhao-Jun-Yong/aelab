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
  expect_equal(result, 44010, tolerance = 1)  # 1 mmol co2 = 44.01g/mol * 1000 µg/mg * 1000 = 44010 µg
})

test_that("convert_ghg_unit throws error for invalid GHG", {
  expect_error(convert_ghg_unit(1, "invalid_gas"), "Invalid ghg")
})

test_that("g_factor is NA for explicit linear fit_type", {
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
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "linear")
  expect_true(is.na(result$g_factor))
})

test_that("g_factor is NA when auto mode selects linear (no upgrade)", {
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
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "auto", window_type = "fixed",
                                 duration_minutes = 7)
  expect_equal(result$fit_used, "linear")
  expect_true(is.na(result$g_factor))
})

test_that("g_factor is computed when auto mode selects quadratic", {
  elapsed <- seq(0, 419)
  # Strong quadratic: lm slope ~0.020, quadratic initial slope 0.01 → g_factor ~0.49
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
                                 fit_type = "auto", window_type = "fixed",
                                 duration_minutes = 7)
  expect_equal(result$fit_used, "quadratic")
  expect_false(is.na(result$g_factor))
  # Quadratic initial slope (0.01) < lm slope (~0.02), so g_factor < 1
  expect_lt(result$g_factor, 1)
  expect_gt(result$g_factor, 0)
})

test_that("g_factor_threshold overrides ok to discard when ratio exceeds limit", {
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.01 * elapsed + 0.00005 * elapsed^2
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result_no_gf <- calculate_regression(data, ref, "CH4",
                                       reference_time = "date_time",
                                       site = "site", analyzer_code = "TEST",
                                       fit_type = "auto", window_type = "fixed",
                                       duration_minutes = 7)
  # g_factor ~0.32; threshold 0.2 triggers discard, threshold 0.5 does not
  result_triggered <- calculate_regression(data, ref, "CH4",
                                           reference_time = "date_time",
                                           site = "site", analyzer_code = "TEST",
                                           fit_type = "auto", window_type = "fixed",
                                           duration_minutes = 7,
                                           g_factor_threshold = 0.2)
  result_not_triggered <- calculate_regression(data, ref, "CH4",
                                               reference_time = "date_time",
                                               site = "site", analyzer_code = "TEST",
                                               fit_type = "auto", window_type = "fixed",
                                               duration_minutes = 7,
                                               g_factor_threshold = 0.5)
  expect_equal(result_no_gf$flag, "ok")
  expect_equal(result_triggered$flag, "discard")    # g_factor ~0.32, threshold 0.2 triggers
  expect_equal(result_not_triggered$flag, "ok")     # g_factor ~0.32, threshold 0.5 does not
})

test_that("per-row duration_minutes overrides global parameter", {
  elapsed <- seq(0, 599)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 10:00:00"),
                               by = "sec", length.out = 600),
    CH4 = 2 + 0.01 * elapsed
  )
  ref <- data.frame(
    date_time  = as.POSIXct("2024-01-01 10:00:00", tz = "Asia/Taipei"),
    site       = "S1",
    analyzer   = "TEST",
    duration_minutes = 1   # override: use 1 min, not the global 7
  )
  # With per-row duration=1 and start_offset_s=0: window is 0-60s
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "linear", window_type = "fixed",
                                 duration_minutes = 7)
  expect_equal(result$flag, "ok")
  expect_true(!is.na(result$end_time))
  # Window should end at 10:01:00, not 10:07:00
  expect_true(grepl("10:01:00", result$end_time))
})

test_that(".pareto_front_select picks the front member closest to the ideal corner", {
  # A: dominated on both axes by B and C.
  # B: best uncertainty width, worst fit.
  # C: slightly worse uncertainty than B, much better fit -> closer to (0,0) once normalized.
  uw <- c(A = 10, B = 2,   C = 2.5)
  nf <- c(A = 10, B = 3,   C = 1)
  idx <- .pareto_front_select(uw, nf)
  expect_equal(idx, 3)  # C selected
  expect_false(idx == 1)  # A (dominated) never selected
})

test_that(".pareto_front_select excludes a strictly dominated candidate", {
  uw <- c(dominated = 10, dominator = 2)
  nf <- c(dominated = 10, dominator = 1)
  idx <- .pareto_front_select(uw, nf)
  expect_equal(idx, 2)
})

test_that(".pareto_front_select handles a single candidate and constant axes", {
  expect_equal(.pareto_front_select(5, 5), 1)
  # all candidates identical on both axes -> normalization is all-zero, first index wins ties
  expect_equal(.pareto_front_select(c(1, 1, 1), c(2, 2, 2)), 1)
})

test_that(".pareto_winners_curse_correct leaves the interval unchanged when the winner dominates the front", {
  # Winner (idx 1) is strictly narrower AND strictly better-fitting than every other candidate,
  # so it dominates the whole grid on the Pareto axes - front size 1 - and the correction must
  # reproduce the pre-correction (uncorrected) interval exactly.
  z68 <- stats::qnorm(0.84)
  se  <- 0.0005
  slopes <- c(0.010, 0.011, 0.009, 0.02)
  candidates <- lapply(slopes, function(s) {
    list(slope = s, uncertainty_lo = s - z68 * se, uncertainty_hi = s + z68 * se)
  })
  uw <- c(1, 5, 5, 5)  # uncertainty width: winner is narrowest
  nf <- c(1, 5, 5, 5)  # negative fit score: winner also has the best fit
  sel_idx <- 1
  uncorrected_lo <- candidates[[sel_idx]]$uncertainty_lo
  uncorrected_hi <- candidates[[sel_idx]]$uncertainty_hi

  corrected <- .pareto_winners_curse_correct(candidates, sel_idx, uw, nf)

  expect_equal(corrected$lo, uncorrected_lo, tolerance = 1e-9)
  expect_equal(corrected$hi, uncorrected_hi, tolerance = 1e-9)
})

test_that(".pareto_winners_curse_correct widens the interval when the front has a genuine trade-off competitor", {
  # Winner (idx 1) has the narrower interval but candidate 2 has the better fit score - neither
  # dominates the other, so the front has 2 members and the correction must strictly widen the
  # winner's reported interval.
  z68 <- stats::qnorm(0.84)
  se  <- 0.0005
  slopes <- c(0.010, 0.011)
  candidates <- lapply(slopes, function(s) {
    list(slope = s, uncertainty_lo = s - z68 * se, uncertainty_hi = s + z68 * se)
  })
  uw <- c(1, 3)  # winner narrower...
  nf <- c(3, 1)  # ...but candidate 2 fits better - a real trade-off, not a domination
  sel_idx <- 1
  uncorrected_width <- candidates[[sel_idx]]$uncertainty_hi - candidates[[sel_idx]]$uncertainty_lo

  corrected <- .pareto_winners_curse_correct(candidates, sel_idx, uw, nf)
  corrected_width <- corrected$hi - corrected$lo

  expect_gt(corrected_width, uncorrected_width)
})

test_that("pareto window_type generates the expected deadband/cutoff candidate grid", {
  set.seed(11)
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.05 * elapsed + rnorm(420, 0, 0.02)  # tiny jitter avoids "perfect fit" noise
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time", site = "site",
                                 analyzer_code = "TEST",
                                 fit_type = "linear", window_type = "pareto",
                                 duration_minutes = 7,
                                 instrument_precision = 0.01, n_boot = 20)
  # deadband grid seq(30,50,10) = 3 values (min_deadband default is 30);
  # cutoff grid seq(90,419,10) = 33 values (cutoff_lo = min_deadband + min_window_size = 90);
  # candidates require cutoff - deadband >= 60 (min_window_size):
  # deadband=30 -> cutoff>=90: 33; deadband=40 -> cutoff>=100: 32; deadband=50 -> cutoff>=110: 31
  # 33 + 32 + 31 = 96
  expect_equal(result$n_candidates, 96)
  expect_true(result$deadband %in% seq(30, 50, 10))
  expect_gte(result$cutoff - result$deadband, 60)
  expect_equal(result$flag, "ok")
  expect_true(all(c("slope_uncertainty_lo", "slope_uncertainty_hi") %in% names(result)))
  expect_lt(result$slope_uncertainty_lo, result$slope_uncertainty_hi)
})

test_that("pareto window_type falls back to discard when no candidate survives", {
  # Only 2 non-NA CH4 readings in the whole closure, far apart in time: every deadband/cutoff
  # candidate window can capture at most those 2 points, below the >= 4 points needed to fit
  # any model family, so every candidate is rejected and the admitted pool is empty.
  ch4 <- rep(NA_real_, 420)
  ch4[c(1, 401)] <- c(2, 6)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = ch4
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time", site = "site",
                                 analyzer_code = "TEST",
                                 fit_type = "linear", window_type = "pareto",
                                 duration_minutes = 7,
                                 instrument_precision = 0.1, n_boot = 15)
  expect_equal(result$flag, "discard")
  expect_equal(result$n_candidates, 0)
  expect_true(is.na(result$slope))
})

test_that("pareto window_type flags a genuinely flat closure as zero, not a confident number", {
  set.seed(2)
  # fit_type = "auto" always admits at least the linear fallback, so the pool is non-empty;
  # the selected window must still fail the existing correlation/min-detectable-slope check.
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + rnorm(420, mean = 0, sd = 0.001)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time", site = "site",
                                 analyzer_code = "TEST",
                                 fit_type = "auto", window_type = "pareto",
                                 duration_minutes = 7,
                                 instrument_precision = 0.1, n_boot = 15)
  expect_equal(result$flag, "zero")
})

test_that("pareto window_type requires instrument_precision to be resolvable", {
  # "CO" is not a gas in the built-in analyzer_precision table (only CH4/CO2/N2O), so
  # auto-resolution fails and instrument_precision stays NULL.
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CO = 2 + 0.01 * seq(0, 419)
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  expect_error(
    calculate_regression(data, ref, "CO",
                         reference_time = "date_time", site = "site",
                         analyzer_code = "TEST",
                         fit_type = "linear", window_type = "pareto"),
    "instrument_precision"
  )
})

test_that(".pareto_select_window bootstrap refits reuse the auto-resolved kmax, not the outer (NULL) kappamax", {
  # Regression test for the bug where fit_type = "auto" candidates had their bootstrap refits
  # routed through .fit_ghg_model()'s literal exp_tz/exp_zhao18 branches (fit_out$fit_used, a
  # resolved model name, not "auto") using the *outer* kappamax argument - which is always NULL
  # coming from calculate_regression()'s auto path - instead of the local kmax that "auto"
  # itself computed and gated the point-estimate fit on. With kappamax = NULL, .fit_ghg_model()'s
  # exp_tz/exp_zhao18 branches never reject a degenerate nls() fit (nls only warns, it doesn't
  # error), so a divergent bootstrap replicate could silently contaminate boot_slopes.
  #
  # .fit_ghg_model() is mocked so this test doesn't depend on nls() actually converging to
  # exp_tz for real data (the model family selected by "auto" is fragile to reproduce on demand);
  # the mock stands in for a real fit that resolved to exp_tz with kmax_used = 0.05, and the test
  # asserts every subsequent bootstrap refit call is gated by that same 0.05, not NULL.
  calls <- list()

  mock_fit <- function(conc, elapsed_s, fit_type, t_zero,
                        kappamax = NULL, instrument_precision = NULL) {
    calls[[length(calls) + 1]] <<- list(fit_type = fit_type, kappamax = kappamax)
    if (fit_type == "auto") {
      # Stand-in for a point-estimate fit that legitimately resolved to exp_tz, gated by a
      # locally-computed kmax of 0.05 (as .fit_ghg_model()'s real auto branch would compute
      # and return via kmax_used).
      list(slope = 0.01, r_square = 0.95, b_param = 0.02, fit_used = "exp_tz",
           residuals = rep(0, length(conc)), slope_lm = 0.009, kmax_used = 0.05)
    } else {
      # Bootstrap refit call: fit_out$fit_used ("exp_tz") is passed as the literal fit_type.
      list(slope = 0.01, r_square = 0.95, b_param = 0.02, fit_used = fit_type,
           residuals = rep(0, length(conc)), kmax_used = NA_real_)
    }
  }
  testthat::local_mocked_bindings(.fit_ghg_model = mock_fit, .package = "aelab")

  sorted_data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                               by = "sec", length.out = 100),
    CH4 = seq(1, 100)
  )
  reference_datetime <- sorted_data$real_datetime[1]

  # Grid collapsed to a single candidate window (deadband = 0, cutoff = 50) so every logged
  # exp_tz call is unambiguously a bootstrap refit of that one candidate.
  result <- .pareto_select_window(
    sorted_data, "CH4", reference_datetime,
    fit_type = "auto", t_zero = 0, kappamax = NULL, instrument_precision = 0.05,
    min_deadband = 0, max_deadband = 10, deadband_step = 10,
    min_window_size = 50, cutoff_step = 1000, n_boot = 15)

  expect_equal(result$status, "selected")

  auto_calls <- Filter(function(x) x$fit_type == "auto", calls)
  boot_calls <- Filter(function(x) x$fit_type == "exp_tz", calls)

  expect_equal(length(auto_calls), 1)      # one point-estimate fit for the one candidate
  expect_equal(length(boot_calls), 15)     # one bootstrap refit per n_boot replicate

  boot_kappamax <- vapply(boot_calls, function(x) x$kappamax, numeric(1))
  expect_true(all(!is.na(boot_kappamax)))                 # not NULL/NA - the bug's exact symptom
  expect_true(all(boot_kappamax == 0.05))                 # reuses auto's own kmax_used, unchanged
})

test_that(".n_fit_params returns the correct parameter count per model family", {
  expect_equal(.n_fit_params("linear"), 2L)
  expect_equal(.n_fit_params("quadratic"), 3L)
  expect_equal(.n_fit_params("exp_tz"), 3L)
  expect_equal(.n_fit_params("exp_zhao18"), 4L)
})

test_that(".dof_corrected_residuals applies the sqrt(n / (n - p)) correction", {
  resid <- c(1, -1, 2, -2, 3)  # n = 5
  out   <- .dof_corrected_residuals(resid, 2)  # p = 2 (linear) -> factor sqrt(5 / 3)
  expect_equal(out, resid * sqrt(5 / 3))
})

test_that(".dof_corrected_residuals falls back to uncorrected (factor 1) residuals when n - p <= 0", {
  resid <- c(0.1, -0.2, 0.05)  # n = 3
  expect_equal(.dof_corrected_residuals(resid, 3), resid)  # df = 0
  expect_equal(.dof_corrected_residuals(resid, 4), resid)  # df < 0
})

test_that(".pareto_select_window bootstrap resamples from a small fixed residual pool, not fresh continuous noise", {
  # Spy on .fit_ghg_model: the first call for a single-candidate grid is the point-estimate fit,
  # every subsequent call is a bootstrap refit. Record the synthetic conc series each refit was
  # actually given, while still delegating to the real (unmocked) implementation so the candidate
  # is genuinely selected and its residual pool is the real one.
  real_fit_ghg_model <- .fit_ghg_model
  captured_conc <- list()
  call_n <- 0L
  spy_fit <- function(conc, elapsed_s, fit_type, t_zero, kappamax = NULL, instrument_precision = NULL) {
    call_n <<- call_n + 1L
    if (call_n > 1L) captured_conc[[length(captured_conc) + 1]] <<- conc
    real_fit_ghg_model(conc, elapsed_s, fit_type, t_zero, kappamax, instrument_precision)
  }
  testthat::local_mocked_bindings(.fit_ghg_model = spy_fit, .package = "aelab")

  sorted_data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                               by = "sec", length.out = 4),
    CH4 = c(1, 2.3, 2.7, 4.1)
  )
  reference_datetime <- sorted_data$real_datetime[1]

  # Grid collapsed to exactly one candidate (deadband = 0, cutoff = 3), n_pts = 4.
  result <- .pareto_select_window(
    sorted_data, "CH4", reference_datetime,
    fit_type = "linear", t_zero = 0, kappamax = NULL, instrument_precision = 0.05,
    min_deadband = 0, max_deadband = 10, deadband_step = 10,
    min_window_size = 3, cutoff_step = 1000, n_boot = 30)

  expect_equal(result$status, "selected")
  expect_equal(length(captured_conc), 30)  # one bootstrap refit per n_boot replicate

  point_fit        <- real_fit_ghg_model(sorted_data$CH4, c(0, 1, 2, 3), "linear", 0)
  fitted_vals      <- sorted_data$CH4 - point_fit$residuals
  corrected_resid  <- .dof_corrected_residuals(point_fit$residuals, .n_fit_params("linear"))

  # Every bootstrap replicate's synthetic series minus fitted_vals must equal one of the 4
  # corrected residuals actually in the pool - not a fresh continuously-distributed draw.
  all_diffs <- unlist(lapply(captured_conc, function(cc) cc - fitted_vals))
  expect_true(all(vapply(all_diffs, function(d) any(abs(d - corrected_resid) < 1e-8), logical(1))))

  # 30 replicates x 4 points = 120 draws from a pool of only 4 distinct values must repeat
  # heavily - fresh Gaussian noise would essentially never produce exact repeats.
  expect_lte(length(unique(round(all_diffs, 8))), 4)
})

test_that(".pareto_select_window candidate-admission floor is 4 points, not 3", {
  # Exactly 3 valid points: below the >= 4 admission floor, must be rejected before any fit is
  # attempted at all (status "no_candidates", n_candidates 0) - not merely admitted and then
  # failing later. Regression guard for the 3 -> 4 floor raise (a 3-point window paired with a
  # 3-parameter model family would otherwise have 0 residual degrees of freedom - a perfect fit
  # by construction, not by evidence).
  sorted_data_3 <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                               by = "sec", length.out = 3),
    CH4 = c(1, 2, 3)
  )
  result_3 <- .pareto_select_window(
    sorted_data_3, "CH4", sorted_data_3$real_datetime[1],
    fit_type = "linear", t_zero = 0, kappamax = NULL, instrument_precision = 0.05,
    min_deadband = 0, max_deadband = 10, deadband_step = 10,
    min_window_size = 2, cutoff_step = 1000, n_boot = 15)
  expect_equal(result_3$status, "no_candidates")
  expect_equal(result_3$n_candidates, 0)

  # Exactly 4 valid points on an otherwise identical grid (min_window_size lowered by 1 so the
  # single candidate window still captures all 4 points): must clear the floor and be admitted.
  sorted_data_4 <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 00:00:00", tz = "UTC"),
                               by = "sec", length.out = 4),
    CH4 = c(1, 2.1, 2.9, 4.05)
  )
  result_4 <- .pareto_select_window(
    sorted_data_4, "CH4", sorted_data_4$real_datetime[1],
    fit_type = "linear", t_zero = 0, kappamax = NULL, instrument_precision = 0.05,
    min_deadband = 0, max_deadband = 10, deadband_step = 10,
    min_window_size = 3, cutoff_step = 1000, n_boot = 15)
  expect_equal(result_4$status, "selected")
  expect_equal(result_4$n_candidates, 1)
})

test_that("per-row start_offset_s overrides global parameter", {
  elapsed <- seq(0, 599)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-01-01 10:00:00"),
                               by = "sec", length.out = 600),
    CH4 = 2 + 0.01 * elapsed
  )
  ref <- data.frame(
    date_time    = as.POSIXct("2024-01-01 10:00:00", tz = "Asia/Taipei"),
    site         = "S1",
    analyzer     = "TEST",
    start_offset_s = 60   # override: start at 60s, not global 0
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "linear", window_type = "fixed",
                                 duration_minutes = 1, start_offset_s = 0)
  expect_equal(result$flag, "ok")
  # Window should start at 10:01:00, not 10:00:00
  expect_true(grepl("10:01:00", result$start_time))
})

# ---------------------------------------------------------------------------------------------
# Synthetic ground-truth recovery / coverage check for the pareto bootstrap interval
# ---------------------------------------------------------------------------------------------
# `slope_uncertainty_lo`/`slope_uncertainty_hi` is reported as a 68% interval built from a
# degrees-of-freedom-corrected residual bootstrap (see `.pareto_select_window()`). Every other
# pareto test in this file checks *point* behaviour (which candidate gets picked, grid size,
# flag logic) against real or synthetic data with an unknown true slope. None of them check
# whether the *interval* actually contains the truth at anywhere near its claimed rate - that
# requires a known ground truth, which real field data never has. This is the same style of
# check the two papers already cited in this feature's `@references` (Hueppi et al. 2018;
# Gaudard et al. 2025's `fluxible`) use to justify their own curvature/uncertainty safeguards.
#
# What this test can and cannot show:
#  - It only checks calibration under an idealised data-generating process (i.i.d. Gaussian
#    noise around a truly linear trend, fit with the matching `fit_type = "linear"` model). It
#    cannot detect miscalibration that depends on real-world noise structure this simulation
#    does not include (autocorrelated noise, ebullition spikes, model misspecification).
#  - A Monte Carlo coverage estimate from a finite number of repetitions has its own sampling
#    noise: even a perfectly calibrated 68% interval will show an observed rate that wanders
#    around 0.68 from run to run. With the pooled sample size used below, the binomial standard
#    error is about 0.027, so a single run reading e.g. 0.63 or 0.72 is expected noise, not
#    evidence of a bug. The gate below uses an exact (Clopper-Pearson) binomial test against
#    p = 0.68 at a stringent 1% significance level specifically so that a genuinely well-
#    calibrated bootstrap only fails this test by chance about 1 run in 100 - see the helper
#    `.assert_binom_coverage()` for the exact logic. A single failing run is suggestive, not
#    proof, of a real problem; a run that fails repeatedly is.
#
# Cost & gating: each repetition reruns the full pareto grid search plus `n_boot` bootstrap
# refits per candidate, three signal regimes x 100 repetitions each. This is too slow for
# routine `devtools::test()`, so it is off by default. Run on demand with:
#   AELAB_RUN_SLOW_TESTS=true Rscript -e 'devtools::test()'

# Small helper so the pass/fail logic itself (rather than the simulation plumbing) is
# unit-testable and easy to reason about in isolation.
.assert_binom_coverage <- function(n_covered, n_total, p = 0.68, alpha = 0.01) {
  stats::binom.test(n_covered, n_total, p = p, alternative = "two.sided")
}

test_that(".assert_binom_coverage rejects badly miscalibrated coverage and accepts nominal coverage", {
  # 40/300 = 0.133 observed vs nominal 0.68 -> obviously, overwhelmingly overconfident.
  bad <- .assert_binom_coverage(40, 300)
  expect_lt(bad$p.value, 0.01)
  # 204/300 = 0.68 observed exactly matches nominal -> cannot reject.
  good <- .assert_binom_coverage(204, 300)
  expect_gt(good$p.value, 0.01)
})

test_that("pareto residual-bootstrap 68% interval achieves nominal coverage on synthetic ground truth", {
  skip_if_not(identical(Sys.getenv("AELAB_RUN_SLOW_TESTS"), "true"),
              "Monte Carlo coverage check skipped by default (slow); set AELAB_RUN_SLOW_TESTS=true to run")
  skip_on_cran()

  n_rep_per_regime <- 100
  n_boot_sim       <- 30   # << production default (200); keeps per-repetition cost modest
  noise_sd         <- 0.02 # stand-in instrument precision (ppm), also passed as instrument_precision
  n_seconds        <- 300  # 5-minute closure @ 1 Hz - representative duration, real defaults
                            # (min_window_size = 60, max_deadband = 60) apply unchanged
  cutoff_step_sim  <- 40   # coarser than the production default (10) - fewer candidate windows
                            # per repetition, so 300 repetitions finish in reasonable time

  # Three regimes spanning strong / weak / near-detection-limit signal, chosen relative to this
  # package's own minimum-detectable-slope threshold (instrument_precision / duration_s), which
  # for noise_sd = 0.02 over a ~200-270s admitted window is on the order of 1e-4 (ppm/s).
  true_slopes <- c(strong = 0.01, weak = 0.001, near_dl = 0.00015)

  run_one <- function(true_slope, seed) {
    set.seed(seed)
    elapsed <- seq(0, n_seconds - 1)
    conc    <- 2 + true_slope * elapsed + stats::rnorm(n_seconds, mean = 0, sd = noise_sd)
    data <- data.frame(
      real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                                 by = "sec", length.out = n_seconds),
      CH4 = conc
    )
    ref <- data.frame(
      date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
      site = "S1", analyzer = "TEST"
    )
    result <- calculate_regression(data, ref, "CH4",
                                   reference_time = "date_time", site = "site",
                                   analyzer_code = "TEST",
                                   fit_type = "linear", window_type = "pareto",
                                   duration_minutes = n_seconds / 60,
                                   instrument_precision = noise_sd,
                                   n_boot = n_boot_sim,
                                   cutoff_step = cutoff_step_sim)
    if (is.na(result$slope_uncertainty_lo) || is.na(result$slope_uncertainty_hi)) {
      return(NA)  # no candidate survived / bootstrap too unstable -> excluded from coverage,
                  # same as a real caller would have no interval to check
    }
    true_slope >= result$slope_uncertainty_lo && true_slope <= result$slope_uncertainty_hi
  }

  # Seeds offset per regime so the three regimes draw independent noise realizations, and are
  # reproducible run to run.
  covered <- lapply(seq_along(true_slopes), function(k) {
    slope <- true_slopes[k]
    vapply(seq_len(n_rep_per_regime), function(i) run_one(slope, seed = 10000 * k + i),
           logical(1))
  })
  names(covered) <- names(true_slopes)

  per_regime_coverage <- vapply(covered, function(x) mean(x, na.rm = TRUE), numeric(1))
  per_regime_n_valid  <- vapply(covered, function(x) sum(!is.na(x)), integer(1))

  # Aggregate (pooled) coverage is the actual pass/fail gate: the calibration claim is about the
  # bootstrap procedure in general, not about any one signal regime, and pooling all regimes
  # gives more Monte Carlo precision than any single regime alone.
  all_covered <- unlist(covered, use.names = FALSE)
  all_covered <- all_covered[!is.na(all_covered)]
  n_total   <- length(all_covered)
  n_covered <- sum(all_covered)

  test_result <- .assert_binom_coverage(n_covered, n_total)

  message(sprintf(
    "pareto bootstrap coverage: %d/%d = %.3f (nominal 0.68, binom.test p = %.4f)",
    n_covered, n_total, n_covered / n_total, test_result$p.value))
  message("per-regime coverage: ", paste(
    sprintf("%s = %.3f (n=%d valid/%d)", names(per_regime_coverage), per_regime_coverage,
           per_regime_n_valid, n_rep_per_regime),
    collapse = "; "))

  # Fails only on strong statistical evidence of miscalibration (either direction) at n = 300;
  # ordinary Monte Carlo noise around 0.68 is expected to pass. See file header comment above
  # for the reasoning behind the 1% significance threshold.
  expect_gt(test_result$p.value, 0.01)
})
