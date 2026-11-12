test_that("tidy_ghg_analyzer works correctly", {
  data <- tidy_ghg_analyzer(test_file("ch4.xlsx"), gas = "ch4")
  expect_true(!is.null(data))
  column_names <- c("DATE", "TIME", "H2O", "CO2", "CH4", "date_time", "CO2_unit", "CH4_unit", "H2O_unit", "CH4_prec", "CO2_prec")
  expect_identical(column_names, names(data))
  expect_equal(unique(data$CH4_unit), "ppb")
  expect_equal(unique(data$CO2_unit), "ppm")
  expect_no_message(tidy_ghg_analyzer(test_file("ch4.xlsx"), gas = "ch4"))
})

test_that("tidy_ghg_analyzer picks LI-COR columns by header", {
  raw <- openxlsx::read.xlsx(test_file("ch4.xlsx"), colNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  shifted <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(cbind(extra = NA, raw), shifted, colNames = FALSE)
  expect_equal(tidy_ghg_analyzer(shifted, "ch4"), tidy_ghg_analyzer(test_file("ch4.xlsx"), "ch4"))
  expect_error(tidy_ghg_analyzer(test_file("ch4.xlsx"), "n2o"), "no N2O column")
  # N2O layout (DATE, TIME, H2O, N2O): the CH4 fixture with CO2 and CH4 headers renamed
  n2o <- raw; n2o[6, n2o[6, ] %in% "CH4"] <- "N2O"; n2o[6, n2o[6, ] %in% "CO2"] <- "X"  # sheet row 6 is the header
  f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(n2o, f, colNames = FALSE)
  out <- tidy_ghg_analyzer(f, "n2o")
  expect_equal(out$N2O, tidy_ghg_analyzer(test_file("ch4.xlsx"), "ch4")$CH4[!is.na(out$N2O)][seq_along(out$N2O)])
  expect_true(all(c("DATE", "TIME", "H2O", "N2O", "N2O_unit", "N2O_prec") %in% names(out)))
})

test_that("slope_unit flows from tidy_ghg_analyzer() to calculate_ghg_flux()", {
  ch4 <- tidy_ghg_analyzer(test_file("ch4.xlsx"), gas = "ch4")
  ref <- data.frame(date_time = ch4$date_time[1], site = "x", analyzer = "a")
  r <- suppressWarnings(calculate_regression(ch4, ref, "CH4", reference_time = "date_time", site = "site",
                                             analyzer_code = "a", window_type = "fixed", duration_minutes = 2))
  expect_equal(r$slope_unit, "ppb s-1")
  d <- cbind(r, area = 1, volume = 1, temp = 25)
  f <- calculate_ghg_flux(d)  # no conc_unit needed
  expect_equal(f$flux, calculate_ghg_flux(transform(d, slope_unit = NA), conc_unit = "ppb")$flux)
  expect_equal(calculate_ghg_flux(d, conc_unit = "ppb")$flux, f$flux)
  expect_error(calculate_ghg_flux(d, conc_unit = "ppm"), "contradicts")
  expect_error(calculate_ghg_flux(transform(d, slope_unit = "mg s-1")), "Unknown slope_unit")
  # data without a unit column: slope_unit NA, conc_unit used
  r2 <- suppressWarnings(calculate_regression(ch4[c("date_time", "CH4")], ref, "CH4", reference_time = "date_time",
                                              site = "site", analyzer_code = "a", window_type = "fixed",
                                              duration_minutes = 2))
  expect_true(is.na(r2$slope_unit))
  r3 <- r2; r3$slope_unit <- NULL
  expect_equal(calculate_ghg_flux(cbind(r3, area = 1, volume = 1, temp = 25))$flux, f$flux * 1000)
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

test_quiet("calculate_regression output includes flag and correlation columns", {
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

test_quiet("calculate_regression slope is in ppm/s (elapsed seconds x-axis)", {
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

test_that("calculate_regression matches date_time-only data on a non-Taipei system", {
  old <- Sys.getenv("TZ", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old))
  Sys.setenv(TZ = "Europe/Berlin")
  set.seed(1)
  data <- data.frame(date_time = as.POSIXct("2024-01-01 08:00:00") + 0:299,
                     CH4 = 2000 + 0.5 * (0:299) + rnorm(300))
  ref <- data.frame(date_time = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"),
                    site = "x", analyzer = "a")
  result <- calculate_regression(data, ref, "CH4", reference_time = "date_time", site = "site",
                                 analyzer_code = "a", duration_minutes = 4)
  expect_equal(result$flag, "ok")
  expect_equal(result$slope, 0.5, tolerance = 0.01)
})

test_that("plot_ghg_flux draws fit_used, labels the unit and warns on no data", {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  e <- 0:299
  set.seed(1)
  d <- data.frame(real_datetime = t0 + e, CH4 = 2000 + 2 * e - 0.004 * e^2 + rnorm(300, 0, 0.5),
                  CH4_unit = "ppb")
  ref <- data.frame(date_time = t0, site = "x", analyzer = "a")
  r <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", fit_type = "quadratic",
                            duration_minutes = 299 / 60)
  p <- plot_ghg_flux(d, r, "CH4")  # default fit_type = "linear", but fit_used is quadratic
  ln <- ggplot2::layer_data(p, 2)
  expect_equal(ln$y, 2000 + 2 * ln$x - 0.004 * ln$x^2, tolerance = 1e-3)
  expect_equal(p$labels$y, "CH4 (ppb)")
  ra <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", fit_type = "auto", instrument_precision = 0.6,
                             duration_minutes = 299 / 60)
  expect_s3_class(plot_ghg_flux(d, ra, "CH4", fit_type = "auto"), "ggplot")
  expect_warning(expect_null(plot_ghg_flux(transform(d, real_datetime = real_datetime + 86400), r, "CH4")),
                 "no window")
})

test_quiet("calculate_regression flags clean linear signal as ok", {
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

test_quiet("instrument_precision marks below_mdf without overriding the flag", {
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
  expect_true(is.na(result_no_prec$below_mdf))
  expect_equal(result_with_prec$flag, "ok")
  expect_true(result_with_prec$below_mdf)
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

test_that("auto: a flat line gives slope 0 (kmax = 0), precision is required", {
  tt <- 0:299
  a <- .fit_ghg_model(rep(400, 300), tt, "auto", t_zero = 0, instrument_precision = 0.5)
  expect_equal(a$slope, 0)
  expect_equal(a$b_param, NA_real_)
  expect_error(.fit_ghg_model(400 + 0.01 * tt, tt, "auto", t_zero = 0), "instrument_precision")
})

test_that("exponential fits recover the initial slope A*b of noisy saturating curves", {
  # C = 400 + A(1 - exp(-b t)), 1 Hz, 300 s, noise sd 0.5: true slope at t = 0 is A * b.
  # The old fixed nls start (b = 0.001) returned 0.08 for a true slope of 1.
  tt <- 0:299
  set.seed(1)
  for (b in c(0.005, 0.01, 0.02)) {
    y <- 400 + 100 * (1 - exp(-b * tt)) + stats::rnorm(300, 0, 0.5)
    for (ft in c("exp_tz", "exp_zhao18")) {
      f <- expect_silent(.fit_ghg_model(y, tt, ft, t_zero = 0))
      expect_equal(f$slope, 100 * b, tolerance = 0.05, info = paste(ft, b))
    }
  }
  set.seed(2)
  y <- 400 + 100 * (1 - exp(-0.01 * tt)) + stats::rnorm(300, 0, 0.5)
  a <- .fit_ghg_model(y, tt, "auto", t_zero = 0, instrument_precision = 0.5)
  expect_identical(a$fit_used, "exp_tz")
  expect_equal(a$slope, 1, tolerance = 0.05)
})

test_that("explicit exponential fits return NA without curvature; auto falls back", {
  tt <- 0:299
  # exact line: the residual sum of squares does not depend on b
  for (ft in c("exp_tz", "exp_zhao18")) {
    f <- expect_silent(.fit_ghg_model(400 + 0.05 * tt, tt, ft, t_zero = 0))
    expect_true(is.na(f$slope))
    expect_true(is.na(f$r_square))
  }
  # noisy line: an explicit exp fit may still return a slope; auto keeps the simpler model
  set.seed(3)
  y <- 400 + 0.05 * tt + stats::rnorm(300, 0, 0.5)
  a <- .fit_ghg_model(y, tt, "auto", t_zero = 0, instrument_precision = 0.5)
  expect_false(a$fit_used %in% c("exp_tz", "exp_zhao18"))
  expect_equal(a$slope, 0.05, tolerance = 0.05)
})

test_that("exp_tz fits C0, so a disturbed first reading does not move the slope", {
  tt <- 0:299
  set.seed(5)
  y <- 400 + 100 * (1 - exp(-0.01 * tt)) + stats::rnorm(300, 0, 3)
  y_spike <- replace(y, 1, y[1] + 15)
  s  <- .fit_ghg_model(y, tt, "exp_tz", t_zero = 0)$slope
  s2 <- .fit_ghg_model(y_spike, tt, "exp_tz", t_zero = 0)$slope
  # with C0 = first reading the spike gave about -40 % (simulation, 200 runs)
  expect_equal(s, 1, tolerance = 0.05)
  expect_equal(s2, 1, tolerance = 0.05)
})

test_that("exp_zhao18 takes C0 from a 15 s linear fit, not the first reading", {
  tt <- 0:299
  set.seed(5)
  y <- 400 + 100 * (1 - exp(-0.01 * tt)) + stats::rnorm(300, 0, 3)
  y_spike <- replace(y, 1, y[1] + 15)
  expect_equal(.exp_curve_fit(y, tt, zhao = TRUE)$C0,
               unname(stats::coef(stats::lm(y[1:16] ~ tt[1:16]))[1]))
  expect_equal(.fit_ghg_model(y, tt, "exp_zhao18", t_zero = 0)$slope, 1, tolerance = 0.05)
  expect_equal(.fit_ghg_model(y_spike, tt, "exp_zhao18", t_zero = 0)$slope, 1, tolerance = 0.1)
})

test_that("auto does not pick exp_tz for an accelerating curve", {
  # A convex exponential (Cm - C0 opposite to the slope) can fit an accelerating rise and read low
  tt <- 0:299
  set.seed(4)
  y <- 400 + 0.05 * tt + 2e-4 * tt^2 + stats::rnorm(300, 0, 0.5)
  expect_lt(.fit_ghg_model(y, tt, "exp_tz", t_zero = 0)$amp, 0)
  a <- .fit_ghg_model(y, tt, "auto", t_zero = 0, instrument_precision = 0.5)
  expect_identical(a$fit_used, "linear")
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
  expect_warning(result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST", window_type = "sliding"),
                 "Fewer readings than num_rows")
  expect_equal(result$flag, "no_data")
})

test_quiet("calculate_regression quadratic fit returns initial slope (linear coefficient at t=0)", {
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
                                      duration_minutes = 1, min_obs = 20)
  result_kmax <- calculate_regression(data, ref, "CH4",
                                      reference_time = "date_time",
                                      site = "site", analyzer_code = "TEST",
                                      fit_type = "exp_tz", window_type = "fixed",
                                      duration_minutes = 1, kappamax = 0.05, min_obs = 20)
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

test_that("calculate_ghg_flux conc_unit = 'ppb' scales slopes by 1/1000", {
  ppm <- data.frame(slope = 1, slope_diffusion = 0.5, slope_ebullition = 0.5,
                    area = 1, volume = 1, temp = 25)
  ppb <- transform(ppm, slope = 1000, slope_diffusion = 500, slope_ebullition = 500)
  cols <- c("flux", "flux_diffusion", "flux_ebullition")
  expect_equal(calculate_ghg_flux(ppb, conc_unit = "ppb")[cols], calculate_ghg_flux(ppm)[cols])
  expect_error(calculate_ghg_flux(ppm, conc_unit = "ppt"))
})

test_that("bubble detection works in seconds: 5 s sampling and a 60 s gap", {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  split_at <- function(sec, ch4) {
    calculate_regression(data.frame(real_datetime = t0 + sec, CH4 = ch4),
                         data.frame(date_time = t0, site = "x", analyzer = "a"), "CH4",
                         reference_time = "date_time", site = "site", analyzer_code = "a",
                         window_type = "fixed", duration_minutes = 299 / 60, bubble_detection = TRUE)
  }
  set.seed(1)
  sec <- 0:299
  line <- 2000 + 1 * sec + rnorm(300, 0, 0.6)  # clean 1 ppb/s, no bubble (Fable review V8)
  expect_equal(split_at(sec, line)$ch4_flag, "no bubble")
  gap <- sec < 100 | sec >= 160
  expect_equal(split_at(sec[gap], line[gap])$ch4_flag, "no bubble")
  five <- sec %% 5 == 0
  expect_equal(split_at(sec[five], 2000 + 3 * sec[five] + rnorm(60, 0, 0.6))$ch4_flag, "no bubble")
  # a 500 ppb step at 100 s is still found at 5 s sampling (a 50 ppb one becomes a 5 s ramp below
  # the 1e-4 variance minimum)
  step <- 2000 + 0.1 * sec + ifelse(sec >= 100, 500, 0) + rnorm(300, 0, 0.3)
  s5 <- split_at(sec[five], step[five])
  expect_equal(s5$n_bubbles, 1L)
  expect_equal(s5$slope_diffusion, 0.1, tolerance = 0.3)
})

# CH4 split in calculate_regression(bubble_detection = TRUE): fixed 299 s window from t0 + off
ch4_split <- function(ch4, off = 0, ...) {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  d <- data.frame(real_datetime = t0 + seq_along(ch4) - 1, CH4 = ch4)
  ref <- data.frame(date_time = t0 - off, site = "x", analyzer = "a")
  calculate_regression(d, ref, "CH4", reference_time = "date_time", site = "site", analyzer_code = "a",
                       window_type = "fixed", start_offset_s = off, duration_minutes = 299 / 60,
                       bubble_detection = TRUE, ch4_options = list(...))
}

test_that("calculate_regression bubble_detection = TRUE splits synthetic CH4 traces", {
  sec <- 0:299
  trace <- function(step_at) 2 + 1e-4 * sec + ifelse(!is.na(step_at) & sec >= step_at, 0.05, 0) + rnorm(300, 0, 3e-4)
  set.seed(1); ramp <- ch4_split(trace(NA))
  expect_equal(ramp$ch4_flag, "no bubble")
  expect_equal(ramp$slope_total, ramp$slope)
  expect_equal(ramp$slope_diffusion, ramp$slope)
  expect_equal(ramp$slope, ramp$slope_regression)
  expect_equal(ramp$slope, 1e-4, tolerance = 0.05)
  expect_equal(ramp$slope_ebullition, 0)
  expect_equal(ramp$slope_source, "regression")

  set.seed(1); invisible(trace(NA)); step <- ch4_split(trace(100))
  expect_equal(step$ch4_flag, "ok")
  expect_equal(step$slope, step$slope_total)
  expect_equal(step$slope_source, "total")
  expect_equal(step$flag, "ok")
  expect_gt(step$r_square, 0.9)  # the diffusion fit, not the regression across the step
  expect_false(isTRUE(all.equal(step$slope, step$slope_regression)))
  expect_equal(step$slope_diffusion, 1e-4, tolerance = 0.1)
  expect_equal(step$slope_total, 1e-4 + 0.05 / 299, tolerance = 0.05)
  expect_equal(step$slope_ebullition, step$slope_total - step$slope_diffusion)
  expect_equal(ramp$n_obs_diffusion, ramp$n_obs)
  expect_equal(step$n_obs, 300)
  in_seg <- sec >= as.numeric(as.POSIXct(step$diffusion_start, tz = "Asia/Taipei") - as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"), units = "secs") &
    sec <= as.numeric(as.POSIXct(step$diffusion_end, tz = "Asia/Taipei") - as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"), units = "secs")
  expect_lte(step$n_obs_diffusion, sum(in_seg))
  expect_gte(step$n_obs_diffusion, sum(in_seg) - 1)  # trim_to_min may drop the lowest reading
  fl <- calculate_ghg_flux(cbind(step, area = 1, volume = 1, temp = 25))
  expect_equal(fl$flux, fl$flux_diffusion + fl$flux_ebullition)

  early <- ch4_split(trace(10))
  expect_equal(early$ch4_flag, "early bubble: total only")
  expect_true(is.na(early$flag) && is.na(early$r_square) && is.na(early$correlation))
  expect_false(is.na(early$flag_regression))
  expect_equal(early$slope, early$slope_total)
  expect_true(is.na(early$slope_diffusion) && is.na(early$slope_ebullition))

  # the default leaves calculate_regression unchanged
  expect_false("slope_source" %in% names(calculate_regression(
    data.frame(real_datetime = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei") + sec, CH4 = trace(NA)),
    data.frame(date_time = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"), site = "x", analyzer = "a"),
    "CH4", "date_time", "site", "a", duration_minutes = 299 / 60)))
  plain <- calculate_regression(data.frame(real_datetime = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei") + sec,
                                           CH4 = trace(100)),
                                data.frame(date_time = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"), site = "x", analyzer = "a"),
                                "CH4", reference_time = "date_time", site = "site", analyzer_code = "a",
                                window_type = "fixed", duration_minutes = 299 / 60)
  expect_false(any(c("slope_total", "ch4_flag") %in% names(plain)))
  expect_false("flux_diffusion" %in% names(calculate_ghg_flux(cbind(plain, area = 1, volume = 1, temp = 25))))
})

test_that("ch4_options change the early-bubble origin and negative ebullition", {
  set.seed(2)
  sec <- 0:299
  ch4 <- 2 + 1e-4 * sec + ifelse(sec >= 10, 0.05, 0) + rnorm(300, 0, 3e-4)
  # window starts 30 s after placement: a bubble 10 s into the window is 40 s after placement
  win <- ch4_split(ch4, off = 30)
  plc <- ch4_split(ch4, off = 30, early_from = "placement")
  expect_equal(win$ch4_flag, "early bubble: total only")
  expect_false(is.na(plc$slope_diffusion))

  # fast rise that levels off at 60 s, bubble at 100 s: the early diffusion slope > total
  cd2 <- 2 + 1e-3 * pmin(sec, 60) + ifelse(sec >= 100, 0.05, 0) + rnorm(300, 0, 3e-4)
  r0 <- ch4_split(cd2)
  rk <- ch4_split(cd2, neg_ebullition = "keep")
  expect_equal(r0$ch4_flag, "diffusion > total flux")
  expect_equal(r0$slope_ebullition, 0)
  expect_lt(rk$slope_ebullition, 0)

  # trace falls for 40 s after placement, then rises 1e-4 ppm/s, bubble at 150 s
  fall <- 2 + ifelse(sec < 40, -2e-4 * sec, -8e-3 + 1e-4 * (sec - 40)) +
    ifelse(sec >= 150, 0.05, 0) + rnorm(300, 0, 1e-4)
  tr <- ch4_split(fall, trim_to_min = TRUE)
  expect_equal(tr$slope_diffusion, 1e-4, tolerance = 0.1)

  expect_error(ch4_split(ch4, sparr = 1), "Unknown ch4_options")
})

test_that("ch4_options min_rise, bubble_pad and min_diffusion_s", {
  sec <- 0:299
  trace <- function(step_at) { set.seed(3); 2 + 1e-4 * sec + ifelse(sec >= step_at, 0.05, 0) + rnorm(300, 0, 3e-4) }
  base <- ch4_split(trace(100))
  expect_equal(ch4_split(trace(100), min_rise = 0, bubble_pad = 0, min_diffusion_s = 20), base)

  # a 0.05 ppm step is a bubble above min_rise 0.01, not above 0.1
  kept <- ch4_split(trace(100), min_rise = 0.01)
  gone <- ch4_split(trace(100), min_rise = 0.1)
  expect_equal(kept$slope_diffusion, base$slope_diffusion)
  expect_equal(gone$ch4_flag, "no bubble: rises <= min_rise")
  expect_equal(gone$slope_ebullition, 0)
  expect_equal(gone$slope, gone$slope_regression)
  expect_equal(gone$slope_total, gone$slope_diffusion)

  # padding over the whole window leaves no unflagged readings for the diffusion
  padded <- ch4_split(trace(100), bubble_pad = 300)
  expect_equal(padded$ch4_flag, "no linear diffusion segment")
  expect_true(is.na(padded$slope_diffusion))

  # bubble at 40 s: the run before it is long enough for 20 s, not for 60 s
  short <- ch4_split(trace(40))
  long <- ch4_split(trace(40), min_diffusion_s = 60)
  expect_false(grepl("diffusion after first bubble", short$ch4_flag))
  expect_match(long$ch4_flag, "diffusion after first bubble")
  expect_equal(long$slope_diffusion, 1e-4, tolerance = 0.1)

  expect_true(long$review_needed)
  expect_false(short$review_needed)

  # CH4 uptake before a bubble: negative diffusion kept (trim_to_min would trim the falling start)
  set.seed(2)
  up <- 2.5 - 2e-4 * (0:299) + rnorm(300, 0, 1e-4) + 0.3 * (0:299 >= 150)
  upt <- ch4_split(up, trim_to_min = FALSE)
  expect_equal(upt$ch4_flag, "ok")
  expect_equal(upt$slope_diffusion, -2e-4, tolerance = 0.05)
  expect_equal(upt$slope_ebullition, upt$slope_total - upt$slope_diffusion)

  expect_error(ch4_split(trace(100), bubble_pad = 1.5), "bubble_pad")
  expect_error(ch4_split(trace(100), min_rise = -1), "min_rise")
  expect_error(ch4_split(trace(100), min_diffusion_s = 0), "min_diffusion_s")
})

test_quiet("bubble_detection guards, new columns, plot and option checks", {
  set.seed(1)
  trace <- 2 + 1e-4 * (0:299) + rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 100)
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  d <- data.frame(real_datetime = t0 + 0:299, CH4 = trace)
  ref <- data.frame(date_time = t0, site = "x", analyzer = "a")
  expect_error(calculate_regression(d, ref, "CH4", "date_time", "site", "a", window_type = "sliding",
                                    bubble_detection = TRUE), "fixed")

  r <- ch4_split(trace)
  expect_equal(format(r$diffusion_start, "%Y/%m/%d %H:%M:%S"), "2024/01/01 08:00:01")  # trim_to_min drops the first reading
  expect_true(r$diffusion_end < as.POSIXct("2024-01-01 08:01:40", tz = "Asia/Taipei"))
  expect_gt(r$slope_diffusion_se, 0)
  expect_true(is.na(r$g_factor))
  f <- calculate_ghg_flux(cbind(r, area = 1, volume = 1, temp = 25))
  expect_equal(f$flux_diffusion_se / f$flux_diffusion, r$slope_diffusion_se / r$slope_diffusion)
  expect_s3_class(plot_ghg_flux(d, r, "CH4"), "ggplot")

  # bubble spans and two-point total end points for the plot
  expect_match(r$bubble_spans, "^9[0-9]-10[0-9]")  # the step at 100 s
  expect_equal(r$total_start, r$start_time)
  expect_equal((r$total_end_conc - r$total_start_conc) / 299, r$slope_total)
  p <- plot_ghg_flux(transform(d, CO2 = 400 + 0:299 / 10), r, "CH4", second_gas = "CO2")
  pd <- ggplot2::ggplot_build(p)$data
  expect_equal(length(unique(ggplot2::ggplot_build(p)$layout$layout$PANEL)), 2)
  expect_true(any(vapply(pd, function(l) all(l$shape == 17), logical(1))))  # bubble readings
  expect_true(any(vapply(pd, function(l) "linetype" %in% names(l) && any(l$linetype == "22"), logical(1))))
  expect_error(plot_ghg_flux(d, r, "CH4", second_gas = "CO2"), "CO2")

  # a 2-reading run spanning >= 20 s is skipped, not an error
  expect_false(.first_linear_ch4(c(0, 25), c(2, 2.1), c(FALSE, FALSE))$ok)

  # constant readings: no trend, flagged zero
  flat <- calculate_regression(transform(d, CH4 = 2), ref, "CH4", "date_time", "site", "a")
  expect_equal(flat$flag, "zero")

  # options checked even without bubble detection
  expect_error(calculate_regression(d, ref, "CH4", "date_time", "site", "a",
                                    ch4_options = list(trim_to_min = "yes")), "trim_to_min")
  expect_error(ch4_split(trace, min_len = 0), "min_len")
  expect_error(ch4_split(trace, bubble_prob = 2), "bubble_prob")
  expect_error(calculate_regression(d, ref, "CH4", "date_time", "site", "a", ch4_options = list(1)),
               "name")
  expect_warning(calculate_regression(rbind(d, d), ref, "CH4", "date_time", "site", "a"),
                 "300 of 600 readings .* repeat an earlier timestamp")
})

test_that("apply_ch4_review applies decisions", {
  set.seed(1)
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  step <- 2 + 1e-4 * (0:299) + rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 100)
  d <- data.frame(real_datetime = rep(t0 + 0:299, 2) + rep(c(0, 3600), each = 300), CH4 = c(step, step))
  ref <- data.frame(date_time = t0 + c(0, 3600), site = c("x", "y"), analyzer = "a")
  reg <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", duration_minutes = 299 / 60,
                              bubble_detection = TRUE)
  plain <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", duration_minutes = 299 / 60)
  expect_equal(reg$flag_regression, plain$flag)
  expect_equal(reg$r_square_regression, plain$r_square)

  dec <- data.frame(site = c("x", "y"), reference_time = c("2024-01-01 08:00:00", "2024-01-01 09:00:00"),
                    decision = c("not_bubble", "total"))
  out <- apply_ch4_review(reg, dec)
  expect_equal(out$slope[1], plain$slope[1])
  expect_equal(out$flag[1], plain$flag[1])
  expect_equal(out$slope_ebullition[1], 0)
  expect_equal(out$slope[2], reg$slope_total[2])
  expect_true(is.na(out$slope_diffusion[2]))
  expect_equal(out$ch4_decision, c("not_bubble", "total"))
  expect_equal(out$slope_source, c("regression", "total"))
  expect_equal(apply_ch4_review(reg, dec[0, ])$slope_source, reg$slope_source)
  # total: the diffusion line's QC is dropped (NA, as for an automatic total-only closure)
  expect_true(is.na(out$flag[2]) && is.na(out$r_square[2]) && is.na(out$correlation[2]))
  expect_false(is.na(out$flag_regression[2]))
  # not_bubble: below_mdf comes back from the regression fit
  reg_p <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", duration_minutes = 299 / 60,
                                bubble_detection = TRUE, instrument_precision = 0.6)
  expect_true("below_mdf_regression" %in% names(reg_p))
  reg_p$below_mdf <- FALSE; reg_p$below_mdf_regression <- TRUE
  out_p <- apply_ch4_review(reg_p, dec)
  expect_equal(out_p$below_mdf, c(TRUE, FALSE))

  tmpl <- ch4_review_template(reg)
  expect_equal(nrow(tmpl), sum(reg$review_needed))
  expect_true(all(is.na(tmpl$decision)))
  expect_equal(names(tmpl), c("site", "reference_time", "ch4_flag", "r_square_regression", "decision"))
  expect_equal(tmpl$r_square_regression, reg$r_square_regression[reg$review_needed])
  tmpl$decision <- "not_bubble"
  expect_equal(apply_ch4_review(reg, tmpl)$ch4_decision[reg$review_needed], rep("not_bubble", nrow(tmpl)))
  expect_error(ch4_review_template(plain), "bubble_detection")

  expect_equal(apply_ch4_review(reg, dec[0, ])$ch4_decision, ifelse(reg$review_needed, "unreviewed", "auto"))
  expect_equal(apply_ch4_review(reg, transform(dec, decision = "discard"))$flag, c("discard", "discard"))
  expect_error(apply_ch4_review(reg, transform(dec, decision = "maybe")), "Unknown decision")
  expect_error(apply_ch4_review(reg, rbind(dec, dec)), "More than one")
  expect_warning(apply_ch4_review(reg, transform(dec, site = "z")), "match no closure")
  expect_error(apply_ch4_review(plain, dec), "bubble_detection")
})

test_that("convert_ghg_unit converts correctly", {
  result <- convert_ghg_unit(1, "co2", mass = "mmol")
  expect_equal(result, 44010, tolerance = 1)  # 1 mmol co2 = 44.01g/mol * 1000 µg/mg * 1000 = 44010 µg
})

test_that("convert_ghg_unit ratio uses C or N content of the gas", {
  expect_equal(convert_ghg_unit(28, "n2o", ratio = TRUE), 28 * 44.013 / 28.014)
  expect_equal(convert_ghg_unit(12, "co2", ratio = TRUE), 12 * 44.009 / 12.011)
  expect_equal(convert_ghg_unit(12, "ch4", ratio = TRUE), 12 * 16.043 / 12.011)
  expect_error(convert_ghg_unit(1, "ch4", mass = "\u00b5mol", ratio = TRUE), "ratio")
})

test_that("convert_ghg_unit takes ASCII units and does not round by default", {
  expect_equal(convert_ghg_unit(2, "ch4", mass = "umol"),
               convert_ghg_unit(2, "ch4", mass = "\u00b5mol"))
  expect_equal(convert_ghg_unit(1500, "ch4", to_mass = "g"), 0.0015)
  expect_equal(convert_ghg_unit(1500, "ch4", to_mass = "g", digits = 2), 0)
})

test_that("convert_ghg_unit converts each number in a string once, not n =", {
  expect_equal(convert_ghg_unit("1.5 (n=3)", "ch4", to_mass = "mg"), "0.0015 (n=3)")
  expect_equal(convert_ghg_unit("2 \u00b1 20", "ch4", to_mass = "mg"), "0.002 \u00b1 0.02")
  expect_equal(convert_ghg_unit("10 \u00b1 10", "ch4", to_mass = "mg"), "0.01 \u00b1 0.01")
  expect_equal(convert_ghg_unit("1.69 + 0.95", "ch4", to_mass = "mg"), "0.00169 + 0.00095")
})

test_that("convert_ghg_unit throws error for invalid GHG", {
  expect_error(convert_ghg_unit(1, "invalid_gas"), "Invalid ghg")
})

test_quiet("g_factor is NA for explicit linear fit_type", {
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

test_quiet("g_factor is NA when auto mode selects linear (no upgrade)", {
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
                                 fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                 duration_minutes = 7)
  expect_equal(result$fit_used, "linear")
  expect_true(is.na(result$g_factor))
})

test_quiet("g_factor is computed when auto mode selects quadratic", {
  elapsed <- seq(0, 419)
  # Decelerating rise: quadratic initial slope 0.03, lm slope ~0.0174 → g_factor ~1.72
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.03 * elapsed - 0.00003 * elapsed^2
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result <- calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                 duration_minutes = 7)
  expect_equal(result$fit_used, "quadratic")
  expect_false(is.na(result$g_factor))
  # Decelerating curve: initial slope (0.03) > lm slope (~0.0174), so g_factor > 1
  expect_gt(result$g_factor, 1)
  expect_lt(result$g_factor, 2)
})

test_that("auto mode keeps linear when the quadratic accelerates (Levy et al. 2011)", {
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
  result <- suppressWarnings(calculate_regression(data, ref, "CH4",
                                 reference_time = "date_time",
                                 site = "site", analyzer_code = "TEST",
                                 fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                 duration_minutes = 7))
  expect_equal(result$fit_used, "linear")
  expect_true(is.na(result$g_factor))
})

test_quiet("g_factor_threshold overrides ok to discard when ratio exceeds limit", {
  elapsed <- seq(0, 419)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = 2 + 0.03 * elapsed - 0.00003 * elapsed^2
  )
  ref <- data.frame(
    date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
    site = "S1", analyzer = "TEST"
  )
  result_no_gf <- calculate_regression(data, ref, "CH4",
                                       reference_time = "date_time",
                                       site = "site", analyzer_code = "TEST",
                                       fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                       duration_minutes = 7)
  # g_factor ~1.72; threshold 1.5 triggers discard, threshold 2 does not
  result_triggered <- calculate_regression(data, ref, "CH4",
                                           reference_time = "date_time",
                                           site = "site", analyzer_code = "TEST",
                                           fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                           duration_minutes = 7,
                                           g_factor_threshold = 1.5)
  result_not_triggered <- calculate_regression(data, ref, "CH4",
                                               reference_time = "date_time",
                                               site = "site", analyzer_code = "TEST",
                                               fit_type = "auto", window_type = "fixed", instrument_precision = 0.6,
                                               duration_minutes = 7,
                                               g_factor_threshold = 2)
  expect_equal(result_no_gf$flag, "ok")
  expect_equal(result_triggered$flag, "discard")    # g_factor ~1.72, threshold 1.5 triggers
  expect_equal(result_not_triggered$flag, "ok")     # g_factor ~1.72, threshold 2 does not
})

test_quiet("per-row duration_minutes overrides global parameter", {
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
  expect_true(grepl("10:01:00", format(result$end_time)))
})

test_that(".pareto_front_select picks the front member closest to the ideal corner", {
  # A: dominated on both axes by B and C.
  # B: best slope SE, worst rmse.
  # C: slightly worse slope SE than B, much better fit -> closer to (0,0) once normalized.
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
                                 instrument_precision = 0.01)
  # deadband grid seq(30,50,10) = 3 values (min_deadband default is 30);
  # cutoff grid seq(90,419,10) = 33 values (cutoff_lo = min_deadband + min_window_size = 90);
  # candidates require cutoff - deadband >= 60 (min_window_size):
  # deadband=30 -> cutoff>=90: 33; deadband=40 -> cutoff>=100: 32; deadband=50 -> cutoff>=110: 31
  # 33 + 32 + 31 = 96
  expect_equal(result$n_candidates, 96)
  expect_true(result$deadband %in% seq(30, 50, 10))
  expect_gte(result$cutoff - result$deadband, 60)
  expect_equal(result$flag, "ok")
  expect_equal(result$slope, 0.05, tolerance = 0.02)
  expect_false(any(c("slope_uncertainty_lo", "slope_uncertainty_hi") %in% names(result)))
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
                                 instrument_precision = 0.1)
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
                                 instrument_precision = 0.1)
  expect_equal(result$flag, "zero")
})

test_that("pareto window_type requires instrument_precision to be resolvable", {
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

test_that(".pareto_select_window slope-error score depends on window shape, not on rmse", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  pick <- function(seed) {
    set.seed(seed)
    d <- data.frame(real_datetime = t0 + 0:419, CH4 = 400 + 0.5 * (0:419) + rnorm(420, 0, 1))
    .pareto_select_window(d, "CH4", t0, "linear", 0, NULL, 1, 30, 60, 10, 60, 10, min_obs = 4)$selected
  }
  sel <- lapply(1:6, pick)
  # approx_se is sigma / sqrt(Sxx): identical windows give identical scores whatever the noise
  same <- Filter(function(x) x$cutoff == sel[[1]]$cutoff && x$deadband == sel[[1]]$deadband, sel)
  expect_equal(unique(round(vapply(same, function(x) x$approx_se, numeric(1)), 12)), round(sel[[1]]$approx_se, 12))
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
    min_window_size = 2, cutoff_step = 1000)
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
    min_window_size = 3, cutoff_step = 1000)
  expect_equal(result_4$status, "selected")
  expect_equal(result_4$n_candidates, 1)
})

test_quiet("per-row start_offset_s overrides global parameter", {
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
  expect_true(grepl("10:01:00", format(result$start_time)))
})

test_that(".pareto_front_select breaks a distance tie toward the smaller x (longer window)", {
  expect_equal(.pareto_front_select(c(1, 0), c(0, 1)), 2)
  expect_equal(.pareto_front_select(c(0, 1), c(1, 0)), 1)
})

# one closure through calculate_regression(window_type = "pareto"); trace is a function of seconds
pareto_trace <- function(f, cutoff_step, ..., n = 360, prec = 0.6) {
  t0 <- as.POSIXct("2024-02-01 09:00:00", tz = "Asia/Taipei")
  d <- data.frame(real_datetime = t0 + 0:(n - 1), CH4 = f(0:(n - 1)))
  ref <- data.frame(date_time = t0, site = "S1", analyzer = "TEST")
  suppressMessages(calculate_regression(
    d, ref, "CH4", reference_time = "date_time", site = "site", analyzer_code = "TEST",
    window_type = "pareto", duration_minutes = n / 60, instrument_precision = prec,
    cutoff_step = cutoff_step, ...))
}

test_that("pareto pick on a saturating closure does not change with cutoff_step", {
  set.seed(7)
  y <- 2000 + 150 * (1 - exp(-0.01 * (0:359))) + rnorm(360, 0, 0.8)
  slopes <- vapply(c(10, 20, 40), function(cs) pareto_trace(function(s) y, cs, fit_type = "auto")$slope,
                   numeric(1))
  expect_true(all(abs(slopes / median(slopes) - 1) < 0.1))
})

test_that("pareto pick on the curved real closure B1 is stable across cutoff_step", {
  skip_on_cran()
  d <- utils::read.csv(test_path("ref", "ch4_bubbles.csv"), stringsAsFactors = FALSE)
  d <- d[d$closure == "B1", ]
  d$date_time <- as.POSIXct(d$date_time, tz = "Asia/Taipei")
  ref <- data.frame(site = "B1", analyzer = "x",
                    date_time = as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei"))
  slopes <- vapply(c(10, 20, 30, 40, 50), function(cs) suppressMessages(calculate_regression(
    d, ref, ghg = "CH4", reference_time = "date_time", site = "site", analyzer_code = "x",
    start_offset_s = 30, duration_minutes = 5, fit_type = "auto", window_type = "pareto",
    instrument_precision = 0.6, cutoff_step = cs))$slope, numeric(1))
  # before the fixed scaling these ranged from 1.7 to 15.4 (a flip to a 330 s quadratic window);
  # neighbouring short windows of a curved trace still differ by up to ~18%
  expect_lt(max(slopes) / min(slopes), 1.25)
})

test_that("pareto picks a long window on a clean line and a short one is not preferred", {
  set.seed(3)
  y <- 2000 + 0.5 * (0:359) + rnorm(360, 0, 0.8)
  r <- pareto_trace(function(s) y, 20, fit_type = "linear")
  expect_gte(r$cutoff - r$deadband, 250)
  expect_equal(r$slope, 0.5, tolerance = 0.02)
})

test_that("pareto applies the kappamax gate to every candidate", {
  set.seed(7)
  y <- 2000 + 150 * (1 - exp(-0.01 * (0:359))) + rnorm(360, 0, 0.8)
  free  <- pareto_trace(function(s) y, 20, fit_type = "exp_tz")
  gated <- pareto_trace(function(s) y, 20, fit_type = "exp_tz", kappamax = 1e-6)
  expect_equal(free$fit_used, "exp_tz")
  # the true curvature (0.01 s-1) exceeds the cap: no exponential candidate is admitted
  expect_true(gated$fit_used != "exp_tz" || gated$flag == "discard")
})

test_that("min_obs: too few readings give no_data; n_obs counts the window", {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  ref <- data.frame(date_time = t0, site = "P1", analyzer = "a")
  reg <- function(n, ...) calculate_regression(
    data.frame(real_datetime = t0 + seq(0, 200, length.out = n), CH4 = 2 + seq(0, 0.5, length.out = n) + rep_len(c(0, 1e-3), n)),
    ref, "CH4", reference_time = "date_time", site = "site", analyzer_code = "a", duration_minutes = 5, ...)
  expect_equal(reg(2, min_obs = 1)$flag, "no_data")       # a line through 2 points always has R2 = 1
  expect_equal(reg(3, min_obs = 1)$flag, "ok")
  expect_equal(reg(3, min_obs = 1, fit_type = "quadratic")$flag, "no_data")
  expect_equal(reg(59)$flag, "no_data")                     # default 60
  expect_equal(reg(59)$n_obs, 59)
  expect_equal(reg(60)$flag, "ok")
  expect_equal(reg(60)$n_obs, 60)
  expect_error(reg(60, min_obs = NA), "min_obs")
  expect_error(reg(60, window_type = "sliding", num_rows = 30), "num_rows")
  expect_warning(reg(60, window_type = "sliding", num_rows = 100), "Fewer readings than num_rows.*P1")
})

test_that("calculate_regression and tidy_ghg_analyzer stop on wrong inputs", {
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  d <- data.frame(real_datetime = t0 + 0:299, CH4 = 2 + 1e-4 * (0:299) + c(0, 1e-5), CO2 = 400 + 0.01 * (0:299) + c(0, 1e-3))
  ref <- data.frame(date_time = t0, site = "P1", analyzer = "LI-7810 1337+1098")
  reg <- function(..., r = ref, ghg = "CH4", code = "1337+1098")
    calculate_regression(d, r, ghg, reference_time = "date_time", site = "site", analyzer_code = code,
                         duration_minutes = 299 / 60, ...)
  expect_equal(nrow(reg()), 1)                                    # "+" is text, not a regex
  expect_equal(nrow(reg(code = "li-7810")), 1)                    # case ignored
  expect_warning(out <- reg(code = "9999"), "No `reference_df\\$analyzer` contains")
  expect_equal(nrow(out), 0)
  expect_error(reg(ghg = "ch4"), "did you mean \"CH4\"")
  expect_error(reg(r = ref[c("date_time", "site")]), "no column\\(s\\): analyzer")
  expect_error(reg(r = transform(ref, date_time = "2024-01-01 08:00:00")), "POSIXct")
  expect_warning(reg(ghg = "CO2", bubble_detection = TRUE), "meant for CH4")
  expect_error(tidy_ghg_analyzer("none.xlsx", "co2"), "Unsupported analyzer/gas")
  expect_error(tidy_ghg_analyzer("none.xlsx", "n2o", analyzer = "lgr"), "Unsupported analyzer/gas")
})

test_that("precision comes from instrument_precision or the <ghg>_prec column, never from analyzer names", {
  x <- tidy_ghg_analyzer(system.file("extdata", "ch4.xlsx", package = "aelab"), "ch4")
  expect_equal(unique(x$CH4_prec), 0.6)
  expect_equal(unique(x$CO2_prec), 3.5)
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  e <- 0:299
  set.seed(1)
  d <- data.frame(real_datetime = t0 + e, CH4 = 2000 + 2 * e - 0.004 * e^2 + rnorm(300, 0, 0.5))
  ref <- data.frame(date_time = t0, site = "x", analyzer = "1337")
  reg <- function(d, ...) calculate_regression(d, ref, "CH4", "date_time", "site", "1337", fit_type = "auto",
                                               duration_minutes = 299 / 60, ...)
  expect_error(reg(d), "needs instrument_precision")             # was: silent LI-7810 fallback
  expect_error(reg(transform(d, CH4_prec = rep(c(0.6, 2), 150))), "one value")
  expect_equal(reg(transform(d, CH4_prec = 0.6)), reg(d, instrument_precision = 0.6))
})

test_that("calculate_ghg_flux pressure and h2o terms; h2o_start from the first reading", {
  base <- data.frame(slope = 1, area = 1, volume = 1, temp = 25)
  f0 <- calculate_ghg_flux(base)$flux
  expect_equal(calculate_ghg_flux(cbind(base, p = 101.325, w = 0), pressure = "p", h2o = "w")$flux, f0)
  expect_equal(calculate_ghg_flux(cbind(base, p = 101.325 / 2), pressure = "p")$flux, f0 / 2)
  expect_equal(calculate_ghg_flux(cbind(base, w = 30000), h2o = "w")$flux, 0.97 * f0)
  expect_error(calculate_ghg_flux(base, h2o = "w"), "do not exist")
  t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
  d <- data.frame(real_datetime = t0 + 0:299, CH4 = 2 + 1e-4 * (0:299) + c(0, 1e-5), H2O = c(NA, 25000 + 0:298))
  ref <- data.frame(date_time = t0, site = "x", analyzer = "a")
  r <- calculate_regression(d, ref, "CH4", "date_time", "site", "a", duration_minutes = 299 / 60)
  expect_equal(r$h2o_start, 25000)
  expect_true(is.na(calculate_regression(d[c("real_datetime", "CH4")], ref, "CH4", "date_time", "site", "a",
                                         duration_minutes = 299 / 60)$h2o_start))
  x <- tidy_ghg_analyzer(system.file("extdata", "ch4.xlsx", package = "aelab"), "ch4")
  expect_true(is.numeric(x$H2O) && all(x$H2O_unit == "ppm"))
})

test_that("slope_diffusion_se uses the effective sample size for autocorrelated residuals (Santer et al. 2000)", {
  opt <- aelab:::.ch4_options(list())
  sec <- 0:299
  ols <- function(s) {  # plain OLS SE of the same segment
    i <- sec >= s$diff_start & sec <= s$diff_end
    summary(lm(y[i] ~ sec[i]))$coefficients[2, 2]
  }
  set.seed(2); y <- 2 + 1e-4 * sec + ifelse(sec >= 150, 0.05, 0) + rnorm(300, 0, 3e-4)
  white <- aelab:::.separate_ch4(sec, y, opt)
  expect_equal(white$diff_se / ols(white), 1, tolerance = 0.25)            # no autocorrelation: about OLS
  set.seed(2); y <- 2 + 1e-4 * sec + ifelse(sec >= 150, 0.05, 0) + as.numeric(stats::arima.sim(list(ar = 0.8), 300, sd = 1.5e-4))
  ar <- aelab:::.separate_ch4(sec, y, opt)
  expect_gt(ar$diff_r1, 0.5)
  expect_gt(ar$diff_se / ols(ar), 2)
})
