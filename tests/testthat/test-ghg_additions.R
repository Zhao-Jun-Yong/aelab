# rmse, data_ratio, start_conc, exp_hm, the LI-COR .data reader and the plot title

t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
ref1 <- data.frame(date_time = t0, site = "S1", analyzer = "a")
fit_rmse <- function(fit_type, ...) {
  set.seed(1)
  y <- 400 + 0.1 * (0:419) + stats::rnorm(420, 0, 0.5)
  dat <- data.frame(real_datetime = t0 + 0:419, CO2 = y)
  list(y = y,
       r = calculate_regression(dat, ref1, "CO2", reference_time = "date_time", site = "site",
                                analyzer_code = "a", fit_type = fit_type, window_type = "fixed",
                                duration_minutes = 5, ...))
}

test_that("rmse, data_ratio and start_conc describe the fitted window", {
  f <- fit_rmse("linear")
  y <- f$y[1:301]  # 0 to 300 s, both ends included
  expect_equal(f$r$rmse, sqrt(mean(stats::residuals(stats::lm(y ~ I(0:300)))^2)))
  expect_equal(f$r$data_ratio, 301 / 300)
  expect_equal(f$r$start_conc, mean(f$y[1:3]))
  expect_equal(f$r$n_obs, 301L)
})

test_that("a window with a gap has a lower data_ratio", {
  dat <- data.frame(real_datetime = t0 + c(0:99, 200:419), CO2 = 400 + 0.1 * c(0:99, 200:419))
  r <- suppressWarnings(calculate_regression(dat, ref1, "CO2", reference_time = "date_time",
                                             site = "site", analyzer_code = "a", duration_minutes = 5))
  expect_equal(r$data_ratio, 201 / 300)
})

test_that("rmse is NA where nothing was fitted", {
  r <- suppressWarnings(calculate_regression(data.frame(real_datetime = t0 + 0:9, CO2 = 400),
                                             ref1, "CO2", reference_time = "date_time", site = "site",
                                             analyzer_code = "a", duration_minutes = 5))
  expect_identical(r$flag, "no_data")
  expect_true(is.na(r$rmse))
})

test_quiet("exp_hm recovers the initial slope A*b of the Hutchinson-Mosier curve", {
  tt <- 0:299
  # C = 400 + A (1 - exp(-b t)): Cm = 500, C0 = 400; slope at t_zero = b (Cm - C0) exp(-b t_zero)
  exact <- .fit_ghg_model(400 + 100 * (1 - exp(-0.01 * tt)), tt, "exp_hm", t_zero = 0)
  expect_identical(exact$fit_used, "exp_hm")
  expect_equal(exact$slope, 1, tolerance = 1e-3)
  expect_equal(exact$b_param, 0.01, tolerance = 1e-3)
  expect_lt(.rmse(exact$residuals), 1e-3)
  expect_equal(.fit_ghg_model(400 + 100 * (1 - exp(-0.01 * tt)), tt, "exp_hm", t_zero = 50)$slope,
               exp(-0.5), tolerance = 1e-3)
  set.seed(1)
  y <- 400 + 100 * (1 - exp(-0.01 * tt)) + stats::rnorm(300, 0, 0.5)
  noisy <- .fit_ghg_model(y, tt, "exp_hm", t_zero = 0)
  expect_equal(noisy$slope, 1, tolerance = 0.05)
  expect_equal(.rmse(noisy$residuals), 0.5, tolerance = 0.1)
  # no curvature: NA, as exp_tz
  expect_true(is.na(.fit_ghg_model(400 + 0.05 * tt, tt, "exp_hm", t_zero = 0)$slope))
})

test_that("exp_hm works through calculate_regression() and plot_ghg_flux()", {
  elapsed <- 0:419
  dat <- data.frame(real_datetime = t0 + elapsed, CO2 = 500 - 100 * exp(-0.01 * elapsed))
  r <- calculate_regression(dat, ref1, "CO2", reference_time = "date_time", site = "site",
                            analyzer_code = "a", fit_type = "exp_hm", duration_minutes = 5)
  expect_identical(r$fit_used, "exp_hm")
  expect_equal(r$slope, 1, tolerance = 1e-3)  # b (Cm - C0) = 0.01 * 100 at t_zero = 0
  expect_s3_class(plot_ghg_flux(dat, r, "CO2"), "ggplot")
})

test_that("the plot title shows RMSE, n and the model; zero is blue, discard yellow", {
  f <- fit_rmse("linear")
  dat <- data.frame(real_datetime = t0 + 0:419, CO2 = f$y)
  p <- plot_ghg_flux(dat, f$r, "CO2")
  lbl <- p$facet$params$labeller(data.frame(panel = "1 CO2"))[[1]]
  expect_match(lbl, "linear  RMSE=", fixed = TRUE)
  expect_match(lbl, "n=301", fixed = TRUE)
  cols <- eval(formals(plot_ghg_flux)$flag_colors)
  expect_identical(unname(cols["zero"]), "#6096BA")
  expect_identical(unname(cols["discard"]), "#f2cc8f")
})

test_that("tidy_ghg_analyzer() reads a LI-COR .data file", {
  f <- tempfile(fileext = ".data")
  on.exit(unlink(f))
  writeLines(c("Model:\tLI-7810", "SN:\tTG10-00001", "Software Version:\t2.3.3",
               "Timestamp:\t2026-11-07 12:22:59", "Timezone:\tAsia/Taipei",
               "DATAH\tSECONDS\tDATE\tTIME\tH2O\tCO2\tCH4\tCAVITY_P",
               "DATAU\tsecs\tdate\ttime\tppm\tppm\tppb\tkPa",
               "DATA\t1\t2026-11-07\t12:22:40\tnan\tnan\tnan\t101.7",
               "DATA\t2\t2026-11-07\t12:22:41\t14000.5\t410.25\t2000.5\t45.1",
               "DATA\t3\t2026-11-07\t12:22:42\t14001.5\t411.25\t2001.5\t45.1"), f)
  d <- tidy_ghg_analyzer(f, "ch4")
  expect_identical(names(d), c("DATE", "TIME", "H2O", "CO2", "CH4", "date_time", "CO2_unit", "CH4_unit",
                               "H2O_unit", "CH4_prec", "CO2_prec"))
  expect_equal(nrow(d), 2L)  # the row with no gas reading is dropped
  expect_identical(d$DATE[1], "2026/11/07")
  expect_identical(d$TIME[1], "12:22:41")
  expect_equal(d$date_time[2] - d$date_time[1], as.difftime(1, units = "secs"))
  expect_equal(d$CH4, c(2000.5, 2001.5))
  expect_identical(d$CH4_unit[1], "ppb")
  expect_identical(d$CO2_unit[1], "ppm")
  expect_error(tidy_ghg_analyzer(f, "n2o"), "no N2O column")
  # a line cut short, or with an extra field, is dropped with a warning, not read as a reading
  writeLines(c(readLines(f)[1:9], "DATA\t4\t2026-11-07\t12:22:43\t14002.5\t412.2",
               "DATA\t5\t2026-11-07\t12:22:44\t14003.5\t413.25\t2003.5\t45.1\textra"), f)
  expect_warning(d2 <- tidy_ghg_analyzer(f, "ch4"), "2 DATA line")
  expect_equal(d2$CH4, 2000.5)  # only the complete reading is left
  writeLines(c("Model:\tLI-7810", "DATAH\tDATE\tTIME\tH2O\tCO2\tCH4", "DATAU\tdate\ttime\tppm\tppm\tppb"), f)
  expect_error(tidy_ghg_analyzer(f, "ch4"), "no DATA lines")
})

test_that("apply_ch4_review() keeps n_obs_diffusion in step with the decision", {
  set.seed(1)
  y <- 2 + 1e-4 * (0:299) + stats::rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 150)
  reg <- calculate_regression(data.frame(real_datetime = t0 + 0:299, CH4 = y), ref1, "CH4",
                              reference_time = "date_time", site = "site", analyzer_code = "a",
                              duration_minutes = 299 / 60, bubble_detection = TRUE)
  dec <- data.frame(site = "S1", reference_time = format(t0, "%Y-%m-%d %H:%M:%S"), decision = "not_bubble")
  expect_lt(reg$n_obs_diffusion, reg$n_obs)
  expect_equal(apply_ch4_review(reg, dec)$n_obs_diffusion, reg$n_obs)
  dec$decision <- "total"
  expect_true(is.na(apply_ch4_review(reg, dec)$n_obs_diffusion))
})

test_that("slope_se is the Santer-corrected standard error of a linear slope", {
  set.seed(4)
  y <- 400 + 0.1 * (0:419) + as.numeric(stats::arima.sim(list(ar = 0.8), 420, sd = 0.3))
  dat <- data.frame(real_datetime = t0 + 0:419, CO2 = y)
  args <- list(dat, ref1, "CO2", reference_time = "date_time", site = "site", analyzer_code = "a",
               duration_minutes = 5)
  r <- do.call(calculate_regression, c(args, fit_type = "linear"))
  m <- stats::lm(y[1:301] ~ I(0:300))
  r1 <- stats::acf(stats::residuals(m), lag.max = 1, plot = FALSE)$acf[2]
  ne <- 301 * (1 - r1) / (1 + r1)
  se_ols <- summary(m)$coefficients[2, 2]
  expect_equal(r$slope_se, se_ols * sqrt((301 - 2) / (ne - 2)))
  expect_gt(r$slope_se, se_ols)  # autocorrelated residuals: the OLS SE is too small
  # flux_se is the slope SE converted like the slope
  f <- calculate_ghg_flux(transform(r, area = 1, volume = 1, temp = 25))
  expect_equal(f$flux_se / f$flux, r$slope_se / r$slope)
  # other models: NA
  sat <- data.frame(real_datetime = t0 + 0:419, CO2 = 500 - 100 * exp(-0.01 * (0:419)))
  hm <- calculate_regression(sat, ref1, "CO2", reference_time = "date_time", site = "site",
                             analyzer_code = "a", fit_type = "exp_hm", duration_minutes = 5)
  expect_true(is.na(hm$slope_se))
})

test_that("slope_se follows the slope in CH4 bubble closures and their review", {
  set.seed(1)
  y <- 2 + 1e-4 * (0:299) + stats::rnorm(300, 0, 2e-4)
  d <- rbind(data.frame(real_datetime = t0 + 0:299, CH4 = y),
             data.frame(real_datetime = t0 + 600 + 0:299, CH4 = y + 0.05 * (0:299 >= 150)))
  ref <- data.frame(date_time = t0 + c(0, 600), site = c("clean", "bubble"), analyzer = "a")
  reg <- calculate_regression(d, ref, "CH4", reference_time = "date_time", site = "site",
                              analyzer_code = "a", duration_minutes = 299 / 60, bubble_detection = TRUE)
  expect_false(is.na(reg$slope_se[reg$site == "clean"]))      # no bubble: the regression slope
  expect_true(is.na(reg$slope_se[reg$site == "bubble"]))      # slope is the total
  expect_false(is.na(reg$slope_se_regression[reg$site == "bubble"]))
  dec <- data.frame(site = "bubble", reference_time = format(t0 + 600, "%Y-%m-%d %H:%M:%S"),
                    decision = "not_bubble")
  rev <- apply_ch4_review(reg, dec)
  expect_equal(rev$slope_se[2], reg$slope_se_regression[2])
  expect_equal(rev$rmse[2], reg$rmse_regression[2])
  expect_equal(rev$r_square[2], reg$r_square_regression[2])
  # "total" drops the regression fit, so its SE and RMSE go too (the clean closure had both)
  dec_t <- data.frame(site = "clean", reference_time = format(t0, "%Y-%m-%d %H:%M:%S"), decision = "total")
  tot <- apply_ch4_review(reg, dec_t)
  expect_false(is.na(reg$slope_se[1]) || is.na(reg$rmse[1]))
  expect_true(is.na(tot$slope_se[1]) && is.na(tot$rmse[1]))
})
