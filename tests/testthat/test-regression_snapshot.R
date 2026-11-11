# Full-output snapshot of calculate_regression(): every column, all fit types, window types and
# the CH4 split, on the real fixtures and on synthetic traces. A refactor of the regression code
# must leave it unchanged. Platform-dependent floating point, so not run on CRAN.

quiet <- function(x) suppressMessages(suppressWarnings(x))
taipei <- function(x) as.POSIXct(x, tz = "Asia/Taipei")

fixture <- function() {
  d <- utils::read.csv(test_file("ch4_bubbles.csv"), stringsAsFactors = FALSE)
  d$date_time <- taipei(d$date_time)
  d
}
fixture_ref <- function(site) {
  start <- c(B1 = "08:00:00", B2 = "08:20:00", B3 = "08:40:00")[site]
  data.frame(site = site, analyzer = "x", date_time = taipei(paste("2024-01-01", start)))
}

# synthetic 1 Hz traces: linear rise, saturating rise, flat noise, constant, with H2O
synthetic <- function() {
  set.seed(42)
  t0 <- taipei("2024-02-01 09:00:00")
  one <- function(start, f) {
    s <- 0:359
    data.frame(date_time = start + s, CH4 = f(s), H2O = 12000 + s, CH4_unit = "ppb", CH4_prec = 0.6)
  }
  list(
    data = rbind(
      one(t0,         function(s) 2000 + 0.5 * s + rnorm(360, 0, 0.8)),
      one(t0 + 600,   function(s) 2000 + 150 * (1 - exp(-0.01 * s)) + rnorm(360, 0, 0.8)),
      one(t0 + 1200,  function(s) 2000 + rnorm(360, 0, 0.8)),
      one(t0 + 1800,  function(s) rep(2000, 360)),
      one(t0 + 2400,  function(s) 2000 + 0.4 * s + rnorm(360, 0, 25))
    ),
    ref = data.frame(site = c("lin", "sat", "flat", "const", "noisy", "late"),
                     analyzer = "x",
                     date_time = taipei("2024-02-01 09:00:00") + c(0, 600, 1200, 1800, 2400, 99999))
  )
}

test_that("calculate_regression output is unchanged: fixtures", {
  skip_on_cran()
  d <- fixture()
  one <- function(site, ...) quiet(calculate_regression(
    d[d$closure == site, ], fixture_ref(site), ghg = "CH4", reference_time = "date_time",
    site = "site", analyzer_code = "x", start_offset_s = 30, duration_minutes = 5, ...))
  out <- list(
    linear    = one("B1", fit_type = "linear"),
    quadratic = one("B1", fit_type = "quadratic"),
    exp_tz    = one("B1", fit_type = "exp_tz"),
    exp_zhao  = one("B1", fit_type = "exp_zhao18"),
    auto_b1   = one("B1", fit_type = "auto"),
    auto_b2   = one("B2", fit_type = "auto"),
    kappamax  = one("B1", fit_type = "exp_tz", kappamax = 0.001),
    g_factor  = one("B1", fit_type = "auto", g_factor_threshold = 1.01),
    precision = one("B2", fit_type = "linear", instrument_precision = 0.6),
    sliding   = quiet(calculate_regression(
      d[d$closure == "B1", ], fixture_ref("B1"), ghg = "CH4", reference_time = "date_time",
      site = "site", analyzer_code = "x", window_type = "sliding", duration_minutes = 5.5,
      num_rows = 150, fit_type = "linear")),
    pareto    = {
      set.seed(1)
      one("B1", fit_type = "auto", window_type = "pareto", instrument_precision = 0.6,
          n_boot = 20, cutoff_step = 40)
    },
    bubble_b1 = one("B1", fit_type = "auto", bubble_detection = TRUE),
    bubble_b2 = one("B2", fit_type = "auto", bubble_detection = TRUE),
    bubble_b3 = one("B3", fit_type = "auto", bubble_detection = TRUE),
    bubble_opts = one("B1", fit_type = "linear", bubble_detection = TRUE,
                      ch4_options = list(min_rise = 100, bubble_pad = 5))
  )
  expect_snapshot_value(out, style = "serialize")
})

test_that("calculate_regression output is unchanged: synthetic traces", {
  skip_on_cran()
  s <- synthetic()
  one <- function(...) quiet(calculate_regression(
    s$data, s$ref, ghg = "CH4", reference_time = "date_time", site = "site",
    analyzer_code = "x", duration_minutes = 5, ...))
  ref_over <- s$ref
  ref_over$duration_minutes <- c(3, NA, NA, NA, 2, NA)
  ref_over$start_offset_s   <- c(NA, 20, NA, NA, NA, NA)
  out <- list(
    linear   = one(fit_type = "linear"),
    quadratic = one(fit_type = "quadratic"),
    exp_tz   = one(fit_type = "exp_tz"),
    exp_zhao = one(fit_type = "exp_zhao18"),
    auto     = one(fit_type = "auto"),
    precision = one(fit_type = "linear", instrument_precision = 0.6),
    thresholds = one(fit_type = "linear", r2_threshold = 0.9, cor_threshold = 0.8),
    min_obs  = one(fit_type = "linear", min_obs = 400),
    overrides = quiet(calculate_regression(
      s$data, ref_over, ghg = "CH4", reference_time = "date_time", site = "site",
      analyzer_code = "x", duration_minutes = 5, fit_type = "linear")),
    sliding  = one(fit_type = "linear", window_type = "sliding", num_rows = 200),
    bubble   = one(fit_type = "linear", bubble_detection = TRUE)
  )
  expect_snapshot_value(out, style = "serialize")
})
