test_that("calculate_MDF follows the formula and scales with molar mass", {
  args <- list(precision_ppm = 1, closure_time_s = 300, data_point_n = 300,
               chamber_volume_m3 = 0.0064, temperature_C = 25, chamber_area_m2 = 0.07)
  co2 <- do.call(calculate_MDF, c(args, ghg = "co2"))
  ch4 <- do.call(calculate_MDF, c(args, ghg = "ch4"))
  # umol m-2 h-1: 1 / (300 * sqrt(300)) * (0.0064 * 101325) / (8.314 * 298.15 * 0.07) * 3600
  umol <- 1 / (300 * sqrt(300)) * (0.0064 * 101325) / (8.314 * 298.15 * 0.07) * 3600
  expect_equal(co2$MDF, umol * (12.011 + 2 * 15.999))
  expect_equal(ch4$MDF / co2$MDF, (12.011 + 4 * 1.008) / (12.011 + 2 * 15.999))
  expect_match(co2$unit, "g")
  expect_error(do.call(calculate_MDF, c(args, ghg = "n2")), "Invalid GHG")
  args$closure_time_s <- 0
  expect_error(do.call(calculate_MDF, c(args, ghg = "co2")), "positive")
})

test_that("calculate_total_co2e uses GWP 1, 27, 273 and converts mg h-1 to g d-1", {
  expect_message(res <- calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003),
                 "Total CO2e")
  expect_equal(res, (4.02 + 0.001 * 27 + 0.003 * 273) * 0.001 * 24)
  expect_equal(suppressMessages(calculate_total_co2e(ch4 = 1)), 27 * 0.024)
  expect_equal(suppressMessages(calculate_total_co2e()), 0)
})

test_that("make_reference parses positional and keyword window overrides", {
  ref <- make_reference("2026-05-22", "
    7:32:00  S1  1337
    07:45    S2  1337  dur=2  off=45
    08:00:00 S3  1337  3.5  20
  ")
  expect_named(ref, c("site", "date_time", "analyzer", "duration_minutes", "start_offset_s"))
  expect_equal(ref$site, c("S1", "S2", "S3"))
  expect_equal(format(ref$date_time, "%H:%M:%S"), c("07:32:00", "07:45:00", "08:00:00"))
  expect_equal(attr(ref$date_time, "tzone"), "Asia/Taipei")
  expect_equal(ref$duration_minutes, c(NA, 2, 3.5))
  expect_equal(ref$start_offset_s, c(NA, 45, 20))
  # no override columns without override tokens
  expect_false("duration_minutes" %in% names(make_reference("2026-05-22", "10:00:00 S1 1337")))
})

test_that("make_reference rejects empty and short lines and writes xlsx on request", {
  expect_error(make_reference("2026-05-22", "  \n "), "no data rows")
  expect_error(make_reference("2026-05-22", "10:00:00 S1"), "time  site  analyzer")
  out <- tempfile(fileext = ".xlsx")
  ref <- make_reference("2026-05-22", "10:00:00 S1 1337", output = out)
  expect_true(file.exists(out))
  expect_equal(nrow(readxl::read_excel(out)), nrow(ref))
  unlink(out)
})

test_that("tidy_ghg_analyzer reads LI-COR N2O exports and adds unit and precision columns", {
  f <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "s")
  openxlsx::writeData(wb, 1, "header", startRow = 1, startCol = 1, colNames = FALSE)
  openxlsx::writeData(wb, 1, as.data.frame(t(c("DATE", "TIME", "H2O", "N2O"))),
                      startRow = 6, startCol = 7, colNames = FALSE)
  openxlsx::writeData(wb, 1,
    data.frame(DATE = 45050, TIME = c(0.4, 0.4 + 1 / 86400, 0.4 + 2 / 86400),
               H2O = 1000, N2O = c("330.5", "NaN", "331.5")),
    startRow = 8, startCol = 7, colNames = FALSE)
  openxlsx::saveWorkbook(wb, f)
  d <- suppressMessages(tidy_ghg_analyzer(f, gas = "n2o"))
  expect_equal(nrow(d), 2)  # the NaN reading is dropped
  expect_true(all(c("N2O", "date_time", "N2O_unit", "N2O_prec", "H2O_unit") %in% names(d)))
  expect_equal(unique(d$N2O_unit), "ppb")
  expect_equal(unique(d$N2O_prec), 0.4)
  expect_equal(d$N2O, c(330.5, 331.5))
  expect_equal(format(d$date_time[1], "%Y/%m/%d %H:%M:%S"), "2023/05/04 09:36:00")
  unlink(f)
})

# ref/ch4_bubbles.csv: three real LI-COR CH4 closures (ppb, 1 Hz) with site names and
# timestamps replaced; each starts at 2024-01-01 08:00, 08:20 and 08:40.
bubble_closure <- function(site) {
  d <- utils::read.csv(test_file("ch4_bubbles.csv"), stringsAsFactors = FALSE)
  start <- c(B1 = "08:00:00", B2 = "08:20:00", B3 = "08:40:00")[[site]]
  d$date_time <- as.POSIXct(d$date_time, tz = "Asia/Taipei")
  ref <- data.frame(site = site, analyzer = "x",
                    date_time = as.POSIXct(paste("2024-01-01", start), tz = "Asia/Taipei"))
  calculate_regression(d[d$closure == site, ], ref, ghg = "CH4", reference_time = "date_time",
                       site = "site", analyzer_code = "x", fit_type = "auto",
                       window_type = "fixed", start_offset_s = 30, duration_minutes = 5,
                       bubble_detection = TRUE)
}

test_that("real closure B1: one bubble, clean diffusion line, split values pinned", {
  r <- bubble_closure("B1")
  expect_equal(r$n_obs, 301)
  expect_equal(r$fit_used, "quadratic")
  expect_equal(r$slope, 4.7997957, tolerance = 1e-6)
  expect_equal(r$slope_diffusion, 1.603922, tolerance = 1e-6)
  expect_equal(r$slope_ebullition, 3.195873, tolerance = 1e-6)
  expect_equal(r$ch4_flag, "ok")
  expect_false(r$review_needed)
})

test_that("real closure B2: large late bubble, short diffusion segment (known limit), labels pinned", {
  r <- bubble_closure("B2")
  # The diffusion line covers only ~25 s before the bubble and is not labelled for review;
  # update this test if that behaviour changes.
  expect_equal(r$ch4_flag, "ok")
  expect_false(r$review_needed)
  expect_gt(r$slope_ebullition, 5 * r$slope_diffusion)
})

test_that("real closure B3: false bubbles in noise are labelled for review", {
  r <- bubble_closure("B3")
  expect_true(r$review_needed)
  expect_match(r$ch4_flag, "diffusion > total flux")
  expect_match(r$ch4_flag, "diffusion after first bubble")
})
