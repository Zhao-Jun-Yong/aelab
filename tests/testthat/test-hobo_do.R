test_that("process_hobo works correctly", {
  data <- process_hobo(test_file("ex_hobo.csv"), no_hobo = "test")

  expect_true(!is.null(data))
  column_names <- c("date_time", "do", "temp", "no_hobo")
  expect_identical(column_names, names(data))
  expect_identical("test", data$no_hobo[1])
  expect_true(all(!is.na(data$do)))
  expect_true(all(!is.na(data$temp)))
})

test_that("process_hobo drops negative DO/temperature sensor errors", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # 2 header rows + 4 readings; rows 2 and 3 have a negative DO / temp
  # all four readings fall in the same 10:30 half-hour bin
  writeLines(c(
    '"Plot Title: test"',
    '"#","Date Time, GMT+08:00","DO conc, mg/L","Temp, °C"',
    '1,02/27/24 10:05:00 AM,8.0,25.0',
    '2,02/27/24 10:10:00 AM,-0.5,25.1',
    '3,02/27/24 10:15:00 AM,8.1,-3.0',
    '4,02/27/24 10:20:00 AM,8.2,25.2'
  ), tmp)

  data <- process_hobo(tmp, no_hobo = "t")
  # only the 2 valid readings (8.0, 8.2) should remain -> mean 8.1
  expect_true(all(data$do >= 0))
  expect_true(all(data$temp >= 0))
  expect_equal(nrow(data), 1L)
  expect_equal(round(data$do[1], 2), 8.1)
})

test_that("process_hobo works with type = 'temp' (HOBO Pro v2)", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Minimal HOBO Pro v2 CSV: 2 header rows + 2 data rows, Chinese AM/PM locale
  writeLines(c(
    '"Plot Title","HOBO Pro v2",,',
    '"#","Date Time","Temp, °C","RH, %"',
    paste0('"1","05/22/2026 上午10時30分00秒","28.5","75.2"'),
    paste0('"2","05/22/2026 上午11時00分00秒","29.0","74.5"')
  ), tmp)

  data <- process_hobo(tmp, no_hobo = "test", type = "temp")

  expect_identical(c("date_time", "air_temp", "rh", "no_hobo"), names(data))
  expect_identical("test", data$no_hobo[1])
  expect_true(is.numeric(data$air_temp))
  expect_true(is.numeric(data$rh))
  expect_true(inherits(data$date_time, "POSIXct"))
  expect_equal(as.integer(format(data$date_time[1], "%Y")), 2026L)
})

test_that("process_weather works correctly", {

  data <- process_weather(test_file("ex_weather.csv"), date = "2024-01-01", zone = "zone_A")

  column_names <- c("pressure_hpa", "wind_ms", "date_time", "zone")
  expect_identical(column_names, names(data))
  expect_true(is.numeric(data$pressure_hpa))
  expect_true(is.numeric(data$wind_ms))
  expect_true(inherits(data$date_time, "POSIXct"))
  expect_identical("zone_A", data$zone[1])
})

test_that("process_weather_mh parses MH format correctly", {
  f <- test_file("ex_weather_mh.txt")

  # hourly (no expansion): 5 data rows -> 5 rows
  hourly <- process_weather_mh(f, expand_30min = FALSE)
  expect_true(all(c("stno", "date_time", "pressure_hpa", "wind_ms") %in% names(hourly)))
  expect_equal(nrow(hourly), 5)
  expect_true(is.numeric(hourly$pressure_hpa))
  expect_true(inherits(hourly$date_time, "POSIXct"))
  expect_identical("Asia/Taipei", attr(hourly$date_time, "tzone"))

  # CWA missing code (-9991) -> NA
  expect_true(any(is.na(hourly$wind_ms)))

  # hour 24 -> 00:00 of the next day
  h24 <- hourly[format(hourly$date_time, "%H:%M") == "00:00", ]
  expect_true(all(format(h24$date_time, "%Y-%m-%d") == "2024-03-02"))

  # station filter
  one <- process_weather_mh(f, station_id = "C0R660", expand_30min = FALSE)
  expect_identical(unique(one$stno), "C0R660")
  expect_error(process_weather_mh(f, station_id = "NOPE"), "No rows")

  # 30-min expansion doubles the rows; hour 1 -> 00:30 and 01:00
  expanded <- process_weather_mh(f, station_id = "C0R660")
  expect_equal(nrow(expanded), 2 * 3)
  expect_true("2024-03-01 00:30:00" %in% format(expanded$date_time))
})

test_that("combine_weather_mh combines and de-duplicates", {
  f <- test_file("ex_weather_mh.txt")
  combined <- combine_weather_mh(c(f, f), expand_30min = FALSE)
  # same file twice -> de-duplicated back to 5 unique station/time rows
  expect_equal(nrow(combined), 5)
})

test_that("filter_complete_days keeps only complete days", {
  # logger A: a full 48-slot day plus a half day; logger B: one full day
  full_day <- seq(as.POSIXct("2024-03-01 00:00:00", tz = "Asia/Taipei"),
                  as.POSIXct("2024-03-01 23:30:00", tz = "Asia/Taipei"),
                  by = "30 min")
  half_day <- seq(as.POSIXct("2024-03-02 00:00:00", tz = "Asia/Taipei"),
                  as.POSIXct("2024-03-02 11:30:00", tz = "Asia/Taipei"),
                  by = "30 min")
  df <- rbind(
    data.frame(no_hobo = "A", date_time = c(full_day, half_day)),
    data.frame(no_hobo = "B", date_time = full_day)
  )

  summ <- filter_complete_days(df, summary_only = TRUE)
  expect_true(all(c("no_hobo", "date", "n_slots", "complete") %in% names(summ)))
  # the full days are complete (48 slots), the half day is not (24 slots)
  expect_equal(sum(summ$complete), 2)

  clean <- filter_complete_days(df)
  expect_true(all(as.Date(clean$date_time, tz = "Asia/Taipei") != as.Date("2024-03-02")))
  expect_setequal(unique(clean$no_hobo), c("A", "B"))
})

test_that("add_sun_times attaches plausible sunrise/sunset", {
  skip_if_not_installed("suncalc")
  df <- data.frame(date_time = as.POSIXct(
    c("2024-02-27 10:00:00", "2024-02-27 14:00:00"), tz = "Asia/Taipei"))
  out <- add_sun_times(df, lat = 22.3900, lon = 120.5777)
  expect_true(all(c("sunrise", "sunset") %in% names(out)))
  expect_true(inherits(out$sunrise, "POSIXct"))
  # FL late-Feb sunrise ~06:2x, sunset ~18:0x
  expect_equal(format(out$sunrise[1], "%H"), "06")
  expect_equal(format(out$sunset[1], "%H"), "18")
})

test_that("process_info works correctly", {

  data <- process_info(test_file("info.xlsx"))

  column_names <- c("zone", "site", "no_hobo", "depth_m",
                    "salinity", "start_date_time", "end_date_time", "sunrise", "sunset")
  expect_identical(column_names, names(data))
})

test_that("calculate_do calculates correctly", {
  df <- data.frame(
    date_time = as.POSIXct(c("2024-12-01 12:00:00", "2024-12-01 12:30:00", "2024-12-01 13:00:00"), tz = "Asia/Taipei"),
    do = c(8.5, 8.6, 8.7),
    temp = c(20, 21, 22),
    salinity = c(0, 0, 0),
    pressure_hpa = c(1013, 1014, 1015),
    wind_ms = c(2.5, 2.6, 2.7),
    sunrise = as.POSIXct("2024-12-01 06:00:00", tz = "Asia/Taipei"),
    sunset = as.POSIXct("2024-12-01 18:00:00", tz = "Asia/Taipei"),
    depth_m = c(1, 1, 1),
    site = c("site_A", "site_A", "site_A"),
    no_hobo  = "code_for_logger"
  )

  data <- calculate_do(df)

  expect_true("gpp" %in% colnames(data))  # Check if GPP is calculated
  expect_true("r_day" %in% colnames(data))  # Check if ER is calculated
  expect_true(all(!is.na(data$gpp)))
  expect_true(all(!is.na(data$r_day)))
})
