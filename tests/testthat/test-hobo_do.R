test_that("process_hobo works correctly", {
  data <- process_hobo(test_file("ex_hobo.csv"), no_hobo = "test")

  expect_true(!is.null(data))
  column_names <- c("date_time", "do", "temp", "no_hobo")
  expect_identical(column_names, names(data))
  expect_identical("test", data$no_hobo[1])
  expect_true(all(!is.na(data$do)))
  expect_true(all(!is.na(data$temp)))
})

test_that("process_hobo works with type = 'temp' (HOBO Pro v2)", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  # Minimal HOBO Pro v2 CSV: 2 header rows + 2 data rows, Chinese AM/PM locale
  writeLines(c(
    '"Plot Title","HOBO Pro v2",,',
    '"#","Date Time","Temp, °C","RH, %"',
    paste0('"1","10/15/2025 上午10時30分00秒","28.5","75.2"'),
    paste0('"2","10/15/2025 上午11時00分00秒","29.0","74.5"')
  ), tmp)

  data <- process_hobo(tmp, no_hobo = "test", type = "temp")

  expect_identical(c("date_time", "air_temp", "rh", "no_hobo"), names(data))
  expect_identical("test", data$no_hobo[1])
  expect_true(is.numeric(data$air_temp))
  expect_true(is.numeric(data$rh))
  expect_true(inherits(data$date_time, "POSIXct"))
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
