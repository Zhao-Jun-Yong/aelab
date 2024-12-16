test_that("tidy_ghg_analyzer works correctly", {
  data <- tidy_ghg_analyzer(test_file("ch4.xlsx"), gas = "ch4")
  expect_true(!is.null(data))
  column_names <- c("DATE", "TIME", "CO2", "CH4", "date_time")
  expect_identical(column_names, names(data))
})

test_that("convert_time adjusts datetime correctly", {
  data <- data.frame(date_time = c("2024/12/16 12:00:00"))
  result <- convert_time(data, hr = 1, min = 30)

  expect_equal(result$real_datetime[1], as.POSIXct("2024-12-16 13:30:00"))
})

test_that("calculate_regression returns correct results", {
  # Prepare mock data
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = 420),
    CH4 = sample(1:100, 420, replace = TRUE)
  )

  reference_time <- as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei")
  result <- calculate_regression(data, "CH4", reference_time)

  expect_equal(nrow(result), 1)  # Expecting one result
  expect_true("slope" %in% colnames(result))  # Check if slope is calculated
})

test_that("convert_ghg_unit converts correctly", {
  result <- convert_ghg_unit(1, "co2", mass = "mmol")
  expect_equal(result, 1000, tolerance = 0.01)  # Example expected value
})

test_that("convert_ghg_unit throws error for invalid GHG", {
  expect_error(convert_ghg_unit(1, "invalid_gas"), "Invalid GHG type. Please use 'co2', 'ch4', or 'n2o'.")
})
