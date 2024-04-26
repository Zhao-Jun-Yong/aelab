test_that("tidy_licor output existing file", {
  data <- tidy_licor(test_file("raw_n2o.xlsx"), gas = "n2o")
  expect_true(!is.null(data))
})

test_that("tidy_licor produces data with specific column names", {
  data <- tidy_licor(test_file("raw_n2o.xlsx"), gas = "n2o")
  column_names <- c("DATE", "TIME", "N2O", "date_time")
  expect_identical(column_names, names(data))
})


