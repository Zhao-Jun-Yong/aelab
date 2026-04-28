test_that("sig_labels returns non-empty data frame for data with clear group differences", {
  df <- data.frame(
    year = rep("2023", 20),
    grp  = rep(c("A", "B", "C", "D"), each = 5),
    val  = c(rep(1, 5), rep(10, 5), rep(1, 5), rep(10, 5))
  )
  result <- sig_labels(df, "val", "grp", by = "year")
  expect_gt(nrow(result), 0)
})

test_that("sig_labels returns empty data frame when no significant difference", {
  set.seed(1)
  df <- data.frame(
    year = rep("2023", 20),
    grp  = rep(c("A", "B", "C", "D"), 5),
    val  = rnorm(20)
  )
  result <- sig_labels(df, "val", "grp", by = "year")
  expect_equal(nrow(result), 0)
})

test_that("sig_labels output contains the by column", {
  set.seed(1)
  df <- data.frame(
    year = rep(c("2023", "2024"), each = 20),
    grp  = rep(c("A", "B", "C", "D"), 10),
    val  = c(rnorm(20, mean = rep(c(1, 3, 2, 6), 5)), rnorm(20))
  )
  result <- sig_labels(df, "val", "grp", by = "year")
  expect_true("year" %in% names(result))
})

test_that("plot_data overrides y_pos relative to using stat_data alone", {
  set.seed(1)
  df <- data.frame(
    year = rep("2023", 20),
    grp  = rep(c("A", "B", "C", "D"), 5),
    val  = rnorm(20, mean = rep(c(1, 3, 2, 6), 5))
  )
  df_inflated <- df
  df_inflated$val <- df$val + 100

  res_default  <- sig_labels(df, "val", "grp", by = "year")
  res_plotdata <- sig_labels(df, "val", "grp", by = "year", plot_data = df_inflated)

  expect_false(identical(res_default$y_pos, res_plotdata$y_pos))
})
