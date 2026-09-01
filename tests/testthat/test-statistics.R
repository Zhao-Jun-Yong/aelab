test_that("descriptive_statistic summarises mean, sd, min, max per group", {
  df <- data.frame(group = c("A", "A", "B", "B"), value = c(1.1, 2.3, 3.5, 4.7))
  res <- descriptive_statistic(df, vars = value, groups = group)
  expect_equal(res$value_mean_sd[res$group == "A"], "1.70 ± 0.85")
  expect_equal(res$value_min_max[res$group == "A"], "1.10 to 2.30")
})

test_that("descriptive_statistic handles multiple variables", {
  df <- data.frame(
    group = c("A", "A", "B", "B"),
    x = c(1, 2, 3, 4),
    y = c(10, 20, 30, 40)
  )
  res <- descriptive_statistic(df, vars = c(x, y), groups = group)
  expect_named(res, c("group", "x_mean_sd", "x_min_max", "y_mean_sd", "y_min_max"))
})

test_that("descriptive_statistic handles multiple grouping columns", {
  df <- data.frame(
    site = c("S1", "S1", "S2", "S2"),
    year = c(2020, 2021, 2020, 2021),
    value = c(1, 2, 3, 4)
  )
  res <- descriptive_statistic(df, vars = value, groups = c(site, year))
  expect_equal(nrow(res), 4)
  expect_named(res, c("site", "year", "value_mean_sd", "value_min_max"))
})

test_that("descriptive_statistic ignores NA within a group", {
  df <- data.frame(group = c("A", "A", "A"), value = c(1, 2, NA))
  res <- descriptive_statistic(df, vars = value, groups = group)
  expect_equal(res$value_mean_sd, "1.50 ± 0.71")
})

test_that("descriptive_statistic returns NA (not Inf/NaN) for an all-NA group", {
  df <- data.frame(group = c("A", "A"), value = c(NA_real_, NA_real_))
  res <- descriptive_statistic(df, vars = value, groups = group)
  expect_equal(res$value_mean_sd, NA_character_)
  expect_equal(res$value_min_max, NA_character_)
})

test_that("descriptive_statistic keeps SD as NA for a single-observation group", {
  df <- data.frame(group = "A", value = 5.2)
  res <- descriptive_statistic(df, vars = value, groups = group)
  expect_equal(res$value_mean_sd, "5.20 ± NA")
  expect_equal(res$value_min_max, "5.20 to 5.20")
})

test_that("descriptive_statistic formats decimals consistently including trailing zeros", {
  df <- data.frame(group = "A", value = 2)
  res <- descriptive_statistic(df, vars = value, groups = group, digits = 3)
  expect_equal(res$value_min_max, "2.000 to 2.000")
})

test_that("descriptive_statistic errors on invalid digits", {
  df <- data.frame(group = "A", value = 1)
  expect_error(descriptive_statistic(df, vars = value, groups = group, digits = -1))
  expect_error(descriptive_statistic(df, vars = value, groups = group, digits = 1.5))
  expect_error(descriptive_statistic(df, vars = value, groups = group, digits = NA))
  expect_error(descriptive_statistic(df, vars = value, groups = group, digits = c(1, 2)))
})

test_that("descriptive_statistic errors on non-numeric vars", {
  df <- data.frame(group = "A", value = "x", stringsAsFactors = FALSE)
  expect_error(descriptive_statistic(df, vars = value, groups = group), "numeric")
})

test_that("descriptive_statistic preserves grouping column classes", {
  df <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    grp  = factor(c("A", "B")),
    value = c(1, 2)
  )
  res <- descriptive_statistic(df, vars = value, groups = c(date, grp))
  expect_s3_class(res$date, "Date")
  expect_s3_class(res$grp, "factor")
})

test_that("descriptive_statistic handles empty data", {
  df <- data.frame(group = character(), value = numeric())
  res <- descriptive_statistic(df, vars = value, groups = group)
  expect_equal(nrow(res), 0)
})

test_that("ks_test returns correct structure", {
  df <- data.frame(
    grp = rep(c("A", "B", "C"), each = 5),
    val = c(1, 2, 1, 2, 1, 3, 4, 3, 4, 3, 5, 6, 5, 6, 5)
  )
  res <- ks_test(df, "val", "grp")
  expect_named(res, c("ks_results", "dunn_results", "mean_summary", "compact_letters"))
  expect_s3_class(res$ks_results, "htest")
  expect_true(all(c("Group", "Letter") %in% names(res$compact_letters)))
})

test_that("ks_test handles group names containing hyphens without error", {
  df <- data.frame(
    grp = rep(c("CR-I", "CR-II", "CR-III"), each = 5),
    val = c(1, 2, 1, 2, 1, 3, 4, 3, 4, 3, 5, 6, 5, 6, 5)
  )
  res <- ks_test(df, "val", "grp")
  expect_true(all(res$compact_letters$Group %in% c("CR-I", "CR-II", "CR-III")))
  expect_true(all(grepl("-", res$dunn_results$Comparison)))
})

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
