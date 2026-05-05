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
