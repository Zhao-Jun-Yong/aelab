utils::globalVariables(c("cur_column"))

#' @title descriptive_statistic
#' @description Compute grouped mean ± SD and min–max summary statistics for
#'   one or more numeric variables.
#' @param data A data frame.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to summarise.
#' @param groups <[`tidy-select`][dplyr::dplyr_tidy_select]> Grouping columns.
#' @param digits Number of decimal places to round to. Default is 2.
#' @return A tibble with one row per group and two summary columns per variable
#'   (`<var>_mean_sd` and `<var>_min_max`).
#' @examples
#' df <- data.frame(group = c("A","A","B","B"), value = c(1.1, 2.3, 3.5, 4.7))
#' descriptive_statistic(df, vars = value, groups = group)
#' @importFrom dplyr group_by summarise across
#' @export
descriptive_statistic <- function(data, vars, groups, digits = 2) {
  data |>
    dplyr::group_by(dplyr::across({{ groups }}, as.character)) |>
    dplyr::summarise(
      dplyr::across(
        {{ vars }},
        .fns = list(
          mean_sd = ~ paste(
            format(round(mean(., na.rm = TRUE), digits), scientific = FALSE),
            format(round(sd(., na.rm = TRUE), digits), scientific = FALSE),
            sep = " \u00B1 "
          ),
          min_max = ~ paste(
            format(round(min(., na.rm = TRUE), digits), scientific = FALSE),
            format(round(max(., na.rm = TRUE), digits), scientific = FALSE),
            sep = " to "
          )
        )
      ),
      .groups = "drop"
    )
}


#' @title normality_test_t
#' @description Test normality of a variable within two groups using Shapiro-Wilk
#'   on raw, square-root, and log10 transforms (for t-test context).
#' @param df A data frame.
#' @param variable_name Name of the numeric variable column (string).
#' @param group <[`data-masking`][dplyr::dplyr_data_masking]> The grouping column.
#' @param group_1 Value identifying the first group.
#' @param group_2 Value identifying the second group.
#' @return A tibble with Shapiro-Wilk p-values for each group × transformation
#'   combination.
#' @examples
#' df <- data.frame(
#'   grp = c("A","A","A","B","B","B"),
#'   val = c(1.1, 2.0, 1.5, 4.2, 3.8, 4.5)
#' )
#' normality_test_t(df, "val", grp, "A", "B")
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom stats shapiro.test
#' @export
normality_test_t <- function(df, variable_name, group, group_1, group_2) {
  df <- df[!is.na(df[[variable_name]]), ]
  variable <- df[[variable_name]]
  max_value <- max(variable) + 1

  df_1 <- dplyr::filter(df, {{ group }} == group_1)
  df_2 <- dplyr::filter(df, {{ group }} == group_2)

  test_1      <- stats::shapiro.test(df_1[[variable_name]])
  test_2      <- stats::shapiro.test(df_2[[variable_name]])
  test_sqrt_1 <- stats::shapiro.test(sqrt(max_value - df_1[[variable_name]]))
  test_sqrt_2 <- stats::shapiro.test(sqrt(max_value - df_2[[variable_name]]))
  test_log_1  <- stats::shapiro.test(log10(max_value - df_1[[variable_name]]))
  test_log_2  <- stats::shapiro.test(log10(max_value - df_2[[variable_name]]))

  tibble::tibble(
    variable       = variable_name,
    group          = c(group_1, group_2, group_1, group_2, group_1, group_2),
    p_value        = c(test_1$p.value, test_2$p.value,
                       test_sqrt_1$p.value, test_sqrt_2$p.value,
                       test_log_1$p.value, test_log_2$p.value),
    transformation = c("None", "None",
                       "Square root", "Square root",
                       "Logarithm", "Logarithm")
  )
}


#' @title normality_test_aov
#' @description Test normality of ANOVA model residuals using Shapiro-Wilk on
#'   raw, square-root, and log10 transforms (one-way or two-way).
#' @param df A data frame.
#' @param variable_name Name of the response variable column (string).
#' @param group_1 Name of the first grouping column (string).
#' @param group_2 Name of the second grouping column (string), or \code{NULL}
#'   for a one-way model.
#' @return A tibble with Shapiro-Wilk p-values for each transformation.
#' @examples
#' df <- data.frame(
#'   grp = c("A","A","B","B"),
#'   val = c(1.1, 1.4, 3.2, 3.8)
#' )
#' normality_test_aov(df, "val", "grp")
#' @importFrom tibble tibble
#' @importFrom stats lm shapiro.test
#' @export
normality_test_aov <- function(df, variable_name, group_1, group_2 = NULL) {
  df       <- as.data.frame(df)
  df       <- df[!is.na(df[[variable_name]]), ]
  variable <- df[[variable_name]]
  max_value <- max(variable) + 1

  if (is.null(group_2)) {
    model1 <- stats::lm(variable ~ df[[group_1]], data = df)
    test1  <- stats::shapiro.test(model1$residuals)
    model2 <- stats::lm(sqrt(max_value - variable) ~ df[[group_1]], data = df)
    test2  <- stats::shapiro.test(model2$residuals)
    model3 <- stats::lm(log10(max_value - variable) ~ df[[group_1]], data = df)
    test3  <- stats::shapiro.test(model3$residuals)
  } else {
    model1 <- stats::lm(variable ~ df[[group_1]] * df[[group_2]], data = df)
    test1  <- stats::shapiro.test(model1$residuals)
    model2 <- stats::lm(sqrt(max_value - variable) ~ df[[group_1]] * df[[group_2]], data = df)
    test2  <- stats::shapiro.test(model2$residuals)
    model3 <- stats::lm(log10(max_value - variable) ~ df[[group_1]] * df[[group_2]], data = df)
    test3  <- stats::shapiro.test(model3$residuals)
  }

  tibble::tibble(
    variable       = variable_name,
    p_value        = c(test1$p.value, test2$p.value, test3$p.value),
    transformation = c("None", "Square root", "Logarithm")
  )
}


#' @title aov_test
#' @description Perform one-way ANOVA followed by Tukey HSD post-hoc test with
#'   compact letter display.
#' @param df A data frame.
#' @param variable_name Name of the response variable column (string).
#' @param group Name of the grouping column (string).
#' @return A named list with elements \code{anova_summary}, \code{tukey_results},
#'   and \code{compact_letters}.
#' @examples
#' df <- data.frame(
#'   grp = rep(c("A","B","C"), each = 5),
#'   val = c(1,2,1,2,1, 3,4,3,4,3, 5,6,5,6,5)
#' )
#' aov_test(df, "val", "grp")
#' @importFrom stats aov TukeyHSD as.formula
#' @importFrom multcompView multcompLetters4
#' @export
aov_test <- function(df, variable_name, group) {
  formula        <- stats::as.formula(paste(variable_name, "~", group))
  results        <- stats::aov(formula, data = df)
  anova_summary  <- summary(results)
  print(anova_summary)
  tukey_results  <- stats::TukeyHSD(results)
  label          <- multcompView::multcompLetters4(results, tukey_results)
  print(label)

  list(
    anova_summary   = anova_summary,
    tukey_results   = tukey_results,
    compact_letters = label
  )
}


#' @title ks_test
#' @description Perform Kruskal-Wallis test followed by Dunn post-hoc test
#'   (Bonferroni correction) with compact letter display.
#' @param df A data frame.
#' @param variable_name Name of the response variable column (string).
#' @param group Name of the grouping column (string).
#' @return A named list with elements \code{ks_results}, \code{dunn_results},
#'   \code{mean_summary}, and \code{compact_letters}.
#' @examples
#' df <- data.frame(
#'   grp = rep(c("A","B","C"), each = 5),
#'   val = c(1,2,1,2,1, 3,4,3,4,3, 5,6,5,6,5)
#' )
#' ks_test(df, "val", "grp")
#' @importFrom stats kruskal.test as.formula
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym !!
#' @importFrom FSA dunnTest
#' @importFrom rcompanion cldList
#' @export
ks_test <- function(df, variable_name, group) {
  if (!variable_name %in% names(df)) {
    stop(paste("Variable", variable_name, "not found in dataframe."))
  }
  if (!group %in% names(df)) {
    stop(paste("Group", group, "not found in dataframe."))
  }

  formula    <- stats::as.formula(paste(variable_name, "~", group))
  ks_results <- stats::kruskal.test(formula, data = df)

  # cldList -> multcompLetters -> vec2mat2 splits comparison names on "-",
  # expecting exactly one per name. Sanitize group names containing "-" first.
  orig_levels <- as.character(unique(df[[group]]))
  safe_levels <- gsub("-", ".", orig_levels, fixed = TRUE)
  level_map   <- stats::setNames(orig_levels, safe_levels)  # safe -> orig

  df_safe          <- df
  df_safe[[group]] <- gsub("-", ".", as.character(df[[group]]), fixed = TRUE)

  dunn_results <- FSA::dunnTest(df_safe[[variable_name]], df_safe[[group]], method = "bonferroni")
  dunn_results <- dunn_results$res

  label <- rcompanion::cldList(
    comparison = dunn_results$Comparison,
    p.value    = dunn_results$P.adj,
    threshold  = 0.05
  )

  # Restore original group names (replace longest safe names first to avoid
  # partial-match corruption, e.g. "CR.I" vs "CR.I1")
  sorted_safe <- names(level_map)[order(nchar(names(level_map)), decreasing = TRUE)]
  label$Group <- level_map[label$Group]
  dunn_results$Comparison <- Reduce(
    function(s, safe) gsub(safe, level_map[[safe]], s, fixed = TRUE),
    sorted_safe,
    dunn_results$Comparison
  )

  mean_summary <- df |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarise(mean = mean(!!rlang::sym(variable_name), na.rm = TRUE),
                     .groups = "drop")

  list(
    ks_results      = ks_results,
    dunn_results    = dunn_results,
    mean_summary    = mean_summary,
    compact_letters = label
  )
}



#' @title sig_labels
#' @description Run \code{\link{ks_test}} separately for each level of a
#'   faceting variable and return compact letter display (CLD) annotations
#'   ready for \code{geom_text}. Only facets where the Kruskal-Wallis p-value
#'   is below \code{alpha} are included in the output.
#' @param stat_data Data frame of raw observations used for the statistical
#'   test. Should already be filtered to the relevant subset (e.g. a single
#'   water type).
#' @param variable Name of the response variable column (string).
#' @param group Name of the grouping column (string).
#' @param by Name of the faceting column whose levels are iterated over
#'   (string, default \code{"year"}).
#' @param plot_data Data frame used to compute label y-positions via
#'   \code{max(variable)} per group. Defaults to \code{stat_data}. Pass the
#'   aggregated/boxplot data here when it differs from the raw test data.
#' @param alpha Significance threshold; facets with KW p-value >=
#'   \code{alpha} are dropped (default \code{0.05}).
#' @return A data frame with columns \code{<group>}, \code{Letter},
#'   \code{MonoLetter}, \code{y_pos}, and \code{<by>}. Returns an empty data
#'   frame when no facet reaches significance.
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   year = rep(c("2023", "2024"), each = 20),
#'   grp  = rep(c("A","B","C","D"), 10),
#'   val  = c(rnorm(20, mean = rep(c(1,3,2,6), 5)), rnorm(20))
#' )
#' sig_labels(df, "val", "grp", by = "year")
#' @importFrom purrr map_df
#' @export
sig_labels <- function(stat_data, variable, group, by = "year",
                       plot_data = NULL, alpha = 0.05) {
  if (is.null(plot_data)) plot_data <- stat_data
  purrr::map_df(as.character(unique(stat_data[[by]])), function(val) {
    d_s <- stat_data[as.character(stat_data[[by]]) == val, ]
    d_p <- plot_data[as.character(plot_data[[by]]) == val, ]
    if (length(unique(d_s[[group]])) < 2) return(data.frame())
    res <- tryCatch(ks_test(d_s, variable, group), error = function(e) NULL)
    if (is.null(res) || res$ks_results$p.value >= alpha) return(data.frame())
    lbl           <- res$compact_letters
    names(lbl)[names(lbl) == "Group"] <- group
    ypos          <- tapply(d_p[[variable]], d_p[[group]], max, na.rm = TRUE)
    lbl$y_pos     <- ypos[as.character(lbl[[group]])]
    lbl[[by]]     <- val
    lbl
  })
}


#' @title df_trans
#' @description Apply a reverse square-root or reverse log transformation to a
#'   numeric column and append the result as a new column.
#' @param df A data frame.
#' @param variable_name Name of the column to transform (string).
#' @param transformation Transformation type: \code{"sqrt"} or \code{"log"}.
#' @return The input data frame with an additional column named
#'   \code{<variable_name>_sqrt} or \code{<variable_name>_log}.
#' @examples
#' df <- data.frame(val = c(1, 4, 9, 16))
#' df_trans(df, "val", "sqrt")
#' @export
df_trans <- function(df, variable_name, transformation) {
  max_val <- max(df[[variable_name]], na.rm = TRUE)
  if (transformation == "sqrt") {
    df[[paste0(variable_name, "_sqrt")]] <- sqrt((max_val + 1) - df[[variable_name]])
  } else if (transformation == "log") {
    df[[paste0(variable_name, "_log")]]  <- log((max_val + 1) - df[[variable_name]])
  }
  return(df)
}


#' @title find_outlier
#' @description Identify outliers in a numeric column using the IQR method
#'   (values outside 1.5 \eqn{\times} IQR from Q1/Q3).
#' @param df A data frame.
#' @param var Name of the column to check for outliers (string).
#' @param other_var Character vector of additional column names to return
#'   alongside the outlier values, or \code{NULL}.
#' @return A tibble with columns \code{row_index}, \code{outlier_value}, and any
#'   requested \code{other_var} columns.
#' @examples
#' df <- data.frame(val = c(1, 2, 2, 3, 100), id = 1:5)
#' find_outlier(df, "val", "id")
#' @importFrom tibble tibble
#' @importFrom stats quantile
#' @export
find_outlier <- function(df, var, other_var = NULL) {
  Q3  <- stats::quantile(df[[var]], 0.75, na.rm = TRUE)
  Q1  <- stats::quantile(df[[var]], 0.25, na.rm = TRUE)
  IQR <- Q3 - Q1
  outlier_indices <- which(df[[var]] < (Q1 - 1.5 * IQR) | df[[var]] > (Q3 + 1.5 * IQR))
  outlier_values  <- df[outlier_indices, var]

  if (!is.null(other_var)) {
    additional_values <- df[outlier_indices, other_var, drop = FALSE]
    tibble::tibble(row_index = outlier_indices, outlier_value = outlier_values,
                   additional_values)
  } else {
    tibble::tibble(row_index = outlier_indices, outlier_value = outlier_values)
  }
}
