utils::globalVariables(c("CO2", "CH4", "N2O"))

#' @title tidy_ghg_analyzer
#' @importFrom lubridate ymd_hms days hours minutes seconds force_tz
#' @importFrom readxl read_excel
#' @importFrom openxlsx convertToDateTime
#' @importFrom dplyr filter
#' @description Tidy the data downloaded from GHG Analyzer, with optional timestamp correction.
#' @param file_path Directory of file.
#' @param gas Choose between CO2/CH4 or N2O LI-COR Trace Gas Analyzer, which is "ch4" and "n2o", respectively.
#' @param analyzer The brand of the analyzer which the data was downloaded from.
#' @param day Day offset to correct the analyzer timestamp.
#' @param hr Hour offset to correct the analyzer timestamp.
#' @param min Minute offset to correct the analyzer timestamp.
#' @param sec Second offset to correct the analyzer timestamp.
#' @return Return the loaded XLSX file after tidying. If any time offset is non-zero, a
#'   \code{real_datetime} column is added via \code{\link{convert_time}}.
#' @examples
#' ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab")
#' tidy_ghg_analyzer(ghg_data_path, "ch4")
#' tidy_ghg_analyzer(ghg_data_path, "ch4", sec = -30)
#' @export

tidy_ghg_analyzer <- function(file_path, gas, analyzer = "licor",
                               day = 0, hr = 0, min = 0, sec = 0) {

  # Import data from the specified Excel file
  data <- readxl::read_excel(file_path)

  # Tidy data based on the type of analyzer
  if(analyzer == "licor") {
    if (gas == "ch4") {
      # Extract column titles for CH4 data
      title <- data[5, c(7:8, 10:11)]
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:6), c(7:8, 10:11)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      # Filter out rows with NaN values in CH4 and CO2
      data <- dplyr::filter(data, CH4 != "NaN", CO2 != "NaN")
    } else if (gas == "n2o") {
      # Extract column titles for N2O data
      title <- data[5, c(7:8, 10)]
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:6), c(7:8, 10)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$N2O <- as.numeric(data$N2O)  # Convert N2O to numeric
      # Filter out rows with NaN values in N2O
      data <- dplyr::filter(data, N2O != "NaN")
    }

    # Convert TIME and DATE columns to date-time format
    data$TIME <- openxlsx::convertToDateTime(data$TIME)
    data$DATE <- openxlsx::convertToDateTime(data$DATE)
    # Format TIME and DATE for readability
    data$TIME <- format(data$TIME, "%H:%M:%S")
    data$DATE <- format(data$DATE, "%Y/%m/%d")
    # Combine DATE and TIME into a single date_time column
    data$date_time <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y/%m/%d %H:%M:%S")
  } else if(analyzer == "lgr") {
    if (gas == "ch4") {
      # Set column titles for LGR CH4 data
      title <- c("date_time", "CH4", "CO2", "TEMP")
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:2), c(1, 8, 10, 14)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      # Clean up date_time format by removing milliseconds
      data$date_time <- sub("\\.\\d+", "", data$date_time)
      # Convert date_time to POSIXct format
      data$date_time <- as.POSIXct(data$date_time, format = "%m/%d/%Y %H:%M:%S")
    }
  }

  # Apply timestamp correction if any offset is provided
  if (day != 0 || hr != 0 || min != 0 || sec != 0) {
    data <- convert_time(data, day = day, hr = hr, min = min, sec = sec)
  }

  return(data)

}

#' @title convert_time
#' @description Convert the time of the LI-COR Trace Gas Analyzer to match the time in real life.
#' @param data Data from the LI-COR Trace Gas Analyzer that had been processed by tidy_licor().
#' @param day Day(s) to add or subtract.
#' @param hr Hour(s) to add or subtract.
#' @param min Minute(s) to add or subtract.
#' @param sec Second(s) to add or subtract.
#' @return The input data with a new column in POSIXct format converted based on the input value.
#' @examples
#' data(n2o)
#' converted_n2o <- convert_time(n2o, min = -10, sec = 5)
#' @export

convert_time <- function(data, day = 0, hr = 0, min = 0, sec = 0) {

  # Convert 'date_time' column to POSIXct format
  data$date_time <- lubridate::ymd_hms(data$date_time)

  # Add specified time adjustments to the date_time (lubridate preserves POSIXct directly)
  data$real_datetime <- data$date_time +
    lubridate::days(day) +
    lubridate::hours(hr) +
    lubridate::minutes(min) +
    lubridate::seconds(sec)

  # Return the modified data frame
  return(data)

}

# Internal helper: fit a GHG concentration curve and return slope, R², curvature b, and fit_used.
# kappamax:            if provided and b > kappamax, exponential fits fall back to linear.
# instrument_precision: only used when fit_type = "auto" to compute kappamax per window.
.fit_ghg_model <- function(conc, elapsed_s, fit_type, t_zero,
                           kappamax = NULL, instrument_precision = NULL) {
  C0     <- conc[1]
  ss_tot <- sum((conc - mean(conc))^2)

  .lm_result <- function() {
    fit <- stats::lm(conc ~ elapsed_s)
    list(slope = unname(stats::coef(fit)[2]), r_square = summary(fit)$r.squared,
         b_param = NA_real_, fit_used = "linear")
  }

  .quad_result <- function() {
    fit <- stats::lm(conc ~ elapsed_s + I(elapsed_s^2))
    list(slope = unname(stats::coef(fit)[2]), r_square = summary(fit)$r.squared,
         b_param = NA_real_, fit_used = "quadratic")
  }

  .exp_tz_result <- function(kmax) {
    Cm_start <- conc[length(conc)]
    fit <- tryCatch(
      stats::nls(conc ~ Cm + a * elapsed_s + (C0 - Cm) * exp(-b * elapsed_s),
                 start   = list(Cm = Cm_start, a = 0, b = 0.001),
                 control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    cf    <- stats::coef(fit)
    b_val <- unname(cf["b"])
    if (!is.null(kmax) && !is.na(b_val) && b_val > kmax) return(NULL)
    slope <- cf["a"] + cf["b"] * (cf["Cm"] - C0) * exp(-cf["b"] * t_zero)
    r_sq  <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
    list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_tz")
  }

  if (fit_type == "linear")    return(.lm_result())
  if (fit_type == "quadratic") return(.quad_result())

  if (fit_type == "exp_tz") {
    Cm_start <- conc[length(conc)]
    fit <- tryCatch(
      stats::nls(conc ~ Cm + a * elapsed_s + (C0 - Cm) * exp(-b * elapsed_s),
                 start   = list(Cm = Cm_start, a = 0, b = 0.001),
                 control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL)
    if (is.null(fit)) return(list(slope = NA_real_, r_square = NA_real_,
                                  b_param = NA_real_, fit_used = "exp_tz"))
    cf    <- stats::coef(fit)
    b_val <- unname(cf["b"])
    if (!is.null(kappamax) && !is.na(b_val) && b_val > kappamax) {
      lin <- stats::lm(conc ~ elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = b_val, fit_used = "linear"))
    }
    slope <- cf["a"] + cf["b"] * (cf["Cm"] - C0) * exp(-cf["b"] * t_zero)
    r_sq  <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
    return(list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_tz"))
  }

  if (fit_type == "exp_zhao18") {
    Cm_start <- conc[length(conc)]
    fit <- tryCatch(
      stats::nls(conc ~ Cm + a * (elapsed_s - t0) + (C0 - Cm) * exp(-b * (elapsed_s - t0)),
                 start   = list(Cm = Cm_start, a = 0, b = 0.001, t0 = 0),
                 control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL)
    if (is.null(fit)) return(list(slope = NA_real_, r_square = NA_real_,
                                  b_param = NA_real_, fit_used = "exp_zhao18"))
    cf    <- stats::coef(fit)
    b_val <- unname(cf["b"])
    if (!is.null(kappamax) && !is.na(b_val) && b_val > kappamax) {
      lin <- stats::lm(conc ~ elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = b_val, fit_used = "linear"))
    }
    slope <- cf["a"] + cf["b"] * (cf["Cm"] - C0)
    r_sq  <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
    return(list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_zhao18"))
  }

  if (fit_type == "auto") {
    # Compute kappamax from instrument precision and initial concentration
    duration_s <- max(elapsed_s) - min(elapsed_s)
    kmax <- if (!is.null(instrument_precision) && C0 > 0 && duration_s > 0)
      instrument_precision / (C0 * duration_s)
    else
      1.5 / max(duration_s, 1)

    # Step 1: linear (baseline)
    best <- .lm_result()

    # Step 2: quadratic — accept if meaningfully better (ΔR² > 0.01)
    quad <- .quad_result()
    if (!is.na(quad$r_square) && !is.na(best$r_square) &&
        quad$r_square > best$r_square + 0.01) {
      best <- quad
    }

    # Step 3: exp_tz with kappamax — accept if κ passes and meaningfully better
    exp_res <- .exp_tz_result(kmax)
    if (!is.null(exp_res) && !is.na(exp_res$r_square) && !is.na(best$r_square) &&
        exp_res$r_square > best$r_square + 0.01) {
      best <- exp_res
    }

    return(best)
  }

  stop(paste("Unknown fit_type:", fit_type,
             ". Use 'linear', 'quadratic', 'exp_tz', 'exp_zhao18', or 'auto'."))
}

#' @import tibble
#' @importFrom stats lm coef nls nls.control residuals cor
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @title calculate_regression
#' @description Calculate the slope of GHG concentration change over time using linear or exponential regression.
#' @param data Data from the GHG analyzer that has been processed and time-converted.
#' @param reference_df A data frame containing measurement reference times, site names, and an \code{analyzer} column.
#' @param ghg Column name in \code{data} containing GHG concentration values (e.g., \code{"CH4"}, \code{"N2O"}).
#' @param reference_time Column name in \code{reference_df} containing the measurement start times (POSIXct).
#' @param site Column name in \code{reference_df} containing site identifiers.
#' @param analyzer_code String pattern used to filter results by the \code{analyzer} column of \code{reference_df}.
#' @param duration_minutes The duration of the measurement window in minutes. Default 7.
#' @param num_rows The number of rows used per sliding regression window. Only used when
#'   \code{window_type = "sliding"}. Default 300.
#' @param fit_type Model used to fit the concentration curve. One of \code{"linear"} (default),
#'   \code{"quadratic"} (second-order polynomial; slope is the initial rate at t=0),
#'   \code{"exp_tz"} (exponential with linear drift and user-specified \code{t_zero}),
#'   \code{"exp_zhao18"} (exponential with linear drift and estimated t0), or
#'   \code{"auto"} (automatic model selection: tries linear → quadratic → exp_tz in order,
#'   accepting a more complex model only when R² improves by > 0.01 and, for exp_tz, the
#'   curvature parameter κ does not exceed \code{kappamax}). When \code{fit_type = "auto"} and
#'   \code{instrument_precision = NULL}, precision is looked up from \code{\link{analyzer_precision}}
#'   by gas name. The model chosen for each measurement is recorded in the \code{fit_used} output column.
#' @param t_zero Reference time in seconds from window start for the \code{"exp_tz"} slope calculation. Default 0.
#' @param window_type \code{"sliding"} (default) selects the \code{num_rows}-length sub-window with
#'   the best R² within the measurement period. \code{"fixed"} fits a single window starting at
#'   \code{start_offset_s} seconds after the reference time and running for \code{duration_minutes}.
#'   Use \code{"fixed"} to ensure all gases are regressed over the same time window.
#' @param start_offset_s Seconds to skip from the reference time before the regression window starts.
#'   Only used when \code{window_type = "fixed"}. Default 0.
#' @param r2_threshold Minimum R-squared for a measurement to pass quality check. Default 0.8.
#' @param cor_threshold Minimum absolute Pearson correlation between concentration and elapsed time.
#'   Measurements below this have no detectable trend and are flagged \code{"zero"}. Default 0.5.
#' @param instrument_precision Optional. Analyser precision in ppm. When provided, measurements
#'   whose absolute slope falls below \code{instrument_precision / window_duration_s} are flagged
#'   \code{"zero"} even if R² and correlation pass.
#' @param kappamax Optional. Maximum acceptable exponential curvature parameter \eqn{b} (s\eqn{^{-1}}).
#'   Only applies when \code{fit_type} is \code{"exp_tz"} or \code{"exp_zhao18"}. When \eqn{b > \kappa_{max}},
#'   the fit implies an implausibly fast equilibration and the slope is replaced by the linear
#'   equivalent. Corresponds to the \emph{kappamax} quality criterion in Hüppi et al. (2018).
#' @return A tibble with columns: \code{start_time}, \code{end_time}, \code{slope} (ppm s\eqn{^{-1}}),
#'   \code{r_square}, \code{correlation}, \code{b_param} (exponential curvature; \code{NA} for
#'   linear/quadratic), \code{fit_used} (model actually used: \code{"linear"}, \code{"quadratic"},
#'   or \code{"exp_tz"}; always matches \code{fit_type} unless \code{fit_type = "auto"}),
#'   \code{flag} (\code{"ok"}, \code{"discard"}, \code{"zero"}, or \code{"no_data"}),
#'   \code{reference_time}, \code{site}, \code{analyzer}.
#' @examples
#' data(n2o)
#' n2o_converted <- convert_time(n2o)
#' ref <- data.frame(
#'   date_time = n2o_converted$real_datetime[1],
#'   site = "S1",
#'   analyzer = "1074"
#' )
#' calculate_regression(n2o_converted, ref, "N2O",
#'                      reference_time = "date_time", site = "site",
#'                      analyzer_code = "1074")
#' @export

calculate_regression <- function(data, reference_df, ghg,
                                 reference_time, site, analyzer_code,
                                 duration_minutes = 7, num_rows = 300,
                                 fit_type = "linear", t_zero = 0,
                                 window_type = "sliding", start_offset_s = 0,
                                 r2_threshold = 0.8, cor_threshold = 0.5,
                                 instrument_precision = NULL,
                                 kappamax = NULL) {

  fit_type    <- match.arg(fit_type,    c("linear", "quadratic", "exp_tz", "exp_zhao18", "auto"))
  window_type <- match.arg(window_type, c("sliding", "fixed"))

  # For fit_type = "auto", resolve instrument_precision from the built-in table if not supplied
  if (fit_type == "auto" && is.null(instrument_precision)) {
    prec_row <- analyzer_precision[analyzer_precision$gas == toupper(ghg), ]
    if (nrow(prec_row) > 0) instrument_precision <- prec_row$precision_ppm[1]
  }

  reference_time_vec <- lubridate::force_tz(reference_df[[reference_time]], tzone = "Asia/Taipei")
  reference_site     <- as.character(reference_df[[site]])
  reference_analyzer <- as.character(reference_df$analyzer)

  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else {
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  results <- purrr::map_dfr(seq_along(reference_time_vec), function(i) {
    reference_datetime <- reference_time_vec[i]

    start_time <- reference_datetime
    end_time   <- reference_datetime + as.numeric(duration_minutes) * 60

    filtered_data <- data[data$real_datetime >= start_time & data$real_datetime <= end_time, ]
    sorted_data   <- filtered_data[order(filtered_data$real_datetime), ]

    best_r_square <- -Inf
    best_slope    <- NA_real_
    best_cor      <- NA_real_
    best_b_param  <- NA_real_
    best_fit_used <- fit_type
    best_start_dt <- structure(NA_real_, class = c("POSIXct", "POSIXt"))
    best_end_dt   <- structure(NA_real_, class = c("POSIXct", "POSIXt"))

    if (window_type == "fixed") {
      win_start     <- reference_datetime + start_offset_s
      win_end       <- win_start + as.numeric(duration_minutes) * 60
      selected_data <- sorted_data[sorted_data$real_datetime >= win_start &
                                   sorted_data$real_datetime <= win_end, ]

      if (nrow(selected_data) >= 2) {
        elapsed_s     <- as.numeric(difftime(selected_data$real_datetime,
                                             selected_data$real_datetime[1],
                                             units = "secs"))
        conc_vec      <- as.numeric(selected_data[[ghg]])
        fit_out       <- .fit_ghg_model(conc_vec, elapsed_s, fit_type, t_zero, kappamax,
                                        instrument_precision)
        best_r_square <- fit_out$r_square
        best_slope    <- fit_out$slope
        best_b_param  <- fit_out$b_param
        best_fit_used <- fit_out$fit_used
        best_cor      <- if (!is.na(fit_out$r_square)) stats::cor(conc_vec, elapsed_s) else NA_real_
        best_start_dt <- selected_data$real_datetime[1]
        best_end_dt   <- selected_data$real_datetime[nrow(selected_data)]
      }
    } else {
      if (nrow(sorted_data) >= num_rows) {
        for (j in 1:(nrow(sorted_data) - num_rows + 1)) {
          selected_data <- sorted_data[j:(j + num_rows - 1), ]

          elapsed_s <- as.numeric(difftime(selected_data$real_datetime,
                                           selected_data$real_datetime[1],
                                           units = "secs"))
          conc_vec <- as.numeric(selected_data[[ghg]])

          fit_out  <- .fit_ghg_model(conc_vec, elapsed_s, fit_type, t_zero, kappamax,
                                     instrument_precision)
          r_square <- fit_out$r_square

          if (!is.na(r_square) && r_square > best_r_square) {
            best_r_square <- r_square
            best_slope    <- fit_out$slope
            best_b_param  <- fit_out$b_param
            best_fit_used <- fit_out$fit_used
            best_cor      <- stats::cor(conc_vec, elapsed_s)
            best_start_dt <- selected_data$real_datetime[1]
            best_end_dt   <- selected_data$real_datetime[nrow(selected_data)]
          }
        }
      }
    }

    # Quality flag: correlation threshold distinguishes "zero" (no trend) from "discard" (poor fit)
    flag <- if (is.na(best_cor) || is.infinite(best_r_square)) {
      "no_data"
    } else if (abs(best_cor) < cor_threshold) {
      "zero"
    } else if (best_r_square >= r2_threshold) {
      "ok"
    } else {
      "discard"
    }

    # Minimum detectable slope check: override "ok" if slope is within instrument noise
    if (!is.null(instrument_precision) && flag == "ok" && !is.na(best_slope)) {
      duration_s <- as.numeric(difftime(best_end_dt, best_start_dt, units = "secs"))
      if (duration_s > 0 && abs(best_slope) < instrument_precision / duration_s) {
        flag <- "zero"
      }
    }

    tibble(
      start_time     = format(best_start_dt, "%Y/%m/%d %H:%M:%S"),
      end_time       = format(best_end_dt, "%Y/%m/%d %H:%M:%S"),
      slope          = best_slope,
      r_square       = best_r_square,
      correlation    = best_cor,
      b_param        = best_b_param,
      fit_used       = best_fit_used,
      flag           = flag,
      reference_time = reference_time_vec[i],
      site           = reference_site[i],
      analyzer       = reference_analyzer[i]
    )
  })

  results <- results[stringr::str_detect(results$analyzer, analyzer_code), ]
  return(results)

}

#' @title plot_ghg_flux
#' @description Plot GHG concentration over time for each measurement window, overlaid with the
#'   fitted regression line and colored by quality flag. Use this for visual QC of
#'   \code{\link{calculate_regression}} output.
#' @param data Analyzer data processed by \code{tidy_ghg_analyzer()} and \code{convert_time()}.
#' @param regression_result Output tibble from \code{calculate_regression()}.
#' @param ghg Column name in \code{data} for the gas concentration (e.g. \code{"CH4"}, \code{"N2O"}).
#' @param fit_type Model used to draw the fitted line. Should match the \code{fit_type} used in
#'   \code{calculate_regression()}. One of \code{"linear"} (default), \code{"exp_tz"},
#'   \code{"exp_zhao18"}.
#' @param t_zero Reference time in seconds for \code{"exp_tz"} slope. Default 0.
#' @param flag_colors Named character vector mapping flag values to colors.
#' @return A \code{ggplot} object with one facet per site. Each panel shows raw concentration
#'   points and the fitted line, with strip text showing slope, R², and flag.
#' @examples
#' data(n2o)
#' n2o_converted <- convert_time(n2o)
#' ref <- data.frame(
#'   date_time = n2o_converted$real_datetime[1],
#'   site = "S1", analyzer = "1074"
#' )
#' result <- calculate_regression(n2o_converted, ref, "N2O",
#'                                reference_time = "date_time", site = "site",
#'                                analyzer_code = "1074")
#' plot_ghg_flux(n2o_converted, result, "N2O")
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual facet_wrap labs
#'   theme_bw theme element_text
#' @importFrom dplyr mutate left_join
#' @export

plot_ghg_flux <- function(data, regression_result, ghg,
                          fit_type = "linear", t_zero = 0,
                          flag_colors = c(ok      = "#2ca25f",
                                          discard = "#fc8d59",
                                          zero    = "#74c7ef",
                                          no_data = "#aaaaaa")) {

  fit_type <- match.arg(fit_type, c("linear", "quadratic", "exp_tz", "exp_zhao18"))

  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else {
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  # Build per-measurement window and fitted-line data
  window_list <- vector("list", nrow(regression_result))
  fitted_list <- vector("list", nrow(regression_result))

  for (i in seq_len(nrow(regression_result))) {
    row  <- regression_result[i, ]
    site <- row$site
    flag <- row$flag
    st   <- as.POSIXct(row$start_time, format = "%Y/%m/%d %H:%M:%S", tz = "Asia/Taipei")
    et   <- as.POSIXct(row$end_time,   format = "%Y/%m/%d %H:%M:%S", tz = "Asia/Taipei")

    slope_label <- if (!is.na(row$slope))    formatC(row$slope,    digits = 5, format = "g") else "NA"
    r2_label    <- if (!is.na(row$r_square)) round(row$r_square, 3)                          else "NA"
    site_label  <- paste0(site, "\nslope=", slope_label, "  R²=", r2_label, "  [", flag, "]")

    empty_win <- data.frame(site_label = character(0), elapsed_s = numeric(0),
                            conc = numeric(0), flag = character(0),
                            stringsAsFactors = FALSE)
    empty_fit <- data.frame(site_label = character(0), elapsed_s = numeric(0),
                            fitted = numeric(0), flag = character(0),
                            stringsAsFactors = FALSE)

    if (is.na(st) || is.na(et)) {
      window_list[[i]] <- empty_win
      fitted_list[[i]] <- empty_fit
      next
    }

    win <- data[data$real_datetime >= st & data$real_datetime <= et, ]
    win <- win[order(win$real_datetime), ]

    if (nrow(win) < 2) {
      window_list[[i]] <- empty_win
      fitted_list[[i]] <- empty_fit
      next
    }

    elapsed_s <- as.numeric(difftime(win$real_datetime, win$real_datetime[1], units = "secs"))
    conc_vec  <- as.numeric(win[[ghg]])

    window_list[[i]] <- data.frame(site_label = site_label, elapsed_s = elapsed_s,
                                   conc = conc_vec, flag = flag,
                                   stringsAsFactors = FALSE)

    # Fitted line over 200 evenly spaced points
    elapsed_seq <- seq(min(elapsed_s), max(elapsed_s), length.out = 200)
    C0          <- conc_vec[1]
    Cm_start    <- conc_vec[length(conc_vec)]

    if (fit_type == "linear") {
      fit    <- stats::lm(conc_vec ~ elapsed_s)
      fitted <- stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq))
    } else if (fit_type == "quadratic") {
      fit    <- stats::lm(conc_vec ~ elapsed_s + I(elapsed_s^2))
      fitted <- stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq))
    } else if (fit_type == "exp_tz") {
      fit <- tryCatch(
        stats::nls(conc_vec ~ Cm + a * elapsed_s + (C0 - Cm) * exp(-b * elapsed_s),
                   start   = list(Cm = Cm_start, a = 0, b = 0.001),
                   control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
        error = function(e) NULL)
      fitted <- if (!is.null(fit))
        tryCatch(stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq)),
                 error = function(e) rep(NA_real_, 200))
      else rep(NA_real_, 200)
    } else {
      fit <- tryCatch(
        stats::nls(conc_vec ~ Cm + a * (elapsed_s - t0) + (C0 - Cm) * exp(-b * (elapsed_s - t0)),
                   start   = list(Cm = Cm_start, a = 0, b = 0.001, t0 = 0),
                   control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
        error = function(e) NULL)
      fitted <- if (!is.null(fit))
        tryCatch(stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq)),
                 error = function(e) rep(NA_real_, 200))
      else rep(NA_real_, 200)
    }

    fitted_list[[i]] <- data.frame(site_label = site_label, elapsed_s = elapsed_seq,
                                   fitted = fitted, flag = flag,
                                   stringsAsFactors = FALSE)
  }

  all_windows <- do.call(rbind, window_list)
  all_fitted  <- do.call(rbind, fitted_list)

  if (nrow(all_windows) == 0) return(NULL)

  # Preserve site_label order as it appears in regression_result
  site_label_order <- vapply(seq_len(nrow(regression_result)), function(i) {
    row        <- regression_result[i, ]
    site       <- row$site
    flag       <- row$flag
    slope_lbl  <- if (!is.na(row$slope))    formatC(row$slope,    digits = 5, format = "g") else "NA"
    r2_lbl     <- if (!is.na(row$r_square)) round(row$r_square, 3)                          else "NA"
    paste0(site, "\nslope=", slope_lbl, "  R²=", r2_lbl, "  [", flag, "]")
  }, character(1))

  all_windows$site_label <- factor(all_windows$site_label, levels = unique(site_label_order))
  all_fitted$site_label  <- factor(all_fitted$site_label,  levels = unique(site_label_order))

  used_colors <- flag_colors[names(flag_colors) %in% unique(c(all_windows$flag, all_fitted$flag))]

  ggplot2::ggplot() +
    ggplot2::geom_point(data  = all_windows,
                        ggplot2::aes(x = elapsed_s, y = conc,   color = flag),
                        size  = 0.6, alpha = 0.5) +
    ggplot2::geom_line(data   = all_fitted,
                       ggplot2::aes(x = elapsed_s, y = fitted,  color = flag),
                       linewidth = 0.9) +
    ggplot2::scale_color_manual(values = used_colors) +
    ggplot2::facet_wrap(~ site_label, scales = "free_y") +
    ggplot2::labs(x = "Elapsed time (s)", y = paste0(ghg, " (ppm)"), color = "Flag") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text      = ggplot2::element_text(size = 7.5),
                   legend.position = "bottom")
}

#' @title calculate_ghg_flux
#' @description Calculate the greenhouse gas (GHG) flux based on input parameters from a data frame.
#' @param data A data frame containing relevant data with columns for slope, area, volume, and temperature.
#' @param slope Name of the column in `data` that contains the slope values of the GHG concentration change (in ppm/s).
#' @param area Name of the column in `data` that contains the values of the area of the chamber (in square meter).
#' @param volume Name of the column in `data` that contains values of the volume of the chamber (in litre).
#' @param temp Name of the column in `data` that contains values of the temperature of the gas (in Celsius).
#' @return A list containing the calculated flux and its unit.
#' @examples
#' data <- data.frame(
#'   slope = c(1.2, 1.5, 1.1),
#'   area = c(100, 150, 120),
#'   volume = c(10, 15, 12),
#'   temp = c(25, 30, 22)
#' )
#' results <- calculate_ghg_flux(data)
#' print(results)
#' @export

calculate_ghg_flux <- function(data, slope = "slope", area = "area", volume = "volume", temp = "temp") {

  # Constants
  s_to_hr <- (1/3600)  # seconds to hours (3600 s h⁻¹)
  gas_constant <- 0.082057  # gas constant (L·atm·K⁻¹·mol⁻¹)
  celsius_to_kelvin <- 273.15  # Celsius to Kelvin conversion
  micro_to_milli <- 0.001  # micromoles to millimoles conversion

  # Check if specified columns exist in the data frame
  required_cols <- c(slope, area, volume, temp)
  if (!all(required_cols %in% names(data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }

  # Extract values from the data frame
  slope <- data[[slope]]
  area <- data[[area]]
  volume <- data[[volume]]
  temp <- data[[temp]]

  # Calculate flux
  data$flux <- (slope * volume * (1/s_to_hr) * micro_to_milli) /
    (gas_constant * (temp + celsius_to_kelvin) * area)

  # Set unit of the result
  data$unit <- "mmol m-2 h-1"

  return(data)
}

#' @title convert_ghg_unit
#' @description Convert a greenhouse gas (GHG) flux value (or a character string
#'   containing one or more numeric values, e.g. \code{"0.002 +/- 0.003"})
#'   to micrograms per square meter per hour.
#' @details Numeric values embedded in a string (e.g. mean +/- SD notation)
#'   are each converted individually and the surrounding text is preserved.
#'   Commas are treated as decimal separators.
#' @param input A single numeric value or a character string containing one or
#'   more numbers.
#' @param ghg The molecular formula of the greenhouse gas: \code{"co2"},
#'   \code{"ch4"}, or \code{"n2o"}.
#' @param mass Mass unit of the input flux. One of \code{"mmol"}, \code{"mg"},
#'   \code{"g"}, \code{"ug"} (micrograms), \code{"nmol"}, \code{"Mg"},
#'   \code{"umol"} (micromoles), \code{"mol"}. Default \code{"ug"}.
#' @param area Area unit of the input flux. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param time Time unit of the input flux. One of \code{"yr"}, \code{"day"},
#'   \code{"hr"}, \code{"sec"}, \code{"min"}. Default \code{"hr"}.
#' @param digits Number of decimal places to round to. Default 2.
#' @param ratio Logical. If \code{TRUE}, apply an elemental-ratio correction
#'   (C-basis for CH4, N-basis for N2O). Default \code{FALSE}.
#' @return A named list with \code{value} (converted string) and \code{unit},
#'   or \code{"EMPTY"} for missing/non-numeric input.
#' @examples
#' convert_ghg_unit(97, ghg = "ch4", mass = "mg", area = "m2", time = "hr")
#' @export
convert_ghg_unit <- function(input, ghg, mass = "\u00b5g", area = "m2",
                             time = "hr", digits = 2, ratio = FALSE) {
  if (is.na(input) || input == "") return("EMPTY")

  input <- gsub(",", ".", as.character(input))

  numeric_values <- unlist(regmatches(input,
    gregexpr("[-+]?[0-9]*\\.?[0-9]+", input)))

  if (length(numeric_values) == 0) return("EMPTY")

  valid_ghgs <- c("co2", "ch4", "n2o")
  molar_mass <- switch(ghg,
    "co2" = 44.01, "ch4" = 16.04, "n2o" = 44.01,
    stop(paste("Invalid GHG type. Please use one of:",
               paste(valid_ghgs, collapse = ", "))))

  valid_masses <- c("mmol", "mg", "g", "\u00b5g", "nmol", "Mg", "\u00b5mol", "mol")
  if (!(mass %in% valid_masses))
    stop(paste("Invalid mass unit. Please use one of:",
               paste(valid_masses, collapse = ", ")))

  valid_areas <- c("ha", "m2")
  if (!(area %in% valid_areas))
    stop(paste("Invalid area unit. Please use one of:",
               paste(valid_areas, collapse = ", ")))

  valid_times <- c("yr", "day", "hr", "sec", "min")
  if (!(time %in% valid_times))
    stop(paste("Invalid time unit. Please use one of:",
               paste(valid_times, collapse = ", ")))

  convert_value <- function(value) {
    # Convert mass to µg
    value <- switch(mass,
      "mmol"      = value * molar_mass * 1000,
      "mg"        = value * 1000,
      "g"         = value * 1000000,
      "\u00b5g"   = value,
      "nmol"      = (value * molar_mass) / 1000,
      "Mg"        = value * 1e+12,
      "\u00b5mol" = value * molar_mass,
      "mol"       = value * molar_mass * 1000000
    )
    # Convert area to m2
    if (area == "ha") value <- value / 10000
    # Convert time to hours
    value <- switch(time,
      "yr"  = value / 8760,
      "day" = value / 24,
      "sec" = value * 3600,
      "min" = value * 60,
      "hr"  = value
    )
    # Optional elemental-ratio correction
    if (ratio) {
      if (ghg == "ch4") value <- value * (16.04 / 12.01)
      if (ghg == "n2o") value <- value * (44.013 / 14.0067)
    }
    round(value, digits)
  }

  for (num in numeric_values) {
    converted_num <- convert_value(as.numeric(num))
    input <- gsub(paste0("\\b", num, "\\b"), as.character(converted_num), input)
  }

  list(value = input, unit = "\u00b5g m\u207b\u00b2 h\u207b\u00b9")
}


#' @title calculate_MDF
#' @description Calculate the Minimum Detectable Flux (MDF) for a static chamber
#'   GHG measurement system.
#' @param precision_ppm Precision of the gas analyser (ppm).
#' @param closure_time_s Closure time of the measurement (seconds).
#' @param data_point_n Number of data points recorded during the closure period.
#' @param chamber_volume_m3 Internal volume of the chamber (m\eqn{^3}).
#' @param temperature_C Air temperature at the measurement location (\eqn{^\circ}C).
#' @param chamber_area_m2 Base area of the chamber (m\eqn{^2}).
#' @param pressure_pa Atmospheric pressure (Pa). Default 101325.
#' @param ideal_constant Ideal gas constant (J mol\eqn{^{-1}} K\eqn{^{-1}}).
#'   Default 8.314.
#' @param ghg Greenhouse gas type: \code{"co2"}, \code{"ch4"}, or \code{"n2o"}.
#'   Default \code{"co2"}.
#' @return A named list with \code{MDF} (numeric,
#'   \eqn{\mu}g m\eqn{^{-2}} h\eqn{^{-1}}) and \code{unit} (string).
#' @examples
#' calculate_MDF(
#'   precision_ppm     = 1,
#'   closure_time_s    = 300,
#'   data_point_n      = 300,
#'   chamber_volume_m3 = 0.0064,
#'   temperature_C     = 25,
#'   chamber_area_m2   = 0.07
#' )
#' @export
calculate_MDF <- function(precision_ppm, closure_time_s, data_point_n,
                          chamber_volume_m3, temperature_C, chamber_area_m2,
                          pressure_pa = 101325, ideal_constant = 8.314,
                          ghg = "co2") {
  if (any(c(precision_ppm, closure_time_s, data_point_n,
            chamber_volume_m3, temperature_C, chamber_area_m2) <= 0)) {
    stop("All parameters must be positive and non-zero.")
  }

  molar_masses <- list(co2 = 44.01, ch4 = 16.04, n2o = 44.01)
  if (!(ghg %in% names(molar_masses)))
    stop("Invalid GHG type. Choose from 'co2', 'ch4', or 'n2o'.")

  part1    <- precision_ppm / (closure_time_s * sqrt(data_point_n))
  part2    <- (chamber_volume_m3 * pressure_pa) /
                (ideal_constant * (temperature_C + 273.15) * chamber_area_m2)
  MDF_umol <- part1 * part2 * 3600
  MDF      <- MDF_umol * molar_masses[[ghg]]

  list(MDF = MDF, unit = "\u00b5g m\u207b\u00b2 h\u207b\u00b9")
}


#' @title calculate_total_co2e
#' @description Convert individual GHG fluxes (mg m\eqn{^{-2}} h\eqn{^{-1}})
#'   to a total CO\eqn{_2}-equivalent flux (g m\eqn{^{-2}} d\eqn{^{-1}}) using
#'   IPCC AR6 100-year GWPs (CO\eqn{_2} = 1, CH\eqn{_4} = 27,
#'   N\eqn{_2}O = 273).
#' @param co2 CO\eqn{_2} flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @param ch4 CH\eqn{_4} flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @param n2o N\eqn{_2}O flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @return Total CO\eqn{_2}e flux as a numeric scalar
#'   (g m\eqn{^{-2}} d\eqn{^{-1}}), printed with a diagnostic message.
#' @examples
#' calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003)
#' @export
calculate_total_co2e <- function(co2 = 0, ch4 = 0, n2o = 0) {
  co2e_mg_hr <- co2 + (ch4 * 27) + (n2o * 273)
  co2e_g_day <- co2e_mg_hr * 0.001 * 24

  message("Calculating total CO2e emissions:")
  message(sprintf("CO2 emissions: %.2f mg m^-2 h^-1", co2))
  message(sprintf("CH4 emissions: %.2f mg m^-2 h^-1 (GWP: 27)", ch4))
  message(sprintf("N2O emissions: %.2f mg m^-2 h^-1 (GWP: 273)", n2o))
  message(sprintf("Total CO2e emissions: %.2f g m^-2 d^-1", co2e_g_day))

  return(co2e_g_day)
}
