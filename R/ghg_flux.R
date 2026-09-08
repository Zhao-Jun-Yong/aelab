utils::globalVariables(c("CO2", "CH4", "N2O", "conc", "analyzer_precision"))

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

# Internal helper: fit a GHG concentration curve and return slope, R\u00b2, curvature b, and fit_used.
# kappamax:            if provided and b > kappamax, exponential fits fall back to linear.
# instrument_precision: only used when fit_type = "auto" to compute kappamax per window
#                       using the H\u00fcppi et al. (2018) formula: kmax = |slope_lm| / precision.
.fit_ghg_model <- function(conc, elapsed_s, fit_type, t_zero,
                           kappamax = NULL, instrument_precision = NULL) {
  C0     <- conc[1]
  ss_tot <- sum((conc - mean(conc))^2)

  .lm_result <- function() {
    fit <- stats::lm(conc ~ elapsed_s)
    list(slope = unname(stats::coef(fit)[2]), r_square = summary(fit)$r.squared,
         b_param = NA_real_, fit_used = "linear", residuals = stats::residuals(fit),
         kmax_used = NA_real_)
  }

  .quad_result <- function() {
    fit <- stats::lm(conc ~ elapsed_s + I(elapsed_s^2))
    list(slope = unname(stats::coef(fit)[2]), r_square = summary(fit)$r.squared,
         b_param = NA_real_, fit_used = "quadratic", residuals = stats::residuals(fit),
         kmax_used = NA_real_)
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
    list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_tz",
         residuals = stats::residuals(fit))
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
                                  b_param = NA_real_, fit_used = "exp_tz",
                                  residuals = NA_real_, kmax_used = NA_real_))
    cf    <- stats::coef(fit)
    b_val <- unname(cf["b"])
    if (!is.null(kappamax) && !is.na(b_val) && b_val > kappamax) {
      lin <- stats::lm(conc ~ elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = b_val, fit_used = "linear",
                  residuals = stats::residuals(lin), kmax_used = NA_real_))
    }
    slope <- cf["a"] + cf["b"] * (cf["Cm"] - C0) * exp(-cf["b"] * t_zero)
    r_sq  <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
    return(list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_tz",
                residuals = stats::residuals(fit), kmax_used = NA_real_))
  }

  if (fit_type == "exp_zhao18") {
    Cm_start <- conc[length(conc)]
    fit <- tryCatch(
      stats::nls(conc ~ Cm + a * (elapsed_s - t0) + (C0 - Cm) * exp(-b * (elapsed_s - t0)),
                 start   = list(Cm = Cm_start, a = 0, b = 0.001, t0 = 0),
                 control = stats::nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL)
    if (is.null(fit)) return(list(slope = NA_real_, r_square = NA_real_,
                                  b_param = NA_real_, fit_used = "exp_zhao18",
                                  residuals = NA_real_, kmax_used = NA_real_))
    cf    <- stats::coef(fit)
    b_val <- unname(cf["b"])
    if (!is.null(kappamax) && !is.na(b_val) && b_val > kappamax) {
      lin <- stats::lm(conc ~ elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = b_val, fit_used = "linear",
                  residuals = stats::residuals(lin), kmax_used = NA_real_))
    }
    slope <- cf["a"] + cf["b"] * (cf["Cm"] - C0)
    r_sq  <- if (ss_tot > 0) 1 - sum(stats::residuals(fit)^2) / ss_tot else NA_real_
    return(list(slope = unname(slope), r_square = r_sq, b_param = b_val, fit_used = "exp_zhao18",
                residuals = stats::residuals(fit), kmax_used = NA_real_))
  }

  if (fit_type == "auto") {
    duration_s <- max(elapsed_s) - min(elapsed_s)

    # Step 1: linear (baseline)
    best     <- .lm_result()
    lm_slope <- best$slope  # saved for g-factor computation

    # kappamax (H\u00fcppi et al. 2018): reject exp_tz if b > |slope_lm| / precision.
    # Strong fluxes tolerate more curvature; near-detection-limit measurements stay linear.
    kmax <- if (!is.null(instrument_precision) && !is.na(best$slope) && abs(best$slope) > 0)
      abs(best$slope) / instrument_precision
    else
      1.5 / max(duration_s, 1)

    # Step 2: quadratic \u2014 accept if meaningfully better (\u0394R\u00b2 > 0.01)
    quad <- .quad_result()
    if (!is.na(quad$r_square) && !is.na(best$r_square) &&
        quad$r_square > best$r_square + 0.01) {
      best <- quad
    }

    # Step 3: exp_tz with kappamax \u2014 accept if \u03ba passes and meaningfully better
    exp_res <- .exp_tz_result(kmax)
    if (!is.null(exp_res) && !is.na(exp_res$r_square) && !is.na(best$r_square) &&
        exp_res$r_square > best$r_square + 0.01) {
      best <- exp_res
    }

    best$slope_lm  <- lm_slope
    best$kmax_used <- kmax
    return(best)
  }

  stop(paste("Unknown fit_type:", fit_type,
             ". Use 'linear', 'quadratic', 'exp_tz', 'exp_zhao18', or 'auto'."))
}

# Internal helper: number of fitted parameters for each model family, used for the
# degrees-of-freedom correction in .pareto_select_window()'s residual bootstrap. C0 (exp_tz) is
# fixed from the data (not fit), so it does not count.
.n_fit_params <- function(fit_used) {
  switch(fit_used,
         linear     = 2L,
         quadratic  = 3L,
         exp_tz     = 3L,
         exp_zhao18 = 4L,
         2L)
}

# Internal helper: degrees-of-freedom-corrected residuals for residual resampling, used by
# .pareto_select_window()'s uncertainty bootstrap. Inflates residuals by sqrt(n / (n - p)) so the
# resampled residual pool's variance matches the estimated (not merely observed) residual
# variance - the standard small-sample correction for residual-resampling bootstraps (Davison &
# Hinkley 1997). Guarded at n - p <= 0 (not expected given min_window_size's default of 60, but a
# floor here avoids a negative-or-zero denominator producing NaN/Inf): falls back to an
# uncorrected (factor 1) copy of resid rather than propagating NaN.
.dof_corrected_residuals <- function(resid, p) {
  n  <- length(resid)
  df <- n - p
  if (df <= 0) return(resid)
  resid * sqrt(n / df)
}

# Internal helper: window_type = "pareto" candidate search for calculate_regression().
# Independently grids deadband (dead-band skip, s) x cutoff (window end, s) relative to
# reference_datetime, fits each candidate via the existing .fit_ghg_model() (so the
# kappamax gate is never bypassed), scores fit quality as a per-point-averaged Gaussian
# log-likelihood (not summed, unlike Kobayashi et al. 2026's FCS - avoids a structural bias
# toward the shortest allowed window), estimates slope uncertainty via a residual bootstrap,
# and picks the non-dominated (Pareto) candidate closest to the normalized ideal corner.
# Returns list(status, n_candidates, selected) - status is "no_grid" (too little data to form
# any candidate), "no_candidates" (candidates existed but none produced a usable fit), or
# "selected".
.pareto_select_window <- function(sorted_data, ghg, reference_datetime,
                                  fit_type, t_zero, kappamax, instrument_precision,
                                  min_deadband, max_deadband, deadband_step,
                                  min_window_size, cutoff_step, n_boot) {

  no_grid       <- list(status = "no_grid",       n_candidates = 0L, selected = NULL)
  no_candidates <- list(status = "no_candidates", n_candidates = 0L, selected = NULL)

  if (nrow(sorted_data) < 2) return(no_grid)

  elapsed_full <- as.numeric(difftime(sorted_data$real_datetime, reference_datetime, units = "secs"))
  max_elapsed  <- max(elapsed_full)

  deadband_hi <- max_deadband - deadband_step
  if (deadband_hi < min_deadband) return(no_grid)
  deadband_grid <- seq(min_deadband, deadband_hi, by = deadband_step)

  cutoff_lo <- min_deadband + min_window_size
  if (max_elapsed < cutoff_lo) return(no_grid)
  cutoff_grid <- seq(cutoff_lo, max_elapsed, by = cutoff_step)

  sigma <- instrument_precision
  candidates <- list()

  for (db in deadband_grid) {
    for (co in cutoff_grid) {
      if (co - db < min_window_size) next

      rows <- elapsed_full >= db & elapsed_full <= co
      # Admission floor of 4 (not 3): matches this package's own cited data-sufficiency floor
      # (Maier et al. 2022) and, specifically for this feature, avoids a 3-point window paired
      # with fit_type resolving to "quadratic" (3 fitted parameters) - a 0-residual-df perfect
      # fit by construction whose near-zero residuals could pull the min-max-normalized Pareto
      # selection toward it for purely arithmetic, not evidential, reasons.
      if (sum(rows) < 4) next  # too few points to fit any of the supported model families

      cand_elapsed <- elapsed_full[rows] - db
      cand_conc    <- as.numeric(sorted_data[[ghg]][rows])

      fit_out <- tryCatch(
        .fit_ghg_model(cand_conc, cand_elapsed, fit_type, t_zero, kappamax, instrument_precision),
        error = function(e) NULL)

      if (is.null(fit_out) || is.na(fit_out$slope) || is.na(fit_out$r_square) ||
          is.infinite(fit_out$r_square) || is.null(fit_out$residuals) ||
          anyNA(fit_out$residuals)) next

      # Fit-quality score: mean (not summed) per-point Gaussian log-likelihood, so a longer
      # window is never penalized merely for having more residual terms.
      fit_score <- mean(-0.5 * ((fit_out$residuals^2) / sigma^2 + log(sigma^2)))

      # Bootstrap refits must reuse the *same* curvature threshold the point-estimate fit was
      # gated on. When fit_type = "auto", fit_out$fit_used is a resolved literal model name
      # (e.g. "exp_tz"), so passing the outer kappamax (always NULL here) into
      # .fit_ghg_model()'s literal branches would leave every bootstrap replicate ungated and
      # let nls() silently return degenerate slopes (singular-gradient warning, not an error).
      # Use the kmax that "auto" itself computed and already applied; for non-auto fit_types,
      # kmax_used is NA_real_ (no such gate was computed) and behavior is unchanged - the
      # caller's own explicit kappamax keeps gating every refit exactly as before.
      boot_kappamax <- if (fit_type == "auto") fit_out$kmax_used else kappamax

      # Residual bootstrap for slope uncertainty: refit the same model family n_boot times on
      # synthetic concentration series built by resampling (with replacement) the candidate's
      # own degrees-of-freedom-corrected residuals around its fitted values - not fresh
      # parametric noise. Parametric noise fixed at instrument precision understates real-world
      # uncertainty whenever true residual variance exceeds instrument spec (turbulence,
      # imperfect chamber seals, ebullition spikes), which is the common case in field data.
      n_pts           <- length(cand_conc)
      fitted_vals     <- cand_conc - fit_out$residuals
      corrected_resid <- .dof_corrected_residuals(fit_out$residuals, .n_fit_params(fit_out$fit_used))
      boot_slopes <- vapply(seq_len(n_boot), function(b) {
        synth_conc <- fitted_vals + sample(corrected_resid, n_pts, replace = TRUE)
        boot_out   <- tryCatch(
          .fit_ghg_model(synth_conc, cand_elapsed, fit_out$fit_used, t_zero,
                         boot_kappamax, instrument_precision),
          error = function(e) NULL)
        if (is.null(boot_out)) NA_real_ else boot_out$slope
      }, numeric(1))
      boot_slopes <- boot_slopes[!is.na(boot_slopes)]
      if (length(boot_slopes) < 10) next  # bootstrap too unstable to trust an uncertainty width

      q <- stats::quantile(boot_slopes, probs = c(0.16, 0.84), names = FALSE, na.rm = TRUE)

      candidates[[length(candidates) + 1]] <- list(
        deadband = db, cutoff = co,
        slope = fit_out$slope, r_square = fit_out$r_square,
        cor = stats::cor(cand_conc, cand_elapsed),
        b_param = fit_out$b_param, fit_used = fit_out$fit_used,
        slope_lm = if (!is.null(fit_out$slope_lm)) fit_out$slope_lm else NA_real_,
        fit_score = fit_score,
        uncertainty_lo = unname(q[1]), uncertainty_hi = unname(q[2]),
        start_dt = sorted_data$real_datetime[rows][1],
        end_dt   = sorted_data$real_datetime[rows][sum(rows)]
      )
    }
  }

  n_candidates <- length(candidates)
  if (n_candidates == 0) return(no_candidates)

  # Pareto front over (uncertainty width, -fit_score), both to be minimized.
  uw <- vapply(candidates, function(x) x$uncertainty_hi - x$uncertainty_lo, numeric(1))
  nf <- vapply(candidates, function(x) -x$fit_score, numeric(1))
  sel_idx <- .pareto_front_select(uw, nf)

  list(status = "selected", n_candidates = n_candidates, selected = candidates[[sel_idx]])
}

# Internal helper: given two equal-length numeric vectors to minimize (uw = uncertainty width,
# nf = negative fit-quality score), min-max normalize both, find the non-dominated (Pareto)
# points, and return the index of the front member closest (Euclidean) to the normalized ideal
# corner (0, 0). A constant vector normalizes to all-zero on that axis rather than dividing by
# zero. Exposed separately from .pareto_select_window() so the selection math itself is
# unit-testable without needing real data/fitting/bootstrap.
.pareto_front_select <- function(uw, nf) {
  .normalize <- function(x) {
    rng <- range(x)
    if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
  }
  nu <- .normalize(uw)
  ns <- .normalize(nf)

  is_dominated <- vapply(seq_along(nu), function(i) {
    any(nu <= nu[i] & ns <= ns[i] & (nu < nu[i] | ns < ns[i]))
  }, logical(1))
  front_idx <- which(!is_dominated)
  dist      <- sqrt(nu[front_idx]^2 + ns[front_idx]^2)
  front_idx[which.min(dist)]
}

#' @import tibble
#' @importFrom stats lm coef nls nls.control residuals cor quantile
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
#'   When \code{window_type = "fixed"} and \code{reference_df} contains a \code{duration_minutes}
#'   column, the per-row value (if not \code{NA}) overrides this global parameter for that
#'   specific measurement. This allows different window lengths for individual chamber closures.
#' @param num_rows The number of rows used per sliding regression window. Only used when
#'   \code{window_type = "sliding"}. Default 300.
#' @param fit_type Model used to fit the concentration curve. One of \code{"linear"} (default),
#'   \code{"quadratic"} (second-order polynomial; slope is the initial rate at t=0),
#'   \code{"exp_tz"} (exponential with linear drift and user-specified \code{t_zero}),
#'   \code{"exp_zhao18"} (exponential with linear drift and estimated t0), or
#'   \code{"auto"} (automatic model selection: tries linear -> quadratic -> exp_tz in order,
#'   accepting a more complex model only when R\eqn{^2} improves by > 0.01 and, for exp_tz, the
#'   curvature parameter kappa does not exceed \code{kappamax}). When \code{fit_type = "auto"} and
#'   \code{instrument_precision = NULL}, precision is looked up from \code{\link{analyzer_precision}}
#'   by gas name. The model chosen for each measurement is recorded in the \code{fit_used} output column.
#' @param t_zero Reference time in seconds from window start for the \code{"exp_tz"} slope calculation. Default 0.
#' @param window_type \code{"sliding"} (default) selects the \code{num_rows}-length sub-window with
#'   the best R\eqn{^2} within the measurement period. \code{"fixed"} fits a single window starting at
#'   \code{start_offset_s} seconds after the reference time and running for \code{duration_minutes}.
#'   Use \code{"fixed"} to ensure all gases are regressed over the same time window.
#'   \code{"pareto"} independently searches deadband (start skip) and cutoff (window end) times,
#'   trading off fit quality against propagated slope uncertainty via a Pareto front - adapted
#'   from the window-selection idea in Kobayashi et al. (2026), with two
#'   deliberate differences from that method: the existing \code{kappamax} gate is always applied
#'   (no candidate can be a numerically degenerate exponential fit), and a candidate pool that is
#'   empty or whose selected window fails the existing \code{zero}/\code{discard} checks is
#'   reported as such rather than returning a confident-looking number (see \code{flag}).
#' @param start_offset_s Seconds to skip from the reference time before the regression window starts.
#'   Only used when \code{window_type = "fixed"}. Default 0.
#'   When \code{reference_df} contains a \code{start_offset_s} column, the per-row value (if not
#'   \code{NA}) overrides this global parameter for that specific measurement.
#' @param min_deadband,max_deadband,deadband_step Only used when \code{window_type = "pareto"}.
#'   Define the candidate deadband (start-skip) grid in seconds, relative to the reference time:
#'   \code{seq(min_deadband, max_deadband - deadband_step, by = deadband_step)}. Defaults 30, 60, 10.
#'   \code{min_deadband} defaults to 30 (rather than 0) to match this package's own existing
#'   \code{start_offset_s = 30} convention used in \code{window_type = "fixed"} batch processing
#'   across this lab's campaigns; still fully overridable, e.g. set to 0 to let the search consider
#'   no dead-band at all.
#' @param min_window_size,cutoff_step Only used when \code{window_type = "pareto"}. \code{min_window_size}
#'   (default 60) is the minimum candidate window length in seconds (\code{cutoff - deadband});
#'   \code{cutoff_step} (default 10) is the step size of the candidate cutoff-time grid, which runs
#'   from \code{min_deadband + min_window_size} to the window's own maximum elapsed time.
#' @param n_boot Only used when \code{window_type = "pareto"}. Number of residual-bootstrap refits
#'   used to estimate each candidate window's slope uncertainty (16th/84th percentile of the
#'   bootstrapped slopes). Default 200. Each refit resamples (with replacement) the candidate's
#'   own degrees-of-freedom-corrected residuals (\code{resid * sqrt(n / (n - p))}, \code{p} = number
#'   of fitted parameters for the model actually used), adds them to the candidate's fitted
#'   values to build a synthetic concentration series, and refits the same model family via
#'   \code{\link{calculate_regression}}'s internal fitting routine - no MCMC and no new dependency.
#' @param r2_threshold Minimum R-squared for a measurement to pass quality check. Default 0.8.
#' @param cor_threshold Minimum absolute Pearson correlation between concentration and elapsed time.
#'   Measurements below this have no detectable trend and are flagged \code{"zero"}. Default 0.5.
#' @param instrument_precision Optional. Analyser precision in ppm. Used in two ways:
#'   (1) minimum-detectable-slope check - measurements whose absolute slope falls below
#'   \code{instrument_precision / window_duration_s} are flagged \code{"zero"} even if R\eqn{^2}
#'   and correlation pass; (2) kappamax for \code{fit_type = "auto"} - following Hueppi et al.
#'   (2018), \eqn{\kappa_{max} = |\text{slope}_{lm}| / \text{precision}}, so that stronger
#'   fluxes permit more curvature and near-detection-limit measurements are kept linear.
#' @param kappamax Optional. Maximum acceptable exponential curvature parameter \eqn{b} (s\eqn{^{-1}}).
#'   Only applies when \code{fit_type} is \code{"exp_tz"} or \code{"exp_zhao18"}. When \eqn{b > \kappa_{max}},
#'   the fit implies an implausibly fast equilibration and the slope is replaced by the linear
#'   equivalent. Corresponds to the \emph{kappamax} quality criterion in Hueppi et al. (2018).
#' @param g_factor_threshold Optional. Maximum acceptable ratio of the chosen model slope to the
#'   linear baseline slope (g-factor; Gaudard et al. 2025). Only computed when
#'   \code{fit_type = "auto"} and the selected model is not linear. Values far from 1 indicate
#'   that the nonlinear model diverges strongly from the linear fit, which may signal noise
#'   artefacts or poor convergence. When this threshold is exceeded, the flag is overridden to
#'   \code{"discard"}. Default \code{NULL} (disabled). A value of 2 flags measurements where
#'   the nonlinear slope is more than twice the linear slope.
#' @return A tibble with columns: \code{start_time}, \code{end_time}, \code{slope} (ppm s\eqn{^{-1}}),
#'   \code{r_square}, \code{correlation}, \code{b_param} (exponential curvature; \code{NA} for
#'   linear/quadratic), \code{fit_used} (model actually used: \code{"linear"}, \code{"quadratic"},
#'   or \code{"exp_tz"}; always matches \code{fit_type} unless \code{fit_type = "auto"}),
#'   \code{g_factor} (ratio of chosen slope to linear baseline slope; \code{NA} when
#'   \code{fit_used = "linear"} or for explicit non-auto fits),
#'   \code{flag} (\code{"ok"}, \code{"discard"}, \code{"zero"}, or \code{"no_data"}),
#'   \code{reference_time}, \code{site}, \code{analyzer}. When \code{window_type = "pareto"}, five
#'   additional columns are included: \code{deadband}/\code{cutoff} (the selected candidate window,
#'   seconds from the reference time; \code{NA} for other window types), \code{slope_uncertainty_lo}/
#'   \code{slope_uncertainty_hi} (16th/84th percentile of the residual-bootstrap slope distribution
#'   for the selected window), and \code{n_candidates} (number of deadband/cutoff candidates that
#'   produced a usable fit; \code{0} when the measurement was flagged \code{"discard"} because no
#'   candidate survived).
#' @references
#' Hutchinson, G. L., & Mosier, A. R. (1981). Improved soil cover method for field measurement of
#' nitrous oxide fluxes. \emph{Soil Science Society of America Journal}, 45(2), 311-316.
#' \doi{10.2136/sssaj1981.03615995004500020017x}
#'
#' Hüppi, R., Felber, R., Krauss, M., Six, J., Leifeld, J., & Fuß, R. (2018). Restricting the
#' nonlinearity parameter in soil greenhouse gas flux calculations for more reliable flux
#' estimates. \emph{PLOS ONE}, 13(7), e0200876. \doi{10.1371/journal.pone.0200876}
#'
#' Gaudard, J., et al. (2025). fluxible: An R package to process ecosystem gas fluxes from
#' closed-loop chambers in an automated and reproducible way. \emph{Methods in Ecology and
#' Evolution}. \doi{10.1111/2041-210X.70161}
#'
#' Kobayashi, A. N. A., Roques, C., Hunkeler, D., & Brunner, P. (2026). Uncertainty
#' quantification in soil gas fluxes: Reducing the user's arbitrariness. \emph{European Journal
#' of Soil Science}, 77, e70415. \doi{10.1111/ejss.70415}
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
                                 kappamax = NULL,
                                 g_factor_threshold = NULL,
                                 min_deadband = 30, max_deadband = 60, deadband_step = 10,
                                 min_window_size = 60, cutoff_step = 10, n_boot = 200) {

  fit_type    <- match.arg(fit_type,    c("linear", "quadratic", "exp_tz", "exp_zhao18", "auto"))
  window_type <- match.arg(window_type, c("sliding", "fixed", "pareto"))

  # For fit_type = "auto" (kappamax) or window_type = "pareto" (fit_score sigma + bootstrap),
  # resolve instrument_precision from the built-in table if not supplied. Try to match by model
  # name appearing in analyzer_code (case-insensitive); fall back to first row for the gas.
  if ((fit_type == "auto" || window_type == "pareto") && is.null(instrument_precision)) {
    for_gas <- analyzer_precision[analyzer_precision$gas == toupper(ghg), ]
    if (nrow(for_gas) > 0) {
      hits <- which(vapply(for_gas$model, function(m)
        grepl(m, analyzer_code, ignore.case = TRUE) ||
        grepl(analyzer_code, m, ignore.case = TRUE), logical(1)))
      prec_row <- if (length(hits) > 0) for_gas[hits[1], ] else for_gas[1, ]
      instrument_precision <- prec_row$precision_ppm[1]
    }
  }

  if (window_type == "pareto" && is.null(instrument_precision)) {
    stop("window_type = 'pareto' requires instrument_precision (supply it directly, or use a ",
         "gas/analyzer combination resolvable via the built-in analyzer_precision table).")
  }

  reference_time_vec <- lubridate::force_tz(reference_df[[reference_time]], tzone = "Asia/Taipei")
  reference_site     <- as.character(reference_df[[site]])
  reference_analyzer <- as.character(reference_df$analyzer)
  duration_minutes_vec <- if ("duration_minutes" %in% names(reference_df)) {
    as.numeric(reference_df$duration_minutes)
  } else {
    rep(NA_real_, nrow(reference_df))
  }
  start_offset_s_vec <- if ("start_offset_s" %in% names(reference_df)) {
    as.numeric(reference_df$start_offset_s)
  } else {
    rep(NA_real_, nrow(reference_df))
  }

  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else {
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  results <- purrr::map_dfr(seq_along(reference_time_vec), function(i) {
    reference_datetime <- reference_time_vec[i]
    dur_min_i     <- if (!is.na(duration_minutes_vec[i])) duration_minutes_vec[i] else duration_minutes
    start_off_i   <- if (!is.na(start_offset_s_vec[i])) start_offset_s_vec[i] else start_offset_s

    start_time <- reference_datetime
    end_time   <- reference_datetime + start_off_i + as.numeric(dur_min_i) * 60

    valid_rows    <- !is.na(data$real_datetime) & !is.na(data[[ghg]])
    filtered_data <- data[valid_rows & data$real_datetime >= start_time &
                            data$real_datetime <= end_time, ]
    sorted_data   <- filtered_data[order(filtered_data$real_datetime), ]

    best_r_square  <- -Inf
    best_slope     <- NA_real_
    best_slope_lm  <- NA_real_
    best_cor       <- NA_real_
    best_b_param   <- NA_real_
    best_fit_used  <- fit_type
    best_start_dt  <- structure(NA_real_, class = c("POSIXct", "POSIXt"))
    best_end_dt    <- structure(NA_real_, class = c("POSIXct", "POSIXt"))

    # pareto-only outputs; stay NA for "fixed"/"sliding"
    deadband_out       <- NA_real_
    cutoff_out          <- NA_real_
    uncertainty_lo_out <- NA_real_
    uncertainty_hi_out <- NA_real_
    n_candidates_out    <- NA_integer_
    force_discard        <- FALSE

    if (window_type == "fixed") {
      win_start     <- reference_datetime + start_off_i
      win_end       <- win_start + as.numeric(dur_min_i) * 60
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
        best_slope_lm <- if (!is.null(fit_out$slope_lm)) fit_out$slope_lm else NA_real_
        best_b_param  <- fit_out$b_param
        best_fit_used <- fit_out$fit_used
        best_cor      <- if (!is.na(fit_out$r_square)) stats::cor(conc_vec, elapsed_s) else NA_real_
        best_start_dt <- selected_data$real_datetime[1]
        best_end_dt   <- selected_data$real_datetime[nrow(selected_data)]
      }
    } else if (window_type == "sliding") {
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
            best_slope_lm <- if (!is.null(fit_out$slope_lm)) fit_out$slope_lm else NA_real_
            best_b_param  <- fit_out$b_param
            best_fit_used <- fit_out$fit_used
            best_cor      <- stats::cor(conc_vec, elapsed_s)
            best_start_dt <- selected_data$real_datetime[1]
            best_end_dt   <- selected_data$real_datetime[nrow(selected_data)]
          }
        }
      }
    } else if (window_type == "pareto") {
      pw <- .pareto_select_window(sorted_data, ghg, reference_datetime,
                                  fit_type, t_zero, kappamax, instrument_precision,
                                  min_deadband, max_deadband, deadband_step,
                                  min_window_size, cutoff_step, n_boot)
      n_candidates_out <- pw$n_candidates

      if (identical(pw$status, "no_candidates")) {
        force_discard <- TRUE
      } else if (identical(pw$status, "selected")) {
        sel <- pw$selected
        best_r_square      <- sel$r_square
        best_slope         <- sel$slope
        best_slope_lm      <- sel$slope_lm
        best_b_param       <- sel$b_param
        best_fit_used      <- sel$fit_used
        best_cor           <- sel$cor
        best_start_dt      <- sel$start_dt
        best_end_dt        <- sel$end_dt
        deadband_out       <- sel$deadband
        cutoff_out          <- sel$cutoff
        uncertainty_lo_out <- sel$uncertainty_lo
        uncertainty_hi_out <- sel$uncertainty_hi
      }
      # status == "no_grid" (too little data): best_* stay at their NA/-Inf defaults,
      # matching how "fixed"/"sliding" fall through to flag = "no_data" below.
    }

    # Quality flag: correlation threshold distinguishes "zero" (no trend) from "discard" (poor fit).
    # For window_type = "pareto", an empty admitted-candidate pool is reported as "discard" rather
    # than falling through to "no_data" (data existed; no candidate window survived the fit).
    flag <- if (force_discard) {
      "discard"
    } else if (is.na(best_cor) || is.infinite(best_r_square)) {
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

    # g-factor: ratio of chosen slope to linear baseline slope (auto mode only).
    # Values far from 1 indicate the nonlinear model diverges strongly from linear.
    g_factor <- if (!is.na(best_slope_lm) && !is.na(best_slope) &&
                    best_slope_lm != 0 && best_fit_used != "linear")
      best_slope / best_slope_lm
    else NA_real_

    # g-factor threshold check: override "ok" to "discard" when divergence is too large.
    if (!is.null(g_factor_threshold) && !is.na(g_factor) && flag == "ok" &&
        abs(g_factor) > g_factor_threshold) {
      flag <- "discard"
    }

    tibble(
      start_time           = format(best_start_dt, "%Y/%m/%d %H:%M:%S"),
      end_time             = format(best_end_dt, "%Y/%m/%d %H:%M:%S"),
      slope                = best_slope,
      r_square             = best_r_square,
      correlation          = best_cor,
      b_param              = best_b_param,
      fit_used             = best_fit_used,
      g_factor             = g_factor,
      flag                 = flag,
      deadband             = deadband_out,
      cutoff               = cutoff_out,
      slope_uncertainty_lo = uncertainty_lo_out,
      slope_uncertainty_hi = uncertainty_hi_out,
      n_candidates         = n_candidates_out,
      reference_time       = reference_time_vec[i],
      site                 = reference_site[i],
      analyzer             = reference_analyzer[i]
    )
  })

  results <- results[stringr::str_detect(tolower(results$analyzer), tolower(analyzer_code)), ]
  return(results)

}

#' @title make_reference
#' @description Build a \code{reference_df} for \code{\link{calculate_regression}} from a
#'   compact inline string of field notes. Each line is one closure measurement with
#'   whitespace-separated columns (any number of spaces or tabs):
#'   \code{time  site  analyzer  [dur=N]  [off=N]}.
#'
#'   The first three columns are positional (required). Optional per-row window overrides
#'   use \code{key=value} syntax in any order: \code{dur=} sets \code{duration_minutes},
#'   \code{off=} sets \code{start_offset_s}. Rows without an override simply omit the token -
#'   no \code{NA} placeholder needed.
#' @param date Sampling date applied to all rows. A single \code{"YYYY-MM-DD"} string or
#'   \code{Date}. For multi-day campaigns, supply a character/Date vector with one value per row.
#' @param measurements A multiline character string, one row per closure:
#'   \code{time  site  analyzer  [dur=N]  [off=N]}.
#'   \code{time} accepts \code{"HH:MM"} or \code{"HH:MM:SS"}.
#' @param output Optional path for the output \code{.xlsx} file. If \code{NULL} (default),
#'   the tibble is returned.
#' @param tz Timezone for \code{date_time}. Default \code{"Asia/Taipei"}.
#' @return A tibble with columns \code{site}, \code{date_time}, \code{analyzer}, and
#'   optionally \code{duration_minutes} and/or \code{start_offset_s} (only present when at
#'   least one row specifies the override). When \code{output} is given, the tibble is written
#'   to Excel and returned invisibly.
#' @examples
#' make_reference(
#'   date = "2026-05-22",
#'   measurements = "
#'     10:30:15  JN1-1  1337_1074
#'     10:42:00  JN1-2  1337_1074
#'     10:54:30  JN2-1  1337_1074
#'   "
#' )
#'
#' # per-row window overrides - no NA placeholders needed
#' make_reference(
#'   date = "2026-05-22",
#'   measurements = "
#'     10:30:15  TT3-1  1450  dur=3.5
#'     10:42:00  TT3-2  1450  dur=2.0
#'     10:54:30  TT3-3  1450  off=45
#'     11:10:00  TT1-1  1450
#'   "
#' )
#' @importFrom lubridate ymd_hms
#' @importFrom tibble tibble
#' @importFrom openxlsx write.xlsx
#' @export
make_reference <- function(date,
                           measurements,
                           output = NULL,
                           tz     = "Asia/Taipei") {

  # --- parse measurements string ---
  lines <- unlist(strsplit(measurements, "\n"))
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) stop("'measurements' contains no data rows.")

  parsed <- lapply(lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]
    parts <- parts[nchar(parts) > 0]
    if (length(parts) < 3)
      stop("Each line needs: time  site  analyzer\n  Got: '", line, "'")

    # first three tokens are positional
    row <- list(time = parts[1], site = parts[2], analyzer = parts[3],
                duration_minutes = NA_character_, start_offset_s = NA_character_)

    # remaining tokens: key=value (dur= / off=) or positional (4th=dur, 5th=off)
    if (length(parts) > 3) {
      rest <- parts[-(1:3)]
      for (tok in rest) {
        if (grepl("^dur=", tok, ignore.case = TRUE)) {
          row$duration_minutes <- sub("^[Dd][Uu][Rr]=", "", tok)
        } else if (grepl("^off=", tok, ignore.case = TRUE)) {
          row$start_offset_s <- sub("^[Oo][Ff][Ff]=", "", tok)
        } else if (is.na(row$duration_minutes)) {
          row$duration_minutes <- tok          # 4th positional token
        } else {
          row$start_offset_s <- tok            # 5th positional token
        }
      }
    }
    row
  })

  n       <- length(parsed)
  has_dur <- any(vapply(parsed, function(r) !is.na(r$duration_minutes), logical(1)))
  has_off <- any(vapply(parsed, function(r) !is.na(r$start_offset_s),   logical(1)))

  # --- date ---
  d_str <- format(as.Date(as.character(rep_len(date, n))), "%Y-%m-%d")

  # --- time ---
  t_str <- vapply(parsed, `[[`, character(1), "time")
  t_str <- sub("^(\\d):", "0\\1:", t_str)
  t_str <- ifelse(nchar(t_str) == 5, paste0(t_str, ":00"), t_str)

  dt <- lubridate::ymd_hms(paste(d_str, t_str), tz = tz)

  ref <- tibble::tibble(
    site      = vapply(parsed, `[[`, character(1), "site"),
    date_time = dt,
    analyzer  = vapply(parsed, `[[`, character(1), "analyzer")
  )

  if (has_dur) {
    raw <- vapply(parsed, function(r) if (is.na(r$duration_minutes)) NA_character_ else r$duration_minutes, character(1))
    ref$duration_minutes <- suppressWarnings(as.numeric(raw))
  }
  if (has_off) {
    raw <- vapply(parsed, function(r) if (is.na(r$start_offset_s)) NA_character_ else r$start_offset_s, character(1))
    ref$start_offset_s <- suppressWarnings(as.numeric(raw))
  }

  if (!is.null(output)) {
    openxlsx::write.xlsx(as.data.frame(ref), output)
    invisible(ref)
  } else {
    ref
  }
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
#'   points and the fitted line, with strip text showing slope, R\eqn{^2}, and flag.
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
    site_label  <- paste0(site, "\nslope=", slope_label, "  R\u00b2=", r2_label, "  [", flag, "]")

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
    paste0(site, "\nslope=", slope_lbl, "  R\u00b2=", r2_lbl, "  [", flag, "]")
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
  s_to_hr <- (1/3600)  # seconds to hours (3600 s h\u207b\u00b9)
  gas_constant <- 0.082057  # gas constant (L\u00b7atm\u00b7K\u207b\u00b9\u00b7mol\u207b\u00b9)
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
#'   between flux units. Defaults to micrograms per square meter per hour.
#' @details Numeric values embedded in a string (e.g. mean +/- SD notation)
#'   are each converted individually and the surrounding text is preserved.
#'   Commas are treated as decimal separators.
#' @param input A numeric vector or a character string containing one or more
#'   numbers. Numeric vectors are handled element-wise, making the function
#'   compatible with \code{dplyr::mutate()}.
#' @param ghg The molecular formula of the greenhouse gas: \code{"co2"},
#'   \code{"ch4"}, or \code{"n2o"}.
#' @param mass Mass unit of the input flux. One of \code{"mmol"}, \code{"mg"},
#'   \code{"g"}, \code{"ug"} (micrograms), \code{"nmol"}, \code{"Mg"},
#'   \code{"umol"} (micromoles), \code{"mol"}. Default \code{"ug"}.
#' @param area Area unit of the input flux. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param time Time unit of the input flux. One of \code{"yr"}, \code{"day"},
#'   \code{"hr"}, \code{"sec"}, \code{"min"}. Default \code{"hr"}.
#' @param to_mass Target mass unit. Same options as \code{mass}. Default
#'   \code{"ug"} (micrograms). Existing calls without this argument are
#'   unaffected.
#' @param to_area Target area unit. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param to_time Target time unit. Same options as \code{time}. Default
#'   \code{"hr"}.
#' @param digits Number of decimal places to round to. Default 2.
#' @param ratio Logical. If \code{TRUE}, apply an elemental-ratio correction
#'   (C-basis for CH4, N-basis for N2O). Default \code{FALSE}.
#' @return For numeric input: a numeric vector of converted values.
#'   For character input: a character vector with numbers converted in-place
#'   (e.g. \code{"0.002 +/- 0.003"} becomes \code{"0.03 +/- 0.05"}).
#'   Returns \code{NA} for missing input.
#' @examples
#' convert_ghg_unit(97, ghg = "ch4", mass = "mg", area = "m2", time = "hr")
#' @export
convert_ghg_unit <- function(input, ghg, mass = "\u00b5g", area = "m2",
                             time = "hr", to_mass = "\u00b5g", to_area = "m2",
                             to_time = "hr", digits = 2, ratio = FALSE) {
  valid_masses <- c("mmol", "mg", "g", "\u00b5g", "nmol", "Mg", "\u00b5mol", "mol")
  valid_areas  <- c("ha", "m2")
  valid_times  <- c("yr", "day", "hr", "sec", "min")

  molar_mass <- switch(ghg,
    "co2" = 44.01, "ch4" = 16.04, "n2o" = 44.01,
    stop(paste("Invalid ghg. One of: co2, ch4, n2o")))
  if (!(mass    %in% valid_masses)) stop(paste("Invalid mass. One of:",    paste(valid_masses, collapse = ", ")))
  if (!(area    %in% valid_areas))  stop(paste("Invalid area. One of:",    paste(valid_areas,  collapse = ", ")))
  if (!(time    %in% valid_times))  stop(paste("Invalid time. One of:",    paste(valid_times,  collapse = ", ")))
  if (!(to_mass %in% valid_masses)) stop(paste("Invalid to_mass. One of:", paste(valid_masses, collapse = ", ")))
  if (!(to_area %in% valid_areas))  stop(paste("Invalid to_area. One of:", paste(valid_areas,  collapse = ", ")))
  if (!(to_time %in% valid_times))  stop(paste("Invalid to_time. One of:", paste(valid_times,  collapse = ", ")))

  # Vectorize \u2014 validation runs once here, not once per element
  if (length(input) > 1) {
    fn <- if (is.numeric(input)) numeric(1) else character(1)
    return(vapply(input, convert_ghg_unit, FUN.VALUE = fn,
                  ghg = ghg, mass = mass, area = area, time = time,
                  to_mass = to_mass, to_area = to_area, to_time = to_time,
                  digits = digits, ratio = ratio))
  }

  if (is.na(input)) return(if (is.character(input)) NA_character_ else NA_real_)

  convert_value <- function(value) {
    # Convert mass to \u00b5g
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
    # Step 2: \u00b5g m\u207b\u00b2 h\u207b\u00b9 \u2192 target units
    # Defaults (\u00b5g, m2, hr) are all identity \u2014 existing calls unchanged
    value <- switch(to_mass,
      "mmol"      = value / (molar_mass * 1000),
      "mg"        = value / 1000,
      "g"         = value / 1000000,
      "\u00b5g"        = value,
      "nmol"      = value * 1000 / molar_mass,
      "Mg"        = value / 1e12,
      "\u00b5mol"      = value / molar_mass,
      "mol"       = value / (molar_mass * 1000000)
    )
    if (to_area == "ha") value <- value * 10000
    value <- switch(to_time,
      "yr"  = value * 8760,
      "day" = value * 24,
      "sec" = value / 3600,
      "min" = value / 60,
      "hr"  = value
    )
    round(value, digits)
  }

  # Numeric scalar: convert directly, return numeric
  if (is.numeric(input)) return(convert_value(input))

  # Character input: convert numbers in-place, return modified string
  if (input == "") return(NA_character_)
  input <- gsub(",", ".", as.character(input))
  numeric_values <- unlist(regmatches(input,
    gregexpr("[-+]?[0-9]*\\.?[0-9]+", input)))
  if (length(numeric_values) == 0) return(NA_character_)

  for (num in numeric_values) {
    converted_num <- convert_value(as.numeric(num))
    input <- gsub(paste0("\\b", num, "\\b"), as.character(converted_num), input)
  }

  input
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
