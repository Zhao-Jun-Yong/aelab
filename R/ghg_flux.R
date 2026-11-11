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
#' @return Return the loaded XLSX file after tidying (for LI-COR also \code{H2O}, ppm), with one
#'   \code{<gas>_unit} column per gas (\code{CO2_unit}, \code{CH4_unit}, \code{N2O_unit},
#'   \code{H2O_unit}: \code{"ppm"} or \code{"ppb"}), which
#'   \code{calculate_regression()} turns into \code{slope_unit}, and for LI-COR data one
#'   \code{<gas>_prec} column per gas (1 s precision from \code{\link{analyzer_precision}}, same
#'   unit), used by \code{fit_type = "auto"} and \code{window_type = "pareto"}. If any time offset is non-zero, a
#'   \code{real_datetime} column is added via \code{\link{convert_time}}.
#' @examples
#' ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab")
#' tidy_ghg_analyzer(ghg_data_path, "ch4")
#' tidy_ghg_analyzer(ghg_data_path, "ch4", sec = -30)
#' @export

tidy_ghg_analyzer <- function(file_path, gas, analyzer = "licor",
                               day = 0, hr = 0, min = 0, sec = 0) {

  supported <- list(licor = c("ch4", "n2o"), lgr = "ch4")
  if (!isTRUE(analyzer %in% names(supported)) || !isTRUE(gas %in% supported[[analyzer]]))
    stop("Unsupported analyzer/gas: \"", analyzer, "\"/\"", gas, "\". Use licor with \"ch4\" or \"n2o\", ",
         "or lgr with \"ch4\".")

  # Import data from the specified Excel file
  data <- readxl::read_excel(file_path)

  # Tidy data based on the type of analyzer
  if(analyzer == "licor") {
    if (gas == "ch4") {
      # Extract column titles for CH4 data
      title <- data[5, 7:11]
      # Remove unnecessary rows and keep relevant columns (DATE, TIME, H2O, CO2, CH4)
      data <- data[-c(1:6), 7:11]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      data$H2O <- as.numeric(data$H2O)
      # Filter out rows with NaN values in CH4 and CO2
      data <- dplyr::filter(data, CH4 != "NaN", CO2 != "NaN")
    } else if (gas == "n2o") {
      # Extract column titles for N2O data
      title <- data[5, 7:10]
      # Remove unnecessary rows and keep relevant columns (DATE, TIME, H2O, N2O)
      data <- data[-c(1:6), 7:10]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$N2O <- as.numeric(data$N2O)  # Convert N2O to numeric
      data$H2O <- as.numeric(data$H2O)
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

  # concentration unit per gas, carried by calculate_regression() into slope_unit
  units <- if (analyzer == "licor") c(CO2 = "ppm", CH4 = "ppb", N2O = "ppb", H2O = "ppm") else c(CO2 = "ppm", CH4 = "ppm")
  for (g in intersect(names(units), names(data))) data[[paste0(g, "_unit")]] <- units[[g]]
  # 1 s precision per gas, as goFlux's import functions add <gas>_prec (LI-COR only for now)
  if (analyzer == "licor") for (g in intersect(analyzer_precision$gas, names(data)))
    data[[paste0(g, "_prec")]] <- analyzer_precision$precision[analyzer_precision$gas == g]

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

# Exponential fits (model and slope as fluxible's exp_tz / exp_zhao18; Gaudard et al. 2025).
# For a fixed b the model is linear in Cm, a and C0, so b is found by a 1-D search on the residual
# sum of squares and the rest by least squares (variable projection; no start values to guess).
# exp_tz fits C0 from all readings, as least-squares fits of exponential chamber models do
# (Kutzbach et al. 2007, p. 1010); fluxible instead fixes it at the intercept of a 15 s linear
# fit (Gaudard et al. 2025, SI pp. 29-30), with no stated reason; a single first reading carries
# its full noise into the slope. exp_zhao18 cannot fit C0: with C0 free, t0 is absorbed by C0 and
# Cm and the model reduces to exp_tz, so C0 is fixed at fluxible's 15 s intercept instead.
# A best b at the edge of the search range [0.05, 50] / window length means the window has no
# usable curvature (linear, flat or a step): NULL, so the caller reports NA or skips the model.
.exp_curve_fit <- function(conc, elapsed_s, zhao = FALSE) {
  # exp_zhao18 only: C0 = intercept of a linear fit over the first 15 s (fluxible cz_window;
  # Gaudard et al. 2025, SI pp. 29-30); the first reading if fewer than 3 readings fall there
  first  <- elapsed_s - min(elapsed_s) <= 15
  C0     <- if (sum(first) >= 3) stats::lm.fit(cbind(1, elapsed_s[first]), conc[first])$coefficients[[1]] else conc[1]
  ss_tot <- sum((conc - mean(conc))^2)
  d  <- max(elapsed_s) - min(elapsed_s)
  if (length(conc) < 5 || d <= 0) return(NULL)
  lo <- log(0.05 / d); hi <- log(50 / d)
  .solve <- function(lb, t0) {
    u <- elapsed_s - t0; e <- exp(-exp(lb) * u)
    if (!all(is.finite(e))) return(NULL)
    # exp_tz: conc = Cm + a u + (C0 - Cm) e; exp_zhao18: conc - C0 e = Cm (1 - e) + a u
    f <- if (zhao) stats::lm.fit(cbind(1 - e, u), conc - C0 * e) else
      stats::lm.fit(cbind(1, u, e), conc)
    if (anyNA(f$coefficients)) NULL else f
  }
  .rss <- function(p) {
    f <- .solve(p[1], if (length(p) > 1) p[2] else 0)
    if (is.null(f)) Inf else sum(f$residuals^2)
  }
  grid <- seq(lo, hi, length.out = 40)
  r    <- vapply(grid, .rss, numeric(1))
  k    <- which.min(r)
  if (!length(k) || !is.finite(r[k])) return(NULL)
  # RSS independent of b (e.g. an exact line): the data hold no curvature information
  if (max(r[is.finite(r)]) - r[k] <= 1e-8 * ss_tot) return(NULL)
  lb <- stats::optimize(.rss, grid[c(max(1, k - 1), min(40, k + 1))])$minimum
  t0 <- 0
  if (zhao) {
    o <- stats::optim(c(lb, 0), .rss)
    if (o$convergence != 0) return(NULL)
    lb <- o$par[1]; t0 <- o$par[2]
  }
  if (lb - lo < 1e-3 || hi - lb < 1e-3) return(NULL)
  f <- .solve(lb, t0)
  if (is.null(f)) return(NULL)
  cf <- f$coefficients
  list(Cm = cf[[1]], a = cf[[2]], b = exp(lb), t0 = t0,
       C0 = if (zhao) C0 else cf[[1]] + cf[[3]], residuals = f$residuals)
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
         kmax_used = NA_real_, curvature = unname(stats::coef(fit)[3]))
  }

  .exp_result <- function(zhao = FALSE) {
    fit_used <- if (zhao) "exp_zhao18" else "exp_tz"
    ef <- .exp_curve_fit(conc, elapsed_s, zhao)
    if (is.null(ef)) return(NULL)
    # exp_zhao18 reports the slope at its fitted t0, exp_tz at the user's t_zero
    slope <- if (zhao) ef$a + ef$b * (ef$Cm - ef$C0) else
      ef$a + ef$b * (ef$Cm - ef$C0) * exp(-ef$b * t_zero)
    r_sq <- if (ss_tot > 0) 1 - sum(ef$residuals^2) / ss_tot else NA_real_
    list(slope = slope, r_square = r_sq, b_param = ef$b, fit_used = fit_used,
         residuals = ef$residuals, kmax_used = NA_real_, amp = ef$Cm - ef$C0)
  }

  .exp_tz_result <- function(kmax) {
    res <- .exp_result()
    if (is.null(res) || (!is.null(kmax) && res$b_param > kmax)) return(NULL)
    res
  }

  if (fit_type == "linear")    return(.lm_result())
  if (fit_type == "quadratic") return(.quad_result())

  if (fit_type %in% c("exp_tz", "exp_zhao18")) {
    res <- .exp_result(zhao = fit_type == "exp_zhao18")
    if (is.null(res)) return(list(slope = NA_real_, r_square = NA_real_,
                                  b_param = NA_real_, fit_used = fit_type,
                                  residuals = NA_real_, kmax_used = NA_real_))
    if (!is.null(kappamax) && res$b_param > kappamax) {
      lin <- stats::lm(conc ~ elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = res$b_param, fit_used = "linear",
                  residuals = stats::residuals(lin), kmax_used = NA_real_))
    }
    return(res)
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

    # A more complex model is accepted only if its AICc is lower (Kutzbach et al. 2007, p. 1011;
    # Leiber-Sauheitl et al. 2014, p. 753; goFlux best.flux "AICc" criterion).
    # k counts the fitted parameters plus the residual variance.
    .aicc <- function(res) {
      n <- length(res$residuals); k <- .n_fit_params(res$fit_used) + 1
      rss <- sum(res$residuals^2)
      if (n - k - 1 <= 0) return(NA_real_)
      if (rss <= 0) return(-Inf)  # exact fit
      n * log(rss / n) + 2 * k + 2 * k * (k + 1) / (n - k - 1)
    }
    best_aicc <- .aicc(best)

    # Step 2: quadratic \u2014 accept if AICc is lower and the curve decelerates (curvature
    # opposite in sign to the linear slope). A curve bending the other way has "no plausible
    # physical process" and the linear model is preferred (Levy et al. 2011, Eur. J. Soil Sci.
    # 62:811, p. 817, said of exponential models and applied here to the quadratic); this also
    # blocks t0-slope sign flips.
    quad <- .quad_result()
    quad_aicc <- .aicc(quad)
    if (!is.na(quad_aicc) && !is.na(best_aicc) && quad_aicc < best_aicc &&
        !is.na(quad$curvature) && sign(quad$curvature) == -sign(lm_slope)) {
      best <- quad
      best_aicc <- quad_aicc
    }

    # Step 3: exp_tz with kappamax - accept if kappa passes, AICc is lower and the curve levels off:
    # C''(t) = -b^2 (Cm - C0) exp(-b t), so it bends against the slope only when Cm - C0 has the
    # sign of the linear slope (concave fits of exponential models: "no plausible physical
    # process", linear preferred; Levy et al. 2011, p. 817).
    exp_res <- .exp_tz_result(kmax)
    if (!is.null(exp_res) && sign(exp_res$amp) == sign(lm_slope)) {
      exp_aicc <- .aicc(exp_res)
      if (!is.na(exp_aicc) && !is.na(best_aicc) && exp_aicc < best_aicc) best <- exp_res
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

# Once-per-session advisory for window_type = "pareto" - see the @section Experimental note on
# calculate_regression()'s window_type param and .pareto_winners_curse_correct()'s source comment
# for what's not yet validated. A plain message(), not a warning(): nothing is wrong with a given
# call, this is a standing caveat about the feature as a whole, so it fires once (env flag) rather
# than once per measurement - a package this processes hundreds of measurements per batch run
# would otherwise be unusable with this on.
.pareto_experimental_notice_env <- new.env(parent = emptyenv())
.pareto_experimental_notice_env$shown <- FALSE

.notify_pareto_experimental <- function() {
  if (!.pareto_experimental_notice_env$shown) {
    message(
      "aelab: window_type = \"pareto\" is experimental. Its slope_uncertainty_lo/hi winner's-",
      "curse correction is calibrated only against idealized synthetic data (i.i.d. Gaussian ",
      "noise around an exactly linear trend) - not yet validated against real chamber closures, ",
      "autocorrelated noise, or non-linear trends. Do not report these intervals as validated ",
      "until broader validation is done (see ?calculate_regression). This message shows once ",
      "per session."
    )
    .pareto_experimental_notice_env$shown <- TRUE
  }
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
                                  min_window_size, cutoff_step, n_boot, min_obs = 4) {

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
      if (sum(rows) < max(4, min_obs)) next  # too few points to fit any of the supported model families

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
      # let the exponential refits return strongly curved slopes.
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

  # Winner's-curse correction: the Pareto front tends to favor whichever candidate's bootstrap
  # interval was spuriously narrow by chance. Widen the reported interval by how many candidates
  # were genuine contenders on the selection axis itself; leaves it unchanged when only one
  # candidate is on the front (see .pareto_winners_curse_correct()).
  corrected <- .pareto_winners_curse_correct(candidates, sel_idx, uw, nf)
  selected  <- candidates[[sel_idx]]
  selected$uncertainty_lo <- corrected$lo
  selected$uncertainty_hi <- corrected$hi

  list(status = "selected", n_candidates = n_candidates, selected = selected)
}

# Internal helper: given two equal-length numeric vectors to minimize (uw = uncertainty width,
# nf = negative fit-quality score), min-max normalize both, find the non-dominated (Pareto)
# points, and return the index of the front member closest (Euclidean) to the normalized ideal
# corner (0, 0). A constant vector normalizes to all-zero on that axis rather than dividing by
# zero. Exposed separately from .pareto_select_window() so the selection math itself is
# unit-testable without needing real data/fitting/bootstrap.
.pareto_front_select <- function(uw, nf) {
  nu <- .pareto_normalize(uw)
  ns <- .pareto_normalize(nf)

  is_dominated <- .pareto_dominance(nu, ns)
  front_idx <- which(!is_dominated)
  dist      <- sqrt(nu[front_idx]^2 + ns[front_idx]^2)
  front_idx[which.min(dist)]
}

# Internal helper: min-max normalize a numeric vector to [0, 1]. A constant vector normalizes to
# all-zero rather than dividing by zero. Shared by .pareto_front_select() and every caller of
# .pareto_dominance() below so there is exactly one definition of "normalized" to keep in sync -
# a code-review finding on an earlier version of this file: .pareto_dominance() re-implemented
# this same closure inline behind a normalized = FALSE flag instead of sharing one helper, so an
# edit to the normalization logic could silently desync the two callers.
.pareto_normalize <- function(x) {
  rng <- range(x)
  if (diff(rng) == 0) rep(0, length(x)) else (x - rng[1]) / diff(rng)
}

# Internal helper: Pareto-dominance flags over two ALREADY min-max normalized (via
# .pareto_normalize()) equal-length vectors to minimize, factored out of .pareto_front_select() so
# the near-winner correction below reuses the exact same domination test. TRUE where a candidate
# is dominated (weakly worse on both axes, strictly worse on at least one).
.pareto_dominance <- function(uw, nf) {
  vapply(seq_along(uw), function(i) {
    any(uw <= uw[i] & nf <= nf[i] & (uw < uw[i] | nf < nf[i]))
  }, logical(1))
}

# Winner's-curse correction for the selected pareto candidate's slope-uncertainty interval.
# INSPIRED BY, but not an implementation of, Zrnic & Fithian (2025) Sec 4 "effective Bonferroni"
# (see Efforts/Academic/summary_Zrnic2025Flexible in the author's vault) - their actual
# construction is a step-down algorithm restricted by each candidate's estimated suboptimality
# gap, with a formal coverage guarantee, and their stated advantage over alternatives is needing
# no tuning parameter. What's implemented here is a much cruder heuristic (front membership raised
# to an empirically-fit power, see front_correlation below) that borrows the general idea - don't
# protect the full candidate set, only genuine near-winners - without that guarantee. Flagged by
# this package's own advisor-panel review (2026-09-08) as a citation that should read as
# inspiration, not lineage; recorded here rather than reimplementing their actual step-down
# construction, which remains a real option if this heuristic doesn't hold up under more scrutiny
# (see the caveats below and in data-raw/calibrate_pareto_front_correlation.R).
#
# The Pareto front selects on (uncertainty width, fit quality) - not on the slope estimate itself
# - so competitiveness has to be measured on *that* axis: two windows of very different length
# routinely land on nearly the same slope estimate on genuinely linear data while differing
# enormously in precision, so a first version of this correction that measured competitiveness via
# slope-value proximity treated almost every candidate as a "near-tie" and wildly over-widened the
# interval (99% empirical coverage against a 68% target, caught by the ground-truth coverage test
# below - see git history). Non-dominated (Pareto-front) candidates are exactly the ones that were
# genuine contenders for selection, so front size is the starting point for "how many candidates
# could plausibly have won instead" - but see front_correlation immediately below: it is NOT used
# as a parameter-free count on its own, because plain Bonferroni over the raw front size
# (front_correlation = 1) still overcorrected (~79% empirical coverage): front members come from
# *overlapping* windows of the same underlying data, so they are highly positively correlated, not
# independent, and Bonferroni's union bound is only tight for independent tests (the effect the
# "effective number of independent tests" literature for correlated multiple testing addresses,
# e.g. Li & Ji 2005 - cited for the general idea, not as the source of this specific formula).
# front_correlation discounts the raw front size toward that correlation. The correction is
# referenced to the *winner's own* bootstrap SE (not each candidate's own, which would let a
# single wide/imprecise front member's own noise dominate the shared radius, reproducing the same
# over-widening failure), and reduces to the unmodified interval exactly when the front has only
# one member (the winner stands alone).
#
# CAVEAT (do not treat 0.3 as a validated constant): it was found by a small empirical scan
# against the ground-truth coverage test below - see data-raw/calibrate_pareto_front_correlation.R
# for the full scan and results - then "confirmed" on that same test's larger fixed-seed sample.
# Both draws come from the identical idealized data-generating process (i.i.d. Gaussian noise
# around an exactly linear trend); this is fitting-then-checking-on-the-same-target, not
# independent validation, and the advisor-panel review above (Statistician, Journal Reviewer
# takes) named this explicitly. 0.3 is not derived analytically, is specific to this function's
# own default grid density (min_deadband/max_deadband/deadband_step, min_window_size/cutoff_step -
# a much coarser or finer grid would change how correlated adjacent candidates are and may need
# recalibration), and has never been checked against a data-generating process with autocorrelated
# residuals, real chamber-closure artifacts (ebullition, imperfect seals), or a non-linear true
# trend - all flagged by the panel as untested territory this synthetic test cannot reach. Treat
# `window_type = "pareto"`'s reported coverage as validated for linear-trend/i.i.d.-Gaussian
# conditions, not yet for real field closures generally.
.pareto_winners_curse_correct <- function(candidates, sel_idx, uw, nf, conf = 0.68,
                                          front_correlation = 0.3) {
  m_eff <- sum(!.pareto_dominance(.pareto_normalize(uw), .pareto_normalize(nf))) ^ front_correlation

  sel    <- candidates[[sel_idx]]
  z_base <- stats::qnorm(0.5 + conf / 2)
  z_adj  <- stats::qnorm(1 - (1 - conf) / (2 * m_eff))
  factor <- z_adj / z_base  # == 1 exactly when the front has one member (m_eff == 1)

  list(
    lo = sel$slope - (sel$slope - sel$uncertainty_lo) * factor,
    hi = sel$slope + (sel$uncertainty_hi - sel$slope) * factor
  )
}

#' @import tibble
#' @importFrom stats lm coef residuals cor quantile
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @title calculate_regression
#' @description Calculate the slope of GHG concentration change over time using linear or exponential regression.
#' @param data Data from the GHG analyzer that has been processed and time-converted.
#' @param reference_df A data frame containing measurement reference times, site names, and an \code{analyzer} column.
#' @param ghg Column name in \code{data} containing GHG concentration values (e.g., \code{"CH4"}, \code{"N2O"}).
#' @param reference_time Column name in \code{reference_df} containing the measurement start times (POSIXct).
#' @param site Column name in \code{reference_df} containing site identifiers.
#' @param analyzer_code Text used to filter rows of \code{reference_df} by its \code{analyzer} column
#'   (plain text, not a regular expression; case is ignored). A warning is given when no row matches.
#' @param duration_minutes The duration of the measurement window in minutes. Default 7.
#'   When \code{window_type = "fixed"} and \code{reference_df} contains a \code{duration_minutes}
#'   column, the per-row value (if not \code{NA}) overrides this global parameter for that
#'   specific measurement. This allows different window lengths for individual chamber closures.
#' @param num_rows The number of rows used per sliding regression window. Only used when
#'   \code{window_type = "sliding"}. Default 300 (5 min at 1 Hz; set it for your logging interval).
#' @param fit_type Model used to fit the concentration curve. One of \code{"linear"} (default),
#'   \code{"quadratic"} (second-order polynomial; slope is the initial rate at t=0),
#'   \code{"exp_tz"} (exponential with linear drift and user-specified \code{t_zero}),
#'   \code{"exp_zhao18"} (exponential with linear drift and estimated t0), or
#'   \code{"auto"} (automatic model selection: tries linear -> quadratic -> exp_tz in order,
#'   accepting a more complex model only when its AICc is lower (Kutzbach et al. 2007), a
#'   quadratic only when it levels off rather than accelerates (Levy et al. 2011), and exp_tz only
#'   when it levels off and its curvature parameter does not exceed \code{kappamax}). When \code{fit_type = "auto"} and
#'   \code{instrument_precision = NULL}, precision is taken from the \code{<ghg>_prec} column that
#'   \code{\link{tidy_ghg_analyzer}} adds (an error without it). The model chosen for each measurement is recorded in the \code{fit_used} output column.
#'   The exponential models and their slope formulas follow fluxible (Gaudard et al. 2025); the curvature
#'   \code{b} is found by a one-dimensional search, with \code{Cm} and \code{a} solved by least squares for
#'   each \code{b}. \code{"exp_tz"} fits the initial concentration \code{C0} as well (Kutzbach et al.
#'   2007); \code{"exp_zhao18"} fixes \code{C0} at the intercept of a linear fit over the first 15 s, as fluxible does
#'   (with \code{C0} free its \code{t0} could not be estimated). When the best \code{b} lies at the edge of its search range, or the fit does not
#'   depend on \code{b} (an exact line), the window has no usable curvature: an explicit exponential
#'   \code{fit_type} gives an \code{NA} slope and flag \code{"no_data"}, and \code{"auto"} keeps the linear or
#'   quadratic fit. A noisy straight line can still return an exponential slope; use \code{"auto"} when
#'   curvature is uncertain.
#' @param t_zero Reference time in seconds from window start for the \code{"exp_tz"} slope calculation. Default 0.
#' @param window_type \code{"fixed"} (default; \code{"sliding"} up to aelab 1.1.6)
#'   fits a single window starting at \code{start_offset_s} seconds after the reference time and
#'   running for \code{duration_minutes}, so all gases are regressed over the same time window.
#'   \code{"sliding"} selects the \code{num_rows}-length sub-window with the best R\eqn{^2} within
#'   the measurement period. \code{bubble_detection = TRUE} needs \code{"fixed"}: the other two
#'   pick the best-fitting stretch, which avoids the bubbles.
#'   \code{"pareto"} independently searches deadband (start skip) and cutoff (window end) times,
#'   trading off fit quality against propagated slope uncertainty via a Pareto front - adapted
#'   from the window-selection idea in Kobayashi et al. (2026), with two
#'   deliberate differences from that method: the existing \code{kappamax} gate is always applied
#'   (no candidate can be a numerically degenerate exponential fit), and a candidate pool that is
#'   empty or whose selected window fails the existing \code{zero}/\code{discard} checks is
#'   reported as such rather than returning a confident-looking number (see \code{flag}).
#'
#'   \strong{Experimental, \code{slope_uncertainty_lo}/\code{slope_uncertainty_hi} not yet
#'   validated for reporting.} The winner's-curse correction applied to this interval (see
#'   \code{.pareto_winners_curse_correct()}'s source comment) is calibrated only against
#'   idealized synthetic data (i.i.d. Gaussian noise around an exactly linear trend) - not yet
#'   checked against real chamber closures, autocorrelated noise, or non-linear true trends. Do
#'   not cite these intervals as validated in a manuscript or report until that broader
#'   validation is done; \code{window_type = "fixed"}/\code{"sliding"} are unaffected by this.
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
#' @param r2_threshold Minimum R-squared for a measurement to pass quality check. Default 0.7
#'   (0.8 up to aelab 1.1.6).
#' @param cor_threshold Minimum absolute Pearson correlation between concentration and elapsed time.
#'   Measurements below this have no detectable trend and are flagged \code{"zero"}. Default 0.5.
#' @param instrument_precision Optional. Analyser precision (1σ) in the same concentration unit as
#'   the \code{ghg} column (ppb for LI-COR CH4/N2O, ppm for CO2). Used in two ways:
#'   (1) minimum-detectable-slope check - \code{below_mdf} is \code{TRUE} when the absolute slope
#'   falls below \code{instrument_precision / window_duration_s}; the flag is not changed, because
#'   low fluxes with a good fit are kept (Gaudard et al. 2025; Maier et al. 2022); (2) kappamax for \code{fit_type = "auto"} - following Hueppi et al.
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
#'   \code{"discard"}. Default \code{NULL} (disabled). goFlux uses 2 as its default
#'   (\code{g.limit}; Rheault et al. 2024), but there it is two-sided (|g| > 2 or < 0.5) and switches to
#'   the linear flux rather than discarding; no published study justifies a value.
#' @param bubble_detection \code{FALSE} (default) returns the regression slope only. \code{TRUE}
#'   (for CH4) splits each closure into total, diffusive and ebullitive slopes following the automated
#'   method of Minaudo et al. (2026, pp. 3-4, \doi{10.1002/lom3.70054}; code archive Minaudo 2026,
#'   \doi{10.5281/zenodo.18710431}). The split uses the readings of the closure's final window,
#'   (\code{window_type = "fixed"} only). Per closure:
#'   \itemize{
#'     \item Bubbles: the readings are linearly interpolated to a 1 s grid (as the code archive,
#'       \code{find_bubbles.R}, lines 35-36, so any logging interval and gaps are handled), the
#'       concentration is divided by its mean, its variance is taken in a moving
#'       window of seconds, and seconds whose variance is above a percentile of all variances
#'       (minimum 1e-4) are flagged; flagged segments close together are merged and short ones
#'       dropped (Minaudo et al. 2026, p. 4). A reading is flagged when its nearest second is.
#'       Because the variance is not detrended, a steep
#'       clean rise (above about 0.3\% of the mean concentration per second, e.g. 2 to 6 ppm in
#'       300 s) is itself flagged as bubbles; removing each window's trend first was tried
#'       and also hid real bubbles, so such closures are left to review (\code{review_needed}).
#'     \item No bubble: \code{slope} is the regression slope, as with \code{bubble_detection = FALSE};
#'       total = diffusion = that slope, ebullition = 0.
#'     \item A bubble starting within \code{early_s} seconds: total only (paper p. 4).
#'     \item Otherwise: the total is the two-point change of the smoothed series (minimum of the
#'       first 30 s, maximum of the last 30 s, each clamped to the readings there, divided by the
#'       duration; paper p. 3, Eq. 1); the diffusion is the linear slope of the first linear
#'       segment of unflagged readings (paper pp. 3-4): within each unbroken run of unflagged
#'       readings, in time order, a window grows from the run start (pass: >= \code{min_diffusion_s}, default 20 s,
#'       adjusted R\eqn{^2} >= 0.7, p < 0.05; slope > 0 as in the code archive, \code{find_first_linear_chunk.R}, line 44, except
#'       that a run starting before the first bubble may also be negative, i.e. CH4 uptake (needs
#'       \code{trim_to_min = FALSE}, else the falling start is trimmed away); a falling run after a
#'       bubble is bubble gas leaving the chamber). Passing window ends less than 10 s apart form one
#'       block; with one block the best adjusted R\eqn{^2} is taken, with several the last end of
#'       the first block (as the code archive); ebullition = total - diffusion.
#'   }
#' @param ch4_options Only for \code{bubble_detection = TRUE} (always checked): a named list
#'   overriding any of the settings below. All lengths are seconds, whatever the
#'   logging interval (\code{bubble_window}, \code{merge_gap} and \code{min_len} on the 1 s grid;
#'   at 1 Hz without gaps a second is a reading). Defaults follow the paper text (Minaudo et al. 2026, pp. 3-4). Settings
#'   marked \emph{pending} are where the paper and its code archive differ or the paper is
#'   silent; they were asked of the author (2026-11-10) and the defaults may change.
#'   \describe{
#'     \item{\code{bubble_window}}{Seconds in the moving variance window. Default 10 (paper, 1 Hz);
#'       the code uses 11. \emph{Pending.}}
#'     \item{\code{bubble_align}}{Which second a window's variance belongs to: \code{"centre"}
#'       (default; second 6 of 10), \code{"start"} or \code{"end"}. The paper does not say.
#'       \emph{Pending.} Seconds that no window can own are never flagged: with
#'       \code{"centre"} the first \code{bubble_window \%/\% 2} and the last
#'       \code{bubble_window - 1 - bubble_window \%/\% 2} (5 and 4 by default), so a bubble in
#'       the last few seconds is missed; with \code{"start"} the last, with \code{"end"} the first
#'       \code{bubble_window - 1}.}
#'     \item{\code{bubble_prob}}{Percentile of the variances above which readings are flagged.
#'       Default 0.75 (paper); the code uses 0.7. \emph{Pending.}}
#'     \item{\code{merge_gap}}{Flagged segments separated by fewer than this many seconds are
#'       merged. Default 10 (paper); the code also merges a gap of exactly 10 (\code{11}).
#'       \emph{Pending.}}
#'     \item{\code{min_len}}{Flagged segments shorter than this many seconds are dropped.
#'       Default 5 (paper, a 5 s segment is kept); the code drops it (\code{6}).
#'       \emph{Pending.}}
#'     \item{\code{early_s}, \code{early_from}}{A bubble starting within \code{early_s} seconds
#'       (default 20, paper) of \code{early_from} gives the total only. \code{"window"} (default)
#'       counts from the first reading of the window, \code{"placement"} from the reference time
#'       (placement). \emph{Pending.}}
#'     \item{\code{total_from}}{Start of the total-flux series: \code{"window"} (default; paper,
#'       "without any trimming") or \code{"segment"} (the start of the diffusion segment, as in
#'       the code). \emph{Pending.}}
#'     \item{\code{spar}, \code{nknots_ratio}}{Smoothing of the total-flux series,
#'       \code{stats::smooth.spline(spar = spar, nknots = round(nknots_ratio * n))}. Defaults 0.5
#'       and 1/3 (code); the published paper gives none, the preprint gives \code{spar = 0.8}.
#'       \emph{Pending.}}
#'     \item{\code{trim_to_min}}{\code{TRUE} drops the readings before the lowest unflagged
#'       reading in the first half of the window, and that reading, before the diffusion segment
#'       is searched, so a
#'       trace falling after placement does not start the segment. Default \code{TRUE}, as the
#'       code archive; not in the paper (the segment there starts one reading after the
#'       minimum).}
#'     \item{\code{neg_ebullition}}{Negative ebullition (diffusion > total): \code{"zero"}
#'       (default, as the code archive and Hoffmann et al. 2017, p. 112), \code{"keep"} or
#'       \code{"na"}. The Minaudo paper does not say. \emph{Pending.}}
#'     \item{\code{min_rise}}{Bubbles whose rise is \code{<=} this value (in the unit of
#'       \code{ghg}, so 1 ppm is 1000 for ppb data) are not bubbles. The rise is the last minus the lowest reading, from the
#'       reading before the flagged segment to the one after; a falling segment has rise 0. As the
#'       \code{concentration_diffusion_cutoff} of FluxSeparator (Sø et al. 2024, p. 5; 1 ppm in
#'       its code, no value in the paper). Applied after the early-bubble rule, so it never
#'       changes an early-bubble closure; with no bubble left the closure is
#'       \code{"no bubble: rises <= min_rise"}. Default 0 (off).}
#'     \item{\code{bubble_pad}}{Seconds added before and after each flagged segment before the
#'       diffusion segment is searched, as FluxSeparator's \code{IndexSpan} (Sø et al. 2024, p. 5;
#'       30 readings in its code, at 1 Hz). Applied after the early-bubble rule. Default 0.}
#'     \item{\code{min_diffusion_s}}{Shortest diffusion segment, in seconds. Default 20 (the
#'       code archive's \code{length.min}); FluxSeparator needs more than 50 readings (Sø et al.
#'       2024, p. 8).}
#'   }
#' @param min_obs Fewest readings in a window (fixed, each sliding window, each pareto
#'   candidate) for a fit; fewer gives \code{flag = "no_data"}. Default 60, goFlux's
#'   \code{warn.length} (about 1 min at 1 Hz; Rheault et al. 2024, goFlux package documentation),
#'   where goFlux only warns. A fit also needs more readings than the model has parameters.
#' @return A tibble with columns: \code{start_time}, \code{end_time}, \code{n_obs} (readings in the
#'   fitted window; for \code{"fixed"}, all readings in the window, also when below \code{min_obs}),
#'   \code{h2o_start} (first \code{H2O} reading of the closure, ppm; \code{NA} without an \code{H2O}
#'   column; for \code{calculate_ghg_flux(h2o = "h2o_start")}), \code{slope} (per second, in the unit of the
#'   \code{ghg} column: ppm for CO2, ppb for LI-COR CH4/N2O), \code{slope_unit} (e.g.
#'   \code{"ppb s-1"}, from the \code{<ghg>_unit} column that \code{tidy_ghg_analyzer()} adds;
#'   \code{NA} without that column),
#'   \code{r_square}, \code{correlation}, \code{b_param} (exponential curvature; \code{NA} for
#'   linear/quadratic), \code{fit_used} (model actually used: \code{"linear"}, \code{"quadratic"},
#'   or \code{"exp_tz"}; always matches \code{fit_type} unless \code{fit_type = "auto"}),
#'   \code{g_factor} (ratio of chosen slope to linear baseline slope; \code{NA} when
#'   \code{fit_used = "linear"} or for explicit non-auto fits),
#'   \code{flag} (\code{"ok"}, \code{"discard"}, \code{"zero"}, or \code{"no_data"}),
#'   \code{below_mdf} (\code{TRUE} when the slope is below the minimum detectable slope;
#'   \code{NA} without \code{instrument_precision}),
#'   \code{reference_time}, \code{site}, \code{analyzer}. When \code{window_type = "pareto"}, five
#'   additional columns are included: \code{deadband}/\code{cutoff} (the selected candidate window,
#'   seconds from the reference time; \code{NA} for other window types), \code{slope_uncertainty_lo}/
#'   \code{slope_uncertainty_hi} (16th/84th percentile of the residual-bootstrap slope distribution
#'   for the selected window), and \code{n_candidates} (number of deadband/cutoff candidates that
#'   produced a usable fit; \code{0} when the measurement was flagged \code{"discard"} because no
#'   candidate survived).
#'   With \code{bubble_detection = TRUE} also \code{slope_regression}, \code{r_square_regression},
#'   \code{correlation_regression}, \code{flag_regression}, \code{g_factor_regression} (the
#'   regression fit, as with \code{bubble_detection = FALSE}; see \code{\link{apply_ch4_review}}),
#'   \code{slope_total}, \code{slope_diffusion}, \code{slope_diffusion_se} (its standard error, with the
#'   effective sample size n(1 - r1)/(1 + r1) in the residual variance, because the OLS SE is biased
#'   for autocorrelated 1 Hz readings (Kutzbach et al. 2007, p. 1011; Santer et al. 2000, Eq. 6 and
#'   AdjSE, pp. 7343-7344); \code{NA} without a bubble or when the effective sample size is <= 2,
#'   which then gets the label \code{"ebullition < diffusion + SE"}), \code{diffusion_r1} (lag-1 autocorrelation of the diffusion-line
#'   residuals), \code{n_obs_diffusion} (readings in the diffusion segment;
#'   \code{n_obs} without a bubble), \code{slope_ebullition} (unit of \code{ghg} per second),
#'   \code{diffusion_start}, \code{diffusion_end} (the diffusion segment; the window without a
#'   bubble), \code{total_start}, \code{total_start_conc}, \code{total_end_conc} (start time and
#'   the two end points of the two-point total, which runs to the last reading; \code{NA} without
#'   a bubble), \code{bubble_spans} (flagged readings as seconds from the first reading of the
#'   window, e.g. \code{"26-31; 227-282"}; \code{NA} without a bubble; drawn by
#'   \code{\link{plot_ghg_flux}}), \code{n_bubbles}, \code{ch4_flag} and \code{review_needed} (\code{TRUE} unless
#'   \code{ch4_flag} is \code{"ok"}, \code{"no bubble"} or \code{"no bubble: rises <= min_rise"}).
#'   For closures with a bubble, \code{slope} is
#'   \code{slope_total} and \code{below_mdf} is for the total, while \code{r_square},
#'   \code{correlation} and \code{flag} (same thresholds) are for the linear diffusion fit (the
#'   R\eqn{^2} and correlation tests are for a straight line, not for the two-point total). With no
#'   diffusion fit (\code{"early bubble: total only"}, or \code{"no linear diffusion segment"}
#'   with no run long enough) they are \code{NA}; the regression's are in the \code{*_regression}
#'   columns. \code{fit_used} and
#'   \code{b_param} describe the regression; \code{g_factor} is \code{NA}, so
#'   \code{g_factor_threshold} does not apply. \code{ch4_flag} is \code{"no bubble"},
#'   \code{"early bubble: total only"}, \code{"too few readings"}, \code{"ok"}, or one or more
#'   of these, joined by \code{"; "}: \code{"diffusion > total flux"} (ebullition was negative,
#'   see \code{neg_ebullition}); \code{"ebullition < diffusion + SE"} (a review label only,
#'   SE = standard error of the diffusion slope: the split may come from a false bubble; the
#'   code archive's "ebullition too low to be trusted" uses SE x sqrt(n) instead); \code{"no linear diffusion segment"} (no segment passed: the best-R\eqn{^2} one is
#'   used, or, when no unflagged run of 3 or more readings lasts \code{min_diffusion_s}, diffusion
#'   and ebullition are \code{NA}); \code{"diffusion after first bubble"} (the segment starts after the first flagged
#'   reading, so back-diffusion may bias it: Minaudo et al. 2026, Guideline 4, p. 10; FluxSeparator
#'   uses only readings before the first bubble, Sø et al. 2024, p. 6). \code{ch4_flag} is
#'   \code{"no bubble: rises <= min_rise"} when \code{min_rise} removed every bubble. Labels never
#'   change the values.
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
#' Zhao, P., Hammerle, A., Zeeman, M., & Wohlfahrt, G. (2018). On the calculation of daytime CO2
#' fluxes measured by automated closed transparent chambers. \emph{Agricultural and Forest
#' Meteorology}, 263, 267-275. \doi{10.1016/j.agrformet.2018.08.022}
#'
#' Kutzbach, L., et al. (2007). CO2 flux determination by closed-chamber methods can be seriously
#' biased by inappropriate application of linear regression. \emph{Biogeosciences}, 4, 1005-1025.
#' \doi{10.5194/bg-4-1005-2007}
#'
#' Levy, P. E., et al. (2011). Quantification of uncertainty in trace gas fluxes measured by the
#' static chamber method. \emph{European Journal of Soil Science}, 62, 811-821.
#' \doi{10.1111/j.1365-2389.2011.01403.x}
#'
#' Kobayashi, A. N. A., Roques, C., Hunkeler, D., & Brunner, P. (2026). Uncertainty
#' quantification in soil gas fluxes: Reducing the user's arbitrariness. \emph{European Journal
#' of Soil Science}, 77, e70415. \doi{10.1111/ejss.70415}
#'
#' Zrnic, T., & Fithian, W. (2025). Locally simultaneous inference. \emph{Annals of Statistics}.
#' The \code{window_type = "pareto"} \code{slope_uncertainty_lo}/\code{slope_uncertainty_hi}
#' interval applies a correction inspired by (not an implementation of) this paper's Section 4
#' "effective Bonferroni" idea to the winner's-curse bias in the selected candidate's bootstrap
#' interval, sized by Pareto-front membership with an empirically-calibrated (not analytically
#' derived) correlation discount - see \code{.pareto_winners_curse_correct()}'s source comment
#' for calibration caveats not yet resolved for real field data.
#'
#' Minaudo, C., et al. (2026). Uncertainty in aquatic greenhouse gas flux estimates arises from
#' subjective processing of floating chamber time series. \emph{Limnology and Oceanography:
#' Methods}, 24, e70054. \doi{10.1002/lom3.70054}
#'
#' Minaudo, C. (2026). camilleminaudo/ghg-flux-expert: Code to reproduce results from Minaudo
#' et al., 2026 (v1.0). Zenodo. \doi{10.5281/zenodo.18710431}
#'
#' Sø, J. S., et al. (2024). Self-made equipment for automatic methane diffusion and ebullition
#' measurements from aquatic environments. \emph{Journal of Geophysical Research:
#' Biogeosciences}, 129, e2024JG008035. \doi{10.1029/2024JG008035}
#'
#' Hoffmann, M., et al. (2017). A simple calculation algorithm to separate high-resolution CH4
#' flux measurements into ebullition- and diffusion-derived components. \emph{Atmospheric
#' Measurement Techniques}, 10, 109-118. \doi{10.5194/amt-10-109-2017}
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
#'
#' # CH4 with a bubble at 150 s: total, diffusion and ebullition slopes
#' t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
#' set.seed(1)
#' ch4 <- data.frame(real_datetime = t0 + 0:299,
#'                   CH4 = 2 + 1e-4 * (0:299) + rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 150))
#' reg <- calculate_regression(ch4, data.frame(date_time = t0, site = "P1", analyzer = "a"), "CH4",
#'                             reference_time = "date_time", site = "site", analyzer_code = "a",
#'                             duration_minutes = 299 / 60, bubble_detection = TRUE)
#' reg[, c("slope", "slope_diffusion", "slope_ebullition", "ch4_flag", "review_needed")]
#' @export

calculate_regression <- function(data, reference_df, ghg,
                                 reference_time, site, analyzer_code,
                                 duration_minutes = 7, num_rows = 300,
                                 fit_type = "linear", t_zero = 0,
                                 window_type = "fixed", start_offset_s = 0,
                                 r2_threshold = 0.7, cor_threshold = 0.5,
                                 instrument_precision = NULL,
                                 kappamax = NULL,
                                 g_factor_threshold = NULL,
                                 min_deadband = 30, max_deadband = 60, deadband_step = 10,
                                 min_window_size = 60, cutoff_step = 10, n_boot = 200,
                                 bubble_detection = FALSE, ch4_options = list(), min_obs = 60) {

  fit_type    <- match.arg(fit_type,    c("linear", "quadratic", "exp_tz", "exp_zhao18", "auto"))
  window_type <- match.arg(window_type, c("sliding", "fixed", "pareto"))
  if (!isTRUE(bubble_detection) && !isFALSE(bubble_detection)) stop("bubble_detection must be TRUE or FALSE.")
  ch4_opt <- .ch4_options(ch4_options)  # checked even when unused, to catch typos
  if (bubble_detection && window_type != "fixed")
    stop("bubble_detection = TRUE needs window_type = \"fixed\": sliding and pareto windows pick ",
         "the best-fitting stretch, which avoids the bubbles.")
  if (bubble_detection && toupper(ghg) != "CH4")
    warning("bubble_detection = TRUE is meant for CH4 ebullition; splitting ", ghg, " anyway.", call. = FALSE)
  if (!is.numeric(min_obs) || length(min_obs) != 1 || is.na(min_obs) || min_obs < 1)
    stop("min_obs must be a single number >= 1.")
  # a fit needs a residual degree of freedom: 2 readings always give a line with R2 = 1
  need <- max(min_obs, .n_fit_params(fit_type) + 1L)
  if (window_type == "sliding" && num_rows < need)
    stop("num_rows (", num_rows, ") is below min_obs or the readings ", fit_type, " needs (", need, ").")
  if (!ghg %in% names(data))
    stop("`data` has no column \"", ghg, "\"",
         if (toupper(ghg) %in% names(data)) paste0("; did you mean \"", toupper(ghg), "\"?") else ".")
  miss <- setdiff(c(reference_time, site, "analyzer"), names(reference_df))
  if (length(miss)) stop("`reference_df` has no column(s): ", paste(miss, collapse = ", "), ".")
  if (!inherits(reference_df[[reference_time]], "POSIXct"))
    stop("`reference_df$", reference_time, "` must be date-times (POSIXct), e.g. from lubridate::ymd_hms().")
  # plain text, not a regex: "1337+1098" must match itself
  keep <- stringr::str_detect(as.character(reference_df$analyzer),
                              stringr::fixed(analyzer_code, ignore_case = TRUE)) %in% TRUE
  if (!any(keep)) warning("No `reference_df$analyzer` contains \"", analyzer_code, "\": no rows returned.",
                          call. = FALSE)
  reference_df <- reference_df[keep, , drop = FALSE]

  # fit_type = "auto" (kappamax) and window_type = "pareto" (fit_score sigma, bootstrap) need a
  # precision: instrument_precision, else the <ghg>_prec column tidy_ghg_analyzer() adds, in the
  # data's unit (as goFlux's prec argument and <gas>_prec columns; no guessing from analyzer names)
  if ((fit_type == "auto" || window_type == "pareto") && is.null(instrument_precision)) {
    p <- unique(stats::na.omit(data[[paste0(ghg, "_prec")]]))
    if (length(p) != 1)
      stop("fit_type = \"auto\" and window_type = \"pareto\" need instrument_precision, or one value in a `",
           ghg, "_prec` column of `data` (added by tidy_ghg_analyzer() for LI-COR data).")
    instrument_precision <- p
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
  }
  # clock times as Taipei, as the reference times; else a non-Taipei system gives no_data
  data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")

  short_sites <- character(0)
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
    # concentration unit from tidy_ghg_analyzer()'s <ghg>_unit column, if any
    u <- unique(stats::na.omit(sorted_data[[paste0(ghg, "_unit")]]))
    if (length(u) > 1) stop("Mixed ", ghg, "_unit values for ", reference_site[i], ": ", paste(u, collapse = ", "))
    slope_unit <- if (length(u) == 1) paste0(u, " s-1") else NA_character_
    # first H2O reading of the closure, as goFlux's flux term (goFlux.R, H2O_flux.term)
    h2o <- if ("H2O" %in% names(sorted_data)) stats::na.omit(as.numeric(sorted_data$H2O)) else numeric(0)
    h2o_start <- if (length(h2o)) h2o[1] else NA_real_
    if (anyDuplicated(sorted_data$real_datetime))
      warning("Duplicated timestamps for ", reference_site[i], ": does `data` hold more than one ",
              "analyzer? Pass one analyzer's readings per call.", call. = FALSE)

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
    n_obs_out            <- 0L

    if (window_type == "fixed") {
      win_start     <- reference_datetime + start_off_i
      win_end       <- win_start + as.numeric(dur_min_i) * 60
      selected_data <- sorted_data[sorted_data$real_datetime >= win_start &
                                   sorted_data$real_datetime <= win_end, ]
      n_obs_out     <- nrow(selected_data)

      if (n_obs_out >= need) {
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
      if (nrow(sorted_data) < num_rows) short_sites <<- c(short_sites, reference_site[i])
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
      .notify_pareto_experimental()
      pw <- .pareto_select_window(sorted_data, ghg, reference_datetime,
                                  fit_type, t_zero, kappamax, instrument_precision,
                                  min_deadband, max_deadband, deadband_step,
                                  min_window_size, cutoff_step, n_boot, need)
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

    if (window_type != "fixed" && !is.na(best_start_dt))
      n_obs_out <- sum(sorted_data$real_datetime >= best_start_dt & sorted_data$real_datetime <= best_end_dt)

    # Quality flag: correlation threshold distinguishes "zero" (no trend) from "discard" (poor fit).
    # For window_type = "pareto", an empty admitted-candidate pool is reported as "discard" rather
    # than falling through to "no_data" (data existed; no candidate window survived the fit).
    qc_flag <- function(cor, r2) {
      # cor is NA with a fit but constant readings: no trend, so "zero"
      if (is.na(r2) || is.infinite(r2)) "no_data" else if (is.na(cor) || abs(cor) < cor_threshold) "zero" else
        if (r2 >= r2_threshold) "ok" else "discard"
    }
    flag <- if (force_discard) "discard" else qc_flag(best_cor, best_r_square)

    # Minimum detectable slope: marked, not used to override the flag. Low fluxes with a good fit
    # are kept as such (Gaudard et al. 2025, p. 4; fluxible SI Table A1) and sub-MDF fluxes
    # "should not be treated as zero fluxes or be discarded" (Maier et al. 2022, p. 457).
    below_mdf <- NA
    if (!is.null(instrument_precision) && !is.na(best_slope)) {
      duration_s <- as.numeric(difftime(best_end_dt, best_start_dt, units = "secs"))
      if (duration_s > 0) below_mdf <- abs(best_slope) < instrument_precision / duration_s
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

    # CH4 split on the final window (Minaudo et al. 2026, pp. 3-4)
    split <- NULL
    if (bubble_detection) {
      win <- if (is.na(best_start_dt)) sorted_data[0, ] else
        sorted_data[sorted_data$real_datetime >= best_start_dt & sorted_data$real_datetime <= best_end_dt, ]
      split <- .separate_ch4(as.numeric(win$real_datetime), as.numeric(win[[ghg]]), ch4_opt,
                             as.numeric(reference_datetime))
      split$regression <- best_slope
      reg_qc <- list(r_square = best_r_square, correlation = best_cor, flag = flag, g_factor = g_factor)
      if (startsWith(split$ch4_flag, "no bubble")) {
        split$total <- split$diffusion <- best_slope  # keep the regression's own fit
        split$diff_start <- as.numeric(best_start_dt)
        split$diff_end <- as.numeric(best_end_dt)
      } else if (!is.na(split$total)) {
        best_slope <- split$total
        g_factor <- NA_real_  # describes the regression curve, no longer the reported slope
        # flag, r_square and correlation always describe the diffusion line; NA when there is
        # none (total only), the regression's stay in *_regression
        if (flag != "no_data") {
          best_r_square <- split$diff_r2
          best_cor <- split$diff_cor
          flag <- if (is.na(split$diff_r2)) NA_character_ else qc_flag(best_cor, best_r_square)
        }
        if (!is.null(instrument_precision))
          below_mdf <- if (split$total_s > 0) abs(best_slope) < instrument_precision / split$total_s else NA
      }
    }

    out <- tibble(
      start_time           = format(best_start_dt, "%Y/%m/%d %H:%M:%S"),
      end_time             = format(best_end_dt, "%Y/%m/%d %H:%M:%S"),
      n_obs                = n_obs_out,
      h2o_start            = h2o_start,
      slope                = best_slope,
      slope_unit           = slope_unit,
      r_square             = best_r_square,
      correlation          = best_cor,
      b_param              = best_b_param,
      fit_used             = best_fit_used,
      g_factor             = g_factor,
      flag                 = flag,
      below_mdf            = below_mdf,
      deadband             = deadband_out,
      cutoff               = cutoff_out,
      slope_uncertainty_lo = uncertainty_lo_out,
      slope_uncertainty_hi = uncertainty_hi_out,
      n_candidates         = n_candidates_out,
      reference_time       = reference_time_vec[i],
      site                 = reference_site[i],
      analyzer             = reference_analyzer[i]
    )
    if (!is.null(split)) {
      out <- dplyr::bind_cols(out, tibble(
        slope_regression = split$regression, r_square_regression = reg_qc$r_square,
        correlation_regression = reg_qc$correlation, flag_regression = reg_qc$flag,
        g_factor_regression = reg_qc$g_factor, slope_total = split$total,
        slope_diffusion = split$diffusion, slope_diffusion_se = split$diff_se,
        diffusion_r1 = split$diff_r1,
        n_obs_diffusion = if (startsWith(split$ch4_flag, "no bubble")) n_obs_out else split$diff_n,
        slope_ebullition = split$ebullition,
        diffusion_start = format(as.POSIXct(split$diff_start, tz = "Asia/Taipei"), "%Y/%m/%d %H:%M:%S"),
        diffusion_end = format(as.POSIXct(split$diff_end, tz = "Asia/Taipei"), "%Y/%m/%d %H:%M:%S"),
        total_start = format(as.POSIXct(split$total_t0, tz = "Asia/Taipei"), "%Y/%m/%d %H:%M:%S"),
        total_start_conc = split$total_c0, total_end_conc = split$total_c1,
        bubble_spans = split$bubble_spans, n_bubbles = split$n_bubbles, ch4_flag = split$ch4_flag,
        review_needed = !split$ch4_flag %in% c("ok", "no bubble", "no bubble: rises <= min_rise")))
    }
    out
  })

  if (length(short_sites))
    warning("Fewer readings than num_rows (", num_rows, "), so no_data, for: ",
            paste(unique(short_sites), collapse = ", "), call. = FALSE)
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

#' @title apply_ch4_review
#' @description Apply review decisions to CH4 closures split by
#'   \code{calculate_regression(bubble_detection = TRUE)}, typically the ones with
#'   \code{review_needed = TRUE}. Values are only changed here, by the decisions given.
#' @param regression_result Output of \code{calculate_regression(bubble_detection = TRUE)}.
#' @param decisions Data frame with the \code{by} columns and \code{decision}, one row per closure:
#'   \describe{
#'     \item{\code{"accept"}}{Keep the split as it is.}
#'     \item{\code{"total"}}{Total only: \code{slope} is \code{slope_total}; diffusion and
#'       ebullition are \code{NA}.}
#'     \item{\code{"not_bubble"}}{Use the regression fit as for a closure without a bubble:
#'       \code{slope}, \code{r_square}, \code{correlation}, \code{flag} and \code{g_factor} come
#'       from the \code{*_regression} columns, total = diffusion = \code{slope_regression},
#'       ebullition = 0, and the diffusion segment is the whole window.}
#'     \item{\code{"discard"}}{\code{flag} is \code{"discard"}; values are kept.}
#'   }
#'   Other columns (e.g. a note) are ignored. To rerun a closure with other \code{ch4_options},
#'   call \code{calculate_regression()} with only its row of \code{reference_df} instead.
#' @param by Columns identifying a closure in both tables. Default \code{c("site",
#'   "reference_time")}; a character \code{reference_time} in \code{decisions} is read as
#'   Asia/Taipei time.
#' @return \code{regression_result} with the decisions applied and a \code{ch4_decision} column:
#'   the decision, \code{"unreviewed"} for \code{review_needed} closures without one, or
#'   \code{"auto"}.
#' @examples
#' t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
#' set.seed(1)
#' ch4 <- data.frame(real_datetime = t0 + 0:299,
#'                   CH4 = 2 + 1e-4 * (0:299) + rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 150))
#' reg <- calculate_regression(ch4, data.frame(date_time = t0, site = "P1", analyzer = "a"), "CH4",
#'                             reference_time = "date_time", site = "site", analyzer_code = "a",
#'                             duration_minutes = 299 / 60, bubble_detection = TRUE)
#' dec <- data.frame(site = "P1", reference_time = "2024-01-01 08:00:00", decision = "not_bubble")
#' apply_ch4_review(reg, dec)[, c("slope", "slope_ebullition", "flag", "ch4_decision")]
#' @export
apply_ch4_review <- function(regression_result, decisions, by = c("site", "reference_time")) {
  reg <- regression_result
  if (!"review_needed" %in% names(reg))
    stop("regression_result needs the columns of calculate_regression(bubble_detection = TRUE).")
  miss <- setdiff(c(by, "decision"), names(decisions))
  if (length(miss)) stop("decisions is missing: ", paste(miss, collapse = ", "))
  if (length(setdiff(by, names(reg)))) stop("regression_result is missing: ", paste(setdiff(by, names(reg)), collapse = ", "))
  bad <- setdiff(decisions$decision, c("accept", "total", "not_bubble", "discard"))
  if (length(bad)) stop("Unknown decision: ", paste(bad, collapse = ", "),
                        ". Use accept, total, not_bubble or discard.")
  if ("reference_time" %in% by && !inherits(decisions$reference_time, "POSIXct"))
    decisions$reference_time <- as.POSIXct(as.character(decisions$reference_time), tz = "Asia/Taipei")
  if ("reference_time" %in% by) reg$reference_time <- lubridate::force_tz(reg$reference_time, "Asia/Taipei")
  key <- function(d) do.call(paste, c(lapply(by, function(b) as.character(d[[b]])), sep = "\r"))
  k_reg <- key(reg); k_dec <- key(decisions)
  if (anyDuplicated(k_dec)) stop("More than one decision for a closure.")
  unmatched <- !k_dec %in% k_reg
  if (any(unmatched)) warning(sum(unmatched), " decision(s) match no closure.", call. = FALSE)
  d <- decisions$decision[match(k_reg, k_dec)]

  tot <- d %in% "total"
  reg$slope[tot] <- reg$slope_total[tot]
  reg[tot, intersect(c("slope_diffusion", "slope_diffusion_se", "diffusion_r1",
                       "slope_ebullition"), names(reg))] <- NA_real_
  nb <- d %in% "not_bubble"
  for (x in c("r_square", "correlation", "flag", "g_factor")) reg[[x]][nb] <- reg[[paste0(x, "_regression")]][nb]
  reg$slope[nb] <- reg$slope_total[nb] <- reg$slope_diffusion[nb] <- reg$slope_regression[nb]
  reg$slope_ebullition[nb] <- 0
  reg$slope_diffusion_se[nb] <- NA_real_
  for (x in intersect("diffusion_r1", names(reg))) reg[[x]][nb] <- NA_real_
  reg$diffusion_start[nb] <- reg$start_time[nb]
  reg$diffusion_end[nb] <- reg$end_time[nb]
  reg$flag[d %in% "discard"] <- "discard"
  reg$ch4_decision <- ifelse(!is.na(d), d, ifelse(reg$review_needed, "unreviewed", "auto"))
  reg
}

utils::globalVariables("line")  # plot_ghg_flux() aes column

#' @title plot_ghg_flux
#' @description Plot GHG concentration over time for each measurement window, overlaid with the
#'   fitted line, colored by its quality flag (\code{flag}). Use this for visual QC of
#'   \code{\link{calculate_regression}} output. Each window is drawn with the model in its
#'   \code{fit_used} column. For CH4 closures split with \code{bubble_detection = TRUE}, the solid
#'   line is the linear diffusion fit over its segment (its colour is the diffusion fit's flag), the dashed white line is the two-point total
#'   (Minaudo et al. 2026, Eq. 1, p. 3), readings flagged as bubbles are coral triangles, and the
#'   panel title adds the total, diffusion and ebullition slopes and \code{ch4_flag}.
#' @param data Analyzer data processed by \code{tidy_ghg_analyzer()} and \code{convert_time()}.
#' @param regression_result Output tibble from \code{calculate_regression()}.
#' @param ghg Column name in \code{data} for the gas concentration (e.g. \code{"CH4"}, \code{"N2O"}).
#' @param fit_type Model used for windows without a \code{fit_used} value (results from older
#'   versions). One of \code{"linear"} (default), \code{"quadratic"}, \code{"exp_tz"},
#'   \code{"exp_zhao18"}, \code{"auto"} (drawn as linear).
#' @param t_zero Reference time in seconds for \code{"exp_tz"} slope. Default 0.
#' @param flag_colors Named character vector mapping flag values to colors.
#' @param second_gas Optional column name in \code{data} (e.g. \code{"CO2"}) drawn in a panel
#'   beside each window, same time axis, readings only. A jump in both gases at once points to a
#'   chamber disturbance rather than a bubble (Minaudo et al. 2026, Guideline 5, p. 10).
#'   Default \code{NULL} (none).
#' @param base_size Base font size of the aelab minimal theme (as \code{\link{plot_point}}). Default 11.
#' @return A \code{ggplot} object with one panel per window (two with \code{second_gas}), or
#'   \code{NULL} with a warning when no window has readings. The y axis gives the unit from
#'   \code{slope_unit} or the \code{<gas>_unit} column of \code{tidy_ghg_analyzer()}.
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
                          flag_colors = c(ok      = "#8ab29a",
                                          discard = "#f2cc8f",
                                          zero    = "#f2cc8f",
                                          no_data = "#aaaaaa"),
                          second_gas = NULL, base_size = 11) {

  fit_type <- match.arg(fit_type, c("linear", "quadratic", "exp_tz", "exp_zhao18", "auto"))
  for (g in c(ghg, second_gas))
    if (!g %in% names(data)) stop("Column '", g, "' not found in data.")

  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  }
  # clock times as Taipei, as the reference times; else a non-Taipei system gives no_data
  data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")

  unit_of <- function(g) {
    u <- if (g == ghg && "slope_unit" %in% names(regression_result)) sub(" s-1$", "", regression_result$slope_unit)
    if (!length(stats::na.omit(u)) && paste0(g, "_unit") %in% names(data)) u <- data[[paste0(g, "_unit")]]
    u <- unique(stats::na.omit(u))
    if (length(u) == 1) paste0(g, " (", u, ")") else g
  }
  as_time <- function(x) as.POSIXct(x, format = "%Y/%m/%d %H:%M:%S", tz = "Asia/Taipei")
  num <- function(x, d = 3) if (is.na(x)) "NA" else trimws(formatC(x, digits = d, format = "g"))

  # bubble closures from calculate_regression(bubble_detection = TRUE): the diffusion line is drawn
  is_split <- function(row) "ch4_flag" %in% names(row) && !startsWith(row$ch4_flag, "no bubble") &&
    !is.na(row$diffusion_start)
  has_total <- function(row) "total_start_conc" %in% names(row) && !is.na(row$total_start_conc)
  # bubble closures: slope is the total, while R2 and flag describe the diffusion fit
  label_of <- function(row) {
    fit_lbl <- paste0("R\u00b2=", if (!is.na(row$r_square)) round(row$r_square, 3) else "NA", "  [", row$flag, "]")
    if ("ch4_flag" %in% names(row) && !startsWith(row$ch4_flag, "no bubble"))
      return(paste0(row$site, "\ntotal=", num(row$slope_total), "  ebullition=", num(row$slope_ebullition),
                    "\ndiffusion=", num(row$slope_diffusion), "  ", fit_lbl, "\n{", row$ch4_flag, "}"))
    paste0(row$site, "\nslope=", num(row$slope, 5), "  ", fit_lbl)
  }

  # one panel per window and gas, keyed by row number (sites may share a label)
  n <- nrow(regression_result)
  gases <- c(ghg, second_gas)
  panel_id <- function(i, g) paste(i, g)
  panel_lbl <- character(0)
  pts <- lns <- bub <- sec <- list()

  for (i in seq_len(n)) {
    row  <- regression_result[i, ]
    flag <- row$flag
    panel_lbl[panel_id(i, ghg)] <- if (is.null(second_gas)) label_of(row) else
      sub("\n", paste0("  ", unit_of(ghg), "\n"), label_of(row), fixed = TRUE)
    if (!is.null(second_gas)) panel_lbl[panel_id(i, second_gas)] <- paste0(row$site, "\n", unit_of(second_gas))
    st <- as_time(row$start_time)
    et <- as_time(row$end_time)
    if (is.na(st) || is.na(et)) next

    win <- data[data$real_datetime >= st & data$real_datetime <= et, ]
    win <- win[order(win$real_datetime), ]
    win <- win[!is.na(win[[ghg]]), ]
    if (nrow(win) < 2) next

    elapsed_s <- as.numeric(difftime(win$real_datetime, win$real_datetime[1], units = "secs"))
    conc_vec  <- as.numeric(win[[ghg]])
    pid <- panel_id(i, ghg)
    pts[[length(pts) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_s, conc = conc_vec)
    if (!is.null(second_gas))
      sec[[length(sec) + 1]] <- data.frame(panel = panel_id(i, second_gas), elapsed_s = elapsed_s,
                                           conc = as.numeric(win[[second_gas]]))

    if ("bubble_spans" %in% names(row) && !is.na(row$bubble_spans)) {
      sp <- matrix(as.numeric(unlist(strsplit(strsplit(row$bubble_spans, "; ")[[1]], "-"))), ncol = 2, byrow = TRUE)
      in_bub <- vapply(elapsed_s, function(x) any(x >= sp[, 1] - 0.05 & x <= sp[, 2] + 0.05), logical(1))
      if (any(in_bub)) bub[[length(bub) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_s[in_bub],
                                                           conc = conc_vec[in_bub])
    }

    elapsed_seq <- seq(min(elapsed_s), max(elapsed_s), length.out = 200)
    model <- if ("fit_used" %in% names(row) && !is.na(row$fit_used)) row$fit_used else fit_type
    if (is_split(row)) {
      seg <- win$real_datetime >= as_time(row$diffusion_start) & win$real_datetime <= as_time(row$diffusion_end)
      if (sum(seg) >= 2) {
        elapsed_seq <- seq(min(elapsed_s[seg]), max(elapsed_s[seg]), length.out = 200)
        fit    <- stats::lm(y ~ x, data.frame(x = elapsed_s[seg], y = conc_vec[seg]))
        fitted <- stats::predict(fit, newdata = data.frame(x = elapsed_seq))
      } else fitted <- rep(NA_real_, 200)
    } else if (has_total(row)) {
      fitted <- rep(NA_real_, 200)  # total only: no diffusion line
    } else if (model == "quadratic") {
      fit    <- stats::lm(conc_vec ~ elapsed_s + I(elapsed_s^2))
      fitted <- stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq))
    } else if (model %in% c("exp_tz", "exp_zhao18")) {
      ef <- .exp_curve_fit(conc_vec, elapsed_s, zhao = model == "exp_zhao18")
      u  <- elapsed_seq - if (is.null(ef)) 0 else ef$t0
      fitted <- if (is.null(ef)) rep(NA_real_, 200) else
        ef$Cm + ef$a * u + (ef$C0 - ef$Cm) * exp(-ef$b * u)
    } else {
      fit    <- stats::lm(conc_vec ~ elapsed_s)
      fitted <- stats::predict(fit, newdata = data.frame(elapsed_s = elapsed_seq))
    }
    lns[[length(lns) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_seq, fitted = fitted,
                                         flag = flag, line = "fit / diffusion")
    if (has_total(row)) {
      t0 <- as.numeric(difftime(as_time(row$total_start), win$real_datetime[1], units = "secs"))
      lns[[length(lns) + 1]] <- data.frame(panel = pid, elapsed_s = c(t0, max(elapsed_s)),
                                           fitted = c(row$total_start_conc, row$total_end_conc),
                                           flag = NA_character_, line = "total (two-point)")
    }
  }

  if (!length(pts)) {
    warning("plot_ghg_flux(): no window has ", ghg, " readings in data; returning NULL.")
    return(NULL)
  }
  levels_panel <- as.vector(t(outer(seq_len(n), gases, panel_id)))
  levels_panel <- levels_panel[levels_panel %in% unlist(lapply(c(pts, sec), `[[`, "panel"))]
  fac <- function(d) { d <- do.call(rbind, d); if (!is.null(d)) d$panel <- factor(d$panel, levels_panel); d }
  pts <- fac(pts); lns <- fac(lns); bub <- fac(bub); sec <- fac(sec)
  lns <- lns[!is.na(lns$fitted), ]

  colors <- flag_colors[names(flag_colors) %in% lns$flag]

  # solid marks on a dark grey panel (all >= 3:1 contrast to it); colour is kept for the fit's flag,
  # bubble readings differ from the ok line by shape as well as colour
  bg <- "#383838"
  reading <- function(d) ggplot2::geom_point(data = d, ggplot2::aes(x = elapsed_s, y = conc), shape = 16,
                                             color = "#a8a8a8", size = 0.9, alpha = 0.8, na.rm = TRUE)
  p <- ggplot2::ggplot() + reading(pts)
  if (!is.null(sec)) p <- p + reading(sec)
  if (!is.null(bub))
    p <- p + ggplot2::geom_point(data = bub, ggplot2::aes(x = elapsed_s, y = conc, shape = "bubble reading"),
                                 color = "#e07a5f", size = 1.4) +
      ggplot2::scale_shape_manual(values = c(`bubble reading` = 17))
  if (nrow(lns)) {
    fl <- lns[lns$line == "fit / diffusion", ]
    tl <- lns[lns$line != "fit / diffusion", ]
    p <- p + ggplot2::geom_line(data = fl, ggplot2::aes(x = elapsed_s, y = fitted, color = flag,
                                                        linetype = line), linewidth = 1.2)
    if (nrow(tl))
      p <- p + ggplot2::geom_line(data = tl, ggplot2::aes(x = elapsed_s, y = fitted, linetype = line),
                                  color = "white", linewidth = 0.6)
    # line-type key only when a total line is drawn (bubble closures); else it explains nothing
    p <- p + ggplot2::scale_linetype_manual(values = c(`fit / diffusion` = "solid", `total (two-point)` = "22"),
                                            guide = if (nrow(tl)) ggplot2::guide_legend(override.aes = list(color = "white")) else "none")
  }
  p +
    ggplot2::scale_color_manual(values = colors, labels = function(x) paste("fit:", x)) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = if (is.null(second_gas)) NULL else 2,
                        labeller = ggplot2::as_labeller(panel_lbl)) +
    ggplot2::labs(x = "Elapsed time (s)", y = if (is.null(second_gas)) unit_of(ghg) else "Concentration",
                  color = NULL, linetype = NULL, shape = NULL) +
    .aelab_theme("minimal", base_size, "white") +
    # default font: Century Gothic is missing from the PDF device and errors there (R CMD check);
    # dark grey background, not the theme's transparent one
    ggplot2::theme(text = ggplot2::element_text(family = ""),
                   plot.background = ggplot2::element_rect(fill = bg, color = NA),
                   panel.grid = ggplot2::element_line(color = "#4d4d4d"),
                   panel.grid.minor = ggplot2::element_line(color = "#424242"))
}

#' @title calculate_ghg_flux
#' @description Calculate the greenhouse gas (GHG) flux based on input parameters from a data frame.
#' @param data A data frame containing relevant data with columns for slope, area, volume, and temperature.
#' @param slope Name of the column in `data` that contains the slope values of the GHG concentration change
#'   (in ppm/s, or ppb/s with \code{conc_unit = "ppb"}).
#' @param area Name of the column in `data` that contains the values of the area of the chamber (in square meter).
#' @param volume Name of the column in `data` that contains values of the volume of the chamber (in litre).
#' @param temp Name of the column in `data` that contains values of the temperature of the gas (in Celsius).
#' @param conc_unit Unit of the concentration the slopes were fitted to: \code{"ppm"} (default) or
#'   \code{"ppb"}. LI-COR CH4 and N2O from \code{tidy_ghg_analyzer()} are in ppb; \code{"ppb"} slopes
#'   are divided by 1000 so the flux stays in mmol m-2 h-1. When \code{data} has a
#'   \code{slope_unit} column (from \code{calculate_regression()} on \code{tidy_ghg_analyzer()}
#'   data), it is used for each row and \code{conc_unit} only fills rows where it is \code{NA};
#'   a \code{conc_unit} given explicitly that contradicts it is an error.
#' @param pressure Optional name of a column with air pressure (kPa). \code{NULL} (default) uses
#'   101.325 kPa (1 atm).
#' @param h2o Optional name of a column with water vapour (ppm), e.g. \code{h2o_start} from
#'   \code{calculate_regression()}. \code{NULL} (default) treats the air as dry. The flux uses the
#'   moles of dry air, V P (1 - H2O) / (R T A), as goFlux's \code{flux.term()} (Rheault et al.
#'   2024), because LI-7810/7820 report dry mole fractions (LI-COR output tables).
#' @return \code{data} with \code{flux} and \code{unit} added. When \code{data} has
#'   \code{slope_diffusion} and \code{slope_ebullition} (from
#'   \code{calculate_regression(bubble_detection = TRUE)}), \code{flux_diffusion} and
#'   \code{flux_ebullition} are added too (and \code{flux_diffusion_se} from
#'   \code{slope_diffusion_se});
#'   \code{flux} is then the total CH4 flux, because
#'   \code{slope} holds the total slope for closures with a bubble.
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

calculate_ghg_flux <- function(data, slope = "slope", area = "area", volume = "volume", temp = "temp",
                               conc_unit = c("ppm", "ppb"), pressure = NULL, h2o = NULL) {

  given <- !missing(conc_unit)
  conc_unit <- match.arg(conc_unit)
  unit_row <- rep(conc_unit, nrow(data))
  if ("slope_unit" %in% names(data)) {  # from calculate_regression() via tidy_ghg_analyzer()
    su <- sub(" s-1$", "", data$slope_unit)
    bad <- setdiff(stats::na.omit(su), c("ppm", "ppb"))
    if (length(bad)) stop("Unknown slope_unit: ", paste(bad, collapse = ", "))
    if (given && any(!is.na(su) & su != conc_unit))
      stop("conc_unit = \"", conc_unit, "\" contradicts the slope_unit column (",
           paste(unique(stats::na.omit(data$slope_unit)), collapse = ", "), ").")
    unit_row[!is.na(su)] <- su[!is.na(su)]
  }
  to_ppm <- ifelse(unit_row == "ppb", 0.001, 1)

  # Constants
  s_to_hr <- (1/3600)  # seconds to hours (3600 s h⁻¹)
  gas_constant <- 0.082057  # gas constant (L·atm·K⁻¹·mol⁻¹)
  celsius_to_kelvin <- 273.15  # Celsius to Kelvin conversion
  micro_to_milli <- 0.001  # micromoles to millimoles conversion

  # Check if specified columns exist in the data frame
  required_cols <- c(slope, area, volume, temp, pressure, h2o)
  if (!all(required_cols %in% names(data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }

  # Extract values from the data frame
  area <- data[[area]]
  volume <- data[[volume]]
  temp <- data[[temp]]
  # goFlux flux term V P (1 - H2O) / (R T A): LI-COR gases are dry mole fractions, so the moles of
  # dry air in the chamber are needed; without the columns, 1 atm and dry air
  p_atm <- if (is.null(pressure)) 1 else data[[pressure]] / 101.325
  dry <- if (is.null(h2o)) 1 else 1 - data[[h2o]] / 1e6
  to_flux <- function(s) (s * to_ppm * volume * p_atm * dry * (1/s_to_hr) * micro_to_milli) /
    (gas_constant * (temp + celsius_to_kelvin) * area)

  data$flux <- to_flux(data[[slope]])
  # CH4 split from calculate_regression(bubble_detection = TRUE)
  for (part in c("diffusion", "diffusion_se", "ebullition")) {
    if (paste0("slope_", part) %in% names(data)) data[[paste0("flux_", part)]] <- to_flux(data[[paste0("slope_", part)]])
  }

  # Set unit of the result
  data$unit <- "mmol m-2 h-1"

  return(data)
}

# ch4_options with their defaults (see calculate_regression(bubble_detection = TRUE)), checked
.ch4_options <- function(ch4_options) {
  opt <- list(bubble_window = 10, bubble_align = "centre", bubble_prob = 0.75, merge_gap = 10,
              min_len = 5, early_s = 20, early_from = "window", total_from = "window",
              spar = 0.5, nknots_ratio = 1/3, trim_to_min = TRUE, neg_ebullition = "zero",
              min_rise = 0, bubble_pad = 0, min_diffusion_s = 20)
  if (!is.list(ch4_options)) stop("ch4_options must be a named list.")
  if (length(ch4_options) && (is.null(names(ch4_options)) || any(names(ch4_options) == "")))
    stop("Every ch4_options element needs a name.")
  unknown <- setdiff(names(ch4_options), names(opt))
  if (length(unknown)) stop("Unknown ch4_options: ", paste(unknown, collapse = ", "))
  opt[names(ch4_options)] <- ch4_options
  num <- function(x, lo, hi = Inf, whole = FALSE, lo_open = FALSE) {
    v <- opt[[x]]
    ok <- is.numeric(v) && length(v) == 1 && !is.na(v) && v <= hi && (if (lo_open) v > lo else v >= lo) &&
      (!whole || v %% 1 == 0)
    if (!ok) stop(x, " must be a ", if (whole) "whole " else "", "number ", if (lo_open) "> " else ">= ", lo,
                  if (is.finite(hi)) paste0(" and <= ", hi) else "", ".", call. = FALSE)
  }
  num("bubble_window", 2, whole = TRUE); num("bubble_prob", 0, 1, lo_open = TRUE)
  num("merge_gap", 0, whole = TRUE); num("min_len", 1, whole = TRUE); num("early_s", 0)
  num("spar", 0, lo_open = TRUE); num("nknots_ratio", 0, 1, lo_open = TRUE); num("min_rise", 0)
  num("bubble_pad", 0, whole = TRUE); num("min_diffusion_s", 0, lo_open = TRUE)
  for (x in "trim_to_min")
    if (!isTRUE(opt[[x]]) && !isFALSE(opt[[x]])) stop(x, " must be TRUE or FALSE.", call. = FALSE)
  opt$bubble_align   <- match.arg(opt$bubble_align, c("centre", "start", "end"))
  opt$early_from     <- match.arg(opt$early_from, c("window", "placement"))
  opt$total_from     <- match.arg(opt$total_from, c("window", "segment"))
  opt$neg_ebullition <- match.arg(opt$neg_ebullition, c("zero", "keep", "na"))
  opt
}

# Bubble detection of Minaudo et al. (2026, p. 4): logical flag per reading; t in seconds from 0.
# Run on readings linearly interpolated to a 1 s grid, as the code archive (find_bubbles.R,
# lines 35-36), so window, merge_gap and min_len are seconds whatever the logging interval or gaps.
.find_ch4_bubbles <- function(t, conc, window = 10, align = "centre", prob = 0.75, min_thresh = 1e-4,
                              merge_gap = 10, min_len = 5) {
  grid <- seq(0, floor(max(t)))
  at <- pmin(round(t), max(grid)) + 1  # grid point of each reading
  conc <- stats::approx(t, conc, grid)$y
  n <- length(conc)
  flag <- rep(FALSE, n)
  if (n < window + 1) return(flag[at])
  z <- conc / mean(conc)
  first <- seq_len(n - window + 1)  # first reading of each window
  v <- vapply(first, function(s) stats::var(z[s:(s + window - 1)]), numeric(1))
  owner <- first + switch(align, centre = window %/% 2, start = 0, end = window - 1)
  r <- rle(v > max(min_thresh, stats::quantile(v, prob)))
  # merge high runs split by fewer than merge_gap low windows
  inner <- seq_along(r$values) > 1 & seq_along(r$values) < length(r$values)
  r$values[!r$values & r$lengths < merge_gap & inner] <- TRUE
  r <- rle(inverse.rle(r))
  r$values[r$values & r$lengths < min_len] <- FALSE
  flag[owner] <- inverse.rle(r)
  flag[at]
}

# Unflag bubbles whose rise is <= min_rise. Rise as FluxSeparator's ebullitive_flux() (Sø et al.
# 2024 p. 5): last - lowest reading, from the reading before the segment to the one after; 0 if the
# segment falls or its lowest reading comes after its highest.
.drop_small_bubbles <- function(conc, flag, min_rise) {
  r <- rle(flag)
  e <- cumsum(r$lengths)
  b <- e - r$lengths + 1
  for (k in which(r$values)) {
    s <- conc[max(1, b[k] - 1):min(length(conc), e[k] + 1)]
    rise <- if (s[1] < s[length(s)] && which.min(s) < which.max(s)) s[length(s)] - min(s) else 0
    if (rise <= min_rise) flag[b[k]:e[k]] <- FALSE
  }
  flag
}

.lm_ch4 <- function(t, conc) {
  f <- suppressWarnings(summary(stats::lm(conc ~ t)))  # "essentially perfect fit" on flat stretches
  c(slope = f$coefficients[2, 1], r2 = f$adj.r.squared, p = f$coefficients[2, 4], se = f$coefficients[2, 2])
}

# First linear segment (>= min_s) of unflagged readings: within each unbroken run, in time order,
# grow a window from the run start; window ends passing adj. R2 >= 0.7, p < 0.05, slope > 0 (any sign before the first bubble)
# that are < 10 s apart form one block: one block -> best adj. R2, several -> last end of the
# first block (as the code archive; it reproduced 5 of 6 changed eye-accepted CarbonIMTA splits
# exactly, 2026-11-10). If none passes, the best-R2 window of the
# first long-enough run (ok = FALSE); fit = NULL when no run is long enough.
# The code archive's 100 s moving-window rising-slope check is not used: with trim_to_min on it
# changed no diffusion value (> 1%) in 97 bubbly CarbonIMTA closures (2026-11-10).
.first_linear_ch4 <- function(t, conc, flag, min_s = 20, gap_s = 10, t_bubble = Inf) {
  r <- rle(!flag)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  fallback <- NULL
  for (k in which(r$values)) {
    idx <- starts[k]:ends[k]
    tt <- t[idx]
    if (length(tt) < 3 || max(tt) - tt[1] < min_s) next
    js <- which(tt - tt[1] >= min_s & seq_along(tt) >= 3)  # a 2-point line has no R2 or p
    fits <- t(vapply(js, function(j) .lm_ch4(tt[1:j], conc[idx[1:j]]), numeric(4)))
    fits <- cbind(fits, start = tt[1], end = tt[js])
    # negative slopes (CH4 uptake) only before the first bubble: after one, a falling stretch is
    # bubble gas leaving the chamber, not uptake (5 of 160 CarbonIMTA closures otherwise)
    pass <- (fits[, "slope"] > 0 | tt[1] < t_bubble) & fits[, "r2"] >= 0.7 & fits[, "p"] < 0.05
    pass[is.na(pass)] <- FALSE
    if (any(pass)) {
      ok_i <- which(pass)
      brk <- which(diff(fits[ok_i, "end"]) > gap_s)
      best <- if (length(brk)) ok_i[brk[1]] else ok_i[which.max(fits[ok_i, "r2"])]
      return(list(fit = fits[best, ], ok = TRUE))
    }
    if (is.null(fallback)) fallback <- fits[which.max(fits[, "r2"]), ]
  }
  list(fit = fallback, ok = FALSE)
}

# Two-point total from the smoothed series (Minaudo et al. 2026, p. 3, Eq. 1)
.total_ch4 <- function(tt, conc, spar, nknots_ratio, edge_s = 30) {
  tt <- tt - tt[1]
  sm <- stats::predict(stats::smooth.spline(tt, conc, nknots = round(nknots_ratio * length(tt)),
                                            spar = spar), tt)$y
  first <- tt < edge_s
  last <- tt > max(tt) - edge_s
  clamp <- function(x, ref) min(max(x, min(ref)), max(ref))
  c0 <- clamp(min(sm[first]), conc[first])
  c1 <- clamp(max(sm[last]), conc[last])
  c(slope = (c1 - c0) / max(tt), c0 = c0, c1 = c1)
}

# Total / diffusion / ebullition split of one CH4 closure (Minaudo et al. 2026, pp. 3-4).
# time in seconds; t_ref = placement time for early_from = "placement".
.separate_ch4 <- function(time, conc, opt, t_ref = NA_real_) {
  out <- data.frame(total = NA_real_, diffusion = NA_real_, ebullition = NA_real_,
                    n_bubbles = NA_integer_, ch4_flag = "too few readings",
                    diff_r2 = NA_real_, diff_cor = NA_real_, diff_se = NA_real_,
                    diff_r1 = NA_real_,
                    diff_start = NA_real_, diff_end = NA_real_, diff_n = NA_integer_, total_s = NA_real_,
                    total_t0 = NA_real_, total_c0 = NA_real_, total_c1 = NA_real_,
                    bubble_spans = NA_character_)
  o <- order(time)
  time <- time[o]; conc <- conc[o]
  keep <- !duplicated(time)
  time <- time[keep]; conc <- conc[keep]
  if (length(time) < 20) return(out)
  tt <- time - time[1]

  flag <- .find_ch4_bubbles(tt, conc, opt$bubble_window, opt$bubble_align, opt$bubble_prob,
                            merge_gap = opt$merge_gap, min_len = opt$min_len)
  spans <- function(f) {
    r <- rle(f); e <- cumsum(r$lengths); s <- e - r$lengths + 1
    if (any(f)) paste(sprintf("%g-%g", round(tt[s[r$values]], 1), round(tt[e[r$values]], 1)),
                      collapse = "; ") else NA_character_
  }
  out$n_bubbles <- sum(rle(flag)$values)
  out$bubble_spans <- spans(flag)
  no_bubble <- function(label) {
    s <- .lm_ch4(tt, conc)[["slope"]]
    out[c("total", "diffusion", "ebullition", "ch4_flag")] <- list(s, s, 0, label)
    out
  }
  if (!any(flag)) return(no_bubble("no bubble"))

  tot <- .total_ch4(tt, conc, opt$spar, opt$nknots_ratio)
  window_total <- tot[["slope"]]
  out[c("total_t0", "total_c0", "total_c1")] <- list(time[1], tot[["c0"]], tot[["c1"]])
  out$total_s <- max(tt)
  t_early <- if (opt$early_from == "placement") time[which(flag)[1]] - t_ref else tt[which(flag)[1]]
  if (t_early <= opt$early_s) {
    out[c("total", "ch4_flag")] <- list(window_total, "early bubble: total only")
    return(out)
  }

  # min_rise and bubble_pad act on splits only, after the early-bubble rule (FluxSeparator's
  # concentration_diffusion_cutoff and IndexSpan, Sø et al. 2024 p. 5)
  if (opt$min_rise > 0) {
    flag <- .drop_small_bubbles(conc, flag, opt$min_rise)
    out$n_bubbles <- sum(rle(flag)$values)
    if (!any(flag)) {
      out[c("total_t0", "total_c0", "total_c1", "bubble_spans")] <- list(NA_real_, NA_real_, NA_real_, NA_character_)
      return(no_bubble("no bubble: rises <= min_rise"))
    }
  }
  if (opt$bubble_pad > 0) {
    ft <- tt[flag]  # pad in seconds
    flag <- flag | vapply(tt, function(x) any(abs(x - ft) <= opt$bubble_pad), logical(1))
  }
  out$bubble_spans <- spans(flag)

  excl <- flag
  if (isTRUE(opt$trim_to_min) && any(!flag & tt < max(tt) / 2)) {
    early <- which(!flag & tt < max(tt) / 2)
    # the lowest reading itself is dropped too: picked as the minimum, it is likely a noise low.
    # The code archive drops it from the final fit only, keeping it in the fits that choose the
    # end; here it is out of both (changes only unstable traces, e.g. one rejected by eye in
    # CarbonIMTA, 2026-11-10)
    excl <- flag | seq_along(tt) <= early[which.min(conc[early])]
  }
  seg <- .first_linear_ch4(tt, conc, excl, min_s = opt$min_diffusion_s, t_bubble = tt[which(flag)[1]])
  if (is.null(seg$fit)) {
    out[c("total", "ch4_flag")] <- list(window_total, "no linear diffusion segment")
    return(out)
  }
  d <- seg$fit[["slope"]]
  from <- tt >= seg$fit[["start"]]
  if (opt$total_from == "segment" && sum(from) >= 20) {
    tot <- .total_ch4(tt[from], conc[from], opt$spar, opt$nknots_ratio)
    out$total <- tot[["slope"]]
    out[c("total_t0", "total_c0", "total_c1")] <- list(time[from][1], tot[["c0"]], tot[["c1"]])
    out$total_s <- max(tt) - seg$fit[["start"]]
  } else out$total <- window_total
  ebul <- out$total - d
  seg_i <- !excl & tt >= seg$fit[["start"]] & tt <= seg$fit[["end"]]
  # 1 Hz residuals are autocorrelated, which biases the OLS SE (Kutzbach et al. 2007, p. 1011): residual
  # variance with the effective sample size n_e = n (1 - r1) / (1 + r1) (Santer et al. 2000, Eq. 6,
  # p. 7343; AdjSE, p. 7344); NA when n_e <= 2
  e <- stats::residuals(stats::lm(conc[seg_i] ~ tt[seg_i]))
  out$diff_r1 <- if (length(e) > 2) stats::acf(e, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[2] else NA_real_
  ne <- length(e) * (1 - out$diff_r1) / (1 + out$diff_r1)
  out$diff_se <- if (isTRUE(ne > 2)) seg$fit[["se"]] * sqrt((length(e) - 2) / (ne - 2)) else NA_real_
  labels <- c("diffusion > total flux"[d > out$total],
              # no SE (n_e <= 2): ebullition cannot be shown to exceed diffusion + SE, so review
              "ebullition < diffusion + SE"[d <= out$total && !isFALSE(ebul < d + out$diff_se)],
              "no linear diffusion segment"[!seg$ok],
              # Sø et al. 2024 p. 6 and Minaudo et al. 2026 Guideline 4 (p. 10): diffusion after a bubble
              "diffusion after first bubble"[seg$fit[["start"]] > tt[which(flag)[1]]])
  out$ch4_flag <- if (length(labels)) paste(labels, collapse = "; ") else "ok"
  out$diffusion <- d
  out$diff_cor <- stats::cor(tt[seg_i], conc[seg_i])
  out$diff_r2 <- out$diff_cor^2
  out$diff_n <- sum(seg_i)
  out$diff_start <- time[1] + seg$fit[["start"]]
  out$diff_end <- time[1] + seg$fit[["end"]]
  out$ebullition <- if (ebul >= 0 || opt$neg_ebullition == "keep") ebul else
    if (opt$neg_ebullition == "zero") 0 else NA_real_
  out
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
