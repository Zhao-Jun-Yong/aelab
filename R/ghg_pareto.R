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

# Widens the selected candidate's interval by the number of front members ^ front_correlation
# (heuristic; front_correlation = 0.3 is fixed, no published source, fitted in
# data-raw/calibrate_pareto_front_correlation.R); unchanged when the front has one member.
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
