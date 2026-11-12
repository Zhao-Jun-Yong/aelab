# Once-per-session advisory for window_type = "pareto": a plain message(), not a warning(), because
# nothing is wrong with a given call. It fires once (env flag) rather than once per measurement,
# so a batch run over hundreds of closures stays usable.
.pareto_experimental_notice_env <- new.env(parent = emptyenv())
.pareto_experimental_notice_env$shown <- FALSE

.notify_pareto_experimental <- function() {
  if (!.pareto_experimental_notice_env$shown) {
    message(
      "aelab: window_type = \"pareto\" is experimental. It picks a window by a rule designed for ",
      "aelab and has not been validated against real chamber closures; it reports no uncertainty ",
      "interval. Compare it with a fixed window (see ?calculate_regression). This message shows ",
      "once per session."
    )
    .pareto_experimental_notice_env$shown <- TRUE
  }
}

# Internal helper: window_type = "pareto" candidate search for calculate_regression().
# Grids deadband (start skip, s) x cutoff (window end, s) relative to reference_datetime and fits
# each candidate with .fit_ghg_model(), so the kappamax gate is never bypassed. Each candidate has
# two scores to minimise: rmse (misfit) and an approximate slope standard error,
# instrument_precision / sqrt(Sxx), where Sxx is the sum of squared time deviations. The sensor
# precision (not rmse) sets the noise level, as in Kobayashi et al. (2026), so this score depends
# only on the window's length and spread and is not redundant with rmse. Both scores are then put
# on fixed anchors (see the pick below): misfit as the rmse excess over instrument_precision, which is
# a monotone transform of the mean Gaussian log-likelihood with sigma = instrument_precision (Kobayashi
# et al. 2026, Eq 6; same Pareto front), and precision loss as the SE relative to the SE of
# all readings. The pick is the non-dominated (Pareto) candidate closest to the ideal corner (0, 0).
# The deadband x cutoff search, Pareto front and closest-to-ideal pick follow Kobayashi et al.
# (2026); their MCMC uncertainty, model and the anchoring here are not theirs.
# Returns list(status, n_candidates, selected) - status is "no_grid" (too little data to form
# any candidate), "no_candidates" (candidates existed but none produced a usable fit), or
# "selected".
.pareto_select_window <- function(sorted_data, ghg, reference_datetime,
                                  fit_type, t_zero, kappamax, instrument_precision,
                                  min_deadband, max_deadband, deadband_step,
                                  min_window_size, cutoff_step, min_obs = 4) {

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

  candidates <- list()

  for (db in deadband_grid) {
    for (co in cutoff_grid) {
      if (co - db < min_window_size) next

      rows <- elapsed_full >= db & elapsed_full <= co
      # Floor of 4 readings (not 3): a 3-point window fitted with a quadratic has 0 residual df,
      # a perfect fit by construction that would win the rmse axis for arithmetic reasons only.
      if (sum(rows) < max(4, min_obs)) next

      cand_elapsed <- elapsed_full[rows] - db
      cand_conc    <- as.numeric(sorted_data[[ghg]][rows])

      fit_out <- tryCatch(
        .fit_ghg_model(cand_conc, cand_elapsed, fit_type, t_zero, kappamax, instrument_precision),
        error = function(e) NULL)

      if (is.null(fit_out) || is.na(fit_out$slope) || is.na(fit_out$r_square) ||
          is.infinite(fit_out$r_square) || is.null(fit_out$residuals) ||
          anyNA(fit_out$residuals)) next

      rmse <- .rmse(fit_out$residuals)
      sxx  <- sum((cand_elapsed - mean(cand_elapsed))^2)
      if (!is.finite(rmse) || !(sxx > 0)) next  # duplicate timestamps: no usable slope SE

      candidates[[length(candidates) + 1]] <- list(
        deadband = db, cutoff = co,
        slope = fit_out$slope, r_square = fit_out$r_square, rmse = rmse,
        slope_se = if (!is.null(fit_out$slope_se)) fit_out$slope_se else NA_real_,
        cor = stats::cor(cand_conc, cand_elapsed),
        b_param = fit_out$b_param, fit_used = fit_out$fit_used,
        slope_lm = if (!is.null(fit_out$slope_lm)) fit_out$slope_lm else NA_real_,
        approx_se = instrument_precision / sqrt(sxx),
        start_dt = sorted_data$real_datetime[rows][1],
        end_dt   = sorted_data$real_datetime[rows][sum(rows)]
      )
    }
  }

  n_candidates <- length(candidates)
  if (n_candidates == 0) return(no_candidates)

  se   <- vapply(candidates, function(x) x$approx_se, numeric(1))
  rmse <- vapply(candidates, function(x) x$rmse, numeric(1))
  # Fixed anchors, not the candidates' own range, so adding or dropping grid points (cutoff_step)
  # cannot rescale the axes: precision loss is SE relative to the SE of all readings from
  # min_deadband on (0 = no loss); misfit is the rmse excess over the noise, in units of sigma.
  el_all <- elapsed_full[elapsed_full >= min_deadband]
  se_ref <- instrument_precision / sqrt(sum((el_all - mean(el_all))^2))
  sel_idx <- .pareto_front_select(se / se_ref - 1, pmax(rmse / instrument_precision - 1, 0))

  list(status = "selected", n_candidates = n_candidates, selected = candidates[[sel_idx]])
}

# Internal helper: given two equal-length, already scaled numeric vectors to minimise (ideal
# corner (0, 0)), find the non-dominated (Pareto) points and return the index of the front member
# closest (Euclidean) to the corner. Ties in distance go to the smaller x (the longer window, as
# x is the precision loss), then to the first index. Separate from .pareto_select_window() so the
# selection math is unit-testable without data or fitting.
.pareto_front_select <- function(x, y) {
  is_dominated <- vapply(seq_along(x), function(i) {
    any(x <= x[i] & y <= y[i] & (x < x[i] | y < y[i]))
  }, logical(1))
  front_idx <- which(!is_dominated)
  front_idx[order(sqrt(x[front_idx]^2 + y[front_idx]^2), x[front_idx])[1]]
}
