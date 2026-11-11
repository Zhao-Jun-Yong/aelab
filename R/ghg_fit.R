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

# Linear or quadratic least-squares fit of concentration on elapsed seconds: the one place both
# the regression (.fit_ghg_model) and the plot (.model_curve) fit these models.
.poly_fit <- function(conc, elapsed_s, quadratic = FALSE) {
  if (quadratic) stats::lm(conc ~ elapsed_s + I(elapsed_s^2)) else stats::lm(conc ~ elapsed_s)
}

# Fitted curve of `model` ("linear", "quadratic", "exp_tz", "exp_zhao18") on the readings, at the
# times `at` (s); NA where an exponential model has no usable fit.
.model_curve <- function(model, conc, elapsed_s, at) {
  if (model %in% c("exp_tz", "exp_zhao18")) {
    ef <- .exp_curve_fit(conc, elapsed_s, zhao = model == "exp_zhao18")
    if (is.null(ef)) return(rep(NA_real_, length(at)))
    u <- at - ef$t0
    return(ef$Cm + ef$a * u + (ef$C0 - ef$Cm) * exp(-ef$b * u))
  }
  stats::predict(.poly_fit(conc, elapsed_s, model == "quadratic"), newdata = data.frame(elapsed_s = at))
}

# Internal helper: fit a GHG concentration curve and return slope, R\u00b2, curvature b, and fit_used.
# kappamax:            if provided and b > kappamax, exponential fits fall back to linear.
# instrument_precision: only used when fit_type = "auto" to compute kappamax per window
#                       using the H\u00fcppi et al. (2018) formula: kmax = |slope_lm| / precision.
.fit_ghg_model <- function(conc, elapsed_s, fit_type, t_zero,
                           kappamax = NULL, instrument_precision = NULL) {
  ss_tot <- sum((conc - mean(conc))^2)

  .lm_result <- function() {
    fit <- .poly_fit(conc, elapsed_s)
    list(slope = unname(stats::coef(fit)[2]), r_square = summary(fit)$r.squared,
         b_param = NA_real_, fit_used = "linear", residuals = stats::residuals(fit),
         kmax_used = NA_real_)
  }

  .quad_result <- function() {
    fit <- .poly_fit(conc, elapsed_s, quadratic = TRUE)
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
      lin <- .poly_fit(conc, elapsed_s)
      return(list(slope    = unname(stats::coef(lin)[2]),
                  r_square = summary(lin)$r.squared,
                  b_param  = res$b_param, fit_used = "linear",
                  residuals = stats::residuals(lin), kmax_used = NA_real_))
    }
    return(res)
  }

  if (fit_type == "auto") {
    # Step 1: linear (baseline)
    best     <- .lm_result()
    lm_slope <- best$slope  # saved for g-factor computation

    # kappamax (H\u00fcppi et al. 2018): reject exp_tz if b > |slope_lm| / precision.
    # Strong fluxes tolerate more curvature; near-detection-limit measurements stay linear.
    # A flat line gives kmax = 0, so it stays linear (as goFlux k.max and fluxible's kappamax).
    if (is.null(instrument_precision)) stop("fit_type = \"auto\" needs instrument_precision.")
    kmax <- abs(best$slope) / instrument_precision

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
