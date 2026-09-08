# Calibration scan for .pareto_winners_curse_correct()'s front_correlation exponent
# (R/ghg_flux.R). Not run automatically - a record of how 0.3 was chosen, and the script to
# rerun if the pareto grid defaults (min_deadband/max_deadband/deadband_step/min_window_size/
# cutoff_step) ever change, per the caveat in .pareto_winners_curse_correct()'s doc comment.
#
# Scans m_eff = front_size^exponent for exponent in a small grid, scored by empirical coverage
# against the synthetic ground-truth in tests/testthat/test-ghg_flux.R's "Synthetic ground-truth
# recovery" block, using a smaller/faster sample (40 reps/regime, n_boot=30, cutoff_step=40) than
# that test's own full gate (100 reps/regime). Not an independent validation of 0.3 - both this
# scan and the coverage test's own confirmatory run draw from the same idealized data-generating
# process (i.i.d. Gaussian noise, exactly linear trend). See the advisor-panel review
# (2026-09-08, Statistician/Journal Reviewer takes) for why that's a real limitation, not just a
# disclosure: a genuine held-out check would need a disjoint DGP (autocorrelated residuals, a
# non-linear true trend) that 0.3 has never been scored against - not built yet.
#
# Results from the run that picked 0.3 (2026-09-08):
#   exponent=0.00  coverage=0.567 (68/120)   <- no correction (front_correlation absent/1^0)
#   exponent=0.30  coverage=0.683 (82/120)   <- selected
#   exponent=0.40  coverage=0.717 (86/120)
#   exponent=0.50  coverage=0.725 (87/120)
#   exponent=0.60  coverage=0.742 (89/120)
#   exponent=0.70  coverage=0.742 (89/120)
#   exponent=1.00  coverage=0.758 (91/120)   <- plain Bonferroni, no correlation discount
# 0.3's full-sample (300 rep) confirmation coverage: 67.7% (203/300), binom.test p = 0.90.

devtools::load_all(quiet = TRUE)

run_one <- function(true_slope, seed, exponent, n_boot_sim = 30, cutoff_step_sim = 40,
                    n_seconds = 300, noise_sd = 0.02) {
  set.seed(seed)
  elapsed <- seq(0, n_seconds - 1)
  conc <- 2 + true_slope * elapsed + stats::rnorm(n_seconds, mean = 0, sd = noise_sd)
  data <- data.frame(
    real_datetime = seq.POSIXt(from = as.POSIXct("2024-12-16 12:00:00"),
                               by = "sec", length.out = n_seconds),
    CH4 = conc)
  ref <- data.frame(date_time = as.POSIXct("2024-12-16 12:00:00", tz = "Asia/Taipei"),
                    site = "S1", analyzer = "TEST")

  # Monkeypatch the exponent so the scan doesn't need a hardcoded copy of
  # .pareto_winners_curse_correct()'s formula.
  orig <- aelab:::.pareto_winners_curse_correct
  patched <- function(candidates, sel_idx, uw, nf, conf = 0.68) {
    m_eff  <- sum(!aelab:::.pareto_dominance(uw, nf)) ^ exponent
    sel    <- candidates[[sel_idx]]
    z_base <- stats::qnorm(0.5 + conf / 2)
    z_adj  <- stats::qnorm(1 - (1 - conf) / (2 * m_eff))
    factor <- z_adj / z_base
    list(lo = sel$slope - (sel$slope - sel$uncertainty_lo) * factor,
         hi = sel$slope + (sel$uncertainty_hi - sel$slope) * factor)
  }
  environment(patched) <- environment(orig)
  assignInNamespace(".pareto_winners_curse_correct", patched, ns = "aelab")

  result <- calculate_regression(data, ref, "CH4", reference_time = "date_time", site = "site",
                                 analyzer_code = "TEST", fit_type = "linear",
                                 window_type = "pareto", duration_minutes = n_seconds / 60,
                                 instrument_precision = noise_sd, n_boot = n_boot_sim,
                                 cutoff_step = cutoff_step_sim)
  if (is.na(result$slope_uncertainty_lo) || is.na(result$slope_uncertainty_hi)) return(NA)
  true_slope >= result$slope_uncertainty_lo && true_slope <= result$slope_uncertainty_hi
}

true_slopes <- c(strong = 0.01, weak = 0.001, near_dl = 0.00015)
n_rep <- 40

for (exponent in c(0, 0.3, 0.4, 0.5, 0.6, 0.7, 1)) {
  covered <- unlist(lapply(seq_along(true_slopes), function(k) {
    slope <- true_slopes[k]
    vapply(seq_len(n_rep), function(i) run_one(slope, seed = 10000 * k + i, exponent = exponent),
           logical(1))
  }))
  covered <- covered[!is.na(covered)]
  cat(sprintf("exponent=%.2f  coverage=%.3f (%d/%d)\n",
             exponent, mean(covered), sum(covered), length(covered)))
}
