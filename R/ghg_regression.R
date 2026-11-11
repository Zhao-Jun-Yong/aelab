#' @import tibble
#' @importFrom stats lm coef residuals cor quantile
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @title calculate_regression
#' @description Calculate the slope of GHG concentration change over time using linear or exponential regression.
#' @param data Data from the GHG analyzer that has been processed and time-converted.
#' @param reference_df A data frame containing measurement reference times, site names, and (when
#'   \code{analyzer_code} is given) an \code{analyzer} column.
#' @param ghg Column name in \code{data} containing GHG concentration values (e.g., \code{"CH4"}, \code{"N2O"}).
#' @param reference_time Column name in \code{reference_df} containing the measurement start times (POSIXct).
#'   Default \code{"date_time"}.
#' @param site Column name in \code{reference_df} containing site identifiers. Default \code{"site"}.
#' @param analyzer_code Text used to filter rows of \code{reference_df} by its \code{analyzer} column
#'   (plain text, not a regular expression; case is ignored). A warning is given when no row matches.
#'   Default \code{NULL}: all rows are used and \code{reference_df} needs no \code{analyzer} column.
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
#'   the linear flux rather than discarding. Leiber-Sauheitl et al. (2014, p. 753) accepted the
#'   nonlinear flux only when it was not more than 4 times the linear flux.
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
#' @return A tibble with columns: \code{start_time}, \code{end_time} (date-times in Asia/Taipei of the
#'   first and last reading in the fitted window), \code{n_obs} (readings in the
#'   fitted window; for \code{"fixed"}, all readings in the window, also when below \code{min_obs}),
#'   \code{h2o_start} (first \code{H2O} reading of the closure, ppm; \code{NA} without an \code{H2O}
#'   column; for \code{calculate_ghg_flux(h2o = "h2o_start")}), \code{slope} (per second, in the unit of the
#'   \code{ghg} column: ppm for CO2, ppb for LI-COR CH4/N2O), \code{slope_unit} (e.g.
#'   \code{"ppb s-1"}, from the \code{<ghg>_unit} column that \code{tidy_ghg_analyzer()} adds;
#'   \code{NA} without that column),
#'   \code{r_square}, \code{correlation}, \code{b_param} (exponential curvature; \code{NA} for
#'   linear/quadratic), \code{fit_used} (model actually used: \code{"linear"}, \code{"quadratic"},
#'   or \code{"exp_tz"}; always matches \code{fit_type} unless \code{fit_type = "auto"}; \code{r_square}
#'   and \code{fit_used} are \code{NA} when nothing was fitted, e.g. \code{flag = "no_data"}),
#'   \code{g_factor} (ratio of chosen slope to linear baseline slope; \code{NA} when
#'   \code{fit_used = "linear"} or for explicit non-auto fits),
#'   \code{flag} (\code{"ok"}, \code{"discard"}, \code{"zero"}, or \code{"no_data"}),
#'   \code{below_mdf} (\code{TRUE} when the slope is below the minimum detectable slope;
#'   \code{NA} without \code{instrument_precision}),
#'   \code{reference_time}, \code{site}, \code{analyzer}. Five more columns are always
#'   present and \code{NA} unless \code{window_type = "pareto"}: \code{deadband}/\code{cutoff} (the
#'   selected candidate window, seconds from the reference time), \code{slope_uncertainty_lo}/
#'   \code{slope_uncertainty_hi} (16th/84th percentile of the residual-bootstrap slope distribution
#'   for the selected window), and \code{n_candidates} (number of deadband/cutoff candidates that
#'   produced a usable fit; \code{0} when the measurement was flagged \code{"discard"} because no
#'   candidate survived).
#'   Only with \code{bubble_detection = TRUE} are there also \code{slope_source} (\code{"total"} when
#'   \code{slope} is \code{slope_total}, \code{"regression"} when it is \code{slope_regression}),
#'   \code{slope_regression}, \code{r_square_regression},
#'   \code{correlation_regression}, \code{flag_regression}, \code{g_factor_regression}, \code{below_mdf_regression} (the
#'   regression fit, as with \code{bubble_detection = FALSE}; see \code{\link{apply_ch4_review}}),
#'   \code{slope_total}, \code{slope_diffusion}, \code{slope_diffusion_se} (its standard error, with the
#'   effective sample size n(1 - r1)/(1 + r1) in the residual variance, because the OLS SE is biased
#'   for autocorrelated 1 Hz readings (Kutzbach et al. 2007, p. 1011; Santer et al. 2000, Eq. 6 and
#'   AdjSE, pp. 7343-7344); \code{NA} without a bubble or when the effective sample size is <= 2,
#'   which then gets the label \code{"ebullition < diffusion + SE"}), \code{diffusion_r1} (lag-1 autocorrelation of the diffusion-line
#'   residuals), \code{n_obs_diffusion} (readings in the diffusion segment;
#'   \code{n_obs} without a bubble), \code{slope_ebullition} (unit of \code{ghg} per second),
#'   \code{diffusion_start}, \code{diffusion_end} (date-times of the diffusion segment; the window
#'   without a bubble), \code{total_start}, \code{total_start_conc}, \code{total_end_conc} (start time and
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
#' Leiber-Sauheitl, K., Fuß, R., Voigt, C., & Freibauer, A. (2014). High CO2 fluxes from grassland
#' on histic Gleysol along soil carbon and drainage gradients. \emph{Biogeosciences}, 11, 749-761.
#' \doi{10.5194/bg-11-749-2014}
#'
#' Kobayashi, A. N. A., Roques, C., Hunkeler, D., & Brunner, P. (2026). Uncertainty
#' quantification in soil gas fluxes: Reducing the user's arbitrariness. \emph{European Journal
#' of Soil Science}, 77, e70415. \doi{10.1111/ejss.70415}
#'
#' Zrnic, T., & Fithian, W. (2025). A flexible defense against the winner's curse. \emph{The Annals
#' of Statistics}, 53(6). \doi{10.1214/25-AOS2553}
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
                                 reference_time = "date_time", site = "site", analyzer_code = NULL,
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
  chk <- .check_regression_inputs(data, reference_df, ghg, reference_time, site, analyzer_code,
                                  fit_type, window_type, bubble_detection, ch4_options, min_obs,
                                  num_rows)
  ch4_opt <- chk$ch4_opt; need <- chk$need; reference_df <- chk$reference_df
  instrument_precision <- .resolve_precision(data, ghg, instrument_precision, fit_type, window_type)

  reference_time_vec <- lubridate::force_tz(reference_df[[reference_time]], tzone = "Asia/Taipei")
  reference_site     <- as.character(reference_df[[site]])
  reference_analyzer <- if ("analyzer" %in% names(reference_df)) as.character(reference_df$analyzer) else
    rep(NA_character_, nrow(reference_df))
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

    cl <- .closure_readings(data, ghg, reference_datetime,
                            reference_datetime + start_off_i + as.numeric(dur_min_i) * 60,
                            reference_site[i])
    sorted_data <- cl$sorted

    fit <- switch(window_type,
      fixed = {
        win_start <- reference_datetime + start_off_i
        .fit_window_fixed(sorted_data, ghg, win_start, win_start + as.numeric(dur_min_i) * 60,
                          need, fit_type, t_zero, kappamax, instrument_precision)
      },
      sliding = {
        if (nrow(sorted_data) < num_rows) short_sites <<- c(short_sites, reference_site[i])
        .fit_window_sliding(sorted_data, ghg, num_rows, fit_type, t_zero, kappamax,
                            instrument_precision)
      },
      pareto = .fit_window_pareto(sorted_data, ghg, reference_datetime, fit_type, t_zero, kappamax,
                                  instrument_precision, min_deadband, max_deadband, deadband_step,
                                  min_window_size, cutoff_step, n_boot, need))

    if (window_type != "fixed" && !is.na(fit$start_dt))
      fit$n_obs <- sum(sorted_data$real_datetime >= fit$start_dt & sorted_data$real_datetime <= fit$end_dt)

    # For window_type = "pareto", an empty admitted-candidate pool is reported as "discard" rather
    # than falling through to "no_data" (data existed; no candidate window survived the fit).
    flag <- if (fit$force_discard) "discard" else
      .qc_flag(fit$cor, fit$r_square, cor_threshold, r2_threshold)

    # Minimum detectable slope: marked, not used to override the flag. Low fluxes with a good fit
    # are kept as such (Gaudard et al. 2025, p. 4; fluxible SI Table A1) and sub-MDF fluxes
    # "should not be treated as zero fluxes or be discarded" (Maier et al. 2022, p. 457).
    below_mdf <- .below_mdf(fit$slope, as.numeric(difftime(fit$end_dt, fit$start_dt, units = "secs")),
                            instrument_precision)

    # g-factor: ratio of chosen slope to linear baseline slope (auto mode only).
    # Values far from 1 indicate the nonlinear model diverges strongly from linear.
    g_factor <- if (!is.na(fit$slope_lm) && !is.na(fit$slope) &&
                    fit$slope_lm != 0 && fit$fit_used != "linear")
      fit$slope / fit$slope_lm
    else NA_real_

    # g-factor threshold check: override "ok" to "discard" when divergence is too large.
    if (!is.null(g_factor_threshold) && !is.na(g_factor) && flag == "ok" &&
        abs(g_factor) > g_factor_threshold) {
      flag <- "discard"
    }

    no_fit <- is.infinite(fit$r_square)  # nothing was fitted: r_square and fit_used are NA

    # CH4 split on the final window (Minaudo et al. 2026, pp. 3-4)
    split <- NULL
    if (bubble_detection) {
      m <- .merge_ch4_split(sorted_data, ghg, ch4_opt, reference_datetime, fit, flag, g_factor,
                            below_mdf, instrument_precision, cor_threshold, r2_threshold)
      split <- m$split; reg_qc <- m$reg_qc
      fit$slope <- m$slope; fit$r_square <- m$r_square; fit$cor <- m$cor
      flag <- m$flag; g_factor <- m$g_factor; below_mdf <- m$below_mdf
    }

    out <- tibble(
      start_time           = .taipei_time(fit$start_dt),
      end_time             = .taipei_time(fit$end_dt),
      n_obs                = fit$n_obs,
      h2o_start            = cl$h2o_start,
      slope                = fit$slope,
      slope_unit           = cl$slope_unit,
      r_square             = if (no_fit) NA_real_ else fit$r_square,
      correlation          = fit$cor,
      b_param              = fit$b_param,
      fit_used             = if (no_fit) NA_character_ else fit$fit_used,
      g_factor             = g_factor,
      flag                 = flag,
      below_mdf            = below_mdf,
      deadband             = fit$deadband,
      cutoff               = fit$cutoff,
      slope_uncertainty_lo = fit$uncertainty_lo,
      slope_uncertainty_hi = fit$uncertainty_hi,
      n_candidates         = fit$n_candidates,
      reference_time       = reference_time_vec[i],
      site                 = reference_site[i],
      analyzer             = reference_analyzer[i]
    )
    if (!is.null(split)) {
      out <- dplyr::bind_cols(out, tibble(
        slope_source = if (startsWith(split$ch4_flag, "no bubble") || is.na(split$total)) "regression" else "total",
        slope_regression = split$regression,
        r_square_regression = if (no_fit) NA_real_ else reg_qc$r_square,
        correlation_regression = reg_qc$correlation, flag_regression = reg_qc$flag,
        g_factor_regression = reg_qc$g_factor, below_mdf_regression = reg_qc$below_mdf,
        slope_total = split$total,
        slope_diffusion = split$diffusion, slope_diffusion_se = split$diff_se,
        diffusion_r1 = split$diff_r1,
        n_obs_diffusion = if (startsWith(split$ch4_flag, "no bubble")) fit$n_obs else split$diff_n,
        slope_ebullition = split$ebullition,
        diffusion_start = .taipei_time(split$diff_start),
        diffusion_end = .taipei_time(split$diff_end),
        total_start = .taipei_time(split$total_t0),
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

# --- helpers of calculate_regression() ---------------------------------------------------------

# Date-time in Asia/Taipei from a POSIXct or epoch seconds (NA stays NA)
.taipei_time <- function(x) as.POSIXct(as.numeric(x), origin = "1970-01-01", tz = "Asia/Taipei")

# Argument checks that need no per-closure data. Returns the validated CH4 options, the readings a
# fit needs, and `reference_df` restricted to the rows of `analyzer_code`.
.check_regression_inputs <- function(data, reference_df, ghg, reference_time, site, analyzer_code,
                                     fit_type, window_type, bubble_detection, ch4_options, min_obs,
                                     num_rows) {
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
  miss <- setdiff(c(reference_time, site, if (!is.null(analyzer_code)) "analyzer"), names(reference_df))
  if (length(miss)) stop("`reference_df` has no column(s): ", paste(miss, collapse = ", "), ".")
  if (!inherits(reference_df[[reference_time]], "POSIXct"))
    stop("`reference_df$", reference_time, "` must be date-times (POSIXct), e.g. from lubridate::ymd_hms().")
  # plain text, not a regex: "1337+1098" must match itself
  keep <- if (is.null(analyzer_code)) rep(TRUE, nrow(reference_df)) else
    stringr::str_detect(as.character(reference_df$analyzer),
                        stringr::fixed(analyzer_code, ignore_case = TRUE)) %in% TRUE
  if (!is.null(analyzer_code) && !any(keep)) warning("No `reference_df$analyzer` contains \"", analyzer_code, "\": no rows returned.",
                          call. = FALSE)
  list(ch4_opt = ch4_opt, need = need, reference_df = reference_df[keep, , drop = FALSE])
}

# fit_type = "auto" (kappamax) and window_type = "pareto" (fit_score sigma, bootstrap) need a
# precision: instrument_precision, else the <ghg>_prec column tidy_ghg_analyzer() adds, in the
# data's unit (as goFlux's prec argument and <gas>_prec columns; no guessing from analyzer names)
.resolve_precision <- function(data, ghg, instrument_precision, fit_type, window_type) {
  if ((fit_type == "auto" || window_type == "pareto") && is.null(instrument_precision)) {
    p <- unique(stats::na.omit(data[[paste0(ghg, "_prec")]]))
    if (length(p) != 1)
      stop("fit_type = \"auto\" and window_type = \"pareto\" need instrument_precision, or one value in a `",
           ghg, "_prec` column of `data` (added by tidy_ghg_analyzer() for LI-COR data).")
    instrument_precision <- p
  }
  instrument_precision
}

# Readings of one closure (valid, inside the search window, in time order) with its slope unit,
# first H2O reading (goFlux's flux term, goFlux.R H2O_flux.term) and a duplicate-timestamp warning.
.closure_readings <- function(data, ghg, start_time, end_time, site_label) {
  valid_rows    <- !is.na(data$real_datetime) & !is.na(data[[ghg]])
  filtered_data <- data[valid_rows & data$real_datetime >= start_time &
                          data$real_datetime <= end_time, ]
  sorted_data   <- filtered_data[order(filtered_data$real_datetime), ]
  # concentration unit from tidy_ghg_analyzer()'s <ghg>_unit column, if any
  u <- unique(stats::na.omit(sorted_data[[paste0(ghg, "_unit")]]))
  if (length(u) > 1) stop("Mixed ", ghg, "_unit values for ", site_label, ": ", paste(u, collapse = ", "))
  h2o <- if ("H2O" %in% names(sorted_data)) stats::na.omit(as.numeric(sorted_data$H2O)) else numeric(0)
  n_dup <- sum(duplicated(sorted_data$real_datetime))
  if (n_dup)
    warning(n_dup, " of ", nrow(sorted_data), " readings for ", site_label, " repeat an earlier timestamp ",
            "(records repeated by the analyzer, or more than one analyzer in `data`); all are used.",
            call. = FALSE)
  list(sorted = sorted_data,
       slope_unit = if (length(u) == 1) paste0(u, " s-1") else NA_character_,
       h2o_start = if (length(h2o)) h2o[1] else NA_real_)
}

# What a window method returns: the chosen fit and window of one closure. The defaults describe
# "no fit" (r_square -Inf, which the QC flag reports as no_data and the output shows as NA); the pareto fields stay NA for
# "fixed" and "sliding".
.empty_window_fit <- function(fit_type) {
  no_time <- structure(NA_real_, class = c("POSIXct", "POSIXt"))
  list(r_square = -Inf, slope = NA_real_, slope_lm = NA_real_, cor = NA_real_, b_param = NA_real_,
       fit_used = fit_type, start_dt = no_time, end_dt = no_time, deadband = NA_real_,
       cutoff = NA_real_, uncertainty_lo = NA_real_, uncertainty_hi = NA_real_,
       n_candidates = NA_integer_, force_discard = FALSE, n_obs = 0L)
}

# Fit one stretch of readings and return the fields of .empty_window_fit() that describe it.
.fit_selected <- function(selected, ghg, fit_type, t_zero, kappamax, instrument_precision) {
  elapsed_s <- as.numeric(difftime(selected$real_datetime, selected$real_datetime[1], units = "secs"))
  conc_vec  <- as.numeric(selected[[ghg]])
  fit_out   <- .fit_ghg_model(conc_vec, elapsed_s, fit_type, t_zero, kappamax, instrument_precision)
  list(r_square = fit_out$r_square, slope = fit_out$slope,
       slope_lm = if (!is.null(fit_out$slope_lm)) fit_out$slope_lm else NA_real_,
       b_param = fit_out$b_param, fit_used = fit_out$fit_used,
       cor = if (!is.na(fit_out$r_square)) stats::cor(conc_vec, elapsed_s) else NA_real_,
       start_dt = selected$real_datetime[1], end_dt = selected$real_datetime[nrow(selected)])
}

# One window of fixed start and length
.fit_window_fixed <- function(sorted_data, ghg, win_start, win_end, need, fit_type, t_zero,
                              kappamax, instrument_precision) {
  fit <- .empty_window_fit(fit_type)
  selected <- sorted_data[sorted_data$real_datetime >= win_start & sorted_data$real_datetime <= win_end, ]
  fit$n_obs <- nrow(selected)
  if (fit$n_obs >= need) {
    cand <- .fit_selected(selected, ghg, fit_type, t_zero, kappamax, instrument_precision)
    fit[names(cand)] <- cand
  }
  fit
}

# The `num_rows`-reading window with the highest R2
.fit_window_sliding <- function(sorted_data, ghg, num_rows, fit_type, t_zero, kappamax,
                                instrument_precision) {
  best <- .empty_window_fit(fit_type)
  if (nrow(sorted_data) < num_rows) return(best)
  for (j in 1:(nrow(sorted_data) - num_rows + 1)) {
    cand <- .fit_selected(sorted_data[j:(j + num_rows - 1), ], ghg, fit_type, t_zero, kappamax,
                          instrument_precision)
    if (!is.na(cand$r_square) && cand$r_square > best$r_square) best[names(cand)] <- cand
  }
  best
}

# The window picked by the Pareto search (.pareto_select_window)
.fit_window_pareto <- function(sorted_data, ghg, reference_datetime, fit_type, t_zero, kappamax,
                               instrument_precision, min_deadband, max_deadband, deadband_step,
                               min_window_size, cutoff_step, n_boot, need) {
  fit <- .empty_window_fit(fit_type)
  .notify_pareto_experimental()
  pw <- .pareto_select_window(sorted_data, ghg, reference_datetime,
                              fit_type, t_zero, kappamax, instrument_precision,
                              min_deadband, max_deadband, deadband_step,
                              min_window_size, cutoff_step, n_boot, need)
  fit$n_candidates <- pw$n_candidates
  if (identical(pw$status, "no_candidates")) {
    fit$force_discard <- TRUE
  } else if (identical(pw$status, "selected")) {
    sel <- pw$selected
    fit[c("r_square", "slope", "slope_lm", "b_param", "fit_used", "cor", "start_dt", "end_dt",
          "deadband", "cutoff", "uncertainty_lo", "uncertainty_hi")] <-
      sel[c("r_square", "slope", "slope_lm", "b_param", "fit_used", "cor", "start_dt", "end_dt",
            "deadband", "cutoff", "uncertainty_lo", "uncertainty_hi")]
  }
  # status == "no_grid" (too little data): the defaults stay, as "fixed"/"sliding" fall through
  # to flag = "no_data"
  fit
}

# Quality flag: the correlation threshold separates "zero" (no trend) from "discard" (poor fit).
.qc_flag <- function(cor, r2, cor_threshold, r2_threshold) {
  # cor is NA with a fit but constant readings: no trend, so "zero"
  if (is.na(r2) || is.infinite(r2)) "no_data" else if (is.na(cor) || abs(cor) < cor_threshold) "zero" else
    if (r2 >= r2_threshold) "ok" else "discard"
}

# TRUE when |slope| is below precision / duration (the minimum detectable slope); NA without a
# precision, a slope or a positive duration
.below_mdf <- function(slope, duration_s, instrument_precision) {
  if (is.null(instrument_precision) || is.na(slope) || !isTRUE(duration_s > 0)) return(NA)
  abs(slope) < instrument_precision / duration_s
}

# CH4 split of one closure's final window (.separate_ch4) merged with its regression result.
# Returns the split, the regression's own QC (kept as *_regression) and the updated slope,
# r_square, cor, flag, g_factor and below_mdf.
.merge_ch4_split <- function(sorted_data, ghg, ch4_opt, reference_datetime, fit, flag, g_factor,
                             below_mdf, instrument_precision, cor_threshold, r2_threshold) {
  win <- if (is.na(fit$start_dt)) sorted_data[0, ] else
    sorted_data[sorted_data$real_datetime >= fit$start_dt & sorted_data$real_datetime <= fit$end_dt, ]
  split <- .separate_ch4(as.numeric(win$real_datetime), as.numeric(win[[ghg]]), ch4_opt,
                         as.numeric(reference_datetime))
  split$regression <- fit$slope
  reg_qc <- list(r_square = fit$r_square, correlation = fit$cor, flag = flag, g_factor = g_factor,
                 below_mdf = below_mdf)
  slope <- fit$slope; r_square <- fit$r_square; cor <- fit$cor
  if (startsWith(split$ch4_flag, "no bubble")) {
    split$total <- split$diffusion <- slope  # keep the regression's own fit
    split$diff_start <- as.numeric(fit$start_dt)
    split$diff_end <- as.numeric(fit$end_dt)
  } else if (!is.na(split$total)) {
    slope <- split$total
    g_factor <- NA_real_  # describes the regression curve, no longer the reported slope
    # flag, r_square and correlation always describe the diffusion line; NA when there is
    # none (total only), the regression's stay in *_regression
    if (flag != "no_data") {
      r_square <- split$diff_r2
      cor <- split$diff_cor
      flag <- if (is.na(split$diff_r2)) NA_character_ else .qc_flag(cor, r_square, cor_threshold, r2_threshold)
    }
    if (!is.null(instrument_precision)) below_mdf <- .below_mdf(slope, split$total_s, instrument_precision)
  }
  list(split = split, reg_qc = reg_qc, slope = slope, r_square = r_square, cor = cor, flag = flag,
       g_factor = g_factor, below_mdf = below_mdf)
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
#'       ebullition are \code{NA}, and so are \code{r_square}, \code{correlation} and
#'       \code{flag} (they described the diffusion line), as for an automatic total-only closure.}
#'     \item{\code{"not_bubble"}}{Use the regression fit as for a closure without a bubble:
#'       \code{slope}, \code{r_square}, \code{correlation}, \code{flag}, \code{g_factor} and
#'       \code{below_mdf} come
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
  for (x in c("r_square", "correlation", "flag")) reg[[x]][tot] <- NA
  reg[tot, intersect(c("slope_diffusion", "slope_diffusion_se", "diffusion_r1",
                       "slope_ebullition"), names(reg))] <- NA_real_
  nb <- d %in% "not_bubble"
  for (x in c("r_square", "correlation", "flag", "g_factor", "below_mdf")) reg[[x]][nb] <- reg[[paste0(x, "_regression")]][nb]
  reg$slope[nb] <- reg$slope_total[nb] <- reg$slope_diffusion[nb] <- reg$slope_regression[nb]
  reg$slope_ebullition[nb] <- 0
  reg$slope_diffusion_se[nb] <- NA_real_
  for (x in intersect("diffusion_r1", names(reg))) reg[[x]][nb] <- NA_real_
  reg$diffusion_start[nb] <- reg$start_time[nb]
  reg$diffusion_end[nb] <- reg$end_time[nb]
  reg$slope_source[tot] <- "total"
  reg$slope_source[nb] <- "regression"
  reg$flag[d %in% "discard"] <- "discard"
  reg$ch4_decision <- ifelse(!is.na(d), d, ifelse(reg$review_needed, "unreviewed", "auto"))
  reg
}

#' @title ch4_review_template
#' @description Table of the closures that need a decision, for \code{\link{apply_ch4_review}}.
#' @param regression_result Output of \code{calculate_regression(bubble_detection = TRUE)}.
#' @param by Columns identifying a closure. Default \code{c("site", "reference_time")}.
#' @return The rows with \code{review_needed = TRUE}: the \code{by} columns, \code{ch4_flag},
#'   \code{r_square_regression} (R\eqn{^2} of the regression fit over the whole window, to sort
#'   the list by) and an empty (\code{NA}) \code{decision} column to fill in with \code{"accept"}, \code{"total"},
#'   \code{"not_bubble"} or \code{"discard"}.
#' @examples
#' t0 <- as.POSIXct("2024-01-01 08:00:00", tz = "Asia/Taipei")
#' set.seed(1)
#' ch4 <- data.frame(real_datetime = t0 + 0:299,
#'                   CH4 = 2 + 1e-4 * (0:299) + rnorm(300, 0, 2e-4) + 0.05 * (0:299 >= 150))
#' reg <- calculate_regression(ch4, data.frame(date_time = t0, site = "P1", analyzer = "a"), "CH4",
#'                             reference_time = "date_time", site = "site", analyzer_code = "a",
#'                             duration_minutes = 299 / 60, bubble_detection = TRUE)
#' ch4_review_template(reg)
#' @export
ch4_review_template <- function(regression_result, by = c("site", "reference_time")) {
  reg <- regression_result
  if (!"review_needed" %in% names(reg))
    stop("regression_result needs the columns of calculate_regression(bubble_detection = TRUE).")
  if (length(setdiff(by, names(reg)))) stop("regression_result is missing: ", paste(setdiff(by, names(reg)), collapse = ", "))
  out <- reg[reg$review_needed %in% TRUE, c(by, "ch4_flag", "r_square_regression")]
  out$decision <- NA_character_
  out
}
