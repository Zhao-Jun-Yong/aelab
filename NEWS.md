# aelab 1.1.6

## New functions

* `gantt_table()` — creates a coloured `flextable` Gantt-style progress table where columns are sampling campaigns and rows are measurement categories × systems. Supports sampling schedules (cells = dates) and processing-progress tables (cells = sample counts). Key parameters: `colors` (named hex fill per system), `system_labels` (short display labels), `done_cols` (campaigns completed — cells in done columns get system fill; others with values get white), `na_color` (fill for empty cells; default `"#BFBFBF"` grey for "no sampling", use `"white"` for "not yet processed"). Footer notes via `notes`. Output works across PDF (xelatex), DOCX, and PPTX via `officer`.
* `gantt_text()` — plain-text Unicode Gantt for terminal and Neovim. Same data contract as `gantt_table()`. Done cells show the value; planned-but-not-done cells are wrapped in parentheses `(value)`; empty cells show `----`.
* `process_weather_mh()` / `combine_weather_mh()` — import the CWA "MH"
  (Multifield Hourly) bulk weather report downloaded from the CODiS portal
  (`LotsDataReports.txt`). Maps `PS01` -> `pressure_hpa` and `WD01` -> `wind_ms`
  (plus temperature, humidity, wind direction, rain), handles the 01-24 hour
  convention and CWA missing-value codes, and optionally expands hourly values to
  30-minute marks. `combine_weather_mh()` reads a folder of monthly files and
  de-duplicates.
* `filter_complete_days()` — keep only the calendar days with (near-)complete
  diel coverage per logger (default >= 44 of 48 half-hour slots), replacing manual
  inspection of which deployment days are full.
* `add_sun_times()` — attach per-date sunrise/sunset to a data frame from a site's
  coordinates using the suncalc package, replacing manual sunrise/sunset lookup.

## Modified functions

* `process_hobo()` — added `type` parameter (`"do"` or `"temp"`). `type = "do"` (default)
  retains existing behaviour for the HOBO U26 Dissolved Oxygen Data Logger. `type = "temp"`
  adds support for the HOBO Pro v2 Temperature/RH Logger; returns columns `date_time`,
  `air_temp`, `rh`, `no_hobo` (one row per reading, no 30-min aggregation). Both types now
  include a year fix for Chinese locale exports that mis-parse the year as 0025.
  Now also parses English-locale exports (AM/PM or 24-hour clock, 2- or 4-digit
  years) in addition to the Chinese (上午/下午) locale, and for `type = "do"` drops
  readings with a negative (sensor-error) DO or temperature value before
  aggregation.
* `calculate_regression()` — added `window_type = "pareto"`: independently searches deadband
  (start skip) and cutoff (window end) times per closure instead of a fixed-length sliding box,
  trading off fit quality against propagated slope uncertainty via a Pareto front. Adapted from
  the window-selection idea in Kobayashi et al. (2026), with two deliberate safeguards that
  differ from that method: the existing `kappamax` gate is always applied to every candidate (no
  numerically degenerate exponential fit can be selected undetected), and an empty
  admitted-candidate pool or a selected window that fails the existing correlation/minimum-
  detectable-slope check is reported as `"discard"`/`"zero"` rather than a fabricated confident
  number. Fit quality is scored as a per-point-averaged (not summed) Gaussian log-likelihood, so
  longer windows are not structurally penalized. Slope uncertainty comes from a residual
  bootstrap (`n_boot`, default 200) — no MCMC, no new dependency. New parameters: `min_deadband`,
  `max_deadband`, `deadband_step`, `min_window_size`, `cutoff_step`, `n_boot`. New output columns
  (pareto mode only): `deadband`, `cutoff`, `slope_uncertainty_lo`, `slope_uncertainty_hi`,
  `n_candidates`. New `@references` section citing Hutchinson & Mosier (1981), Hüppi et al.
  (2018), Gaudard et al. (2025), and Kobayashi et al. (2026).
* `calculate_regression()` — fixed the `window_type = "pareto"` winner's-curse coverage shortfall
  reported below (pooled 57.7%/173/300 against nominal 68%). New internal
  `.pareto_winners_curse_correct()` widens the selected candidate's bootstrap interval by a
  Bonferroni-style correction sized by Pareto-front membership (how many candidates were genuine
  contenders for selection, not merely present in the grid), referenced to the winner's own
  bootstrap SE, inspired by (not an implementation of) Zrnic & Fithian (2025) Section 4's
  "effective Bonferroni" idea. Reduces to the unmodified interval when the front has one member.
  Two intermediate designs overcorrected first — slope-value proximity as the competitiveness
  measure (99% coverage) and plain front-size Bonferroni with no correlation discount (~79%) —
  before landing on a front-size exponent (`front_correlation = 0.3`) found by an empirical scan
  against the coverage test below, confirmed at 67.7% (203/300, `binom.test` p = 0.90) on the
  test's full fixed-seed sample. **Caveat, not yet resolved**: that scan and confirmation draw
  from the same idealized data-generating process (i.i.d. Gaussian noise, exactly linear trend) —
  an advisor-panel review of this fix (2026-09-08; Statistician and Journal Reviewer takes, in
  particular) flagged this as fitting-then-checking-on-the-same-target rather than independent
  validation, and noted the correction has not been checked against autocorrelated residuals,
  real chamber artifacts (ebullition, imperfect seals), or a non-linear true trend. See
  `.pareto_winners_curse_correct()`'s source comment and `data-raw/calibrate_pareto_front_correlation.R`
  for the full calibration record and open validation gap before trusting this on real closures
  generally.
* `calculate_regression()` — fixed a `window_type = "pareto"` bug where the residual bootstrap
  used to estimate slope uncertainty could silently admit numerically degenerate refits for
  `fit_type = "auto"` candidates that resolved to `exp_tz`/`exp_zhao18`. Each bootstrap replicate
  is refit via the candidate's already-resolved model name (e.g. `"exp_tz"`, not `"auto"`), which
  routed into `.fit_ghg_model()`'s literal exp branches gated only by the outer `kappamax`
  argument — always `NULL` on the `"auto"` path, since `"auto"` computes its curvature threshold
  internally and never populates it. With no gate, a non-converging `nls()` refit (which warns,
  not errors) could return a wildly divergent slope that entered `boot_slopes` unfiltered,
  contaminating `slope_uncertainty_lo`/`slope_uncertainty_hi`. `.fit_ghg_model()`'s `auto` branch
  now returns the internally-computed curvature threshold as `kmax_used`; every bootstrap refit
  for an `"auto"` candidate is gated by that same threshold instead of the outer `kappamax`,
  matching the gate the point-estimate fit itself was selected under. Behaviour for explicit
  `fit_type = "exp_tz"`/`"exp_zhao18"` (with a caller-supplied `kappamax`) is unchanged.
* `calculate_regression()` — switched the `window_type = "pareto"` slope-uncertainty bootstrap
  from parametric noise to degrees-of-freedom-corrected residual resampling. Each candidate's own
  residuals (already computed once by `.fit_ghg_model()`) are inflated by `sqrt(n / (n - p))`
  (`p` = number of fitted parameters for the model actually used — 2 for `linear`, 3 for
  `quadratic`/`exp_tz`, 4 for `exp_zhao18`) and then resampled with replacement around the
  candidate's fitted values to build each of the `n_boot` synthetic series, instead of adding
  fresh `rnorm(n, 0, instrument_precision)` noise. Parametric noise fixed at instrument spec
  understated real-world uncertainty whenever true residual variance exceeds instrument precision
  (turbulence, imperfect chamber seals, ebullition spikes) — the common case in field data, not
  the exception. This changes the uncertainty axis used by the Pareto front, so it can affect
  which `(deadband, cutoff)` is selected, not just the reported uncertainty width; it does not
  change `slope` (which comes from the winning candidate's point-estimate fit, never the
  bootstrap). `instrument_precision` is unaffected elsewhere (still used for the fit-quality score
  and the minimum-detectable-slope check).
* `calculate_regression()` — changed the `window_type = "pareto"` default `min_deadband` from `0`
  to `30`, matching the `start_offset_s = 30` convention this package's own `window_type = "fixed"`
  batch processing already uses across this lab's campaigns. Still fully overridable; only the
  out-of-the-box deadband search floor changes (candidate grid `seq(min_deadband, max_deadband -
  deadband_step, by = deadband_step)` now starts at 30 rather than 0 by default).
* `calculate_regression()` — raised the `window_type = "pareto"` candidate-admission floor from 3
  to 4 non-NA concentration points per candidate window. Matches this package's own cited
  data-sufficiency floor (Maier et al. 2022: fewer than 4 concentration measurements produces
  unreliable flux/uncertainty estimates), and specifically avoids a 3-point candidate paired with
  a `fit_type` that resolves to `"quadratic"` (3 fitted parameters) — 0 residual degrees of
  freedom, i.e. a perfect fit by construction with near-zero residuals, which could pull the
  min-max-normalized Pareto selection toward it for purely arithmetic (not evidential) reasons.
  With `min_window_size` defaulting to 60 and `min_deadband` defaulting to 30, this is a no-op
  under default settings at realistic sampling rates; it only matters as a safety floor for
  unusually small `min_window_size` values or sparse/gappy data.

## Tests

* Added a synthetic ground-truth recovery/coverage test for `window_type = "pareto"`'s
  `slope_uncertainty_lo`/`slope_uncertainty_hi` (the DF-corrected residual-bootstrap 68%
  interval), the one claim in this feature that had never been checked against a known-true
  slope — every other pareto test validates point behaviour on real/synthetic data where the
  true slope is unknown. Simulates three signal regimes (strong/weak/near-detection-limit)
  against a truly linear ground truth with Gaussian noise, runs the full pareto search +
  bootstrap 100x per regime, and gates on a pooled exact binomial test of observed coverage
  against the nominal 68% at a stringent 1% significance level (so a well-calibrated bootstrap
  only fails this Monte Carlo check by chance about 1 run in 100). Off by default (opt in with
  `AELAB_RUN_SLOW_TESTS=true`, ~1-2 min) since it reruns the full search+bootstrap per repetition;
  `devtools::test()`/`devtools::check()` skip it and stay fast. With current defaults this test
  fails: pooled coverage was 57.7% (173/300, `binom.test` p = 0.0002) against nominal 68%, flat
  across signal strength (56-61% per regime), while a single-fixed-candidate control with no
  Pareto-front selection among candidates showed 65.3% (p = 0.32, not distinguishable from
  nominal). This isolates the shortfall to a selection effect — the Pareto front's own
  uncertainty-width axis preferentially picks whichever of the ~16 candidate windows happened to
  draw the narrowest bootstrap interval, which is a biased (too-narrow) estimate of that
  candidate's true sampling variability — rather than a flaw in the residual-DF correction
  itself. Fixed by the winner's-curse correction documented above (with an open validation
  caveat — see that entry).

# aelab 1.1.5

## New functions

* `make_reference()` — builds a `reference_df` for `calculate_regression()` from a field-notes
  data frame. Combines separate `date` and `time` columns (or a pre-combined `datetime` column)
  into the `date_time` POSIXct column required by `calculate_regression()`. Accepts any column
  names via arguments. Optionally writes the result to `.xlsx` via `file_path`. Supports
  per-row `duration_minutes` and `start_offset_s` overrides.

## Modified functions

* `calculate_regression()` — fixed a case-sensitivity bug where `analyzer_code` was matched
  against `results$analyzer` with a case-sensitive `str_detect`, causing all results to be
  dropped when the two differed in case (e.g., `analyzer_code = "LGR"` vs reference entry
  `"lgr"`). Match is now case-insensitive. The `fit_type = "auto"` precision lookup now
  matches the analyzer model name against `analyzer_code` (case-insensitive substring) before
  falling back to the first row for the gas, so LGR data correctly uses LGR UGGA precision
  rather than LI-7810 precision.

## Data

* `analyzer_precision` — added LGR UGGA entries: CH4 0.002 ppm and CO2 1.0 ppm (1σ, 1 s).

# aelab 1.1.4

## Modified functions

* `calculate_regression()` — `reference_df` may now include optional `duration_minutes` and `start_offset_s`
  columns (numeric). When present and non-`NA` for a row, they override the global `duration_minutes` and
  `start_offset_s` parameters for that specific measurement. This allows per-measurement window control
  when individual closures have early saturation or artifact periods. Fixed a pre-existing minor issue where
  the data-load window was up to `start_offset_s` seconds shorter than the regression window. Rows with
  `NA` timestamps or `NA` concentration values are now excluded before regression (previously they could
  propagate through and cause spurious `"no_data"` flags).

# aelab 1.1.3

## New functions

* `sig_labels()` — runs Kruskal-Wallis + Dunn tests per facet level and returns a `geom_text`-ready data frame with compact letter display (CLD) annotations. Facets where Kruskal-Wallis p ≥ `alpha` are silently dropped.

## Bug fixes

* `ks_test()` now correctly handles group names that contain hyphens. Labels were previously corrupted because `cldList` splits comparison strings on `-`; group names are now sanitised to dots before Dunn testing and restored in the output.

# aelab 1.1.2

## Modified functions

* `calculate_regression()` — function signature updated. `reference_time` is replaced by a `reference_df` argument (a data frame with measurement start times, site, and analyzer identifier). New `site` and `analyzer_code` parameters carry metadata through to the output tibble, which now includes `site` and `analyzer` columns.

# aelab 1.1.0

## New functions

* `descriptive_statistic()` — grouped mean ± SD and min–max summary table.
* `normality_test_t()` — Shapiro-Wilk normality tests (raw, sqrt, log) for two-group comparisons.
* `normality_test_aov()` — Shapiro-Wilk on ANOVA residuals for one- or two-way designs.
* `aov_test()` — one-way ANOVA + Tukey HSD + compact letter display.
* `ks_test()` — Kruskal-Wallis + Dunn post-hoc (Bonferroni) + compact letter display.
* `df_trans()` — reverse square-root / log data transformation helper.
* `find_outlier()` — IQR-based outlier detection.
* `plot_point()`, `plot_line()`, `plot_box()`, `plot_bar()` — opinionated ggplot2 wrappers with aelab theme.
* `aelab_palettes()` — custom colour palette manager (7 built-in palettes).
* `scale_colour_aelab_d()`, `scale_fill_aelab_d()` — discrete ggplot2 colour/fill scales.
* `scale_colour_aelab_c()`, `scale_fill_aelab_c()` — continuous ggplot2 colour/fill scales.
* `plot_map_taiwan()` — Taiwan site map with north arrow and scale bar.
* `calc_chla_trichromatic()` — chlorophyll-a from trichromatic spectrophotometric absorbance.
* `calculate_MDF()` — Minimum Detectable Flux for static chamber GHG measurements.
* `calculate_total_co2e()` — sum GHG fluxes to CO₂e using IPCC AR6 GWPs.
* `process_weather_month()` — import monthly Taiwan weather station CSV files.
* `combine_weather_month()` — batch-import a range of monthly weather files.

## Modified functions

* `convert_ghg_unit()` — vectorized over numeric and character vectors (works directly in `dplyr::mutate()`); added `to_mass`, `to_area`, `to_time` parameters for configurable output unit (default remains µg m⁻² h⁻¹); numeric input now returns numeric (not a list); character string input returns the converted string directly (not `list(value, unit)`); `NA` input returns `NA` without error.

# aelab 0.4.0

# aelab 0.2.0

# aelab 0.1.0

* Initial CRAN submission.
