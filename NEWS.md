# aelab (development version)

## Diffusion SE with autocorrelated readings

* `slope_diffusion_se` now uses the effective sample size n(1 - r1)/(1 + r1) in the residual
  variance (Santer et al. 2000, Eq. 6, AdjSE), because the OLS SE is biased for autocorrelated
  1 Hz readings (Kutzbach et al. 2007, p. 1011); on 97 CarbonIMTA diffusion segments the residual
  r1 was significant in 96 (median 0.83) and the SE grew a median 4.5 times. New column
  `diffusion_r1`. The SE is `NA` when the effective sample size is <= 2, and those closures get the
  review label "ebullition < diffusion + SE" (2 of 160 CarbonIMTA closures; no flux changes).

## Pressure and water vapour in the flux

* `calculate_ghg_flux()` gains `pressure` (column, kPa) and `h2o` (column, ppm), both `NULL` by
  default (1 atm, dry air: results unchanged). The flux uses the moles of dry air,
  V P (1 - H2O) / (R T A), as goFlux's `flux.term()`, because LI-7810/7820 report dry mole
  fractions; leaving out H2O reads about 2-4 % high in humid air.
* `tidy_ghg_analyzer()` keeps the LI-COR `H2O` column (ppm, with `H2O_unit`);
  `calculate_regression()` adds `h2o_start`, the first H2O reading of the closure (as goFlux), for
  `calculate_ghg_flux(h2o = "h2o_start")`.

## Instrument precision

* `fit_type = "auto"` and `window_type = "pareto"` no longer guess the precision by matching
  `analyzer_code` against model names (a serial number silently got LI-7810 values, in ppb even
  for ppm data). The precision is now `instrument_precision`, else the `<ghg>_prec` column, else
  an error, as goFlux's `prec` argument and `<gas>_prec` columns.
* `tidy_ghg_analyzer()` adds `CH4_prec`, `CO2_prec` (LI-7810) and `N2O_prec` (LI-7820) to LI-COR
  data, in the data's unit; the `n2o` dataset gains `N2O_prec`.
* `analyzer_precision`: LGR UGGA rows removed (no vendor source found).

## Input checks and reading counts

* New `min_obs` in `calculate_regression()` (default 60, goFlux's `warn.length`): a window with
  fewer readings gives `flag = "no_data"` (goFlux only warns). A fit also needs more readings than
  the model has parameters, so 2 readings no longer give `ok` with R² = 1. Applies to fixed,
  each sliding window and each pareto candidate.
* New columns `n_obs` (readings in the fitted window) and, with `bubble_detection = TRUE`,
  `n_obs_diffusion`.
* `analyzer_code` is matched as plain text (case ignored), not as a regular expression, so
  `"1337+1098"` matches itself; a warning is given when no `reference_df` row matches.
* Clear errors for a `ghg` that is not a column of `data`, missing `reference_df` columns, a
  non-date-time `reference_time`, `num_rows` below `min_obs`, and unsupported analyzer/gas pairs
  in `tidy_ghg_analyzer()`; warnings for sliding closures with fewer readings than `num_rows` and
  for `bubble_detection = TRUE` on a gas other than CH4.

## CH4 bubble flux (`calculate_regression()`)

* Bubble detection now runs on the readings linearly interpolated to a 1 s grid, as the code
  archive (`find_bubbles.R`, lines 35-36), so `bubble_window`, `merge_gap`, `min_len` and
  `bubble_pad` are seconds whatever the logging interval. Results are unchanged for 1 Hz data
  without gaps; they change for other intervals and for closures with missing readings (a clean
  line with a 60 s gap or 5 s sampling no longer shows false bubbles).

* New `bubble_detection` in `calculate_regression()` (default `FALSE`, unchanged); `TRUE` splits each
  CH4 closure into total, diffusive and ebullitive slopes with the automated method of Minaudo et al.
  (2026, *Limnology and Oceanography: Methods* 24, e70054, pp. 3-4, doi:10.1002/lom3.70054; code
  archive Minaudo 2026, Zenodo, doi:10.5281/zenodo.18710431): running-variance bubble detection,
  total only when a bubble starts in the first 20 s, two-point total from the smoothed full window,
  linear diffusion on the first linear segment of unflagged readings. The split runs on each
  closure's final window. For closures with a bubble, `slope` is the total slope, while `r_square`,
  `correlation` and `flag` are for the linear diffusion fit (`NA` when there is none, e.g. an
  early bubble); the regression slope is kept in `slope_regression`. New columns `slope_regression`,
  `slope_total`, `slope_diffusion`, `slope_ebullition`, `n_bubbles`, `ch4_flag` and `review_needed`
  (`TRUE` unless `ch4_flag` is `"ok"` or a `"no bubble"` label). The diffusion segment may have a
  negative slope (CH4 uptake) when it starts before the first bubble; the code archive keeps
  positive slopes only. Also `slope_diffusion_se`, `diffusion_start` and `diffusion_end`. For
  bubble closures `g_factor` is `NA`. `bubble_detection = TRUE` needs `window_type = "fixed"`.
* New `apply_ch4_review(regression_result, decisions)` applies review decisions (`accept`,
  `total`, `not_bubble`, `discard`) per closure, matched by `site` and `reference_time`, and adds
  `ch4_decision` (`"unreviewed"` for `review_needed` closures without a decision). For it, the
  regression's own `r_square`, `correlation`, `flag` and `g_factor` are kept in `*_regression`
  columns.
* `plot_ghg_flux()` draws the diffusion fit, and shows the diffusion slope and `ch4_flag`, for
  bubble closures.
* `ch4_options` are checked for type and range (also when `bubble_detection = FALSE`).
* `calculate_ghg_flux()` adds `flux_diffusion` and `flux_ebullition` (and `flux_diffusion_se`) when `slope_diffusion` and
  `slope_ebullition` are present; `flux` is then the total CH4 flux.
* `ch4_options` sets the detection window, its alignment, percentile, merge gap, minimum
  segment length, the 20 s early-bubble rule and its origin, the total-flux start, the smoothing
  and the treatment of negative ebullition. Defaults follow the paper text; where the paper and
  its code differ or the paper is silent, the setting is marked pending the author's reply.
* Three `ch4_options` borrowed from FluxSeparator (Sø et al. 2024), off by default so results are
  unchanged: `min_rise` (bubbles with a smaller rise are not bubbles), `bubble_pad` (readings
  added around each bubble before the diffusion search) and `min_diffusion_s` (shortest
  diffusion segment, default 20 s).
* Review labels in `ch4_flag`, which change no values, include `"diffusion after first bubble"`
  (Minaudo et al. 2026, Guideline 4; Sø et al. 2024, p. 6).

## Flux QC changes (`calculate_regression()`) — slopes and flags change on re-run

* Default `window_type` is now `"fixed"` (was `"sliding"`) and default `r2_threshold` is 0.7 (was
  0.8). Pass `window_type = "sliding"` and `r2_threshold = 0.8` to keep earlier results.
* `exp_tz` and `exp_zhao18` fits (also inside `"auto"` and `plot_ghg_flux()`) no longer start
  `nls()` at `b = 0.001` and return unconverged coefficients. For a given curvature `b` the model
  is linear in `Cm` and `a`, so `b` is now found by a one-dimensional search and `Cm`, `a` by least
  squares. On noisy saturating test curves the slope was off by up to 20 times (e.g. 0.08 for a true
  1.0) and is now within 5 %. When the best `b` is at the edge of its search range (no usable
  curvature), an explicit `exp_*` fit gives `NA` slope and flag `"no_data"`, and `"auto"` keeps the
  linear or quadratic fit. A noisy straight line can still return an exponential slope. Model and slope formula are those of fluxible (Gaudard et al. 2025).
* `exp_tz` now fits the initial concentration `C0` with the other parameters (as in Kutzbach et
  al. 2007, p. 1010) instead of taking the first reading. In simulations with noise sd 3 this cut
  the spread of the slope from 8-18 % to 3-5 % of its true value. fluxible fixes `C0` at the
  intercept of a 15 s linear fit (Gaudard et al. 2025, SI pp. 29-30). `exp_zhao18` now uses that
  15 s intercept too, instead of the first reading: with `C0` free its `t0` could not be estimated
  (the model reduces to `exp_tz`). The gain from fitting `C0` shrinks with autocorrelated noise
  (RMSE 9.5 % vs 13 % with the first reading, AR(1) 0.8), and a mixing transient at the window
  start still biases every method (+16 % to +54 % in simulations); a dead band (`start_offset_s`)
  is the defence against that.
* `fit_type = "auto"` accepts `exp_tz` only when the curve levels off (`Cm - C0` has the sign of
  the linear slope), the rule it already applied to the quadratic (Levy et al. 2011, p. 817).
* A window whose readings are all equal is flagged `"zero"` (no trend), not `"no_data"`.
* A warning is given when a window has duplicated timestamps (usually `data` holding more than
  one analyzer).

* `fit_type = "auto"` accepts a quadratic or exponential fit only when its AICc is lower than the
  current model's (Kutzbach et al. 2007; Leiber-Sauheitl et al. 2014; goFlux), replacing the
  unsourced "R² improves by > 0.01" rule.
* `fit_type = "auto"` rejects a quadratic that accelerates instead of levelling off (curvature with
  the same sign as the linear slope), keeping the linear fit (Levy et al. 2011, p. 817). This also
  stops t0-slope sign flips (negative g-factor).
* The minimum-detectable-slope check no longer turns `"ok"` into `"zero"`. It sets the new
  `below_mdf` column instead (Gaudard et al. 2025, p. 4; Maier et al. 2022, p. 457).
* `analyzer_precision`: LI-COR 1 s values (CH4 0.60 ppb, CO2 3.5 ppm, N2O 0.40 ppb) replace values
  that did not match the LI-COR specifications. Column `precision_ppm` is replaced by `precision`
  and `unit`, stored in the unit each analyzer reports, so CH4/N2O are no longer compared in ppm
  against ppb data.

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

* `plot_ghg_flux()`:
  - Draws each window with the model in its `fit_used` column. Before, it always used
    `fit_type`, default `"linear"`, so quadratic fits were drawn as straight lines. `fit_type` now
    only applies to results without `fit_used`, and `"auto"` is accepted (it was an error).
  - The y axis gives the unit (ppm or ppb) from `slope_unit` or the `<gas>_unit` column; it always
    said ppm.
  - Readings are solid light grey points; only the fitted line is coloured by `flag`. For CH4 bubble closures that
    line is the diffusion fit, which is what `flag` and `r_square` describe.
  - For CH4 bubble closures, it draws flagged readings as coral triangles and the two-point total as
    a dashed white line. The panel title gives the total and ebullition on one line and the diffusion
    with its R² and flag on the next (it showed the total twice, as `slope` and `total`).
  - New `second_gas` (default `NULL`) draws e.g. CO2 beside each window. A jump in both gases at
    once points to a chamber disturbance rather than a bubble (Minaudo et al. 2026, Guideline 5,
    p. 10).
  - Uses the aelab minimal theme (as `plot_point(style = "minimal")`; new `base_size`, default 11)
    on a dark grey background with white text and the default font. Colours: readings
    grey `"#a8a8a8"`, bubble readings `"#e07a5f"`. The `flag_colors` defaults are now
    `"#8ab29a"` for `ok` (was green `"#2ca25f"`) and `"#f2cc8f"` for both `discard` and `zero`
    (were orange `"#fc8d59"` and blue `"#74c7ef"`), since both need a look.
  - The line-type key (fit / diffusion, total) is shown only when a total line is drawn.
  - Warns when no window has readings (it returned `NULL` silently), and keeps windows with the
    same label apart.
* The `n2o` dataset has an `N2O_unit` column (`"ppb"`), as `tidy_ghg_analyzer()` output, so its
  examples carry `slope_unit = "ppb s-1"` and plots label the axis "N2O (ppb)".
* `calculate_regression(bubble_detection = TRUE)` returns `bubble_spans`, `total_start`,
  `total_start_conc` and `total_end_conc` for the plot. No values change.
* `calculate_regression()` and `plot_ghg_flux()` read `date_time` (data without `real_datetime`)
  as Asia/Taipei clock time, as they already did for `real_datetime` and the reference times.
  Before, on a computer not set to Asia/Taipei, every closure returned `no_data`. Unchanged on
  Asia/Taipei systems.
* `calculate_ghg_flux()` — new `conc_unit = c("ppm", "ppb")` (default `"ppm"`, unchanged).
  `"ppb"` divides slopes by 1000, so ppb slopes (LI-COR CH4/N2O from `tidy_ghg_analyzer()`) give
  mmol m-2 h-1 as labelled; before, they gave a value 1000 times too large under that label. A
  `slope_unit` column in `data` is used per row instead; an explicit `conc_unit` that contradicts
  it is an error. The vignette CH4 example now gives the right value, and its text says h-1 (not d-1).
* `tidy_ghg_analyzer()` adds `CO2_unit`, `CH4_unit` and `N2O_unit` columns (LI-COR: ppm CO2,
  ppb CH4/N2O; LGR: ppm). `calculate_regression()` returns `slope_unit` (e.g. `"ppb s-1"`) from
  them, `NA` when `data` has no such column.

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
