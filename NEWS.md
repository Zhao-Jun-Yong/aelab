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
