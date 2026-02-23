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

* `convert_ghg_unit()` — now accepts character strings (e.g. "0.002 ± 0.003") and converts each embedded number; supports wider unit set (`nmol`, `Mg`, `µmol`, `mol`; time: `yr`, `day`, `hr`, `sec`, `min`); returns a named list `list(value, unit)` instead of a bare numeric.

# aelab 0.4.0

# aelab 0.2.0

# aelab 0.1.0

* Initial CRAN submission.
