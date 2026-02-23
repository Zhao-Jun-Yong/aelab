## Resubmission — v1.1.0

This submission adds 19 new exported functions across five new/modified source files
covering statistics, visualisation, GHG calculations, lab analysis, and weather-data
processing. The existing `convert_ghg_unit()` function has been updated to accept
character-string inputs (e.g. "mean ± SD" notation) and now returns a named list.

### Changes since v1.0.1

* Added `R/statistics.R`: 7 statistical helpers
  (`descriptive_statistic`, `normality_test_t`, `normality_test_aov`,
  `aov_test`, `ks_test`, `df_trans`, `find_outlier`).
* Added `R/visualization.R`: 4 plot wrappers, a palette system (5 functions),
  and `plot_map_taiwan`.
* Added `R/lab_analysis.R`: `calc_chla_trichromatic`.
* Updated `R/ghg_flux.R`: replaced `convert_ghg_unit`, added `calculate_MDF`
  and `calculate_total_co2e`.
* Updated `R/hobo_do.R`: added `process_weather_month` and
  `combine_weather_month`.
* New `Imports`: `rlang`, `grDevices`, `multcompView`, `FSA`, `rcompanion`,
  `rnaturalearth`, `sf`, `ggspatial`.
* `rnaturalearthdata` moved to `Suggests` (used internally by `rnaturalearth`
  but not called directly).

### Notes on `\dontrun` in examples

Plot function examples (`plot_point`, `plot_line`, `plot_box`, `plot_bar`,
and related scale/palette functions) are wrapped in `\dontrun{}` because they
set `family = "Century Gothic"` in the ggplot2 theme. This system font is not
available in the PostScript font database on check servers, which causes a hard
error (not merely a warning) during example rendering. The examples are
syntactically correct and functional on systems where the font is installed.

### R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: "unable to verify current time" — network access restricted in check
  environment; not a package issue.
* NOTE: "Non-standard file/directory found at top level: 'CLAUDE.md'" — this
  is a project-level AI assistant configuration file used during development;
  it does not affect package functionality. We will remove it from the repo
  root prior to the next submission if CRAN requests it.
