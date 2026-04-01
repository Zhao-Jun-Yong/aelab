## Resubmission — v1.1.2

This submission updates `calculate_regression()` to accept a reference
data frame that carries site and analyzer metadata through to the output.

### Changes since v1.1.1

* `calculate_regression()`: function signature updated — `reference_time`
  is now a column name in a new `reference_df` argument (a data frame
  containing the measurement start times, site identifiers, and analyzer
  identifier). New `site` and `analyzer_code` parameters added. Output
  tibble now includes `site` and `analyzer` columns.
* `inst/extdata/reference.xlsx`: added `site` and `analyzer` columns to
  the bundled example reference file.
* Vignette and tests updated to reflect the new signature.

### Notes on `\dontrun` in examples

Plot function examples (`plot_point`, `plot_line`, `plot_box`, `plot_bar`,
and related scale/palette functions) are wrapped in `\dontrun{}` because they
set `family = "Century Gothic"` in the ggplot2 theme. This system font is not
available in the PostScript font database on check servers, which causes a hard
error (not merely a warning) during example rendering. The examples are
syntactically correct and functional on systems where the font is installed.

### R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: "unable to verify current time" — network access restricted in check
  environment; not a package issue.
