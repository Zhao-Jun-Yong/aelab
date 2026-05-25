# Overview

**aelab** is an R package for aquatic ecology data analysis. It provides tools for:

1. **GHG flux calculation** — process raw data from LI-COR and LGR gas analyzers, calculate chamber flux, convert units, compute Minimum Detectable Flux (MDF), and aggregate to CO₂-equivalents.
2. **Net ecosystem metabolism (NEM)** — calculate gross primary production (GPP), ecosystem respiration (ER), and net ecosystem production (NEP) from high-frequency dissolved oxygen data.
3. **Statistical analysis** — descriptive statistics, outlier detection, normality testing, data transformation, one-way ANOVA (Tukey HSD), and Kruskal-Wallis (Dunn post-hoc) with compact letter display.
4. **Visualization** — ggplot2-based plot wrappers (point, line, box, bar) with a consistent theme and custom colour palettes.

## GHG Flux — What's New (v1.1.3+)

`calculate_regression()` has been substantially extended:

- **`fit_type = "auto"`** — automatic model selection per measurement window: tries linear → quadratic → `exp_tz` in order, accepting a more complex model only when R² improves by > 0.01 and, for the exponential fit, the curvature κ does not exceed `kappamax`. The model chosen is recorded in the new `fit_used` output column.
- **`window_type = "fixed"` + `start_offset_s`** — regresses a single fixed window starting at a user-specified dead-bar offset (e.g. 30 s) after the reference time. Ensures all gases are regressed over the same time period. The default `"sliding"` window remains available for backward compatibility.
- **`kappamax`** — caps exponential curvature at a physically plausible maximum. When `fit_type = "auto"`, `kappamax` is computed automatically per window from the built-in `analyzer_precision` dataset (instrument 1σ precision ÷ C₀ ÷ window duration).
- **`analyzer_precision` dataset** — built-in lookup table of 1-second precision values (ppm, 1σ) for LI-7810 (CH₄, CO₂) and LI-7820 (N₂O). Used automatically by `fit_type = "auto"`.
- **`fit_used` column** — new output column recording which model was actually selected (`"linear"`, `"quadratic"`, or `"exp_tz"`). Always matches `fit_type` unless `fit_type = "auto"`.

# Installation

The `aelab` package can be installed from:

1. GitHub (development version):

```r
# install.packages("devtools")
devtools::install_github("Zhao-Jun-Yong/aelab")
```

2. CRAN (stable version):

```r
install.packages("aelab")
```

# Features

For a full walkthrough with worked examples and code output, see the package vignette:

```r
vignette("aelab")
```
