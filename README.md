# Overview

**aelab** is an R package for aquatic ecology data analysis. It provides tools for:

1. **GHG flux calculation** — process raw data from LI-COR and LGR gas analyzers, calculate chamber flux, convert units, compute Minimum Detectable Flux (MDF), and aggregate to CO₂-equivalents.
2. **Net ecosystem metabolism (NEM)** — calculate gross primary production (GPP), ecosystem respiration (ER), and net ecosystem production (NEP) from high-frequency dissolved oxygen data.
3. **Statistical analysis** — descriptive statistics, outlier detection, normality testing, data transformation, one-way ANOVA (Tukey HSD), and Kruskal-Wallis (Dunn post-hoc) with compact letter display.
4. **Visualization** — ggplot2-based plot wrappers (point, line, box, bar) with a consistent theme and custom colour palettes.
5. **Gantt-style progress tables** — `gantt_table()` renders a coloured `flextable` (PDF/DOCX/PPTX-compatible) where columns are sampling campaigns and rows are measurement categories × systems; `gantt_text()` renders the same data as Unicode text for terminal/Neovim viewing.

## GHG Flux — What's New (v1.1.5)

`calculate_regression()` has been substantially extended:

- **`fit_type = "auto"`** — automatic model selection per measurement window: tries linear → quadratic → `exp_tz` in order, accepting a more complex model only when its AICc is lower (Kutzbach et al. 2007), a quadratic only when it levels off rather than accelerates (Levy et al. 2011), and the exponential fit only when its curvature κ does not exceed `kappamax`. The model chosen is recorded in the new `fit_used` output column.
- **`window_type = "fixed"` + `start_offset_s`** — regresses a single fixed window starting at a user-specified dead-bar offset (e.g. 30 s) after the reference time. Ensures all gases are regressed over the same time period. The default `"sliding"` window remains available for backward compatibility.
- **Per-row window overrides** — `reference_df` may include optional `duration_minutes` and/or `start_offset_s` columns. When a row has a non-`NA` value, it overrides the global parameter for that measurement. Useful when individual closures have different deployment lengths or early-saturation artefacts.
- **`kappamax`** — caps exponential curvature at a physically plausible maximum. When `fit_type = "auto"`, `kappamax` is computed automatically per window from the built-in `analyzer_precision` dataset (κmax = |linear slope| ÷ instrument 1σ precision; Hüppi et al. 2018).
- **`analyzer_precision` dataset** — built-in lookup table of 1-second precision values (1σ, in each analyzer's reporting unit: ppb for LI-COR CH₄/N₂O, otherwise ppm) for LI-7810 (CH₄, CO₂), LI-7820 (N₂O), and LGR UGGA (CH₄, CO₂). Used automatically by `fit_type = "auto"`.
- **`fit_used` column** — new output column recording which model was actually selected (`"linear"`, `"quadratic"`, or `"exp_tz"`). Always matches `fit_type` unless `fit_type = "auto"`.

`make_reference()` builds a `reference_df` for `calculate_regression()` from a compact multiline string of field notes — one closure per line: `time  site  analyzer  [dur=N]  [off=N]`. Optionally writes the result to `.xlsx`.

`window_type = "pareto"` independently searches deadband (start skip) and cutoff (window end) times per closure, trading off model fit against propagated slope uncertainty via a Pareto front — adapted from Kobayashi et al. (2026), with the existing `kappamax` gate and `zero`/`discard` fallback always applied so no candidate can be a numerically degenerate fit and no empty or failing candidate pool is reported as a confident number. New output columns: `deadband`, `cutoff`, `slope_uncertainty_lo`, `slope_uncertainty_hi`, `n_candidates`.

## References

- Yong, Z.-J., Lin, W.-J., Lin, C.-W., & Lin, H.-J. (2024). Tidal influence on carbon dioxide and methane fluxes from tree stems and soils in mangrove forests. *Biogeosciences*, 21(22), 5247–5260. https://doi.org/10.5194/bg-21-5247-2024
- Staehr, P. A., Bade, D., Van de Bogert, M. C., Koch, G. R., Williamson, C., Hanson, P., Cole, J. J., & Kratz, T. (2010). Lake metabolism and the diel oxygen technique: State of the science. *Limnology and Oceanography: Methods*, 8(11), 628–644. https://doi.org/10.4319/lom.2010.8.0628
- Hutchinson, G. L., & Mosier, A. R. (1981). Improved soil cover method for field measurement of nitrous oxide fluxes. *Soil Science Society of America Journal*, 45(2), 311–316. https://doi.org/10.2136/sssaj1981.03615995004500020017x
- Hüppi, R., Felber, R., Krauss, M., Six, J., Leifeld, J., & Fuß, R. (2018). Restricting the nonlinearity parameter in soil greenhouse gas flux calculations for more reliable flux estimates. *PLOS ONE*, 13(7), e0200876. https://doi.org/10.1371/journal.pone.0200876
- Gaudard, J., Telford, R. J., Chacon-Labella, J., Dawson, H. R., Enquist, B. J., Töpper, J. P., Trepel, J., Vandvik, V., Baumane, M., Birkeli, K., Holle, M. J. M., Hupp, J. R., Santos-Andrade, P. E., Satriawan, T. W., & Halbritter, A. H. (2025). fluxible: An R package to process ecosystem gas fluxes from closed-loop chambers in an automated and reproducible way. *Methods in Ecology and Evolution*, 16(11), 2560–2568. https://doi.org/10.1111/2041-210X.70161
- Kobayashi, A. N. A., Roques, C., Hunkeler, D., & Brunner, P. (2026). Uncertainty quantification in soil gas fluxes: Reducing the user's arbitrariness. *European Journal of Soil Science*, 77, e70415. https://doi.org/10.1111/ejss.70415
- Jeffrey, S. W., & Humphrey, G. F. (1975). New spectrophotometric equations for determining chlorophylls a, b, c1 and c2 in higher plants, algae and natural phytoplankton. *Biochemie und Physiologie der Pflanzen*, 167(2), 191–194. https://doi.org/10.1016/S0015-3796(17)30778-3

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
