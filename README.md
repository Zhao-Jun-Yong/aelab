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
- **`kappamax`** — caps exponential curvature at a physically plausible maximum. When `fit_type = "auto"`, `kappamax` is computed automatically per window from the instrument precision (κmax = |linear slope| ÷ instrument 1σ precision; Hüppi et al. 2018).
- **`analyzer_precision` dataset** — built-in lookup table of 1-second precision values (1σ, in each analyzer's reporting unit: ppb for LI-COR CH₄/N₂O, otherwise ppm) for LI-7810 (CH₄, CO₂) and LI-7820 (N₂O). `tidy_ghg_analyzer()` adds them to LI-COR data as `<gas>_prec` columns, which `fit_type = "auto"` and `window_type = "pareto"` use.
- **`fit_used` column** — new output column recording which model was actually selected (`"linear"`, `"quadratic"`, or `"exp_tz"`). Always matches `fit_type` unless `fit_type = "auto"`.

`make_reference()` builds a `reference_df` for `calculate_regression()` from a compact multiline string of field notes — one closure per line: `time  site  analyzer  [dur=N]  [off=N]`. Optionally writes the result to `.xlsx`.

`window_type = "pareto"` (experimental) searches deadband (start skip) and cutoff (window end) times per closure and scores each candidate on `rmse` and an approximate slope standard error (`instrument_precision / sqrt(Sxx)`), then picks the front member closest to the ideal corner. The deadband x cutoff search, Pareto front and closest-to-ideal pick follow Kobayashi et al. (2026); the two scores and their fixed scaling (rmse above the noise level in units of `instrument_precision`; slope SE relative to that of all readings, so the pick does not change with `cutoff_step`) are aelab's own (like the paper, the uncertainty uses the sensor precision, not the residuals). `instrument_precision` is required. The `kappamax` gate and the `zero`/`discard` flags always apply, and no uncertainty interval is reported. New output columns: `deadband`, `cutoff`, `n_candidates`.

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

# GHG chamber-flux workflow

## Overview

```mermaid
flowchart TD
  a1["Analyzer file<br/>LI-COR .data, Excel export or LGR"]
  a2["<b>tidy_ghg_analyzer</b><br/>then <b>convert_time</b>"]
  f1["Field sheet"]
  f2["<b>make_reference</b><br/>gives reference_df"]
  r1["<b>calculate_regression</b><br/>window, fit, QC flag"]
  t1["<b>Regression table</b><br/>slope, flag, ..."]
  rv["<b>CH4 review</b><br/>optional, see sections 4 and 5"]
  fx["<b>calculate_ghg_flux</b><br/>unit = mmol m-2 h-1"]
  cv["<b>convert_ghg_unit</b><br/><b>calculate_total_co2e</b>"]
  a1 --> a2 --> r1
  f1 --> f2 --> r1
  r1 --> t1
  t1 -->|"bubble_detection"| rv --> fx
  t1 --> fx
  fx --> cv
  class a1,f1 input
  class a2,f2,r1,rv,fx,cv func
  class t1 tbl

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

## 1 Window selection

```mermaid
flowchart TD
  r1["<b>calculate_regression</b><br/>window_type"]
  dw{"window_type?"}
  wf["<b>fixed</b><br/>start = reference time + start_offset_s<br/>length = duration_minutes"]
  ws["<b>sliding</b><br/>all windows of num_rows readings<br/>keep the highest r_square"]
  wp1["<b>pareto</b> (experimental)<br/>grid of deadband and cutoff<br/>window >= min_window_size"]
  wp2["Fit each candidate, score it on rmse above noise<br/>and approximate slope SE, fixed scales<br/>pick the front member closest to the ideal"]
  nd["flag = no_data"]
  di["flag = discard"]
  w["<b>Window</b><br/>start_time, end_time, n_obs<br/>pareto adds deadband, cutoff,<br/>n_candidates"]
  r1 --> dw
  dw -->|"fixed"| wf --> w
  dw -->|"sliding"| ws --> w
  dw -->|"pareto"| wp1 --> wp2 --> w
  wp1 -->|"no grid"| nd
  wp2 -->|"no usable candidate"| di
  class r1 func
  class dw dec
  class wf,ws,wp1,wp2 func
  class w tbl
  class nd,di bad

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

| Setting | Default |
|---|---|
| `window_type` | `"fixed"` |
| `start_offset_s`, `duration_minutes` | 0 s, 7 min (`duration_minutes` can also be a `reference_df` column) |
| `num_rows` (sliding) | 300 readings |
| `min_deadband`, `max_deadband`, `deadband_step` | 30, 60, 10 s (grid 30 to 50 s) |
| `min_window_size`, `cutoff_step` | 60 s, 10 s (cutoffs from 90 s to the last reading) |
| `instrument_precision` | none: needed for `fit_type = "auto"`; falls back to the single value in the `<gas>_prec` column that `tidy_ghg_analyzer()` adds |

## 2 Fit and QC

```mermaid
flowchart TD
  w["<b>Window</b><br/>readings of the closure"]
  d1{"n_obs >= need?"}
  nd["flag = no_data"]
  da{"fit_type = auto?"}
  au["<b>Auto model choice</b> (section 3)<br/>linear, quadratic or exp_tz<br/>by AICc and kmax"]
  fit["<b>Fit the model</b><br/>slope, slope_se, r_square, rmse<br/>fit_used, g_factor if not linear"]
  d2{"abs correlation >= cor_threshold?"}
  ze["flag = zero<br/>also when correlation is NA"]
  d3{"r_square >= r2_threshold?"}
  di["flag = discard"]
  dg{"g_factor_threshold set and<br/>abs g_factor above it?"}
  d4{"bubble_detection = TRUE?"}
  sp["<b>CH4 split</b> (section 4)<br/>total, diffusion, ebullition, ch4_flag<br/>flag, r_square recomputed on the diffusion line<br/>NA if total only, g_factor = NA"]
  out["<b>Regression table</b><br/>flag: ok, zero, discard, no_data or NA<br/>below_mdf, data_ratio, start_conc, h2o_start"]
  qc["<b>plot_ghg_flux</b><br/>title = model, RMSE, n<br/>colours by flag"]
  w --> d1
  d1 -->|"no"| nd --> out
  d1 -->|"yes"| da
  da -->|"no"| fit
  da -->|"yes"| au --> fit
  fit --> d2
  d2 -->|"no"| ze --> d4
  d2 -->|"yes"| d3
  d3 -->|"no"| di --> d4
  d3 -->|"yes"| dg
  dg -->|"yes"| di
  dg -->|"no, flag = ok"| d4
  d4 -->|"yes"| sp --> out
  d4 -->|"no"| out
  out --> qc
  class w,out tbl
  class d1,da,d2,d3,dg,d4 dec
  class au,fit,sp,qc func
  class nd,ze,di bad

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

| Setting | Default |
|---|---|
| `fit_type` | `"linear"` |
| `min_obs` | 60; `need = max(min_obs, parameters of the model + 1)`. Sliding stops with an error if `num_rows < need`; pareto applies the floor to each candidate |
| `cor_threshold`, `r2_threshold` | 0.5, 0.7 |
| `g_factor_threshold` | `NULL` (off); applied before the CH4 split, so a split closure no longer carries it |
| `kappamax`, `t_zero` | `NULL`, 0 |
| `bubble_detection` | `FALSE` |

Flagged rows stay in the table and `calculate_ghg_flux()` does not filter on `flag`.

## 3 Auto model choice (`fit_type = "auto"`)

```mermaid
flowchart TD
  s["fit_type = auto<br/>window readings"]
  lin["<b>Linear baseline</b><br/>slope_lm, best = linear<br/>kmax = abs of slope_lm / instrument_precision"]
  dq{"quadratic: AICc lower and<br/>curvature opposes the slope?"}
  qa["best = quadratic"]
  de{"exp_tz: b within kmax, curve<br/>levels off and AICc lower than best?"}
  ee["best = exp_tz"]
  res["<b>Result</b><br/>fit_used, slope, slope_lm<br/>g_factor = slope / slope_lm if not linear"]
  s --> lin --> dq
  dq -->|"yes"| qa --> de
  dq -->|"no"| de
  de -->|"yes"| ee --> res
  de -->|"no"| res
  class s tbl
  class lin,qa,ee func
  class dq,de dec
  class res tbl

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

AICc = n ln(RSS / n) + 2k + 2k(k + 1) / (n - k - 1), with k the fitted parameters plus 1 (Kutzbach et al. 2007; Leiber-Sauheitl et al. 2014). `kmax` follows Hüppi et al. 2018. A curve that bends the same way as the slope is rejected (Levy et al. 2011). `g_factor_threshold` is described in section 2.

## 4 CH4 bubble detection

```mermaid
flowchart TD
  s0["<b>CH4 readings</b><br/>final window, in time order<br/>duplicate times dropped"]
  d0{"at least 20 readings?"}
  tf["ch4_flag = too few readings<br/>slopes NA"]
  b1["<b>Find bubbles</b><br/>1 s grid, z = conc / mean<br/>variance of z in windows of bubble_window<br/>high if above max of 1e-4 and the bubble_prob quantile<br/>merge runs split by fewer than merge_gap windows<br/>drop runs shorter than min_len"]
  d1{"any bubble?"}
  nb["ch4_flag = no bubble<br/>slope = linear slope of the window<br/>ebullition = 0"]
  b3["<b>Two-point total</b><br/>smoothed series, spar<br/>c0 = low of first 30 s, c1 = high of last 30 s<br/>total = (c1 - c0) / duration"]
  d2{"first bubble within early_s<br/>of the start?"}
  eb["ch4_flag = early bubble: total only"]
  b4["<b>Clean the bubble set</b><br/>min_rise: drop bubbles that rise by that much or less<br/>none left: no bubble, rises <= min_rise<br/>bubble_pad widens each bubble"]
  b6["<b>Diffusion segment</b><br/>trim_to_min drops readings up to the lowest in the first half<br/>first run of clean readings, at least min_diffusion_s<br/>grow it until adj R2 >= 0.7, p < 0.05, slope > 0<br/>a run starting before the first bubble may be negative (uptake)"]
  d5{"a clean run long enough?"}
  tl["ch4_flag = no linear diffusion segment<br/>total only"]
  b7["<b>Split</b><br/>diffusion = slope of the segment<br/>ebullition = total - diffusion<br/>negative: neg_ebullition = zero, keep or na"]
  d6{"any review label?<br/>diffusion above total<br/>ebullition below diffusion + SE<br/>no passing segment<br/>diffusion after the first bubble"}
  rn["ch4_flag = the labels"]
  ok["ch4_flag = ok"]
  rv["review_needed = TRUE<br/>slope = slope_total"]
  au["review_needed = FALSE<br/>ch4_decision = auto"]
  s0 --> d0
  d0 -->|"no"| tf --> rv
  d0 -->|"yes"| b1 --> d1
  d1 -->|"no"| nb --> au
  d1 -->|"yes"| b3 --> d2
  d2 -->|"yes"| eb --> rv
  d2 -->|"no"| b4 --> b6 --> d5
  d5 -->|"no"| tl --> rv
  d5 -->|"yes"| b7 --> d6
  d6 -->|"yes"| rn --> rv
  d6 -->|"no"| ok --> au
  class s0 tbl
  class b1,b3,b4,b6,b7 func
  class d0,d1,d2,d5,d6 dec
  class tf,eb,tl,rn,rv bad
  class nb,ok,au tbl

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

| `ch4_options` | Default |
|---|---|
| `bubble_window`, `bubble_align` | 10 s, `"centre"` |
| `bubble_prob`, `merge_gap`, `min_len` | 0.75, 10 s, 5 s |
| `early_s`, `early_from` | 20 s, `"window"` |
| `total_from`, `spar`, `nknots_ratio` | `"window"`, 0.5, 1/3 |
| `trim_to_min`, `neg_ebullition` | `TRUE`, `"zero"` |
| `min_rise`, `bubble_pad`, `min_diffusion_s` | 0 (off), 0, 20 s |

Fixed in the code, not options: variance floor 1e-4, at least 20 readings, adj R2 0.7 and p 0.05 for the diffusion segment (slope must be positive, except in a run that starts before the first bubble), 30 s edges for the total, 10 s block gap. Sources are in the code comments (Minaudo et al. 2026; So et al. 2024).

## 5 CH4 bubble review (optional)

```mermaid
flowchart TD
  out["<b>Regression table</b><br/>bubble_detection = TRUE"]
  tp["<b>ch4_review_template</b><br/>rows with review_needed = TRUE<br/>empty decision column"]
  pl["<b>plot_ghg_flux</b><br/>second_gas = CO2<br/>visual check"]
  mn["<b>Manual decision</b><br/>one per closure"]
  ap["<b>apply_ch4_review</b><br/>regression_result, decisions"]
  dd{"decision?"}
  acc["accept<br/>keep the split"]
  tot["total<br/>slope = slope_total<br/>flag, r_square, rmse, slope_se, correlation = NA"]
  nb["not_bubble<br/>regression fit restored<br/>ebullition = 0"]
  dis["discard<br/>flag = discard, values kept"]
  cd["<b>ch4_decision</b><br/>the decision, unreviewed or auto"]
  fx["<b>calculate_ghg_flux</b>"]
  out --> tp --> mn
  out --> pl
  pl -.->|"informs"| mn
  mn --> ap --> dd
  dd -->|"accept"| acc --> cd
  dd -->|"total"| tot --> cd
  dd -->|"not_bubble"| nb --> cd
  dd -->|"discard"| dis --> cd
  cd --> fx
  class out,cd tbl
  class tp,pl,ap,fx func
  class dd dec
  class mn man
  class acc,tot,nb,dis func

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111,stroke-width:1.5px
classDef func fill:#dbeafe,stroke:#2563eb,color:#111,stroke-width:1.5px
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111,stroke-width:1.5px
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111,stroke-width:1.5px
classDef bad fill:#fecaca,stroke:#dc2626,color:#111,stroke-width:1.5px
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111,stroke-width:1.5px
```

Closures without `review_needed` get `ch4_decision = "auto"`; those with it and no decision get `"unreviewed"`, and `calculate_ghg_flux()` warns.
