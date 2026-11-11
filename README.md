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

# GHG chamber-flux workflow

```mermaid
flowchart TD
subgraph S1["1 Import"]
  a1["Analyzer file<br/>LI-COR .data, Excel export or LGR"]
  a2["<b>tidy_ghg_analyzer</b><br/>file, gas = ch4 or n2o<br/>adds H2O, gas_unit, gas_prec"]
  a3["<b>convert_time</b><br/>day, hr, min, sec offsets<br/>adds real_datetime"]
  f1["Field sheet"]
  f2["<b>make_reference</b><br/>field sheet to reference table"]
  f3["<b>reference_df</b><br/>date_time, site, analyzer<br/>optional duration_minutes, start_offset_s"]
  a1 --> a2 --> a3
  f1 --> f2 --> f3
end

subgraph S2["2 Fit and QC"]
  r1["<b>calculate_regression</b><br/>window_type = fixed, sliding or pareto<br/>fit_type = linear, quadratic, exp_tz, exp_zhao18, exp_hm or auto<br/>min_obs = 60, bubble_detection = TRUE or FALSE"]
  d1{"enough readings?<br/>n_obs >= min_obs"}
  nd["flag = no_data<br/>pareto with no admitted window: discard"]
  fit["Fit model in window<br/>slope, r_square, correlation"]
  d2{"correlation below threshold?"}
  ze["flag = zero"]
  d3{"R2 below threshold?"}
  di["flag = discard"]
  out["<b>Regression table</b><br/>slope, slope_se, r_square, rmse, n_obs, data_ratio<br/>start_conc, fit_used, g_factor, below_mdf, flag, h2o_start"]
  qc["<b>plot_ghg_flux</b><br/>QC plot: title = model, RMSE, n<br/>colours by flag"]
  au["Side note: fit_type = auto picks linear, quadratic or exp_tz<br/>by AICc and kappamax. exp_hm is comparison only.<br/>If g_factor_threshold is set, ok with a larger g_factor becomes discard"]
  r1 --> d1
  d1 -->|"no"| nd
  d1 -->|"yes"| fit --> d2
  d2 -->|"yes"| ze
  d2 -->|"no"| d3
  d3 -->|"yes"| di
  d3 -->|"no, flag = ok"| out
  nd --> out
  ze --> out
  di --> out
  out --> qc
  au -.-> r1
end

subgraph S3["3 CH4 bubble review (optional)"]
  d4{"bubble_detection = TRUE?"}
  sp["<b>Split slopes</b><br/>total, diffusion, ebullition"]
  fl["<b>ch4_flag, review_needed</b><br/>per closure"]
  tp["<b>ch4_review_template</b><br/>rows with review_needed = TRUE<br/>empty decision column"]
  pl["<b>plot_ghg_flux</b><br/>second_gas = CO2<br/>visual check"]
  mn["<b>Manual decisions</b><br/>accept, total, not_bubble or discard"]
  ap["<b>apply_ch4_review</b><br/>regression_result, decisions<br/>total: flag = NA, no diffusion fit"]
  cd["<b>ch4_decision</b><br/>column added to the table"]
  d4 -->|"yes"| sp --> fl --> tp --> mn
  fl --> pl
  pl -.->|"informs"| mn
  mn --> ap --> cd
end

subgraph S4["4 Flux and reporting"]
  fx["<b>calculate_ghg_flux</b><br/>area, volume, temp, pressure<br/>h2o = h2o_start, conc_unit<br/>flux, flux_se, flux_diffusion, flux_ebullition<br/>unit = mmol m-2 h-1"]
  cv["<b>convert_ghg_unit</b><br/>mass, area, time, to_*"]
  ce["<b>calculate_total_co2e</b><br/>GWPs"]
  fx --> cv --> ce
end

a3 --> r1
f3 --> r1
out --> d4
d4 -->|"no, slope as fitted"| fx
cd --> fx

classDef input fill:#e5e7eb,stroke:#6b7280,color:#111
classDef func fill:#dbeafe,stroke:#2563eb,color:#111
classDef dec fill:#fed7aa,stroke:#ea580c,color:#111
classDef tbl fill:#dcfce7,stroke:#16a34a,color:#111
classDef man fill:#e9d5ff,stroke:#9333ea,color:#111
class a1,f1,f3 input
class a2,a3,f2,r1,fit,qc,sp,tp,pl,ap,fx,cv,ce func
class d1,d2,d3,d4 dec
class nd,ze,di,out,fl,cd tbl
class mn man
```
