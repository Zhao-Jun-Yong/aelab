# Instrument precision (1σ, 1 s averaging) in the unit each analyzer reports for that gas,
# so it can be compared directly with slopes from tidy_ghg_analyzer() data.
# LI-7810: https://www.licor.com/support/LI-7810/topics/specifications.html
#   (CH4 0.60 ppb at 2 ppm; CO2 3.5 ppm at 400 ppm; checked 2026-10-21)
# LI-7820: https://www.licor.com/support/LI-7820/topics/specifications.html
#   (N2O 0.40 ppb at 330 ppb; checked 2026-10-21)
# One row per gas: tidy_ghg_analyzer() copies these into <gas>_prec columns for LI-COR data.
# LGR UGGA rows removed 2026-11-11 (no vendor source found; LI-COR only for now).

analyzer_precision <- data.frame(
  model     = c("LI-7810", "LI-7810", "LI-7820"),
  gas       = c("CH4",     "CO2",     "N2O"),
  precision = c(0.60,      3.5,       0.40),
  unit      = c("ppb",     "ppm",     "ppb"),
  stringsAsFactors = FALSE
)

usethis::use_data(analyzer_precision, overwrite = TRUE)
