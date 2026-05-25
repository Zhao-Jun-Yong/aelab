# Instrument precision (1σ, 1 s) for LI-COR trace gas analyzers.
# Source: LI-COR product specifications.
# Add rows for new analyzer models as needed.

analyzer_precision <- data.frame(
  model         = c("LI-7810",  "LI-7810",  "LI-7820"),
  gas           = c("CH4",      "CO2",      "N2O"),
  precision_ppm = c(3e-4,       0.1,        3e-5),
  stringsAsFactors = FALSE
)

usethis::use_data(analyzer_precision, overwrite = TRUE)
