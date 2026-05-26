# Instrument precision (1σ, 1 s) for supported trace gas analyzers.
# Sources: LI-COR product specifications; LGR UGGA product specifications.
# Add rows for new analyzer models as needed.

analyzer_precision <- data.frame(
  model         = c("LI-7810",  "LI-7810",  "LI-7820",  "LGR UGGA", "LGR UGGA"),
  gas           = c("CH4",      "CO2",      "N2O",      "CH4",      "CO2"),
  precision_ppm = c(3e-4,       0.1,        3e-5,       2e-3,       1.0),
  stringsAsFactors = FALSE
)

usethis::use_data(analyzer_precision, overwrite = TRUE)
