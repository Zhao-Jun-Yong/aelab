#' Instrument precision for LI-COR trace gas analyzers
#'
#' A lookup table of 1-second (1σ) precision values used to compute
#' \code{kappamax} in \code{\link{calculate_regression}} when
#' \code{fit_type = "auto"}. Add rows for additional analyzer models as needed.
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{model}{Analyzer model name (character).}
#'   \item{gas}{Gas measured: \code{"CH4"}, \code{"CO2"}, or \code{"N2O"} (character).}
#'   \item{precision_ppm}{Instrument precision in ppm (1σ, 1 s), from LI-COR product specifications (numeric).}
#' }
#' @source LI-COR Biosciences product specifications.
#' @examples
#' analyzer_precision
#' analyzer_precision[analyzer_precision$gas == "N2O", "precision_ppm"]
#' @name analyzer_precision
NULL
