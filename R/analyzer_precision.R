#' Instrument precision for supported trace gas analyzers
#'
#' 1-second (\eqn{1\sigma}{1 sigma}) precision values, one row per gas. \code{\link{tidy_ghg_analyzer}} copies them
#' into \code{<gas>_prec} columns for LI-COR data, which \code{\link{calculate_regression}} uses
#' for \code{fit_type = "auto"} (kappamax) and \code{window_type = "pareto"}.
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{model}{Analyzer model name (character).}
#'   \item{gas}{Gas measured: \code{"CH4"}, \code{"CO2"}, or \code{"N2O"} (character).}
#'   \item{precision}{Instrument precision (\eqn{1\sigma}{1 sigma}, 1 s averaging) in \code{unit} (numeric).}
#'   \item{unit}{Unit the analyzer reports for that gas, so \code{precision} matches the data
#'     from \code{\link{tidy_ghg_analyzer}} (\code{"ppb"} for LI-COR CH4 and N2O, otherwise
#'     \code{"ppm"}) (character).}
#' }
#' @source LI-7810 and LI-7820 specifications pages (LI-COR Biosciences; CH4 0.60 ppb at 2 ppm,
#'   CO2 3.5 ppm at 400 ppm, N2O 0.40 ppb at 330 ppb; checked 2026-10-21).
#' @examples
#' analyzer_precision
#' analyzer_precision[analyzer_precision$gas == "N2O", "precision"]
#' @name analyzer_precision
NULL
