#' Processed data from N2O LI-COR Trace Gas Analyzer.
#' A dataset containing 567 N2O concentrations changed over time.
#' @format A data.frame with 567 rows and 6 variables:
#' \itemize{
#'   \item DATE: Date in character format.
#'   \item TIME: Time in character format.
#'   \item N2O: Concentrations of nitrous oxide (N2O), in ppb.
#'   \item date_time: Date and time in POSIXct format.
#'   \item N2O_unit: Unit of \code{N2O} (\code{"ppb"}), as added by \code{tidy_ghg_analyzer()}.
#'   \item N2O_prec: 1 s precision of \code{N2O} (0.4 ppb, LI-7820), as added by \code{tidy_ghg_analyzer()}.
#' }
#' @source own data.
#' @name n2o
NULL
