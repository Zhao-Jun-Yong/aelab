#' @title calculate_ghg_flux
#' @description Calculate the greenhouse gas (GHG) flux based on input parameters from a data frame.
#' @param data A data frame containing relevant data with columns for slope, area, volume, and temperature.
#' @param slope Name of the column in `data` that contains the slope values of the GHG concentration change
#'   (in ppm/s, or ppb/s with \code{conc_unit = "ppb"}).
#' @param area Name of the column in `data` that contains the values of the area of the chamber (in square meter).
#' @param volume Name of the column in `data` that contains values of the volume of the chamber (in litre).
#' @param temp Name of the column in `data` that contains values of the temperature of the gas (in Celsius).
#' @param conc_unit Unit of the concentration the slopes were fitted to: \code{"ppm"} (default) or
#'   \code{"ppb"}. LI-COR CH4 and N2O from \code{tidy_ghg_analyzer()} are in ppb; \code{"ppb"} slopes
#'   are divided by 1000 so the flux stays in mmol m-2 h-1. When \code{data} has a
#'   \code{slope_unit} column (from \code{calculate_regression()} on \code{tidy_ghg_analyzer()}
#'   data), it is used for each row and \code{conc_unit} only fills rows where it is \code{NA};
#'   a \code{conc_unit} given explicitly that contradicts it is an error.
#' @param pressure Optional name of a column with air pressure (kPa). \code{NULL} (default) uses
#'   101.325 kPa (1 atm).
#' @param h2o Optional name of a column with water vapour (ppm), e.g. \code{h2o_start} from
#'   \code{calculate_regression()}. \code{NULL} (default) treats the air as dry. The flux uses the
#'   moles of dry air, V P (1 - H2O) / (R T A), as goFlux's \code{flux.term()} (Rheault et al.
#'   2024), because LI-7810/7820 report dry mole fractions (LI-COR output tables).
#' @return \code{data} with \code{flux} and \code{unit} added. When \code{data} has
#'   \code{slope_diffusion} and \code{slope_ebullition} (from
#'   \code{calculate_regression(bubble_detection = TRUE)}), \code{flux_diffusion} and
#'   \code{flux_ebullition} are added too (and \code{flux_diffusion_se} from
#'   \code{slope_diffusion_se});
#'   \code{flux} is then the total CH4 flux, because
#'   \code{slope} holds the total slope for closures with a bubble.
#' @examples
#' data <- data.frame(
#'   slope = c(1.2, 1.5, 1.1),
#'   area = c(100, 150, 120),
#'   volume = c(10, 15, 12),
#'   temp = c(25, 30, 22)
#' )
#' results <- calculate_ghg_flux(data)
#' print(results)
#' @export

calculate_ghg_flux <- function(data, slope = "slope", area = "area", volume = "volume", temp = "temp",
                               conc_unit = c("ppm", "ppb"), pressure = NULL, h2o = NULL) {

  given <- !missing(conc_unit)
  conc_unit <- match.arg(conc_unit)
  unit_row <- rep(conc_unit, nrow(data))
  if ("slope_unit" %in% names(data)) {  # from calculate_regression() via tidy_ghg_analyzer()
    su <- sub(" s-1$", "", data$slope_unit)
    bad <- setdiff(stats::na.omit(su), c("ppm", "ppb"))
    if (length(bad)) stop("Unknown slope_unit: ", paste(bad, collapse = ", "))
    if (given && any(!is.na(su) & su != conc_unit))
      stop("conc_unit = \"", conc_unit, "\" contradicts the slope_unit column (",
           paste(unique(stats::na.omit(data$slope_unit)), collapse = ", "), ").")
    unit_row[!is.na(su)] <- su[!is.na(su)]
  }
  to_ppm <- ifelse(unit_row == "ppb", 0.001, 1)

  # Constants
  s_to_hr <- (1/3600)  # seconds to hours (3600 s h⁻¹)
  gas_constant <- 0.082057  # gas constant (L·atm·K⁻¹·mol⁻¹)
  celsius_to_kelvin <- 273.15  # Celsius to Kelvin conversion
  micro_to_milli <- 0.001  # micromoles to millimoles conversion

  # Check if specified columns exist in the data frame
  required_cols <- c(slope, area, volume, temp, pressure, h2o)
  if (!all(required_cols %in% names(data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }

  # Extract values from the data frame
  area <- data[[area]]
  volume <- data[[volume]]
  temp <- data[[temp]]
  # goFlux flux term V P (1 - H2O) / (R T A): LI-COR gases are dry mole fractions, so the moles of
  # dry air in the chamber are needed; without the columns, 1 atm and dry air
  p_atm <- if (is.null(pressure)) 1 else data[[pressure]] / 101.325
  dry <- if (is.null(h2o)) 1 else 1 - data[[h2o]] / 1e6
  to_flux <- function(s) (s * to_ppm * volume * p_atm * dry * (1/s_to_hr) * micro_to_milli) /
    (gas_constant * (temp + celsius_to_kelvin) * area)

  data$flux <- to_flux(data[[slope]])
  # CH4 split from calculate_regression(bubble_detection = TRUE)
  for (part in c("diffusion", "diffusion_se", "ebullition")) {
    if (paste0("slope_", part) %in% names(data)) data[[paste0("flux_", part)]] <- to_flux(data[[paste0("slope_", part)]])
  }

  # Set unit of the result
  data$unit <- "mmol m-2 h-1"

  return(data)
}
