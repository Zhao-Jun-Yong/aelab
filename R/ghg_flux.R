utils::globalVariables(c("CO2", "CH4", "N2O"))

#' @title tidy_ghg_analyzer
#' @importFrom lubridate ymd_hms days hours minutes seconds force_tz
#' @importFrom readxl read_excel
#' @importFrom openxlsx convertToDateTime
#' @importFrom dplyr filter
#' @description Tidy the data downloaded from GHG Analyzer.
#' @param file_path Directory of file.
#' @param gas Choose between CO2/CH4 or N2O LI-COR Trace Gas Analyzer, which is "ch4" and "n2o", respectively.
#' @param analyzer The brand of the analyzer which the data was downloaded from.
#' @return Return the loaded XLSX file after tidying for further analysis.
#' @examples
#' ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab")
#' tidy_ghg_analyzer(ghg_data_path, "ch4")
#' @export

tidy_ghg_analyzer <- function(file_path, gas, analyzer = "licor") {

  # Import data from the specified Excel file
  data <- readxl::read_excel(file_path)

  # Tidy data based on the type of analyzer
  if(analyzer == "licor") {
    if (gas == "ch4") {
      # Extract column titles for CH4 data
      title <- data[5, c(7:8, 10:11)]
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:6), c(7:8, 10:11)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      # Filter out rows with NaN values in CH4 and CO2
      data <- dplyr::filter(data, CH4 != "NaN", CO2 != "NaN")
    } else if (gas == "n2o") {
      # Extract column titles for N2O data
      title <- data[5, c(7:8, 10)]
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:6), c(7:8, 10)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$N2O <- as.numeric(data$N2O)  # Convert N2O to numeric
      # Filter out rows with NaN values in N2O
      data <- dplyr::filter(data, N2O != "NaN")
    }

    # Convert TIME and DATE columns to date-time format
    data$TIME <- openxlsx::convertToDateTime(data$TIME)
    data$DATE <- openxlsx::convertToDateTime(data$DATE)
    # Format TIME and DATE for readability
    data$TIME <- format(data$TIME, "%H:%M:%S")
    data$DATE <- format(data$DATE, "%Y/%m/%d")
    # Combine DATE and TIME into a single date_time column
    data$date_time <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y/%m/%d %H:%M:%S")
  } else if(analyzer == "lgr") {
    if (gas == "ch4") {
      # Set column titles for LGR CH4 data
      title <- c("date_time", "CH4", "CO2", "TEMP")
      # Remove unnecessary rows and keep relevant columns
      data <- data[-c(1:2), c(1, 8, 10, 14)]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      # Clean up date_time format by removing milliseconds
      data$date_time <- sub("\\.\\d+", "", data$date_time)
      # Convert date_time to POSIXct format
      data$date_time <- as.POSIXct(data$date_time, format = "%m/%d/%Y %H:%M:%S")
    }
  }

  # Return the tidied data frame
  return(data)

}

#' @title convert_time
#' @description Convert the time of the LI-COR Trace Gas Analyzer to match the time in real life.
#' @param data Data from the LI-COR Trace Gas Analyzer that had been processed by tidy_licor().
#' @param day Day(s) to add or subtract.
#' @param hr Hour(s) to add or subtract.
#' @param min Minute(s) to add or subtract.
#' @param sec Second(s) to add or subtract.
#' @return The input data with a new column in POSIXct format converted based on the input value.
#' @examples
#' data(n2o)
#' converted_n2o <- convert_time(n2o, min = -10, sec = 5)
#' @export

convert_time <- function(data, day = 0, hr = 0, min = 0, sec = 0) {

  # Convert 'date_time' column to POSIXct format
  data$date_time <- lubridate::ymd_hms(data$date_time)

  # Add specified time adjustments to the date_time (lubridate preserves POSIXct directly)
  data$real_datetime <- data$date_time +
    lubridate::days(day) +
    lubridate::hours(hr) +
    lubridate::minutes(min) +
    lubridate::seconds(sec)

  # Return the modified data frame
  return(data)

}

#' @import tibble
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom purrr map_dfr
#' @title calculate_regression
#' @description Calculate the slope of greenhouse gas (GHG) concentration change over time using simple linear regression.
#' @param data Data from the LI-COR Trace Gas Analyzer that has been processed and time-converted.
#' @param ghg Column name of the file containing data on GHG concentration (e.g., "CH4", "N2O").
#' @param reference_time The date and time at which the measurement started.
#' @param duration_minutes The duration  of the measurement, default to 7.
#' @param num_rows The number of rows used to perform the regression, default to 300.
#' @return A tibble containing the time range (POSIXct format) of the slope and R2 (both numeric) from the simple linear regression.
#' @examples
#' data(n2o)
#' calculate_regression(n2o, "N2O", as.POSIXct("2023-05-04 09:16:15", tz = "UTC"))
#' @export

calculate_regression <- function(data, ghg, reference_time,
                                 duration_minutes = 7, num_rows = 300) {

  # Check if 'real_datetime' exists; if not, use 'date_time' as a fallback
  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else {
    # Ensure 'real_datetime' is in the correct timezone
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  # Ensure the reference time is in the correct timezone
  reference_time <- lubridate::force_tz(reference_time, tzone = "Asia/Taipei")

  # Use map_dfr to iterate through each reference time (avoids O(n^2) rbind)
  results <- purrr::map_dfr(seq_along(reference_time), function(i) {
    reference_datetime <- reference_time[i]

    # Define the start and end time for the regression window
    start_time <- reference_datetime
    end_time <- reference_datetime + (as.numeric(duration_minutes)) * 60

    # Filter data to include only rows within the specified time window
    filtered_data <- data[data$real_datetime >= start_time & data$real_datetime <= end_time, ]

    # Sort the filtered data by 'real_datetime'
    sorted_data <- filtered_data[order(filtered_data$real_datetime), ]

    # Initialize variables to track the best regression results
    best_r_square <- -Inf
    best_slope <- NA
    best_start_time <- NA
    best_end_time <- NA

    # Loop through the sorted data to find the best regression fit
    for (j in 1:(nrow(sorted_data) - num_rows + 1)) {
      # Select a subset of data for regression
      selected_data <- sorted_data[j:(j + num_rows - 1), ]

      # Perform linear regression on the selected data
      regression <- stats::lm(as.numeric(selected_data[[ghg]]) ~ seq_along(selected_data[[ghg]]))

      # Calculate R-squared value from the regression summary
      r_square <- summary(regression)$r.squared

      # Update best results if the current R-squared is better
      if (r_square > best_r_square) {
        best_r_square <- r_square
        best_slope <- stats::coef(regression)[2]  # Get the slope from the regression coefficients
        best_start_time <- selected_data$real_datetime[1]  # Record the start time of the best fit
        best_end_time <- selected_data$real_datetime[length(selected_data$real_datetime)]  # Record the end time of the best fit
      }
    }

    # Return the best results for this reference time
    tibble(
      start_time = format(best_start_time, "%Y/%m/%d %H:%M:%S"),
      end_time = format(best_end_time, "%Y/%m/%d %H:%M:%S"),
      slope = best_slope,
      r_square = best_r_square,
      reference_time = reference_time[i]
    )
  })

  # Return the results tibble containing regression analysis results
  return(results)

}

#' @title calculate_ghg_flux
#' @description Calculate the greenhouse gas (GHG) flux based on input parameters from a data frame.
#' @param data A data frame containing relevant data with columns for slope, area, volume, and temperature.
#' @param slope Name of the column in `data` that contains the slope values of the GHG concentration change (in ppm/s).
#' @param area Name of the column in `data` that contains the values of the area of the chamber (in square meter).
#' @param volume Name of the column in `data` that contains values of the volume of the chamber (in litre).
#' @param temp Name of the column in `data` that contains values of the temperature of the gas (in Celsius).
#' @return A list containing the calculated flux and its unit.
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

calculate_ghg_flux <- function(data, slope = "slope", area = "area", volume = "volume", temp = "temp") {

  # Constants
  s_to_day <- (1/3600)  # seconds to days
  gas_constant <- 0.082  # gas constant
  celsius_to_kelvin <- 273.15  # Celsius to Kelvin conversion
  micro_to_milli <- 0.001  # micromoles to millimoles conversion

  # Check if specified columns exist in the data frame
  required_cols <- c(slope, area, volume, temp)
  if (!all(required_cols %in% names(data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }

  # Extract values from the data frame
  slope <- data[[slope]]
  area <- data[[area]]
  volume <- data[[volume]]
  temp <- data[[temp]]

  # Calculate flux
  data$flux <- (slope * volume * (1/s_to_day) * micro_to_milli) /
    (gas_constant * (temp + celsius_to_kelvin) * area)

  # Set unit of the result
  data$unit <- "mmol m-2 d-1"

  return(data)
}

#' @title convert_ghg_unit
#' @description Convert a greenhouse gas (GHG) flux value (or a character string
#'   containing one or more numeric values, e.g. \code{"0.002 +/- 0.003"})
#'   to micrograms per square meter per hour.
#' @details Numeric values embedded in a string (e.g. mean +/- SD notation)
#'   are each converted individually and the surrounding text is preserved.
#'   Commas are treated as decimal separators.
#' @param input A single numeric value or a character string containing one or
#'   more numbers.
#' @param ghg The molecular formula of the greenhouse gas: \code{"co2"},
#'   \code{"ch4"}, or \code{"n2o"}.
#' @param mass Mass unit of the input flux. One of \code{"mmol"}, \code{"mg"},
#'   \code{"g"}, \code{"ug"} (micrograms), \code{"nmol"}, \code{"Mg"},
#'   \code{"umol"} (micromoles), \code{"mol"}. Default \code{"ug"}.
#' @param area Area unit of the input flux. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param time Time unit of the input flux. One of \code{"yr"}, \code{"day"},
#'   \code{"hr"}, \code{"sec"}, \code{"min"}. Default \code{"hr"}.
#' @param digits Number of decimal places to round to. Default 2.
#' @param ratio Logical. If \code{TRUE}, apply an elemental-ratio correction
#'   (C-basis for CH4, N-basis for N2O). Default \code{FALSE}.
#' @return A named list with \code{value} (converted string) and \code{unit},
#'   or \code{"EMPTY"} for missing/non-numeric input.
#' @examples
#' convert_ghg_unit(97, ghg = "ch4", mass = "mg", area = "m2", time = "hr")
#' @export
convert_ghg_unit <- function(input, ghg, mass = "\u00b5g", area = "m2",
                             time = "hr", digits = 2, ratio = FALSE) {
  if (is.na(input) || input == "") return("EMPTY")

  input <- gsub(",", ".", as.character(input))

  numeric_values <- unlist(regmatches(input,
    gregexpr("[-+]?[0-9]*\\.?[0-9]+", input)))

  if (length(numeric_values) == 0) return("EMPTY")

  valid_ghgs <- c("co2", "ch4", "n2o")
  molar_mass <- switch(ghg,
    "co2" = 44.01, "ch4" = 16.04, "n2o" = 44.01,
    stop(paste("Invalid GHG type. Please use one of:",
               paste(valid_ghgs, collapse = ", "))))

  valid_masses <- c("mmol", "mg", "g", "\u00b5g", "nmol", "Mg", "\u00b5mol", "mol")
  if (!(mass %in% valid_masses))
    stop(paste("Invalid mass unit. Please use one of:",
               paste(valid_masses, collapse = ", ")))

  valid_areas <- c("ha", "m2")
  if (!(area %in% valid_areas))
    stop(paste("Invalid area unit. Please use one of:",
               paste(valid_areas, collapse = ", ")))

  valid_times <- c("yr", "day", "hr", "sec", "min")
  if (!(time %in% valid_times))
    stop(paste("Invalid time unit. Please use one of:",
               paste(valid_times, collapse = ", ")))

  convert_value <- function(value) {
    # Convert mass to µg
    value <- switch(mass,
      "mmol"      = value * molar_mass * 1000,
      "mg"        = value * 1000,
      "g"         = value * 1000000,
      "\u00b5g"   = value,
      "nmol"      = (value * molar_mass) / 1000,
      "Mg"        = value * 1e+12,
      "\u00b5mol" = value * molar_mass,
      "mol"       = value * molar_mass * 1000000
    )
    # Convert area to m2
    if (area == "ha") value <- value / 10000
    # Convert time to hours
    value <- switch(time,
      "yr"  = value / 8760,
      "day" = value / 24,
      "sec" = value * 3600,
      "min" = value * 60,
      "hr"  = value
    )
    # Optional elemental-ratio correction
    if (ratio) {
      if (ghg == "ch4") value <- value * (16.04 / 12.01)
      if (ghg == "n2o") value <- value * (44.013 / 14.0067)
    }
    round(value, digits)
  }

  for (num in numeric_values) {
    converted_num <- convert_value(as.numeric(num))
    input <- gsub(paste0("\\b", num, "\\b"), as.character(converted_num), input)
  }

  list(value = input, unit = "\u00b5g m\u207b\u00b2 h\u207b\u00b9")
}


#' @title calculate_MDF
#' @description Calculate the Minimum Detectable Flux (MDF) for a static chamber
#'   GHG measurement system.
#' @param precision_ppm Precision of the gas analyser (ppm).
#' @param closure_time_s Closure time of the measurement (seconds).
#' @param data_point_n Number of data points recorded during the closure period.
#' @param chamber_volume_m3 Internal volume of the chamber (m\eqn{^3}).
#' @param temperature_C Air temperature at the measurement location (\eqn{^\circ}C).
#' @param chamber_area_m2 Base area of the chamber (m\eqn{^2}).
#' @param pressure_pa Atmospheric pressure (Pa). Default 101325.
#' @param ideal_constant Ideal gas constant (J mol\eqn{^{-1}} K\eqn{^{-1}}).
#'   Default 8.314.
#' @param ghg Greenhouse gas type: \code{"co2"}, \code{"ch4"}, or \code{"n2o"}.
#'   Default \code{"co2"}.
#' @return A named list with \code{MDF} (numeric,
#'   \eqn{\mu}g m\eqn{^{-2}} h\eqn{^{-1}}) and \code{unit} (string).
#' @examples
#' calculate_MDF(
#'   precision_ppm     = 1,
#'   closure_time_s    = 300,
#'   data_point_n      = 300,
#'   chamber_volume_m3 = 0.0064,
#'   temperature_C     = 25,
#'   chamber_area_m2   = 0.07
#' )
#' @export
calculate_MDF <- function(precision_ppm, closure_time_s, data_point_n,
                          chamber_volume_m3, temperature_C, chamber_area_m2,
                          pressure_pa = 101325, ideal_constant = 8.314,
                          ghg = "co2") {
  if (any(c(precision_ppm, closure_time_s, data_point_n,
            chamber_volume_m3, temperature_C, chamber_area_m2) <= 0)) {
    stop("All parameters must be positive and non-zero.")
  }

  molar_masses <- list(co2 = 44.01, ch4 = 16.04, n2o = 44.01)
  if (!(ghg %in% names(molar_masses)))
    stop("Invalid GHG type. Choose from 'co2', 'ch4', or 'n2o'.")

  part1    <- precision_ppm / (closure_time_s * sqrt(data_point_n))
  part2    <- (chamber_volume_m3 * pressure_pa) /
                (ideal_constant * (temperature_C + 273.15) * chamber_area_m2)
  MDF_umol <- part1 * part2 * 3600
  MDF      <- MDF_umol * molar_masses[[ghg]]

  list(MDF = MDF, unit = "\u00b5g m\u207b\u00b2 h\u207b\u00b9")
}


#' @title calculate_total_co2e
#' @description Convert individual GHG fluxes (mg m\eqn{^{-2}} h\eqn{^{-1}})
#'   to a total CO\eqn{_2}-equivalent flux (g m\eqn{^{-2}} d\eqn{^{-1}}) using
#'   IPCC AR6 100-year GWPs (CO\eqn{_2} = 1, CH\eqn{_4} = 27,
#'   N\eqn{_2}O = 273).
#' @param co2 CO\eqn{_2} flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @param ch4 CH\eqn{_4} flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @param n2o N\eqn{_2}O flux in mg m\eqn{^{-2}} h\eqn{^{-1}}. Default 0.
#' @return Total CO\eqn{_2}e flux as a numeric scalar
#'   (g m\eqn{^{-2}} d\eqn{^{-1}}), printed with a diagnostic message.
#' @examples
#' calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003)
#' @export
calculate_total_co2e <- function(co2 = 0, ch4 = 0, n2o = 0) {
  co2e_mg_hr <- co2 + (ch4 * 27) + (n2o * 273)
  co2e_g_day <- co2e_mg_hr * 0.001 * 24

  message("Calculating total CO2e emissions:")
  message(sprintf("CO2 emissions: %.2f mg m^-2 h^-1", co2))
  message(sprintf("CH4 emissions: %.2f mg m^-2 h^-1 (GWP: 27)", ch4))
  message(sprintf("N2O emissions: %.2f mg m^-2 h^-1 (GWP: 273)", n2o))
  message(sprintf("Total CO2e emissions: %.2f g m^-2 d^-1", co2e_g_day))

  return(co2e_g_day)
}
