CO2 <- "To remove R CMD note"
CH4 <- "To remove R CMD note"
N2O <- "To remove R CMD note"

#' @title tidy_ghg_analyzer
#' @import lubridate
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
#' @import lubridate
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

  # Add specified time adjustments to the date_time
  data$real_datetime <- data$date_time + days(day) + hours(hr) + minutes(min) + seconds(sec)

  # Format 'real_datetime' as a string in "YYYY/MM/DD HH:MM:SS"
  data$real_datetime <- format(data$real_datetime, "%Y/%m/%d %H:%M:%S")

  # Convert the formatted string back to POSIXct
  data$real_datetime <- as.POSIXct(data$real_datetime, format = "%Y/%m/%d %H:%M:%S")

  # Return the modified data frame
  return(data)

}

#' @import tibble
#' @importFrom stats lm
#' @importFrom stats coef
#' @import lubridate
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

  # Initialize an empty tibble to store results
  results <- tibble(
    reference_time = character(),
    slope = numeric(),
    r_square = numeric(),
    start_time = POSIXct(),
    end_time = POSIXct()
  )

  # Check if 'real_datetime' exists; if not, use 'date_time' as a fallback
  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else {
    # Ensure 'real_datetime' is in the correct timezone
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  # Ensure the reference time is in the correct timezone
  reference_time <- lubridate::force_tz(reference_time, tzone = "Asia/Taipei")

  # Loop through each reference time to perform regression analysis
  for (i in seq_along(reference_time)) {
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

    # Append the best results for this reference time to the results tibble
    results <- rbind(
      results,
      tibble(
        start_time = format(best_start_time, "%Y/%m/%d %H:%M:%S"),
        end_time = format(best_end_time, "%Y/%m/%d %H:%M:%S"),
        slope = best_slope,
        r_square = best_r_square,
        reference_time = reference_time[i]
      )
    )
  }

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
#' @description Convert the greenhouse gas (GHG) flux to micromoles per square meter per hour.
#' @param ghg_value The value of the flux.
#' @param ghg The molecular formula of greenhouse gases (co2: carbon dioxide; ch4: methane; n2o: nitrous oxide).
#' @param mass The mass component of the input GHG flux, default to micromoles.
#' @param area The area component of the input GHG flux, default to square meter.
#' @param time The time component of the input GHG flux, default to hour.
#' @return A numeric value.
#' @examples
#' convert_ghg_unit(1, ghg = "co2")
#' @export

convert_ghg_unit <- function(ghg_value, ghg, mass = "\u00B5mol", area = "m2", time = "h") {

  # Define molar masses for GHGs in g/mol
  molar_mass <- c(co2 = 44.01, ch4 = 16.04, n2o = 44.01)

  # Check if the specified GHG is valid
  if (!ghg %in% names(molar_mass)) {
    stop("Invalid GHG type. Please use 'co2', 'ch4', or 'n2o'.")
  }

  # Get the molar mass for the specified GHG
  mm <- molar_mass[[ghg]]

  # Convert mass to micromoles
  mass_conversion <- switch(mass,
                            "mmol" = ghg_value * 1000,
                            "mg" = (ghg_value / mm) * 1000,
                            "g" = (ghg_value / mm) * 1000000,
                            "\u00B5g" = (ghg_value / mm),
                            "\u00B5mol" = ghg_value,
                            stop("Unsupported mass unit."))

  # Convert area to per square meter
  area_conversion <- switch(area,
                            "ha" = mass_conversion / 10000,
                            "m2" = mass_conversion,
                            stop("Unsupported area unit."))

  # Convert time to per hour
  time_conversion <- switch(time,
                            "yr" = area_conversion / 8760,
                            "d" = area_conversion / 24,
                            "h" = area_conversion,
                            stop("Unsupported time unit."))

  return(time_conversion)
}

