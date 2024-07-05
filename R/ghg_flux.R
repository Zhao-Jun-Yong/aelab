CO2 <- "To remove R CMD note"
CH4 <- "To remove R CMD note"
N2O <- "To remove R CMD note"

#' @title tidy_licor
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
#' tidy_licor(ghg_data_path, "ch4")
#' @export

tidy_licor <- function(file_path, gas, analyzer = "licor") {
  data <- readxl::read_excel(file_path)

  if(analyzer == "licor") {
    if (gas == "ch4") {
    title <- data[5, c(7:8, 10:11)]
    data <- data[-c(1:6), c(7:8, 10:11)]
    colnames(data) <- title
    data <- as.data.frame(data)
    data$CH4 <- as.numeric(data$CH4)
    data$CO2 <- as.numeric(data$CO2)
    data <- dplyr::filter(data, CH4 != "NaN", CO2 != "NaN")
    } else if (gas == "n2o") {
    title <- data[5, c(7:8, 10)]
    data <- data[-c(1:6), c(7:8, 10)]
    colnames(data) <- title
    data <- as.data.frame(data)
    data$N2O <- as.numeric(data$N2O)
    data <- dplyr::filter(data, N2O != "NaN")
    }

    data$TIME <- openxlsx::convertToDateTime(data$TIME)
    data$DATE <- openxlsx::convertToDateTime(data$DATE)
    data$TIME <- format(data$TIME, "%H:%M:%S")
    data$DATE <- format(data$DATE, "%Y/%m/%d")
    data$date_time <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y/%m/%d %H:%M:%S")

  } else if(analyzer == "lgr"){
    if (gas == "ch4") {
      title <- c("date_time", "CH4", "CO2", "TEMP")
      data <- data[-c(1:2), c(1, 8, 10, 14)]
      colnames(data) <- title
      data <- as.data.frame(data)
      data$CH4 <- as.numeric(data$CH4)
      data$CO2 <- as.numeric(data$CO2)
      data <- dplyr::filter(data, CH4 != "NaN", CO2 != "NaN")
      data$date_time <- sub("\\.\\d+", "", data$date_time)
      data$date_time <- as.POSIXct(data$date_time, format = "%m/%d/%Y %H:%M:%S")
    }
  }

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

  data$date_time <- lubridate::ymd_hms(data$date_time)
  data$real_datetime <- data$date_time + days(day) + hours(hr) + minutes(min) + seconds(sec)
  data$real_datetime <- format(data$real_datetime, "%Y/%m/%d %H:%M:%S")
  data$real_datetime <- as.POSIXct(data$real_datetime, format = "%Y/%m/%d %H:%M:%S")

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


  results <- tibble(
    reference_time = character(),
    slope = numeric(),
    r_square = numeric(),
    start_time = POSIXct(),
    end_time = POSIXct()
  )

  # Handle the situation when the input data was not converted using `convert_time()`
  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  } else{
    data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")
  }

  # Make sure the time zone is the same
  reference_time <- lubridate::force_tz(reference_time, tzone = "Asia/Taipei")

  for (i in seq_along(reference_time)) {
    reference_datetime <- reference_time[i]

    start_time <- reference_datetime
    end_time <- reference_datetime + (as.numeric(duration_minutes)) * 60

    filtered_data <- data[data$real_datetime >= start_time & data$real_datetime <= end_time, ]

    sorted_data <- filtered_data[order(filtered_data$real_datetime), ]

    best_r_square <- -Inf
    best_slope <- NA
    best_start_time <- NA
    best_end_time <- NA

    for (j in 1:(nrow(sorted_data) - num_rows + 1)) {
      selected_data <- sorted_data[j:(j + num_rows - 1), ]

      regression <- stats::lm(as.numeric(selected_data[[ghg]]) ~ seq_along(selected_data[[ghg]]))

      r_square <- summary(regression)$r.squared

      if (r_square > best_r_square) {
        best_r_square <- r_square
        best_slope <- stats::coef(regression)[2]
        best_start_time <- selected_data$real_datetime[1]
        best_end_time <- selected_data$real_datetime[length(selected_data$real_datetime)]  # Assign the actual end time
      }
    }

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

  return(results)
}

