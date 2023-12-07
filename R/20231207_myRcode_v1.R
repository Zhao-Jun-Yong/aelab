#' @import tibble
#' @import stats
#' @import lubridate
#' @title calculate_regression
#' @description Calculate the slope of greenhouse gas (GHG) concentration change over time.
#' @param data File downloaded containing data of GHG concentration.
#' @param date Column name of the file containing date information (e.g., "DATE").
#' @param time Column name of the file containing time information (e.g., "TIME").
#' @param ghg Column name of the file containing data on GHG concentration (e.g., "CH4", "N2O").
#' @param reference_time The start time at which the measurement took place.
#' @param duration_minutes The duration(minutes) of the measurement, default to 7.
#' @param num_rows The number of rows used to perform the regression, default to 300.
#' @export

calculate_regression <- function(data, date, time, ghg, reference_time,
                                 duration_minutes = 7, num_rows = 300) {

  # Check if the required columns exist in the data
  if (!(date %in% colnames(data) && time %in% colnames(data) && ghg %in% colnames(data))) {
    stop("Required columns missing in the data. Please check the column names.")
  }

  # Check if the date and time columns are in the correct format
  if (!inherits(data[[date]], "Date")) {
    stop("The '", date, "' column is not in date format.")
  }
  if (!inherits(data[[time]], "POSIXct")) {
    stop("The '", time, "' column is not in time format.")
  }

  # Check if the ghg column is numeric
  if (!is.numeric(data[[ghg]])) {
    stop("The '", ghg, "' column is not numeric.")
  }

  # Convert date and time columns to POSIXct format
  data[[time]] <- format(data[[time]], "%H:%M:%S")
  data[[date]] <- format(data[[date]], "%Y/%m/%d")
  data[[ghg]] <- as.numeric(data[[ghg]])

  # Convert date and time columns to POSIXct format
  data$date_time <- as.POSIXct(paste(data[[date]], data[[time]]), format = "%Y/%m/%d %H:%M:%S")

  # Initialize an empty tibble to store the results
  results <- tibble(
    reference_time = character(),
    slope = numeric(),
    r_square = numeric(),
    start_time = POSIXct(),
    end_time = POSIXct()
  )

  # Iterate through each reference time
  for (rt in reference_time) {
    # Find the reference time in the dataset
    reference_datetime <- as.POSIXct(rt, format = "%Y/%m/%d %H:%M:%S")

    # Calculate the start and end time based on the reference time and duration
    start_time <- reference_datetime
    end_time <- reference_datetime + (as.numeric(duration_minutes)) * 60

    # Filter data within the specified time range
    filtered_data <- data[data$date_time >= start_time & data$date_time <= end_time, ]

    # Sort the filtered data by the time variable in ascending order
    sorted_data <- filtered_data[order(filtered_data$date_time), ]

    # Initialize variables to store the best regression result
    best_r_square <- -Inf
    best_slope <- NA
    best_start_time <- NA
    best_end_time <- NA

    # Iterate through different subsets of rows
    for (i in 1:(nrow(sorted_data) - num_rows + 1)) {
      # Select the current subset of rows
      selected_data <- sorted_data[i:(i + num_rows - 1), ]

      # Perform linear regression using the selected subset
      regression <- lm(as.numeric(selected_data[[ghg]]) ~ seq_along(selected_data[[ghg]]))

      # Get the R-square value
      r_square <- summary(regression)$r.squared

      # Check if the current subset has a higher R-square value
      if (r_square > best_r_square) {
        best_r_square <- r_square
        best_slope <- coef(regression)[2]
        best_start_time <- selected_data$date_time[1]
        best_end_time <- selected_data$date_time[length(selected_data$date_time)]  # Assign the actual end time
      }
    }

    # Append the results to the tibble
    results <- rbind(
      results,
      tibble(
        start_time = format(best_start_time, "%H:%M:%S"),
        end_time = format(best_end_time, "%H:%M:%S"),
        slope = best_slope,
        r_square = best_r_square,
        reference_time = rt
      )
    )
  }

  # Return the tibble with results
  return(results)
}

#' @title convert time
#' @description Convert the recorded time to match the time of the LI-COR Trace Gas Analyzer.
#' @param datetime_value Recorded time in the unit of year/month/day hour:minutes:seconds.
#' @param day The day(s) difference between real time and the LI-COR Trace Gas Analyzer.
#' @param hr The hour(s) difference between real time and the LI-COR Trace Gas Analyzer.
#' @param min The minute(s) difference between real time and the LI-COR Trace Gas Analyzer.
#' @param sec The second(s) difference between real time and the LI-COR Trace Gas Analyzer.
#' @export

convert_time <- function(datetime_value, day = 0, hr = 0, min = 0, sec = 0) {
  datetime <- lubridate::ymd_hms(datetime_value)
  updated_datetime <- datetime + days(day) + hours(hr) + minutes(min) + seconds(sec)
  formatted_datetime <- format(updated_datetime, "%Y/%m/%d %H:%M:%S")
  return(formatted_datetime)
}


