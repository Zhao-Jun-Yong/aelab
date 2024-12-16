sunset <- "To remove R CMD note"
sunrise <- "To remove R CMD note"
date_time <- "To remove R CMD note"
temp <- "To remove R CMD note"
temp_k <- "To remove R CMD note"
salinity <- "To remove R CMD note"
c_o2 <- "To remove R CMD note"
o2_saturation <- "To remove R CMD note"
pressure_hpa <- "To remove R CMD note"
wind_ms <- "To remove R CMD note"
k600 <- "To remove R CMD note"
sc <- "To remove R CMD note"
cor_o2_saturation_pressure <- "To remove R CMD note"
k <- "To remove R CMD note"
rate_do_change <- "To remove R CMD note"
depth_m <- "To remove R CMD note"
flux <- "To remove R CMD note"
time <- "To remove R CMD note"
sunrise_time <- "To remove R CMD note"
sunset_time <- "To remove R CMD note"
site <- "To remove R CMD note"
no_hobo <- "To remove R CMD note"
nep_hr <- "To remove R CMD note"
daylight_hr <- "To remove R CMD note"
r_hr <- "To remove R CMD note"
nep_daytime <- "To remove R CMD note"
r_daytime <- "To remove R CMD note"
gpp <- "To remove R CMD note"
r_day <- "To remove R CMD note"
nep <- "To remove R CMD note"
hobo <- "To remove R CMD note"



#' @title process_hobo
#' @importFrom lubridate ceiling_date
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#' @importFrom stats aggregate
#' @importFrom stats time
#' @description Tidy the data retrieved from HOBO U26 Dissolved Oxygen Data Logger.
#' @param file_path Directory of file.
#' @param no_hobo The code for the data logger.
#' @return A dataframe.
#' @examples
#' hobo_data_path <- system.file("extdata", "ex_hobo.csv", package = "aelab")
#' df <- process_hobo(hobo_data_path, "code_for_logger")
#' @export

process_hobo <- function(file_path, no_hobo) {
  # Read the CSV file without headers
  df <- utils::read.csv(file_path, header = F)

  # Remove the first two rows and columns 5 to 9
  df <- df[-c(1:2), -c(5:9)]

  # Set column names for the dataframe
  colnames(df) <- c("no", "date_time", "do", "temp")

  # Convert to a data frame and remove rows with NA values
  df <- as.data.frame(df)
  df <- stats::na.omit(df)

  # Check if date_time contains "上午" (morning) or "下午" (afternoon)
  if (any(grepl("\u4e0a\u5348|\u4e0b\u5348", df$date_time))) {

    # Create a new variable to indicate morning or afternoon
    df$new_variable <- ifelse(grepl("\u4e0a\u5348", df$date_time), "morning", "afternoon")

    # Clean up the date_time string by removing unnecessary characters
    df$date_time <- gsub("\u4e0a\u5348|\u4e0b\u5348", "", df$date_time)  # Remove "上午" and "下午"
    df$date_time <- gsub("\u6642", ":", df$date_time)                     # Replace "時" with ":"
    df$date_time <- gsub("\u5206", ":", df$date_time)                     # Replace "分" with ":"
    df$date_time <- gsub("\u79d2", "", df$date_time)                      # Remove "秒"

    # Convert the cleaned date_time to POSIXlt format
    df$date_time <- strptime(df$date_time, format = "%m/%d/%Y %H:%M:%S")

    # Create subsets for afternoon and morning based on time conditions
    subset_afternoon <- df[df$new_variable == "afternoon" &
                             format(df$date_time, "%H:%M:%S") >= "01:00:00" &
                             format(df$date_time, "%H:%M:%S") <= "11:59:59", ]
    subset_morning <- df[df$new_variable == "morning" &
                           format(df$date_time, "%H:%M:%S") >= "12:00:00" &
                           format(df$date_time, "%H:%M:%S") <= "12:59:59", ]

    # Adjust the date_time for afternoon and morning subsets
    subset_afternoon$date_time <- subset_afternoon$date_time + lubridate::hours(12)  # Convert to 24-hour format
    subset_morning$date_time <- subset_morning$date_time + lubridate::hours(12) - lubridate::days(1)  # Adjust for previous day

    # Update the original dataframe with the adjusted subsets
    df[df$new_variable == "afternoon" &
         format(df$date_time, "%H:%M:%S") >= "01:00:00" &
         format(df$date_time, "%H:%M:%S") <= "11:59:59", ] <- subset_afternoon
    df[df$new_variable == "morning" &
         format(df$date_time, "%H:%M:%S") >= "12:00:00" &
         format(df$date_time, "%H:%M:%S") <= "12:59:59", ] <- subset_morning

    # Clean up temporary variables
    remove(subset_afternoon)
    remove(subset_morning)
    df$new_variable <- NULL  # Remove the new_variable column

  } else {
    # No action needed if date_time does not contain morning or afternoon indicators
  }

  # Convert 'do' and 'temp' columns to numeric
  df$do <- as.numeric(df$do)
  df$temp <- as.numeric(df$temp)

  # Extract time from date_time for further analysis
  df$time <- format(df$date_time, "%H:%M:%S")

  # Round date_time to the nearest 30 minutes
  df$date_time <- as.factor(lubridate::ceiling_date(df$date_time, unit = "30 minutes"))

  # Aggregate data by date_time, calculating the mean for 'do' and 'temp'
  tidy_df <- stats::aggregate(cbind(do, temp) ~ date_time, df, function(x) mean(x, na.rm = TRUE))

  # Arrange the tidy dataframe by date_time
  tidy_df <- arrange(tidy_df, date_time)

  # Convert date_time back to character and then to POSIXct format
  tidy_df$date_time <- as.character(tidy_df$date_time)
  tidy_df$date_time <- as.POSIXct(tidy_df$date_time, format = "%Y-%m-%d %H:%M:%S")

  # Set the timezone to Asia/Taipei
  tidy_df$date_time <- force_tz(tidy_df$date_time, tzone = "Asia/Taipei")

  # Add the no_hobo parameter to the tidy dataframe
  tidy_df$no_hobo <- {{no_hobo}}

  # Return the cleaned and aggregated dataframe
  return(tidy_df)
}


#' @title convert_time
#' @importFrom readr read_csv
#' @importFrom tidyr fill
#' @import lubridate
#' @description Tidy the daily weather data downloaded from weather station in Taiwan.
#' @param file_path Directory of file.
#' @param date Date of the daily weather data in yyyy-mm-dd format.
#' @param zone Code for the region of the weather station.
#' @return A dataframe.
#'@examples
#' weather_data_path <- system.file("extdata", "ex_weather.csv", package = "aelab")
#' df <- process_weather(weather_data_path, "2024-01-01", "site_A")
#' @export

process_weather <- function(file_path, date, zone) {

  # This function processes a CSV file downloaded with a specific format
  # Read the CSV file into a dataframe
  df <- readr::read_csv(file_path)

  # Identify the columns for hours, pressure (hPa), and wind speed (m/s)
  hours <- grep("\u89c0\u6e2c\u6642\u9593", names(df), ignore.case = F)  # "觀測時間"
  pressure_hpa <- grep("\u6e2c\u7ad9\u6c23", names(df), ignore.case = F)  # "測站氣"
  wind_speed <- grep("\u98a8\u901f", names(df), ignore.case = F)  # "風速"

  # Duplicate the dataframe and remove unnecessary rows and columns
  df <- rbind(df, df)  # Duplicate the dataframe
  df <- df[-c(1, 26), c(hours, pressure_hpa, wind_speed)]  # Remove the first row and the 26th row, keep relevant columns

  # Rename the columns for clarity
  colnames(df) <- c("hours", "pressure_hpa", "wind_ms")

  # Convert columns to numeric type
  df$hours <- as.numeric(df$hours)
  df$pressure_hpa <- as.numeric(df$pressure_hpa)
  df$wind_ms <- as.numeric(df$wind_ms)

  # Sort the dataframe by hours
  df <- df[order(df$hours), ]

  # Fill missing values in pressure and wind speed columns using down-up method
  df <- tidyr::fill(df, pressure_hpa, wind_ms, .direction = "downup")

  # Check if each hour from 1 to 24 has exactly 2 entries
  values <- 1:24
  results <- all(sapply(values, function(x) sum(df$hours == x) == 2))

  # If any hour is missing duplicates, issue a warning and return NULL
  if (!results) {
    warning("Some hours are missing duplicates in the dataset.")
    return(NULL)
  } else {
    # Create a sequence of time labels in 30-minute intervals
    time_sequence <- format(
      seq(
        from = as.POSIXct("00:30:00", format = "%H:%M:%S", tz = "Asia/Taipei"),
        by = "30 min",
        length.out = nrow(df)  # Length matches the number of rows in df
      ),
      format = "%H:%M:%S"
    )

    # Convert the input date to POSIXlt format
    df$date <- as.POSIXlt(date, format = "%Y-%m-%d", tz = "Asia/Taipei")

    # Add the time sequence to the dataframe
    df$time <- time_sequence

    # Combine date and time into a single datetime column
    df$date_time <- as.POSIXlt(paste(date, time_sequence), format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei")
    df$date_time <- as.POSIXct(df$date_time)  # Convert to POSIXct for easier manipulation
    df$date_time <- force_tz(df$date_time, tzone = "Asia/Taipei")  # Ensure timezone is set correctly

    # Adjust the last entry's date_time to the next day
    new_date <- lubridate::as_datetime(df$date_time[48])
    df$date_time[48] <- new_date + lubridate::days(1)

    # Clean up unnecessary columns
    df$date <- NULL
    df$time <- NULL
    df$hours <- NULL
    df$zone <- {{zone}}  # Assign the zone parameter to the dataframe

    # Convert the dataframe to a standard data frame format and return it
    return(as.data.frame(df))
  }
}


#' @title process_info
#' @importFrom readxl read_excel
#' @description Import and process the necessary information,
#' including the sunrise and sunset times of the day,
#' the date and time range of the deployment,
#' and the code for the data logger.
#' @param file_path Directory of file.
#' @return A dataframe.
#' @examples
#' info_data_path <- system.file("extdata", "info.xlsx", package = "aelab")
#' df <- process_info(info_data_path)
#' @export

process_info <- function(file_path) {

  # Read the Excel file into a dataframe
  info <- readxl::read_excel(file_path)

  # Convert sunrise, sunset, start_date_time, and end_date_time columns to Asia/Taipei timezone
  info$sunrise <- force_tz(info$sunrise, tzone = "Asia/Taipei")
  info$sunset <- force_tz(info$sunset, tzone = "Asia/Taipei")
  info$start_date_time <- force_tz(info$start_date_time, tzone = "Asia/Taipei")
  info$end_date_time <- force_tz(info$end_date_time, tzone = "Asia/Taipei")

  # Ensure no_hobo is treated as a character type
  info <- info %>%
    mutate(no_hobo = as.character(no_hobo))

  # Return the processed dataframe
  return(info)
}

#' @title plot_hobo
#' @import ggplot2
#' @description Plot the dissolved oxygen concentration over time series grouped by different data loggers to observe the variations.
#' @param df Dataframe produced by process_hobo() function.
#' @return A plot generated by ggplot2.
#' @examples
#' data(hobo)
#' plot_hobo(hobo)
#' @export

plot_hobo <- function(df) {

  # Create a scatter plot using ggplot2
  ggplot2::ggplot(df, aes(x = date_time, y = do)) +
    geom_point(size = 1) +
    facet_grid(no_hobo ~ ., scales = "free")
}

#' @title calculate_do
#' @import dplyr
#' @description Calculate the Net Ecosystem Production,
#' Gross Primary Production and Ecosystem respiration based on the change in dissolved oxygen concentration.
#' @param df Merged dataframe produced by process_hobo(), process_weather() and process_info() functions.
#' @return A dataframe.
#' @examples
#' data(hobo)
#' calculate_do(hobo)
#' @export

calculate_do <- function(df) {

  # Calculate various metrics related to dissolved oxygen and ecosystem productivity
  df <- df %>%
    dplyr::mutate(
      # Calculate daylight hours as the difference between sunset and sunrise
      daylight_hr = as.numeric(sunset - sunrise),

      # Extract time and date from date_time
      time = format(date_time, "%H:%M:%S"),
      date = as.Date(date_time, tz = "Asia/Taipei"),

      # Format sunset and sunrise times
      sunset_time = format(sunset, "%H:%M:%S"),
      sunrise_time = format(sunrise, "%H:%M:%S"),

      # Convert temperature from Celsius to Kelvin
      temp_k = temp + 273.15,

      # Calculate the concentration of oxygen (c_o2) using the temperature and salinity
      c_o2 = -173.4292 + 249.6336 * (100/temp_k) +
        143.3483 * log(temp_k/100) -
        21.8492 * (temp_k/100) +
        salinity * (-0.033096 + 0.014259 * (temp_k/100) - 0.0017 * (temp_k/100)^2),

      # Calculate oxygen saturation based on concentration
      o2_saturation = exp(c_o2) * 1.423,

      # Correct oxygen saturation pressure based on atmospheric pressure
      cor_o2_saturation_pressure = o2_saturation * (pressure_hpa * 0.0987 - 0.0112) / 100,

      # Calculate the rate of change in dissolved oxygen (DO)
      rate_do_change = (c(NA, diff(do))) * 2,  # Multiply by 2 to adjust for the time interval

      # Calculate Schmidt number (sc) based on temperature
      sc = 0.0476 * temp^3 + 3.7818 * temp^2 - 120.1 * temp + 1800.6,

      # Calculate gas exchange coefficient (k600) based on wind speed
      k600 = (2.07 + 0.215 * (wind_ms^1.7)) / 100,

      # Calculate the gas exchange coefficient (k) adjusted for Schmidt number
      k = k600 * (sc / 600)^(-0.5),

      # Calculate the flux of dissolved oxygen
      flux = (do - cor_o2_saturation_pressure) * k,

      # Calculate net ecosystem production (NEP) per hour
      nep_hr = rate_do_change * depth_m - flux
    )

  # Calculate average NEP for nighttime (outside of daylight hours)
  tidy_do_r <- df %>%
    dplyr::filter(time < sunrise_time | time >= sunset_time) %>%
    dplyr::group_by(site, no_hobo, date) %>%
    dplyr::summarise(r_hr = mean(nep_hr, na.rm = TRUE)) %>%
    dplyr::ungroup()

  # Calculate average NEP for daytime (within daylight hours)
  tidy_do_nep <- df %>%
    dplyr::filter(time >= sunrise_time, time < sunset_time) %>%
    dplyr::group_by(site, no_hobo, date) %>%
    dplyr::summarise(nep_hr = mean(nep_hr, na.rm = TRUE),
                     daylight_hr = mean(daylight_hr, na.rm = TRUE)) %>%
    dplyr::mutate(nep_daytime = nep_hr * daylight_hr) %>%
    dplyr::ungroup()

  # Merge nighttime and daytime NEP results
  tidy_do <- merge(tidy_do_r, tidy_do_nep, by = c("site", "no_hobo", "date"))

  # Calculate total respiration and gross primary production (GPP)
  tidy_do <- tidy_do %>%
    dplyr::mutate(r_daytime = r_hr * daylight_hr,
                  r_day = r_hr * 24,  # Total respiration over 24 hours
                  gpp = nep_daytime - r_daytime,  # Gross primary production
                  nep = gpp + r_day  # Net ecosystem production
    ) %>%
    dplyr::select(site, no_hobo, date, r_day, gpp, nep)  # Select relevant columns for output

  # Return the tidy dataframe with calculated metrics
  return(tidy_do)
}

#' @title combine_weather
#' @importFrom dplyr bind_rows
#' @description Tidy multiple daily weather data downloaded from weather station in Taiwan.
#' @param file_path Directory of folder containing the files (including the character in the file name that precedes the date).
#' @param start_date Date of the daily weather data in yyyy-mm-dd format.
#' @param end_date Date of the daily weather data in yyyy-mm-dd format.
#' @param zone Code for the region of the weather station.
#' @return A dataframe.
#'@examples
#' weather_data_path <- system.file("extdata", package = "aelab")
#' modified_data_path <- paste0(weather_data_path, "/ex_")
#' df <- combine_weather(modified_data_path,
#' start_date = "2024-01-01",
#' end_date = "2024-01-02", "site_A")
#' @export

combine_weather <- function(file_path, start_date, end_date, zone) {

  # Generate a sequence of dates from start_date to end_date
  dates <- as.character(seq(as.Date(start_date), as.Date(end_date), by = "day"))

  # Initialize an empty list to store data frames for each date
  df <- list()

  # Loop through each date to read and process the corresponding weather data
  for (date in dates) {
    # Construct the file name based on the date
    file_name <- paste0(file_path, date, ".csv")

    # Process the weather data for the current date and store it in the list
    df[[date]] <- process_weather(file_name, date, zone)
  }

  # Combine all data frames in the list into a single data frame
  weather <- dplyr::bind_rows(df)

  # Return the combined weather data
  return(weather)
}

#' @title combine_hobo
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @description Tidy multiple data retrieved from HOBO U26 Dissolved Oxygen Data Logger.
#' @param file_path Directory of the folder containing the files.
#' @param file_prefix The prefix before the code for the data logger, defaults to "no."
#' @return A dataframe.
#' @examples
#' hobo_data_path <- system.file("extdata", package = "aelab")
#' df <- combine_hobo(hobo_data_path, file_prefix = "ex_ho")
#' @export

combine_hobo <- function(file_path, file_prefix = "no.") {

  # List all files in the specified directory that match the given prefix
  file_names <- list.files(file_path, pattern = paste0("^", file_prefix))

  # Initialize an empty data frame to store combined results
  df <- data.frame()

  # Loop through each file name to process the corresponding data
  for (file_name in file_names) {
    # Extract the hobo number from the file name
    no_hobo <- stringr::str_replace(file_name, paste0("^", file_prefix), "")
    no_hobo <- stringr::str_remove(no_hobo, ".csv")

    # Construct the full file path for the current file
    file <- file.path(file_path, file_name)

    # Process the hobo data using the process_hobo function
    do <- process_hobo(file_path = file, no_hobo = no_hobo)

    # Combine the processed data into the main data frame
    df <- rbind(df, do)
  }

  # Return the combined data frame containing all processed hobo data
  return(df)
}

