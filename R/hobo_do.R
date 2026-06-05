utils::globalVariables(c(
  "sunset", "sunrise", "date_time", "temp", "temp_k", "salinity",
  "c_o2", "o2_saturation", "pressure_hpa", "wind_ms", "k600", "sc",
  "cor_o2_saturation_pressure", "k", "rate_do_change", "depth_m",
  "flux", "time", "sunrise_time", "sunset_time", "site", "no_hobo",
  "nep_hr", "daylight_hr", "r_hr", "nep_daytime", "r_daytime",
  "gpp", "r_day", "nep", "hobo", "do", "date", "air_temp", "rh",
  "stno", "yyyymmddhh"
))


#' @title process_hobo
#' @importFrom lubridate ceiling_date hours days force_tz parse_date_time
#' @importFrom dplyr arrange
#' @importFrom utils read.csv
#' @importFrom stats na.omit aggregate
#' @description Tidy data exported from a HOBO data logger. Supports the HOBO
#'   U26 Dissolved Oxygen Data Logger (\code{type = "do"}) and the HOBO Pro v2
#'   Temperature/RH Logger (\code{type = "temp"}). Handles both Chinese
#'   (上午/下午) and English (AM/PM or 24-hour) HOBOware locale exports, with
#'   2- or 4-digit years.
#' @param file_path Path to the CSV file exported from HOBOware.
#' @param no_hobo The code for the data logger.
#' @param type Logger type: \code{"do"} (default) for HOBO U26 DO logger, or
#'   \code{"temp"} for HOBO Pro v2 temperature/RH logger.
#' @return A dataframe. For \code{type = "do"}: columns \code{date_time},
#'   \code{do}, \code{temp}, \code{no_hobo}, aggregated to 30-minute intervals.
#'   For \code{type = "temp"}: columns \code{date_time}, \code{air_temp},
#'   \code{rh}, \code{no_hobo}, one row per logger reading.
#' @examples
#' hobo_data_path <- system.file("extdata", "ex_hobo.csv", package = "aelab")
#' df <- process_hobo(hobo_data_path, "code_for_logger")
#' @export

process_hobo <- function(file_path, no_hobo, type = c("do", "temp")) {
  type <- match.arg(type)

  df <- utils::read.csv(file_path, header = FALSE)
  df <- df[-c(1:2), 1:4]
  colnames(df) <- if (type == "do") {
    c("no", "date_time", "do", "temp")
  } else {
    c("no", "date_time", "air_temp", "rh")
  }
  df <- as.data.frame(df)
  df <- stats::na.omit(df)

  # Handle Chinese AM/PM locale export (上午 = morning, 下午 = afternoon)
  if (any(grepl("上午|下午", df$date_time))) {
    df$new_variable <- ifelse(grepl("上午", df$date_time), "morning", "afternoon")
    df$date_time <- gsub("上午|下午", "", df$date_time)
    df$date_time <- gsub("時", ":", df$date_time)
    df$date_time <- gsub("分", ":", df$date_time)
    df$date_time <- gsub("秒", "", df$date_time)
    df$date_time <- strptime(df$date_time, format = "%m/%d/%Y %H:%M:%S")

    subset_afternoon <- df[df$new_variable == "afternoon" &
      format(df$date_time, "%H:%M:%S") >= "01:00:00" &
      format(df$date_time, "%H:%M:%S") <= "11:59:59", ]
    subset_morning <- df[df$new_variable == "morning" &
      format(df$date_time, "%H:%M:%S") >= "12:00:00" &
      format(df$date_time, "%H:%M:%S") <= "12:59:59", ]

    subset_afternoon$date_time <- subset_afternoon$date_time + lubridate::hours(12)
    subset_morning$date_time <- subset_morning$date_time + lubridate::hours(12) - lubridate::days(1)

    df[df$new_variable == "afternoon" &
      format(df$date_time, "%H:%M:%S") >= "01:00:00" &
      format(df$date_time, "%H:%M:%S") <= "11:59:59", ] <- subset_afternoon
    df[df$new_variable == "morning" &
      format(df$date_time, "%H:%M:%S") >= "12:00:00" &
      format(df$date_time, "%H:%M:%S") <= "12:59:59", ] <- subset_morning

    rm(subset_afternoon, subset_morning)
    df$new_variable <- NULL

    # Fix year mis-parsed as 00XX instead of 20XX in Chinese locale exports
    df$date_time <- as.POSIXct(gsub("^00(\\d{2})", "20\\1", format(df$date_time)))
  } else {
    # English locale export: 12-hour AM/PM or 24-hour clock, 2- or 4-digit year
    df$date_time <- lubridate::parse_date_time(
      df$date_time,
      orders = c("mdy IMS p", "mdY IMS p", "mdy HMS", "mdY HMS"),
      tz = "Asia/Taipei", quiet = TRUE
    )
  }
  df$date_time <- lubridate::force_tz(df$date_time, tzone = "Asia/Taipei")

  if (type == "do") {
    df$do <- as.numeric(df$do)
    df$temp <- as.numeric(df$temp)

    df$time <- format(df$date_time, "%H:%M:%S")
    df$date_time <- as.factor(lubridate::ceiling_date(df$date_time, unit = "30 minutes"))

    tidy_df <- stats::aggregate(cbind(do, temp) ~ date_time, df, function(x) mean(x, na.rm = TRUE))
    tidy_df <- dplyr::arrange(tidy_df, date_time)

    tidy_df$date_time <- as.POSIXct(as.character(tidy_df$date_time), format = "%Y-%m-%d %H:%M:%S")
    tidy_df$date_time <- lubridate::force_tz(tidy_df$date_time, tzone = "Asia/Taipei")
    tidy_df$no_hobo <- no_hobo
    return(tidy_df)
  } else {
    df$air_temp <- as.numeric(df$air_temp)
    df$rh <- as.numeric(df$rh)
    df$no_hobo <- no_hobo
    return(df[, c("date_time", "air_temp", "rh", "no_hobo")])
  }
}


#' @title convert_time
#' @importFrom readr read_csv
#' @importFrom tidyr fill
#' @importFrom lubridate as_datetime days force_tz
#' @description Tidy the daily weather data downloaded from weather station in Taiwan.
#' @param file_path Directory of file.
#' @param date Date of the daily weather data in yyyy-mm-dd format.
#' @param zone Code for the region of the weather station.
#' @return A dataframe.
#' @examples
#' weather_data_path <- system.file("extdata", "ex_weather.csv", package = "aelab")
#' df <- process_weather(weather_data_path, "2024-01-01", "site_A")
#' @export

process_weather <- function(file_path, date, zone) {
  # This function processes a CSV file downloaded with a specific format
  # Read the CSV file into a dataframe
  df <- readr::read_csv(file_path)

  # Identify the columns for hours, pressure (hPa), and wind speed (m/s)
  hours <- grep("觀測時間", names(df), ignore.case = F) # "觀測時間"
  pressure_hpa <- grep("測站氣", names(df), ignore.case = F) # "測站氣"
  wind_speed <- grep("風速", names(df), ignore.case = F) # "風速"

  # Duplicate the dataframe and remove unnecessary rows and columns
  df <- rbind(df, df) # Duplicate the dataframe
  df <- df[-c(1, 26), c(hours, pressure_hpa, wind_speed)] # Remove the first row and the 26th row, keep relevant columns

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
        length.out = nrow(df) # Length matches the number of rows in df
      ),
      format = "%H:%M:%S"
    )

    # Convert the input date to POSIXlt format
    df$date <- as.POSIXlt(date, format = "%Y-%m-%d", tz = "Asia/Taipei")

    # Add the time sequence to the dataframe
    df$time <- time_sequence

    # Combine date and time into a single datetime column
    df$date_time <- as.POSIXlt(paste(date, time_sequence), format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei")
    df$date_time <- as.POSIXct(df$date_time) # Convert to POSIXct for easier manipulation
    df$date_time <- lubridate::force_tz(df$date_time, tzone = "Asia/Taipei") # Ensure timezone is set correctly

    # Adjust the last entry's date_time to the next day
    new_date <- lubridate::as_datetime(df$date_time[48])
    df$date_time[48] <- new_date + lubridate::days(1)

    # Clean up unnecessary columns
    df$date <- NULL
    df$time <- NULL
    df$hours <- NULL
    df$zone <- zone # Assign the zone parameter to the dataframe

    # Convert the dataframe to a standard data frame format and return it
    return(as.data.frame(df))
  }
}


#' @title process_info
#' @importFrom readxl read_excel
#' @importFrom lubridate force_tz
#' @importFrom dplyr mutate
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
  info$sunrise <- lubridate::force_tz(info$sunrise, tzone = "Asia/Taipei")
  info$sunset <- lubridate::force_tz(info$sunset, tzone = "Asia/Taipei")
  info$start_date_time <- lubridate::force_tz(info$start_date_time, tzone = "Asia/Taipei")
  info$end_date_time <- lubridate::force_tz(info$end_date_time, tzone = "Asia/Taipei")

  # Ensure no_hobo is treated as a character type
  info <- dplyr::mutate(info, no_hobo = as.character(no_hobo))

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
#' @importFrom dplyr mutate filter summarise select
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
  df <- df |>
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
      c_o2 = -173.4292 + 249.6336 * (100 / temp_k) +
        143.3483 * log(temp_k / 100) -
        21.8492 * (temp_k / 100) +
        salinity * (-0.033096 + 0.014259 * (temp_k / 100) - 0.0017 * (temp_k / 100)^2),

      # Calculate oxygen saturation based on concentration
      o2_saturation = exp(c_o2) * 1.423,

      # Correct oxygen saturation pressure based on atmospheric pressure
      cor_o2_saturation_pressure = o2_saturation * (pressure_hpa * 0.0987 - 0.0112) / 100,

      # Calculate the rate of change in dissolved oxygen (DO)
      rate_do_change = (c(NA, diff(do))) * 2, # Multiply by 2 to adjust for the time interval

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
  tidy_do_r <- df |>
    dplyr::filter(time < sunrise_time | time >= sunset_time) |>
    dplyr::summarise(r_hr = mean(nep_hr, na.rm = TRUE), .by = c(site, no_hobo, date))

  # Calculate average NEP for daytime (within daylight hours)
  tidy_do_nep <- df |>
    dplyr::filter(time >= sunrise_time, time < sunset_time) |>
    dplyr::summarise(
      nep_hr = mean(nep_hr, na.rm = TRUE),
      daylight_hr = mean(daylight_hr, na.rm = TRUE),
      .by = c(site, no_hobo, date)
    ) |>
    dplyr::mutate(nep_daytime = nep_hr * daylight_hr)

  # Merge nighttime and daytime NEP results
  tidy_do <- merge(tidy_do_r, tidy_do_nep, by = c("site", "no_hobo", "date"))

  # Calculate total respiration and gross primary production (GPP)
  tidy_do <- tidy_do |>
    dplyr::mutate(
      r_daytime = r_hr * daylight_hr,
      r_day = r_hr * 24, # Total respiration over 24 hours
      gpp = nep_daytime - r_daytime, # Gross primary production
      nep = gpp + r_day # Net ecosystem production
    ) |>
    dplyr::select(site, no_hobo, date, r_day, gpp, nep) # Select relevant columns for output

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
#' @examples
#' weather_data_path <- system.file("extdata", package = "aelab")
#' modified_data_path <- paste0(weather_data_path, "/ex_")
#' df <- combine_weather(modified_data_path,
#'   start_date = "2024-01-01",
#'   end_date = "2024-01-02", "site_A"
#' )
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
#' @importFrom purrr map_dfr
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

  # Use map_dfr to process all files and combine results (avoids O(n^2) rbind)
  df <- purrr::map_dfr(file_names, function(file_name) {
    # Extract the hobo number from the file name
    no_hobo <- stringr::str_replace(file_name, paste0("^", file_prefix), "")
    no_hobo <- stringr::str_remove(no_hobo, ".csv")

    # Construct the full file path for the current file
    file <- file.path(file_path, file_name)

    # Process the hobo data using the process_hobo function
    process_hobo(file_path = file, no_hobo = no_hobo)
  })

  # Return the combined data frame containing all processed hobo data
  return(df)
}


#' @title process_weather_month
#' @description Import and tidy a monthly weather CSV file downloaded from a
#'   Taiwan Central Weather Administration station. Column selection is done via
#'   regex so minor header changes are handled gracefully.
#' @param file_path Path to the monthly CSV file.
#' @param month Month number (1–12) covered by the file.
#' @param year Four-digit year. Default 2024.
#' @param zone Character label for the weather station / region.
#' @return A data frame with columns \code{day}, \code{pressure_hpa},
#'   \code{temp}, \code{humidity_percent}, \code{wind_ms}, \code{rain_mm},
#'   \code{daylight_hr}, \code{radiation}, \code{date}, and \code{zone}.
#' @examples
#' \dontrun{
#' df <- process_weather_month("path/to/2024-01.csv",
#'   month = 1, year = 2024,
#'   zone = "site_A"
#' )
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across where
#' @export
process_weather_month <- function(file_path, month, year = 2024, zone) {
  df <- readr::read_csv(file_path, show_col_types = FALSE)

  regex_patterns <- c(
    day              = "^觀測時間", # 觀測時間
    pressure_hpa     = "^測站氣壓", # 測站氣壓
    temp             = "^氣溫", # 氣溫
    humidity_percent = "^相對濕度", # 相對溼度
    wind_ms          = "^風速", # 風速
    rain_mm          = "^降水量", # 降水量
    daylight_hr      = "^日照時數", # 日照時數
    radiation        = "^全天空日射量" # 全天空日射量
  )

  selected_cols <- sapply(regex_patterns, function(pattern) {
    match_cols <- grep(pattern, colnames(df), ignore.case = TRUE)
    if (length(match_cols) > 0) match_cols[1] else NULL
  })
  selected_cols <- unlist(selected_cols)

  df <- df[-1, selected_cols]
  colnames(df) <- names(selected_cols)

  df <- dplyr::mutate(df, dplyr::across(dplyr::where(is.character), as.numeric))
  df <- df[order(df$day), ]

  day_sequence <- seq(
    from       = as.Date(paste(year, month, 1, sep = "-"), format = "%Y-%m-%d"),
    by         = "1 day",
    length.out = nrow(df)
  )
  df$date <- as.Date(as.POSIXct(day_sequence, tz = "Asia/Taipei"))
  df$zone <- zone

  return(df)
}


#' @title combine_weather_month
#' @description Batch-import monthly weather CSV files from a Taiwan Central
#'   Weather Administration station for a consecutive range of months.
#' @details File names are expected to follow the pattern
#'   \code{<file_path><year>-0<month>.csv} (e.g. \code{2024-01.csv}).
#' @param file_path Path prefix (directory + filename prefix before the date
#'   portion, e.g. \code{"data/weather/"}).
#' @param start_month First month to import (1–9; two-digit months not yet
#'   supported).
#' @param end_month Last month to import.
#' @param year Four-digit year. Default 2024.
#' @param zone Character label for the weather station / region.
#' @return A combined data frame produced by \code{\link{process_weather_month}}.
#' @examples
#' \dontrun{
#' df <- combine_weather_month("data/weather/",
#'   start_month = 1,
#'   end_month = 6, year = 2024, zone = "site_A"
#' )
#' }
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @export
combine_weather_month <- function(file_path, start_month, end_month,
                                  year = 2024, zone) {
  months <- as.character(seq(as.numeric(start_month), as.numeric(end_month)))

  purrr::map_dfr(months, function(month) {
    file_name <- paste0(file_path, year, "-0", month, ".csv")
    process_weather_month(file_name, month, year = year, zone = zone)
  })
}


#' @title process_weather_mh
#' @description Import and tidy an hourly weather report in the CWA "MH"
#'   (Multifield Hourly) text format, as bulk-downloaded from the CODiS portal
#'   (\code{LotsDataReports.txt}). The file is fixed-/space-delimited with a
#'   \code{'#'} column-title row listing element codes (e.g. \code{PS01},
#'   \code{WD01}) and \code{'*'} comment rows. One file may contain multiple
#'   stations and a full month of hourly data.
#' @details Element codes are renamed as: \code{PS01} -> \code{pressure_hpa},
#'   \code{WD01} -> \code{wind_ms}, \code{TX01} -> \code{air_temp},
#'   \code{RH01} -> \code{humidity}, \code{WD02} -> \code{wind_dir},
#'   \code{PP01} -> \code{rain_mm}. Only the codes present in the file are
#'   returned. CWA missing-value codes (large negatives, e.g. -9991) are set to
#'   \code{NA}. The timestamp \code{yyyymmddhh} uses hours 01-24, where hour 24
#'   denotes 00:00 of the following day.
#' @param file_path Path to the MH-format \code{.txt} file.
#' @param station_id Optional character vector of station codes (e.g.
#'   \code{"C0R660"}) to keep. Default \code{NULL} keeps all stations.
#' @param expand_30min If \code{TRUE} (default), each hourly value is expanded to
#'   the two 30-minute marks (H-1):30 and H:00, matching
#'   \code{\link{process_weather}} so it merges with 30-minute HOBO data. If
#'   \code{FALSE}, hourly rows are returned unchanged.
#' @param tz Time zone of the observations. Default \code{"Asia/Taipei"}.
#' @return A data frame with columns \code{stno}, \code{date_time}, and the
#'   renamed weather elements present in the file.
#' @examples
#' \dontrun{
#' df <- process_weather_mh("LotsDataReports.txt", station_id = "C0R660")
#' }
#' @importFrom lubridate hours minutes force_tz
#' @importFrom utils read.table
#' @export

process_weather_mh <- function(file_path, station_id = NULL,
                               expand_30min = TRUE, tz = "Asia/Taipei") {
  lines <- readLines(file_path, warn = FALSE)

  # The column-title row begins with '#'; element codes are space-separated.
  header_line <- lines[grepl("^#", lines)][1]
  if (is.na(header_line)) stop("No '#' header row found; not an MH-format file.")
  col_names <- strsplit(trimws(sub("^#", "", header_line)), "\\s+")[[1]]

  # Data rows: anything not starting with '*' or '#', and non-blank.
  data_lines <- lines[!grepl("^[*#]", lines) & trimws(lines) != ""]
  df <- utils::read.table(text = data_lines, col.names = col_names,
                          colClasses = "character", stringsAsFactors = FALSE,
                          fill = TRUE)

  if (!is.null(station_id)) {
    df <- df[df$stno %in% station_id, , drop = FALSE]
    if (nrow(df) == 0) {
      stop("No rows for station_id = ", paste(station_id, collapse = ", "))
    }
  }

  # MH element code -> friendly name (only those we use downstream are renamed).
  code_map <- c(PS01 = "pressure_hpa", WD01 = "wind_ms", TX01 = "air_temp",
                RH01 = "humidity", WD02 = "wind_dir", PP01 = "rain_mm")

  # Coerce numerics; CWA missing-value codes are large negatives (-999x).
  to_num <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    v[!is.na(v) & v <= -90] <- NA
    v
  }

  # Parse yyyymmddhh; hour runs 01-24 where 24 == 00:00 of the next day.
  ts <- df$yyyymmddhh
  base_date <- as.POSIXct(
    sprintf("%s-%s-%s", substr(ts, 1, 4), substr(ts, 5, 6), substr(ts, 7, 8)),
    tz = tz
  )
  hh <- as.integer(substr(ts, 9, 10))
  date_time <- base_date + lubridate::hours(hh)

  out <- data.frame(stno = df$stno, date_time = date_time,
                    stringsAsFactors = FALSE)
  for (code in intersect(names(code_map), col_names)) {
    out[[code_map[[code]]]] <- to_num(df[[code]])
  }

  # Expand each hour H to the two 30-min marks (H-1):30 and H:00, matching
  # process_weather()'s convention so it merges with 30-minute HOBO data.
  if (expand_30min) {
    half <- out
    half$date_time <- half$date_time - lubridate::minutes(30)
    out <- rbind(half, out)
  }

  out <- out[order(out$stno, out$date_time), , drop = FALSE]
  out$date_time <- lubridate::force_tz(out$date_time, tzone = tz)
  rownames(out) <- NULL
  out
}


#' @title combine_weather_mh
#' @description Batch-import and combine multiple CWA "MH" (Multifield Hourly)
#'   weather reports. Hourly CODiS downloads are capped at a one-month range, so
#'   a date span typically yields several \code{LotsDataReports.txt} files; this
#'   reads them all and row-binds the result.
#' @param file_paths Either a character vector of MH file paths, or a single
#'   directory path (all \code{.txt} files within are read, recursively).
#' @param station_id Optional character vector of station codes to keep. Passed
#'   to \code{\link{process_weather_mh}}.
#' @param expand_30min Passed to \code{\link{process_weather_mh}}. Default
#'   \code{TRUE}.
#' @param tz Time zone. Default \code{"Asia/Taipei"}.
#' @return A combined data frame, de-duplicated and ordered by station and time.
#' @examples
#' \dontrun{
#' df <- combine_weather_mh("data/raw/sw_mono_do/weather_mh",
#'   station_id = c("C0R660", "C0R850")
#' )
#' }
#' @importFrom purrr map_dfr
#' @export

combine_weather_mh <- function(file_paths, station_id = NULL,
                               expand_30min = TRUE, tz = "Asia/Taipei") {
  if (length(file_paths) == 1 && dir.exists(file_paths)) {
    file_paths <- list.files(file_paths, pattern = "\\.txt$",
                             full.names = TRUE, recursive = TRUE)
  }
  if (length(file_paths) == 0) stop("No MH .txt files found.")

  out <- purrr::map_dfr(file_paths, function(f) {
    tryCatch(
      process_weather_mh(f, station_id = station_id,
                         expand_30min = expand_30min, tz = tz),
      error = function(e) {
        warning("Skipping ", basename(f), ": ", conditionMessage(e))
        NULL
      }
    )
  })

  out <- out[!duplicated(out[c("stno", "date_time")]), , drop = FALSE]
  out <- out[order(out$stno, out$date_time), , drop = FALSE]
  rownames(out) <- NULL
  out
}


#' @title filter_complete_days
#' @description Keep only the calendar days that have (near-)complete diel
#'   coverage, per logger. A day qualifies when the number of distinct
#'   time-of-day slots present reaches \code{min_slots}. This replaces the manual
#'   inspection of which deployment days run a full 00:00-24:00, so daily DO
#'   metabolism is computed only from complete days.
#' @param df A data frame of HOBO readings, e.g. from \code{\link{process_hobo}}
#'   or \code{\link{combine_hobo}}, containing a POSIXct time column.
#' @param group_cols Character vector of grouping columns identifying one logger
#'   deployment, default \code{"no_hobo"}.
#' @param time_col Name of the POSIXct time column. Default \code{"date_time"}.
#' @param min_slots Minimum number of distinct 30-minute slots a day must have to
#'   count as complete. Default \code{44} (of 48; allows minor gaps).
#' @param tz Time zone used to derive the calendar date. Default
#'   \code{"Asia/Taipei"}.
#' @param summary_only If \code{TRUE}, return a per-day summary
#'   (\code{group_cols}, \code{date}, \code{n_slots}, \code{complete}) instead of
#'   the filtered readings. Useful for inspecting coverage. Default \code{FALSE}.
#' @return Either the input rows restricted to complete days (default), or a
#'   coverage summary when \code{summary_only = TRUE}.
#' @examples
#' \dontrun{
#' hobo <- combine_hobo("data/raw/sw_mono_do/csv/FL_T1")
#' filter_complete_days(hobo, summary_only = TRUE)   # inspect coverage
#' clean <- filter_complete_days(hobo)               # keep complete days
#' }
#' @importFrom dplyr mutate summarise filter semi_join n_distinct all_of
#' @export

filter_complete_days <- function(df, group_cols = "no_hobo",
                                 time_col = "date_time", min_slots = 44L,
                                 tz = "Asia/Taipei", summary_only = FALSE) {
  if (!time_col %in% names(df)) stop("Column '", time_col, "' not found.")
  n_slots <- complete <- NULL

  d <- dplyr::mutate(
    df,
    date = as.Date(.data[[time_col]], tz = tz),
    slot_id = format(.data[[time_col]], "%H:%M")
  )

  summ <- dplyr::summarise(
    d,
    n_slots = dplyr::n_distinct(slot_id),
    .by = dplyr::all_of(c(group_cols, "date"))
  )
  summ <- dplyr::mutate(summ, complete = n_slots >= min_slots)

  if (summary_only) return(summ)

  keep <- dplyr::filter(summ, complete)
  out <- dplyr::semi_join(d, keep, by = c(group_cols, "date"))
  out$slot_id <- NULL
  out
}


#' @title add_sun_times
#' @description Attach per-day sunrise and sunset times to a data frame of
#'   readings, computed from a site's coordinates with the \pkg{suncalc}
#'   package. Replaces the manual lookup of sunrise/sunset for each deployment
#'   date; the resulting \code{sunrise}/\code{sunset} columns are consumed by
#'   \code{\link{calculate_do}}.
#' @param df A data frame containing a POSIXct time column.
#' @param lat,lon Site latitude and longitude in decimal degrees (length 1).
#' @param time_col Name of the POSIXct time column. Default \code{"date_time"}.
#' @param tz Time zone for the returned sun times. Default \code{"Asia/Taipei"}.
#' @return \code{df} with two added POSIXct columns, \code{sunrise} and
#'   \code{sunset}, matched to each row's calendar date.
#' @examples
#' \dontrun{
#' df <- add_sun_times(df, lat = 22.3900, lon = 120.5777)
#' }
#' @export

add_sun_times <- function(df, lat, lon, time_col = "date_time",
                          tz = "Asia/Taipei") {
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("Package 'suncalc' is required; install it with ",
         "install.packages('suncalc').")
  }
  if (!time_col %in% names(df)) stop("Column '", time_col, "' not found.")

  dates <- as.Date(df[[time_col]], tz = tz)
  st <- suncalc::getSunlightTimes(
    date = sort(unique(dates)), lat = lat, lon = lon,
    keep = c("sunrise", "sunset"), tz = tz
  )
  idx <- match(dates, st$date)
  df$sunrise <- st$sunrise[idx]
  df$sunset <- st$sunset[idx]
  df
}
