#' @title process_hobo
#' @importFrom lubridate ceiling_date
#' @description Tidy the data retrieved from HOBO U26 Dissolved Oxygen Data Logger.
#' @param file_path Directory of file.
#' @param no_hobo The code for the data logger.
#'@examples
#'\dontrun{
#'df <- process_hobo("data/hobo.csv", "no.1")
#'}
#' @export

process_hobo <- function(file_path, no_hobo) {
  df <- read.csv(file_path, header = F)
  df <- df[-c(1:2), -c(5:9)]
  colnames(df) <- c("no","date_time", "do", "temp")
  df <- as.data.frame(df)
  df <- na.omit(df)

  if (any(grepl("上午|下午", df$date_time))) {

    df$new_variable <- ifelse(grepl("上午", df$date_time),
                              "morning", "afternoon")
    df$date_time <- gsub("上午|下午", "", df$date_time)
    df$date_time <- gsub("(\\d+)時(\\d+)分(\\d+)秒", "\\1:\\2:\\3",
                         df$date_time)
    df$date_time <- strptime(df$date_time, format = "%m/%d/%Y %H:%M:%S")

    subset_afternoon <- df[df$new_variable == "afternoon" &
                             format(df$date_time, "%H:%M:%S") >= "01:00:00" &
                             format(df$date_time, "%H:%M:%S") <= "11:59:59", ]
    subset_morning <- df[df$new_variable == "morning" &
                           format(df$date_time, "%H:%M:%S") >= "12:00:00" &
                           format(df$date_time, "%H:%M:%S") <= "12:59:59", ]

    subset_afternoon$date_time <- subset_afternoon$date_time + hours(12)
    subset_morning$date_time <- subset_morning$date_time + hours(12) - days(1)

    df[df$new_variable == "afternoon" &
         format(df$date_time, "%H:%M:%S") >= "01:00:00" &
         format(df$date_time, "%H:%M:%S") <= "11:59:59", ] <- subset_afternoon
    df[df$new_variable == "morning" &
         format(df$date_time, "%H:%M:%S") >= "12:00:00" &
         format(df$date_time, "%H:%M:%S") <= "12:59:59", ] <- subset_morning

    remove(subset_afternoon)
    remove(subset_morning)
    df$new_variable <- NULL

  } else {

  }

  df$do <- as.numeric(df$do)
  df$temp <- as.numeric(df$temp)

  df$time <- format(df$date_time, "%H:%M:%S")

  df$date_time <- as.factor(lubridate::ceiling_date(
    df$date_time, unit = "30 minutes"))

  tidy_df <- aggregate(cbind(do, temp) ~
                         date_time, df,
                       function(x) mean(x, na.rm = TRUE))
  tidy_df <- arrange(tidy_df, date_time)
  tidy_df$date_time <- as.POSIXct(tidy_df$date_time)
  tidy_df$date_time <- force_tz(tidy_df$date_time,
                                tzone = "Asia/Taipei")
  tidy_df$no_hobo <- {{no_hobo}}

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
#'@examples
#'\dontrun{
#'df <- process_weather("weather/day_one.csv", "2024-01-01", "site_A")
#'}
#' @export

process_weather <- function(file_path, date, zone) {
  #this function process csv file downloaded with specific format

  hour <- grep("觀測時間", names(df), ignore.case = TRUE)
  air_temp <- grep("氣溫", names(df), ignore.case = TRUE)
  wind_speed <- grep("風速", names(df), ignore.case = TRUE)

  df <- readr::read_csv(file_path)
  df <- rbind(df, df)
  df <- df[-c(1, 26), c(hour, air_temp, wind_speed)]
  colnames(df) <- c("hours", "pressure_hpa", "wind_ms")
  df$hours <- as.numeric(df$hours)
  df$pressure_hpa <- as.numeric(df$pressure_hpa)
  df$wind_ms <- as.numeric(df$wind_ms)
  df <- df[order(df$hours), ]
  df <- tidyr::fill(df, pressure_hpa, wind_ms, .direction = "downup")

  values <- 1:24
  results <- all(sapply(values, function(x) sum(df$hours == x) == 2))

  if (!results) {
    warning("Some hours are missing duplicates in the dataset.")
    return(NULL)
  } else {
    time_sequence <- format(
      seq(
        from = as.POSIXct("00:30:00", format = "%H:%M:%S", tz = "Asia/Taipei"),
        by = "30 min",
        length.out = nrow(df)
      ),
      format = "%H:%M:%S"
    )

    df$date <- as.POSIXlt(date, format = "%Y-%m-%d", tz = "Asia/Taipei")
    df$time <- time_sequence
    df$date_time <- as.POSIXlt(paste(date, time_sequence), format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Taipei")
    df$date_time <- as.POSIXct(df$date_time)
    df$date_time <- force_tz(df$date_time, tzone = "Asia/Taipei")
    new_date <- lubridate::as_datetime(df$date_time[48])
    df$date_time[48] <- new_date + lubridate::days(1)
    df$date <- NULL
    df$time <- NULL
    df$hours <- NULL
    df$zone <- {{zone}}

    as.data.frame(df)
    return(df)
  }
}

#' @title process_info
#' @importFrom readxl read_excel
#' @description Import and process the necessary information,
#' including the sunrise and sunset times of the day,
#' the date and time range of the deployment,
#' and the code for the data logger.
#' @param file_path Directory of file.
#'@examples
#'\dontrun{
#'df <- process_info("info.xlsx")
#'}
#' @export

process_info <- function(file_path) {

  info <- readxl::read_excel(file_path)
  info$sunrise <- force_tz(info$sunrise, tzone = "Asia/Taipei")
  info$sunset <- force_tz(info$sunset, tzone = "Asia/Taipei")
  info$start_date_time <- force_tz(info$start_date_time, tzone = "Asia/Taipei")
  info$end_date_time <- force_tz(info$end_date_time, tzone = "Asia/Taipei")
  info <- info %>%
    mutate(no_hobo = as.character(no_hobo))

  return(info)
}

#' @title combine_weather
#' @importFrom dplyr bind_rows
#' @description Apply the process_weather function to multiple file at once.
#' @param file_path Directory of file, also need to include the character in the file name before the date.
#' @param start_date Start date of the file of daily weather data in yyyy-mm-dd format.
#' @param end_date End date of the file of daily weather data in yyyy-mm-dd format.
#' @param zone Code for the region of the weather station.
#'@examples
#'\dontrun{
#'df <- combine_weather(file_path = "weather/1B3D5F-", start_date = "2024-01-01",
#'end_date = "2024-01-07", zone = "site_A")
#'}
#' @export

combine_weather <- function(file_path, start_date, end_date, zone) {
dates <- as.character(seq(as.Date(start_date), as.Date(end_date), by = "day"))
df <- list()

for (date in dates) {
  file_name <- paste0(file_path, date, ".csv")
  df[[date]] <- process_weather(file_name, date, zone)
}

weather <- dplyr::bind_rows(df)
return(weather)
}

#' @title combine_hobo
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @description Apply the process_hobo function to multiple file at once.
#' @param file_path Directory of file.
#' @param file_prefix Specify a prefix that the file names must start with in the file directory.
#'@examples
#'\dontrun{
#'df <- combine_hobo(file_path = "data/", file_prefix = "hobo_")
#'}
#' @export

combine_hobo <- function(file_path, file_prefix = "no.") {

  file_names <- list.files(file_path, pattern = paste0("^", file_prefix))

  df <- data.frame()

  for (file_name in file_names) {
    no_hobo <- stringr::str_replace(file_name, paste0("^", file_prefix), "")
    no_hobo <- stringr::str_remove(no_hobo, ".csv")
    file <- file.path(file_path, file_name)

    do <- process_hobo(file_path = file, no_hobo = no_hobo)

    df <- rbind(df, do)
  }

  return(df)
}

#' @title plot_hobo
#' @import ggplot2
#' @description Plot the dissolved oxygen concentration over time series grouped by different data loggers to observe the variations.
#' @param df Dataframe produced by process_hobo() function.
#'@examples
#'\dontrun{
#'plot_hobo(df)
#'}
#' @export

plot_hobo <- function(df) {
  ggplot2::ggplot(df, aes(x = date_time, y = do)) +
    geom_point(size = 1) +
    facet_grid(no_hobo ~ ., scales = "free")
}

#' @title calculate_do
#' @import dplyr
#' @description Calculate the Net Ecosystem Production,
#' Gross Primary Production and Ecosystem respiration based on the change in dissolved oxygen concentration.
#' @param df Merged dataframe produced by process_hobo(), process_weather() and process_info() functions.
#'@examples
#'\dontrun{
#'calculate_do(df)
#'}
#' @export

calculate_do <- function(df) {

  df <- df %>%
    dplyr::mutate(
      daylight_hr = as.numeric(sunset - sunrise),
      time =  format(date_time, "%H:%M:%S"),
      date = as.Date(date_time, tz = "Asia/Taipei"),
      sunset_time = format(sunset, "%H:%M:%S"),
      sunrise_time = format(sunrise, "%H:%M:%S"),
      temp_k = temp + 273.15,
      c_o2 = -173.4292 + 249.6336 * (100/temp_k) + 143.3483 * log(temp_k/100) - 21.8492 * (temp_k/100) +
        salinity * (-0.033096 + 0.014259 * (temp_k/100) - 0.0017 * (temp_k/100)^2),
      o2_saturation = exp(c_o2) * 1.423,
      cor_o2_saturation_pressure = o2_saturation * (pressure_hpa * 0.0987 - 0.0112) / 100,
      rate_do_change = (c(NA, diff(do)))*2,
      sc = 0.0476 * temp^3 + 3.7818 * temp^2 - 120.1 * temp + 1800.6,
      k600 = (2.07 + 0.215 * (wind_ms^1.7)) / 100,
      k = k600 * (sc / 600)^(-0.5),
      flux = (do - cor_o2_saturation_pressure) * k,
      nep_hr = rate_do_change * depth_m - flux
    )

  tidy_do_r <- df %>%
    dplyr::filter(time < sunrise_time | time >= sunset_time) %>%
    dplyr::group_by(site, no_hobo, date) %>%
    dplyr::summarise(r_hr = mean(nep_hr, na.rm = TRUE)) %>%
    dplyr::ungroup()

  tidy_do_nep <- df %>%
    dplyr::filter(time >= sunrise_time, time < sunset_time) %>%
    dplyr::group_by(site, no_hobo, date) %>%
    dplyr::summarise(nep_hr = mean(nep_hr, na.rm = TRUE),
                     daylight_hr = mean(daylight_hr, na.rm = TRUE)) %>%
    dplyr::mutate(nep_daytime = nep_hr * daylight_hr) %>%
    dplyr::ungroup()

  tidy_do <- merge(tidy_do_r, tidy_do_nep, by = c("site", "no_hobo", "date"))

  tidy_do <- tidy_do %>%
    dplyr::mutate(r_daytime = r_hr * daylight_hr,
                  r_day = r_hr * 24,
                  gpp = nep_daytime - r_daytime,
                  nep = gpp + r_day
    ) %>%
    dplyr::select(site, no_hobo, date, r_day, gpp, nep)

  return(tidy_do)
}

