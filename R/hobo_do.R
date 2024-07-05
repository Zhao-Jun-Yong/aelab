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
  df <- utils::read.csv(file_path, header = F)
  df <- df[-c(1:2), -c(5:9)]
  colnames(df) <- c("no", "date_time", "do", "temp")
  df <- as.data.frame(df)
  df <-  stats::na.omit(df)

  if (any(grepl("\u4e0a\u5348|\u4e0b\u5348", df$date_time))) {

    df$new_variable <- ifelse(grepl("\u4e0a\u5348", df$date_time),
                              "morning", "afternoon")
    df$date_time <- gsub("\u4e0a\u5348|\u4e0b\u5348", "", df$date_time)
    df$date_time <- gsub("\u6642", ":", df$date_time)
    df$date_time <- gsub("\u5206", ":", df$date_time)
    df$date_time <- gsub("\u79d2", "", df$date_time)
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

  tidy_df <- stats::aggregate(cbind(do, temp) ~
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
#' @return A dataframe.
#'@examples
#' weather_data_path <- system.file("extdata", "ex_weather.csv", package = "aelab")
#' df <- process_weather(weather_data_path, "2024-01-01", "site_A")
#' @export

process_weather <- function(file_path, date, zone) {
  #this function process csv file downloaded with specific format

  df <- readr::read_csv(file_path)
  hours <- grep("\u89c0\u6e2c\u6642\u9593", names(df), ignore.case = F)
  pressure_hpa <- grep("\u6e2c\u7ad9\u6c23", names(df), ignore.case = F)
  wind_speed <- grep("\u98a8\u901f", names(df), ignore.case = F)
  df <- rbind(df, df)
  df <- df[-c(1, 26), c(hours, pressure_hpa, wind_speed)]
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
#' @return A dataframe.
#' @examples
#' info_data_path <- system.file("extdata", "info.xlsx", package = "aelab")
#' df <- process_info(info_data_path)
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

