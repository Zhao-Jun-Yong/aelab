utils::globalVariables(c("CO2", "CH4", "N2O", "conc", "analyzer_precision"))

# Readings of a LI-COR .data file (LI-7810, LI-7820): the line starting DATAH names the columns, the
# line starting DATAU gives their units, the DATA lines are the readings. Returns the table the Excel
# export gives (DATE as "%Y/%m/%d", TIME as "%H:%M:%S", date_time, H2O, the gas columns, rows with
# no gas reading dropped) and the unit of each column.
.read_licor_data <- function(file_path, gas) {
  lines <- readLines(file_path, warn = FALSE)
  h <- grep("^DATAH\t", lines)[1]
  u <- grep("^DATAU\t", lines)[1]
  if (is.na(h) || is.na(u)) stop("No DATAH/DATAU header lines in ", basename(file_path), ".")
  nm <- strsplit(lines[h], "\t", fixed = TRUE)[[1]]
  keep <- c("DATE", "TIME", "H2O", if (gas == "ch4") c("CO2", "CH4") else "N2O")
  if (!all(keep %in% nm))
    stop("Header of ", basename(file_path), " has no ", paste(setdiff(keep, nm), collapse = ", "), " column.")
  rows <- lines[startsWith(lines, "DATA\t")]
  # a line cut short (or with an extra tab in REMARK) has another number of fields than the header
  full <- lengths(regmatches(rows, gregexpr("\t", rows, fixed = TRUE))) == length(nm) - 1L
  if (any(!full))
    warning(sum(!full), " DATA line(s) of ", basename(file_path), " do not have the ", length(nm),
            " fields of the header and are dropped.", call. = FALSE)
  rows <- rows[full]
  if (!length(rows)) stop(basename(file_path), " has no DATA lines.")
  d <- utils::read.delim(text = rows, header = FALSE, quote = "", col.names = nm,
                         colClasses = "character", fill = TRUE)
  data <- d[keep]
  for (g in setdiff(keep, c("DATE", "TIME"))) data[[g]] <- as.numeric(data[[g]])
  data <- data[stats::complete.cases(data[setdiff(keep, c("DATE", "TIME", "H2O"))]), ]
  rownames(data) <- NULL
  data$DATE <- gsub("-", "/", data$DATE, fixed = TRUE)
  data$date_time <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y/%m/%d %H:%M:%S")
  un <- strsplit(lines[u], "\t", fixed = TRUE)[[1]]
  un <- stats::setNames(un, nm)
  list(data = data, units = un[intersect(c("CO2", "CH4", "N2O", "H2O"), keep)])  # order of the Excel path
}

#' @title tidy_ghg_analyzer
#' @importFrom lubridate ymd_hms days hours minutes seconds force_tz
#' @importFrom readxl read_excel
#' @importFrom openxlsx convertToDateTime
#' @importFrom dplyr filter
#' @description Tidy the data downloaded from GHG Analyzer, with optional timestamp correction.
#' @param file_path Path of the file: the Excel export of the analyzer, or, for \code{analyzer = "licor"},
#'   the LI-COR \code{.data} text file itself (chosen by the \code{.data} extension). Both give the
#'   same table.
#' @param gas Choose between CO2/CH4 or N2O LI-COR Trace Gas Analyzer, which is "ch4" and "n2o", respectively.
#' @param analyzer The brand of the analyzer which the data was downloaded from.
#' @param day Day offset to correct the analyzer timestamp.
#' @param hr Hour offset to correct the analyzer timestamp.
#' @param min Minute offset to correct the analyzer timestamp.
#' @param sec Second offset to correct the analyzer timestamp.
#' @return Return the loaded XLSX file after tidying (for LI-COR also \code{H2O}, ppm), with one
#'   \code{<gas>_unit} column per gas (\code{CO2_unit}, \code{CH4_unit}, \code{N2O_unit},
#'   \code{H2O_unit}: \code{"ppm"} or \code{"ppb"}), which
#'   \code{calculate_regression()} turns into \code{slope_unit}, and for LI-COR data one
#'   \code{<gas>_prec} column per gas (1 s precision from \code{\link{analyzer_precision}}, same
#'   unit), used by \code{fit_type = "auto"} and \code{window_type = "pareto"}. If any time offset is non-zero, a
#'   \code{real_datetime} column is added via \code{\link{convert_time}}.
#' @examples
#' ghg_data_path <- system.file("extdata", "ch4.xlsx", package = "aelab")
#' tidy_ghg_analyzer(ghg_data_path, "ch4")
#' tidy_ghg_analyzer(ghg_data_path, "ch4", sec = -30)
#' @export

tidy_ghg_analyzer <- function(file_path, gas, analyzer = "licor",
                               day = 0, hr = 0, min = 0, sec = 0) {

  supported <- list(licor = c("ch4", "n2o"), lgr = "ch4")
  if (!isTRUE(analyzer %in% names(supported)) || !isTRUE(gas %in% supported[[analyzer]]))
    stop("Unsupported analyzer/gas: \"", analyzer, "\"/\"", gas, "\". Use licor with \"ch4\" or \"n2o\", ",
         "or lgr with \"ch4\".")

  # LI-COR text file (.data) is read directly; anything else is the Excel export
  is_raw <- analyzer == "licor" && grepl("\\.data$", file_path, ignore.case = TRUE)
  if (is_raw) {
    raw <- .read_licor_data(file_path, gas)
    data <- raw$data
  } else {
    data <- readxl::read_excel(file_path, .name_repair = "minimal")
  }

  # Tidy data based on the type of analyzer
  if (is_raw) {
    NULL  # already tidy
  } else if(analyzer == "licor") {
    # the header is row 5; columns are picked by their title
    keep <- c("DATE", "TIME", "H2O", if (gas == "ch4") c("CO2", "CH4") else "N2O")
    header <- as.character(unlist(data[5, ]))
    if (!all(keep %in% header))
      stop("Header row 5 of the file has no ", paste(setdiff(keep, header), collapse = ", "), " column.")
    cols <- match(keep, header)
    if (gas == "ch4") {
      title <- data[5, cols]
      data <- data[-c(1:6), cols]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$CH4 <- as.numeric(data$CH4)  # Convert CH4 to numeric
      data$CO2 <- as.numeric(data$CO2)  # Convert CO2 to numeric
      data$H2O <- as.numeric(data$H2O)
      # Filter out rows with NaN values in CH4 and CO2
      data <- dplyr::filter(data, !is.na(CH4), !is.na(CO2))
    } else if (gas == "n2o") {
      title <- data[5, cols]
      data <- data[-c(1:6), cols]
      colnames(data) <- title  # Set new column names
      data <- as.data.frame(data)  # Convert to data frame
      data$N2O <- as.numeric(data$N2O)  # Convert N2O to numeric
      data$H2O <- as.numeric(data$H2O)
      # Filter out rows with NaN values in N2O
      data <- dplyr::filter(data, !is.na(N2O))
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

  # concentration unit per gas, carried by calculate_regression() into slope_unit
  units <- if (is_raw) raw$units else if (analyzer == "licor") c(CO2 = "ppm", CH4 = "ppb", N2O = "ppb", H2O = "ppm") else c(CO2 = "ppm", CH4 = "ppm")
  for (g in intersect(names(units), names(data))) data[[paste0(g, "_unit")]] <- units[[g]]
  # 1 s precision per gas, as goFlux's import functions add <gas>_prec (LI-COR only for now)
  if (analyzer == "licor") for (g in intersect(analyzer_precision$gas, names(data)))
    data[[paste0(g, "_prec")]] <- analyzer_precision$precision[analyzer_precision$gas == g]

  # Apply timestamp correction if any offset is provided
  if (day != 0 || hr != 0 || min != 0 || sec != 0) {
    data <- convert_time(data, day = day, hr = hr, min = min, sec = sec)
  }

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

#' @title make_reference
#' @description Build a \code{reference_df} for \code{\link{calculate_regression}} from a
#'   compact inline string of field notes. Each line is one closure measurement with
#'   whitespace-separated columns (any number of spaces or tabs):
#'   \code{time  site  analyzer  [dur=N]  [off=N]}.
#'
#'   The first three columns are positional (required). Optional per-row window overrides
#'   use \code{key=value} syntax in any order: \code{dur=} sets \code{duration_minutes},
#'   \code{off=} sets \code{start_offset_s}. Rows without an override simply omit the token -
#'   no \code{NA} placeholder needed.
#' @param date Sampling date applied to all rows. A single \code{"YYYY-MM-DD"} string or
#'   \code{Date}. For multi-day campaigns, supply a character/Date vector with one value per row.
#' @param measurements A multiline character string, one row per closure:
#'   \code{time  site  analyzer  [dur=N]  [off=N]}.
#'   \code{time} accepts \code{"HH:MM"} or \code{"HH:MM:SS"}.
#' @param output Optional path for the output \code{.xlsx} file. If \code{NULL} (default),
#'   the tibble is returned.
#' @param tz Timezone for \code{date_time}. Default \code{"Asia/Taipei"}.
#' @return A tibble with columns \code{site}, \code{date_time}, \code{analyzer}, and
#'   optionally \code{duration_minutes} and/or \code{start_offset_s} (only present when at
#'   least one row specifies the override). When \code{output} is given, the tibble is written
#'   to Excel and returned invisibly.
#' @examples
#' make_reference(
#'   date = "2026-05-22",
#'   measurements = "
#'     10:30:15  JN1-1  1337_1074
#'     10:42:00  JN1-2  1337_1074
#'     10:54:30  JN2-1  1337_1074
#'   "
#' )
#'
#' # per-row window overrides - no NA placeholders needed
#' make_reference(
#'   date = "2026-05-22",
#'   measurements = "
#'     10:30:15  TT3-1  1450  dur=3.5
#'     10:42:00  TT3-2  1450  dur=2.0
#'     10:54:30  TT3-3  1450  off=45
#'     11:10:00  TT1-1  1450
#'   "
#' )
#' @importFrom lubridate ymd_hms
#' @importFrom tibble tibble
#' @importFrom openxlsx write.xlsx
#' @export
make_reference <- function(date,
                           measurements,
                           output = NULL,
                           tz     = "Asia/Taipei") {

  # --- parse measurements string ---
  lines <- unlist(strsplit(measurements, "\n"))
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0]
  if (length(lines) == 0) stop("'measurements' contains no data rows.")

  parsed <- lapply(lines, function(line) {
    parts <- strsplit(line, "\\s+")[[1]]
    parts <- parts[nchar(parts) > 0]
    if (length(parts) < 3)
      stop("Each line needs: time  site  analyzer\n  Got: '", line, "'")

    # first three tokens are positional
    row <- list(time = parts[1], site = parts[2], analyzer = parts[3],
                duration_minutes = NA_character_, start_offset_s = NA_character_)

    # remaining tokens: key=value (dur= / off=) or positional (4th=dur, 5th=off)
    if (length(parts) > 3) {
      rest <- parts[-(1:3)]
      for (tok in rest) {
        if (grepl("^dur=", tok, ignore.case = TRUE)) {
          row$duration_minutes <- sub("^[Dd][Uu][Rr]=", "", tok)
        } else if (grepl("^off=", tok, ignore.case = TRUE)) {
          row$start_offset_s <- sub("^[Oo][Ff][Ff]=", "", tok)
        } else if (is.na(row$duration_minutes)) {
          row$duration_minutes <- tok          # 4th positional token
        } else {
          row$start_offset_s <- tok            # 5th positional token
        }
      }
    }
    row
  })

  n       <- length(parsed)
  has_dur <- any(vapply(parsed, function(r) !is.na(r$duration_minutes), logical(1)))
  has_off <- any(vapply(parsed, function(r) !is.na(r$start_offset_s),   logical(1)))

  # --- date ---
  d_str <- format(as.Date(as.character(rep_len(date, n))), "%Y-%m-%d")

  # --- time ---
  t_str <- vapply(parsed, `[[`, character(1), "time")
  t_str <- sub("^(\\d):", "0\\1:", t_str)
  t_str <- ifelse(nchar(t_str) == 5, paste0(t_str, ":00"), t_str)

  dt <- lubridate::ymd_hms(paste(d_str, t_str), tz = tz)

  ref <- tibble::tibble(
    site      = vapply(parsed, `[[`, character(1), "site"),
    date_time = dt,
    analyzer  = vapply(parsed, `[[`, character(1), "analyzer")
  )

  if (has_dur) {
    raw <- vapply(parsed, function(r) if (is.na(r$duration_minutes)) NA_character_ else r$duration_minutes, character(1))
    ref$duration_minutes <- suppressWarnings(as.numeric(raw))
  }
  if (has_off) {
    raw <- vapply(parsed, function(r) if (is.na(r$start_offset_s)) NA_character_ else r$start_offset_s, character(1))
    ref$start_offset_s <- suppressWarnings(as.numeric(raw))
  }

  if (!is.null(output)) {
    openxlsx::write.xlsx(as.data.frame(ref), output)
    invisible(ref)
  } else {
    ref
  }
}
