# Abridged standard atomic weights, IUPAC (Prohaska et al. 2022, Pure Appl. Chem.
# 94(5): 573-600, Table 1, pp. 581-582). Molar masses and C/N element ratios are built from these.
.atomic_weight <- c(H = 1.008, C = 12.011, N = 14.007, O = 15.999)

.molar_mass <- function(ghg) {
  w <- .atomic_weight
  switch(ghg,
    "co2" = unname(w["C"] + 2 * w["O"]),
    "ch4" = unname(w["C"] + 4 * w["H"]),
    "n2o" = unname(2 * w["N"] + w["O"]),
    stop("Invalid ghg. One of: co2, ch4, n2o"))
}

# Mass of gas per mass of its C (co2, ch4) or N (n2o) content
.element_ratio <- function(ghg) {
  w <- .atomic_weight
  .molar_mass(ghg) / switch(ghg, "co2" = , "ch4" = w[["C"]], "n2o" = 2 * w[["N"]])
}

#' @title convert_ghg_unit
#' @description Convert a greenhouse gas (GHG) flux value (or a character string
#'   containing one or more numeric values, e.g. \code{"0.002 +/- 0.003"})
#'   between flux units. Defaults to micrograms per square meter per hour.
#' @details Numeric values embedded in a string (e.g. mean +/- SD notation)
#'   are each converted individually and the surrounding text is preserved.
#'   Commas are treated as decimal separators. A number written as
#'   \code{n = 3} (sample size) is left unchanged.
#'
#'   Molar masses (CO2 44.009, CH4 16.043, N2O 44.013 g mol\eqn{^{-1}}) are
#'   computed from the IUPAC abridged standard atomic weights (Prohaska et al. 2022,
#'   \emph{Pure and Applied Chemistry} 94: 573--600).
#' @param input A numeric vector or a character string containing one or more
#'   numbers. Numeric vectors are handled element-wise, making the function
#'   compatible with \code{dplyr::mutate()}.
#' @param ghg The molecular formula of the greenhouse gas: \code{"co2"},
#'   \code{"ch4"}, or \code{"n2o"}.
#' @param mass Mass unit of the input flux. One of \code{"mmol"}, \code{"mg"},
#'   \code{"g"}, \code{"ug"} (micrograms), \code{"nmol"}, \code{"Mg"},
#'   \code{"umol"} (micromoles), \code{"mol"}. \code{"ug"}/\code{"umol"} and the
#'   micro-sign spellings are interchangeable. Default
#'   \code{"ug"}.
#' @param area Area unit of the input flux. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param time Time unit of the input flux. One of \code{"yr"}, \code{"day"},
#'   \code{"hr"}, \code{"sec"}, \code{"min"}. Default \code{"hr"}.
#' @param to_mass Target mass unit. Same options as \code{mass}. Default
#'   \code{"ug"} (micrograms). Existing calls without this argument are
#'   unaffected.
#' @param to_area Target area unit. One of \code{"ha"}, \code{"m2"}.
#'   Default \code{"m2"}.
#' @param to_time Target time unit. Same options as \code{time}. Default
#'   \code{"hr"}.
#' @param digits Number of decimal places to round to. Default \code{NULL}
#'   (no rounding).
#' @param ratio Logical. If \code{TRUE}, the input is the mass of carbon
#'   (CO2, CH4) or nitrogen (N2O) only and is converted to the mass of the
#'   gas (factor molar mass of the gas / mass of its C or N atoms: CO2 3.664,
#'   CH4 1.336, N2O 1.571). Not allowed with a molar \code{mass} unit.
#'   Default \code{FALSE}.
#' @return For numeric input: a numeric vector of converted values.
#'   For character input: a character vector with numbers converted in-place
#'   (e.g. \code{"0.002 +/- 0.003"} becomes \code{"0.03 +/- 0.05"}).
#'   Returns \code{NA} for missing input.
#' @examples
#' convert_ghg_unit(97, ghg = "ch4", mass = "mg", area = "m2", time = "hr")
#' @export
convert_ghg_unit <- function(input, ghg, mass = "\u00b5g", area = "m2",
                             time = "hr", to_mass = "\u00b5g", to_area = "m2",
                             to_time = "hr", digits = NULL, ratio = FALSE) {
  valid_masses <- c("mmol", "mg", "g", "\u00b5g", "nmol", "Mg", "\u00b5mol", "mol")
  valid_areas  <- c("ha", "m2")
  valid_times  <- c("yr", "day", "hr", "sec", "min")

  molar_mass <- .molar_mass(ghg)
  micro <- function(x) sub("^[u\u03bc](g|mol)$", "\u00b5\\1", x)
  mass <- micro(mass)
  to_mass <- micro(to_mass)
  if (!(mass    %in% valid_masses)) stop(paste("Invalid mass. One of:",    paste(valid_masses, collapse = ", ")))
  if (!(area    %in% valid_areas))  stop(paste("Invalid area. One of:",    paste(valid_areas,  collapse = ", ")))
  if (!(time    %in% valid_times))  stop(paste("Invalid time. One of:",    paste(valid_times,  collapse = ", ")))
  if (!(to_mass %in% valid_masses)) stop(paste("Invalid to_mass. One of:", paste(valid_masses, collapse = ", ")))
  if (!(to_area %in% valid_areas))  stop(paste("Invalid to_area. One of:", paste(valid_areas,  collapse = ", ")))
  if (!(to_time %in% valid_times))  stop(paste("Invalid to_time. One of:", paste(valid_times,  collapse = ", ")))

  if (ratio && mass %in% c("mmol", "nmol", "\u00b5mol", "mol"))
    stop("ratio = TRUE needs a mass unit (not mol): 1 mol of C or N is 1 mol of gas.")

  # Vectorize \u2014 validation runs once here, not once per element
  if (length(input) > 1) {
    fn <- if (is.numeric(input)) numeric(1) else character(1)
    return(vapply(input, convert_ghg_unit, FUN.VALUE = fn,
                  ghg = ghg, mass = mass, area = area, time = time,
                  to_mass = to_mass, to_area = to_area, to_time = to_time,
                  digits = digits, ratio = ratio))
  }

  if (is.na(input)) return(if (is.character(input)) NA_character_ else NA_real_)

  convert_value <- function(value) {
    # Convert mass to \u00b5g
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
    if (ratio) value <- value * .element_ratio(ghg)
    # Step 2: \u00b5g m\u207b\u00b2 h\u207b\u00b9 \u2192 target units
    # Defaults (\u00b5g, m2, hr) are all identity \u2014 existing calls unchanged
    value <- switch(to_mass,
      "mmol"      = value / (molar_mass * 1000),
      "mg"        = value / 1000,
      "g"         = value / 1000000,
      "\u00b5g"        = value,
      "nmol"      = value * 1000 / molar_mass,
      "Mg"        = value / 1e12,
      "\u00b5mol"      = value / molar_mass,
      "mol"       = value / (molar_mass * 1000000)
    )
    if (to_area == "ha") value <- value * 10000
    value <- switch(to_time,
      "yr"  = value * 8760,
      "day" = value * 24,
      "sec" = value / 3600,
      "min" = value / 60,
      "hr"  = value
    )
    if (is.null(digits)) value else round(value, digits)
  }

  # Numeric scalar: convert directly, return numeric
  if (is.numeric(input)) return(convert_value(input))

  # Character input: convert numbers in-place, return modified string
  if (input == "") return(NA_character_)
  input <- gsub(",", ".", as.character(input))
  m <- gregexpr("[nN]\\s*=\\s*[0-9]+|[-+]?[0-9]*\\.?[0-9]+", input)
  tokens <- regmatches(input, m)[[1]]
  if (length(tokens) == 0) return(NA_character_)
  tokens <- vapply(tokens, function(tok) {
    if (grepl("^[nN]", tok)) return(tok)
    format(signif(convert_value(as.numeric(tok)), 15), scientific = FALSE,
           trim = TRUE)
  }, character(1), USE.NAMES = FALSE)
  regmatches(input, m) <- list(tokens)

  input
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

  if (!(ghg %in% c("co2", "ch4", "n2o")))
    stop("Invalid GHG type. Choose from 'co2', 'ch4', or 'n2o'.")

  part1    <- precision_ppm / (closure_time_s * sqrt(data_point_n))
  part2    <- (chamber_volume_m3 * pressure_pa) /
                (ideal_constant * (temperature_C + 273.15) * chamber_area_m2)
  MDF_umol <- part1 * part2 * 3600
  MDF      <- MDF_umol * .molar_mass(ghg)

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
#' @param verbose If \code{TRUE}, print the fluxes, the GWPs used and the total as messages.
#'   Default \code{FALSE}.
#' @return Total CO\eqn{_2}e flux as a numeric scalar
#'   (g m\eqn{^{-2}} d\eqn{^{-1}}).
#' @examples
#' calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003)
#' calculate_total_co2e(co2 = 4.02, ch4 = 0.001, n2o = 0.003, verbose = TRUE)
#' @export
calculate_total_co2e <- function(co2 = 0, ch4 = 0, n2o = 0, verbose = FALSE) {
  co2e_mg_hr <- co2 + (ch4 * 27) + (n2o * 273)
  co2e_g_day <- co2e_mg_hr * 0.001 * 24

  if (verbose) {
    message("Calculating total CO2e emissions:")
    message(sprintf("CO2 emissions: %.2f mg m^-2 h^-1 (GWP: 1)", co2))
    message(sprintf("CH4 emissions: %.2f mg m^-2 h^-1 (GWP: 27)", ch4))
    message(sprintf("N2O emissions: %.2f mg m^-2 h^-1 (GWP: 273)", n2o))
    message(sprintf("Total CO2e emissions: %.2f g m^-2 d^-1", co2e_g_day))
  }

  return(co2e_g_day)
}
