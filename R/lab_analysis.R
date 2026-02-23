#' @title calc_chla_trichromatic
#' @description Calculate chlorophyll-a concentration from trichromatic
#'   spectrophotometric absorbance readings using the Jeffrey & Humphrey (1975)
#'   equations.
#' @details Absorbance values should be measured in a 1 cm path-length cuvette.
#'   The 750 nm reading is used as a turbidity blank correction.
#'   Formula: \eqn{11.85 \times E_{664} - 1.54 \times E_{647} - 0.08 \times E_{630}}
#'   where \eqn{E_{\lambda} = A_{\lambda} - A_{750}}.
#' @param wl_630 Absorbance at 630 nm.
#' @param wl_647 Absorbance at 647 nm.
#' @param wl_664 Absorbance at 664 nm.
#' @param wl_750 Absorbance at 750 nm (turbidity blank).
#' @return Chlorophyll-a concentration in µg L\eqn{^{-1}} (assuming a 1 cm path
#'   length and standard extraction volume).
#' @examples
#' calc_chla_trichromatic(wl_630 = 0.05, wl_647 = 0.08, wl_664 = 0.20, wl_750 = 0.01)
#' @export
calc_chla_trichromatic <- function(wl_630, wl_647, wl_664, wl_750) {
  e_630 <- wl_630 - wl_750
  e_647 <- wl_647 - wl_750
  e_664 <- wl_664 - wl_750
  (11.85 * e_664) - (1.54 * e_647) - (0.08 * e_630)
}
