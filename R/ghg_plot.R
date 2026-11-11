utils::globalVariables("line")  # plot_ghg_flux() aes column

#' @title plot_ghg_flux
#' @description Plot GHG concentration over time for each measurement window, overlaid with the
#'   fitted line, colored by its quality flag (\code{flag}). Use this for visual QC of
#'   \code{\link{calculate_regression}} output. Each window is drawn with the model in its
#'   \code{fit_used} column. For CH4 closures split with \code{bubble_detection = TRUE}, the solid
#'   line is the linear diffusion fit over its segment (its colour is the diffusion fit's flag), the dashed white line is the two-point total
#'   (Minaudo et al. 2026, Eq. 1, p. 3), readings flagged as bubbles are coral triangles, and the
#'   panel title adds the total, diffusion and ebullition slopes and \code{ch4_flag}. When the result has an
#'   \code{rmse} column, the title also gives the RMSE (unit of the gas) and the number of readings
#'   of the fit, and, for closures without a bubble, the model in \code{fit_used}.
#' @param data Analyzer data processed by \code{tidy_ghg_analyzer()} and \code{convert_time()}.
#' @param regression_result Output tibble from \code{calculate_regression()}.
#' @param ghg Column name in \code{data} for the gas concentration (e.g. \code{"CH4"}, \code{"N2O"}).
#' @param fit_type Model used for windows without a \code{fit_used} value (results from older
#'   versions). One of \code{"linear"} (default), \code{"quadratic"}, \code{"exp_tz"},
#'   \code{"exp_zhao18"}, \code{"exp_hm"}, \code{"auto"} (drawn as linear).
#' @param t_zero Reference time in seconds for the \code{"exp_tz"} and \code{"exp_hm"} slope. Default 0.
#' @param flag_colors Named character vector mapping flag values to colors.
#' @param second_gas Optional column name in \code{data} (e.g. \code{"CO2"}) drawn in a panel
#'   beside each window, same time axis, readings only. A jump in both gases at once points to a
#'   chamber disturbance rather than a bubble (Minaudo et al. 2026, Guideline 5, p. 10).
#'   Default \code{NULL} (none).
#' @param base_size Base font size of the aelab minimal theme (as \code{\link{plot_point}}). Default 11.
#' @return A \code{ggplot} object with one panel per window (two with \code{second_gas}), or
#'   \code{NULL} with a warning when no window has readings. The y axis gives the unit from
#'   \code{slope_unit} or the \code{<gas>_unit} column of \code{tidy_ghg_analyzer()}.
#' @examples
#' data(n2o)
#' n2o_converted <- convert_time(n2o)
#' ref <- data.frame(
#'   date_time = n2o_converted$real_datetime[1],
#'   site = "S1", analyzer = "1074"
#' )
#' result <- calculate_regression(n2o_converted, ref, "N2O",
#'                                reference_time = "date_time", site = "site",
#'                                analyzer_code = "1074")
#' plot_ghg_flux(n2o_converted, result, "N2O")
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual facet_wrap labs
#'   theme_bw theme element_text
#' @importFrom dplyr mutate left_join
#' @export

plot_ghg_flux <- function(data, regression_result, ghg,
                          fit_type = "linear", t_zero = 0,
                          flag_colors = c(ok      = "#8ab29a",
                                          discard = "#f2cc8f",
                                          zero    = "#6096BA",
                                          no_data = "#aaaaaa"),
                          second_gas = NULL, base_size = 11) {

  fit_type <- match.arg(fit_type, c("linear", "quadratic", "exp_tz", "exp_zhao18", "exp_hm", "auto"))
  for (g in c(ghg, second_gas))
    if (!g %in% names(data)) stop("Column '", g, "' not found in data.")

  if (!"real_datetime" %in% colnames(data) && "date_time" %in% colnames(data)) {
    data$real_datetime <- data$date_time
  }
  # clock times as Taipei, as the reference times; else a non-Taipei system gives no_data
  data$real_datetime <- lubridate::force_tz(data$real_datetime, tzone = "Asia/Taipei")

  unit_of <- function(g) {
    u <- if (g == ghg && "slope_unit" %in% names(regression_result)) sub(" s-1$", "", regression_result$slope_unit)
    if (!length(stats::na.omit(u)) && paste0(g, "_unit") %in% names(data)) u <- data[[paste0(g, "_unit")]]
    u <- unique(stats::na.omit(u))
    if (length(u) == 1) paste0(g, " (", u, ")") else g
  }
  # POSIXct from calculate_regression(); text "%Y/%m/%d %H:%M:%S" from older saved results
  as_time <- function(x) if (inherits(x, "POSIXct")) x else
    as.POSIXct(x, format = "%Y/%m/%d %H:%M:%S", tz = "Asia/Taipei")
  num <- function(x, d = 3) if (is.na(x)) "NA" else trimws(formatC(x, digits = d, format = "g"))

  # bubble closures from calculate_regression(bubble_detection = TRUE): the diffusion line is drawn
  is_split <- function(row) "ch4_flag" %in% names(row) && !startsWith(row$ch4_flag, "no bubble") &&
    !is.na(row$diffusion_start)
  has_total <- function(row) "total_start_conc" %in% names(row) && !is.na(row$total_start_conc)
  # bubble closures: slope is the total, while R2 and flag describe the diffusion fit
  label_of <- function(row) {
    fit_lbl <- paste0("R\u00b2=", if (!is.na(row$r_square)) round(row$r_square, 3) else "NA", "  [", row$flag, "]")
    # RMSE (unit of the gas) and readings of the fit shown; older results have no rmse column
    has_rmse <- "rmse" %in% names(row)
    if ("ch4_flag" %in% names(row) && !startsWith(row$ch4_flag, "no bubble")) {
      n_fit <- if ("n_obs_diffusion" %in% names(row)) row$n_obs_diffusion else NA
      return(paste0(row$site, "\ntotal=", num(row$slope_total), "  ebullition=", num(row$slope_ebullition),
                    "\ndiffusion=", num(row$slope_diffusion), "  ", fit_lbl,
                    if (has_rmse) paste0("\nRMSE=", num(row$rmse), "  n=", n_fit), "\n{", row$ch4_flag, "}"))
    }
    paste0(row$site, "\nslope=", num(row$slope, 5), "  ", fit_lbl,
           if (has_rmse) paste0("\n", if ("fit_used" %in% names(row)) paste0(row$fit_used, "  "),
                                "RMSE=", num(row$rmse), "  n=", row$n_obs))
  }

  # one panel per window and gas, keyed by row number (sites may share a label)
  n <- nrow(regression_result)
  gases <- c(ghg, second_gas)
  panel_id <- function(i, g) paste(i, g)
  panel_lbl <- character(0)
  pts <- lns <- bub <- sec <- list()

  for (i in seq_len(n)) {
    row  <- regression_result[i, ]
    flag <- row$flag
    panel_lbl[panel_id(i, ghg)] <- if (is.null(second_gas)) label_of(row) else
      sub("\n", paste0("  ", unit_of(ghg), "\n"), label_of(row), fixed = TRUE)
    if (!is.null(second_gas)) panel_lbl[panel_id(i, second_gas)] <- paste0(row$site, "\n", unit_of(second_gas))
    st <- as_time(row$start_time)
    et <- as_time(row$end_time)
    if (is.na(st) || is.na(et)) next

    win <- data[data$real_datetime >= st & data$real_datetime <= et, ]
    win <- win[order(win$real_datetime), ]
    win <- win[!is.na(win[[ghg]]), ]
    if (nrow(win) < 2) next

    elapsed_s <- as.numeric(difftime(win$real_datetime, win$real_datetime[1], units = "secs"))
    conc_vec  <- as.numeric(win[[ghg]])
    pid <- panel_id(i, ghg)
    pts[[length(pts) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_s, conc = conc_vec)
    if (!is.null(second_gas))
      sec[[length(sec) + 1]] <- data.frame(panel = panel_id(i, second_gas), elapsed_s = elapsed_s,
                                           conc = as.numeric(win[[second_gas]]))

    if ("bubble_spans" %in% names(row) && !is.na(row$bubble_spans)) {
      sp <- matrix(as.numeric(unlist(strsplit(strsplit(row$bubble_spans, "; ")[[1]], "-"))), ncol = 2, byrow = TRUE)
      in_bub <- vapply(elapsed_s, function(x) any(x >= sp[, 1] - 0.05 & x <= sp[, 2] + 0.05), logical(1))
      if (any(in_bub)) bub[[length(bub) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_s[in_bub],
                                                           conc = conc_vec[in_bub])
    }

    elapsed_seq <- seq(min(elapsed_s), max(elapsed_s), length.out = 200)
    model <- if ("fit_used" %in% names(row) && !is.na(row$fit_used)) row$fit_used else fit_type
    if (is_split(row)) {
      seg <- win$real_datetime >= as_time(row$diffusion_start) & win$real_datetime <= as_time(row$diffusion_end)
      if (sum(seg) >= 2) {
        elapsed_seq <- seq(min(elapsed_s[seg]), max(elapsed_s[seg]), length.out = 200)
        fitted <- .model_curve("linear", conc_vec[seg], elapsed_s[seg], elapsed_seq)
      } else fitted <- rep(NA_real_, 200)
    } else if (has_total(row)) {
      fitted <- rep(NA_real_, 200)  # total only: no diffusion line
    } else {
      fitted <- .model_curve(if (model %in% c("quadratic", "exp_tz", "exp_zhao18", "exp_hm")) model else "linear",
                             conc_vec, elapsed_s, elapsed_seq)
    }
    lns[[length(lns) + 1]] <- data.frame(panel = pid, elapsed_s = elapsed_seq, fitted = fitted,
                                         flag = flag, line = "fit / diffusion")
    if (has_total(row)) {
      t0 <- as.numeric(difftime(as_time(row$total_start), win$real_datetime[1], units = "secs"))
      lns[[length(lns) + 1]] <- data.frame(panel = pid, elapsed_s = c(t0, max(elapsed_s)),
                                           fitted = c(row$total_start_conc, row$total_end_conc),
                                           flag = NA_character_, line = "total (two-point)")
    }
  }

  if (!length(pts)) {
    warning("plot_ghg_flux(): no window has ", ghg, " readings in data; returning NULL.")
    return(NULL)
  }
  levels_panel <- as.vector(t(outer(seq_len(n), gases, panel_id)))
  levels_panel <- levels_panel[levels_panel %in% unlist(lapply(c(pts, sec), `[[`, "panel"))]
  fac <- function(d) { d <- do.call(rbind, d); if (!is.null(d)) d$panel <- factor(d$panel, levels_panel); d }
  pts <- fac(pts); lns <- fac(lns); bub <- fac(bub); sec <- fac(sec)
  lns <- lns[!is.na(lns$fitted), ]

  colors <- flag_colors[names(flag_colors) %in% lns$flag]

  # solid marks on a dark grey panel (all >= 3:1 contrast to it); colour is kept for the fit's flag,
  # bubble readings differ from the ok line by shape as well as colour
  bg <- "#383838"
  reading <- function(d) ggplot2::geom_point(data = d, ggplot2::aes(x = elapsed_s, y = conc), shape = 16,
                                             color = "#a8a8a8", size = 0.9, alpha = 0.8, na.rm = TRUE)
  p <- ggplot2::ggplot() + reading(pts)
  if (!is.null(sec)) p <- p + reading(sec)
  if (!is.null(bub))
    p <- p + ggplot2::geom_point(data = bub, ggplot2::aes(x = elapsed_s, y = conc, shape = "bubble reading"),
                                 color = "#e07a5f", size = 1.4) +
      ggplot2::scale_shape_manual(values = c(`bubble reading` = 17))
  if (nrow(lns)) {
    fl <- lns[lns$line == "fit / diffusion", ]
    tl <- lns[lns$line != "fit / diffusion", ]
    p <- p + ggplot2::geom_line(data = fl, ggplot2::aes(x = elapsed_s, y = fitted, color = flag,
                                                        linetype = line), linewidth = 1.2)
    if (nrow(tl))
      p <- p + ggplot2::geom_line(data = tl, ggplot2::aes(x = elapsed_s, y = fitted, linetype = line),
                                  color = "white", linewidth = 0.6)
    # line-type key only when a total line is drawn (bubble closures); else it explains nothing
    p <- p + ggplot2::scale_linetype_manual(values = c(`fit / diffusion` = "solid", `total (two-point)` = "22"),
                                            guide = if (nrow(tl)) ggplot2::guide_legend(override.aes = list(color = "white")) else "none")
  }
  p +
    ggplot2::scale_color_manual(values = colors, labels = function(x) paste("fit:", x)) +
    ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = if (is.null(second_gas)) NULL else 2,
                        labeller = ggplot2::as_labeller(panel_lbl)) +
    ggplot2::labs(x = "Elapsed time (s)", y = if (is.null(second_gas)) unit_of(ghg) else "Concentration",
                  color = NULL, linetype = NULL, shape = NULL) +
    .aelab_theme("minimal", base_size, "white") +
    # default font: Century Gothic is missing from the PDF device and errors there (R CMD check);
    # dark grey background, not the theme's transparent one
    ggplot2::theme(text = ggplot2::element_text(family = ""),
                   plot.background = ggplot2::element_rect(fill = bg, color = NA),
                   panel.grid = ggplot2::element_line(color = "#4d4d4d"),
                   panel.grid.minor = ggplot2::element_line(color = "#424242"))
}
