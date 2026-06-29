# ── Gantt-style progress tables ───────────────────────────────────────────────
# Two renderers from the same data contract:
#   gantt_table() → flextable (PDF / DOCX / PPTX via officer)
#   gantt_text()  → Unicode text (terminal / Neovim)
#
# Data contract (df):
#   category | system | <campaign1> | <campaign2> | ...
#
# Cell coloring:
#   NA / ""          → na_color (default grey = no sampling)
#   value + done     → system fill color (completed)
#   value + not done → white (planned, not yet done)
#   If done_cols is NULL, all values use system fill color.
#
# For processing-progress tables where NA means "not yet processed" (not
# "no sampling"), pass na_color = "white".

#' Gantt-style coloured progress table
#'
#' Creates a coloured \code{flextable} where columns are sampling campaigns and
#' rows are measurement categories × systems. Works for both a sampling schedule
#' (cells = dates) and a processing-progress table (cells = sample counts).
#'
#' @param df Data frame with columns \code{category}, \code{system}, then one
#'   column per campaign. Campaign column names become header labels.
#' @param colors Named character vector mapping system identifiers to hex fill
#'   colors, e.g. \code{c(tilapia = "#D8AEFF", prawn = "#F7CAAC")}.
#' @param system_labels Named character vector of short display labels for the
#'   system column (Col 2). Defaults to \code{names(colors)}.
#' @param done_cols Character vector of campaign column names that are
#'   completed. Completed cells with values get system fill; others with values
#'   get white fill (planned). If \code{NULL} (default), all values get fill.
#' @param na_color Hex fill for \code{NA} / empty cells. Default grey
#'   (\code{"#BFBFBF"}) suits sampling schedules. Use \code{"white"} for
#'   processing-progress tables where \code{NA} means "not yet processed".
#' @param notes Optional character string added as a small-font footer.
#' @return A \code{flextable} object.
#' @export
gantt_table <- function(df, colors, system_labels = NULL,
                        done_cols = NULL, na_color = "#BFBFBF",
                        notes = NULL) {
  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Package 'flextable' required. Install with install.packages('flextable').")
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Package 'officer' required. Install with install.packages('officer').")

  if (is.null(system_labels))
    system_labels <- stats::setNames(names(colors), names(colors))

  campaigns <- setdiff(names(df), c("category", "system"))
  n_camp    <- length(campaigns)

  df_disp <- df
  df_disp$system <- unname(system_labels[df$system])

  thin  <- officer::fp_border(color = "#BBBBBB", width = 0.5)
  thick <- officer::fp_border(color = "#555555", width = 1.5)

  ft <- flextable::flextable(df_disp) |>
    flextable::set_header_labels(category = "量測項目", system = "") |>
    flextable::merge_v(j = "category") |>
    flextable::valign(j = "category", valign = "center") |>
    flextable::bold(part = "header") |>
    flextable::align(part = "header", align = "center") |>
    flextable::align(j = "system",  align = "center", part = "body") |>
    flextable::align(j = campaigns, align = "center", part = "body") |>
    flextable::bg(part = "header", bg = "#D9D9D9") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::border_outer(part = "all", border = thick) |>
    flextable::border_inner(part = "all", border = thin)

  # ── Cell fill ──────────────────────────────────────────────────────────────
  for (i in seq_len(nrow(df))) {
    sys  <- df$system[i]
    fill <- unname(colors[sys])
    ft   <- flextable::bg(ft, i = i, j = "system", bg = fill)

    for (camp in campaigns) {
      val <- df[[camp]][i]
      empty <- is.na(val) || nchar(trimws(as.character(val))) == 0
      if (empty) {
        cell_bg <- na_color
      } else if (!is.null(done_cols) && !camp %in% done_cols) {
        cell_bg <- "white"   # planned but not yet done
      } else {
        cell_bg <- fill      # completed
      }
      ft <- flextable::bg(ft, i = i, j = camp, bg = cell_bg)
    }
  }

  # ── Category group separators ──────────────────────────────────────────────
  boundaries <- cumsum(rle(df$category)$lengths)
  boundaries <- boundaries[-length(boundaries)]
  if (length(boundaries) > 0)
    ft <- flextable::hline(ft, i = boundaries, border = thick)

  # ── Column widths (fixed layout for cross-format consistency) ──────────────
  total_w <- 6.0
  cat_w   <- 1.4
  sys_w   <- 0.28
  camp_w  <- round((total_w - cat_w - sys_w) / max(n_camp, 1L), 2)

  ft <- ft |>
    flextable::set_table_properties(layout = "fixed") |>
    flextable::width(j = "category", width = cat_w) |>
    flextable::width(j = "system",   width = sys_w) |>
    flextable::width(j = campaigns,  width = camp_w)

  # ── Footer notes ───────────────────────────────────────────────────────────
  if (!is.null(notes)) {
    ft <- flextable::add_footer_lines(ft, notes) |>
      flextable::fontsize(part = "footer", size = 8) |>
      flextable::italic(part = "footer") |>
      flextable::color(part = "footer", color = "#444444") |>
      flextable::border_outer(part = "footer", border = thick) |>
      flextable::hline_top(part = "footer", border = thin)
  }

  ft
}


#' Plain-text Gantt for terminal and Neovim
#'
#' Prints a Unicode box-drawing table to the console. Same data contract as
#' \code{\link{gantt_table}}. Run from the project root with:
#' \code{Rscript -e "source('R/gantt_config.R'); aelab::gantt_text(gantt_schedule)"}
#'
#' @inheritParams gantt_table
#' @return Invisibly returns \code{df}.
#' @export
gantt_text <- function(df, colors = NULL, system_labels = NULL,
                       done_cols = NULL, na_color = "#BFBFBF", notes = NULL) {
  campaigns <- setdiff(names(df), c("category", "system"))
  cw <- max(max(nchar(campaigns), na.rm = TRUE), 10L)

  hdr_fmt <- paste0("%-18s %-6s", paste(rep(paste0(" %-", cw, "s"), length(campaigns)), collapse = ""))

  cat(do.call(sprintf, c(list(hdr_fmt), list("量測項目", "系統"), as.list(campaigns))), "\n")
  cat(strrep("─", 18 + 7 + (cw + 1) * length(campaigns)), "\n")

  prev_cat   <- ""
  prev_group <- ""
  for (i in seq_len(nrow(df))) {
    if (i > 1 && df$category[i] != prev_group)
      cat(strrep("─", 18 + 7 + (cw + 1) * length(campaigns)), "\n")
    prev_group <- df$category[i]

    cat_lbl <- if (df$category[i] != prev_cat) df$category[i] else ""
    prev_cat <- df$category[i]

    sys_lbl <- if (!is.null(system_labels)) {
      paste0("[", unname(system_labels[df$system[i]]), "]")
    } else {
      paste0("[", df$system[i], "]")
    }

    vals <- lapply(campaigns, function(c) {
      v     <- df[[c]][i]
      empty <- is.na(v) || nchar(trimws(as.character(v))) == 0
      if (empty) return("----")
      if (!is.null(done_cols) && !c %in% done_cols) return(paste0("(", v, ")"))
      v
    })

    cat(do.call(sprintf, c(list(hdr_fmt), list(cat_lbl, sys_lbl), vals)), "\n")
  }

  if (!is.null(notes)) {
    cat("\n― 備註 ―\n")   # ― 備註 ―
    cat(notes, "\n")
  }

  invisible(df)
}
