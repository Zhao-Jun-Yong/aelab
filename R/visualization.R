utils::globalVariables(c("long", "lat", "names"))

# ── Internal helper ─────────────────────────────────────────────────────────────

.aelab_theme <- function(style, base_size, text_color) {
  plot_theme <- switch(
    style,
    "bw"        = ggplot2::theme_bw(base_size = base_size),
    "minimal"   = ggplot2::theme_minimal(base_size = base_size),
    "classic"   = ggplot2::theme_classic(base_size = base_size),
    "graycolor" = ggplot2::theme_gray(base_size = base_size),
    "light"     = ggplot2::theme_light(base_size = base_size),
    ggplot2::theme_bw(base_size = base_size)
  )
  plot_theme +
    ggplot2::theme(
      text             = ggplot2::element_text(family = "Century Gothic", color = text_color),
      strip.text       = ggplot2::element_text(face = "bold", color = text_color),
      axis.text        = ggplot2::element_text(face = "bold", color = text_color),
      legend.text      = ggplot2::element_text(face = "bold", color = text_color),
      plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.title     = ggplot2::element_blank(),
      legend.position  = "top",
      legend.key       = ggplot2::element_blank()
    )
}

.aelab_facet <- function(plot, facet, facet_x, facet_y) {
  if (!facet) return(plot)
  if (is.null(facet_x)) {
    plot + ggplot2::facet_grid(stats::as.formula(paste(facet_y, "~ .")), scales = "free")
  } else if (is.null(facet_y)) {
    plot + ggplot2::facet_grid(stats::as.formula(paste(". ~", facet_x)), scales = "free")
  } else {
    plot + ggplot2::facet_grid(stats::as.formula(paste(facet_y, "~", facet_x)), scales = "free")
  }
}

# ── Plot functions ───────────────────────────────────────────────────────────────

#' @title plot_point
#' @description Create a scatter plot using the aelab theme.
#' @param df A data frame.
#' @param x <[`data-masking`][ggplot2::aes]> Column mapped to the x-axis.
#' @param y <[`data-masking`][ggplot2::aes]> Column mapped to the y-axis.
#' @param z <[`data-masking`][ggplot2::aes]> Optional column mapped to fill colour.
#' @param base_size Base font size passed to the ggplot2 theme. Default 25.
#' @param point_size Point size. Default 3.
#' @param stroke_size Point stroke width. Default 1.
#' @param text_color Text colour. Default \code{"black"}.
#' @param facet Logical; add facet grid? Default \code{FALSE}.
#' @param facet_x Column name (string) for the horizontal facet dimension.
#' @param facet_y Column name (string) for the vertical facet dimension.
#' @param style Theme style. One of \code{"bw"}, \code{"minimal"},
#'   \code{"classic"}, \code{"graycolor"}, \code{"light"}. Default \code{"bw"}.
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:5, y = c(2,4,1,5,3), g = c("A","A","B","B","A"))
#' plot_point(df, x, y, g)
#' }
#' @import ggplot2
#' @importFrom stats as.formula
#' @export
plot_point <- function(df, x, y, z = NULL,
                       base_size = 25, point_size = 3, stroke_size = 1,
                       text_color = "black", facet = FALSE,
                       facet_x = NULL, facet_y = NULL,
                       style = "bw") {
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ z }})) +
    ggplot2::geom_point(shape = 21, size = point_size, stroke = stroke_size) +
    .aelab_theme(style, base_size, text_color)
  .aelab_facet(plot, facet, facet_x, facet_y)
}


#' @title plot_line
#' @description Create a line plot using the aelab theme.
#' @param df A data frame.
#' @param x <[`data-masking`][ggplot2::aes]> Column mapped to the x-axis.
#' @param y <[`data-masking`][ggplot2::aes]> Column mapped to the y-axis.
#' @param z <[`data-masking`][ggplot2::aes]> Optional column mapped to colour and group.
#' @param base_size Base font size. Default 25.
#' @param line_width Line width. Default 3.
#' @param text_color Text colour. Default \code{"black"}.
#' @param facet Logical; add facet grid? Default \code{FALSE}.
#' @param facet_x Column name (string) for the horizontal facet dimension.
#' @param facet_y Column name (string) for the vertical facet dimension.
#' @param style Theme style. Default \code{"bw"}.
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' df <- data.frame(x = 1:6, y = c(1,3,2,5,4,6), g = rep(c("A","B"), 3))
#' plot_line(df, x, y, g)
#' }
#' @import ggplot2
#' @importFrom stats as.formula
#' @export
plot_line <- function(df, x, y, z = NULL,
                      base_size = 25, line_width = 3,
                      text_color = "black", facet = FALSE,
                      facet_x = NULL, facet_y = NULL,
                      style = "bw") {
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = {{ x }}, y = {{ y }}, color = {{ z }})) +
    ggplot2::geom_line(ggplot2::aes(group = {{ z }}), linewidth = line_width) +
    .aelab_theme(style, base_size, text_color)
  .aelab_facet(plot, facet, facet_x, facet_y)
}


#' @title plot_box
#' @description Create a box plot with mean overlay using the aelab theme.
#' @param df A data frame.
#' @param x <[`data-masking`][ggplot2::aes]> Column mapped to the x-axis.
#' @param y <[`data-masking`][ggplot2::aes]> Column mapped to the y-axis.
#' @param z <[`data-masking`][ggplot2::aes]> Optional column mapped to fill colour.
#' @param base_size Base font size. Default 25.
#' @param line_width Box outline width. Default 0.5.
#' @param outlier_size Outlier point size. Default 1.5.
#' @param text_color Text colour. Default \code{"black"}.
#' @param facet Logical; add facet grid? Default \code{FALSE}.
#' @param facet_x Column name (string) for the horizontal facet dimension.
#' @param facet_y Column name (string) for the vertical facet dimension.
#' @param style Theme style. Default \code{"bw"}.
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' df <- data.frame(x = rep(c("A","B"), each = 5), y = c(1:5, 3:7))
#' plot_box(df, x, y)
#' }
#' @import ggplot2
#' @importFrom stats as.formula
#' @export
plot_box <- function(df, x, y, z = NULL,
                     base_size = 25, line_width = 0.5,
                     outlier_size = 1.5,
                     text_color = "black", facet = FALSE,
                     facet_x = NULL, facet_y = NULL,
                     style = "bw") {
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ z }})) +
    ggplot2::geom_boxplot(linewidth = line_width, outlier.size = outlier_size) +
    .aelab_theme(style, base_size, text_color) +
    ggplot2::stat_summary(
      ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ z }}),
      fun = mean, geom = "point", shape = 22,
      size = 2, color = "black",
      stroke = line_width,
      position = ggplot2::position_dodge(width = 0.75)
    ) +
    ggplot2::theme(legend.key = NULL)
  .aelab_facet(plot, facet, facet_x, facet_y)
}


#' @title plot_bar
#' @description Create a bar plot using the aelab theme.
#' @param df A data frame.
#' @param x <[`data-masking`][ggplot2::aes]> Column mapped to the x-axis.
#' @param y <[`data-masking`][ggplot2::aes]> Column mapped to the y-axis.
#' @param z <[`data-masking`][ggplot2::aes]> Optional column mapped to fill colour.
#' @param base_size Base font size. Default 25.
#' @param line_width Bar outline width. Default 1.
#' @param text_color Text colour. Default \code{"black"}.
#' @param facet Logical; add facet grid? Default \code{FALSE}.
#' @param facet_x Column name (string) for the horizontal facet dimension.
#' @param facet_y Column name (string) for the vertical facet dimension.
#' @param style Theme style. Default \code{"bw"}.
#' @param position Bar position: \code{"dodge"} or \code{"stack"}. Default \code{"dodge"}.
#' @param stat Stat type: \code{"identity"} or \code{"count"}. Default \code{"identity"}.
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' df <- data.frame(x = c("A","B","A","B"), g = c("X","X","Y","Y"), y = c(1,2,3,4))
#' plot_bar(df, x, y, g)
#' }
#' @import ggplot2
#' @importFrom stats as.formula
#' @export
plot_bar <- function(df, x, y, z = NULL,
                     base_size = 25, line_width = 1,
                     text_color = "black", facet = FALSE,
                     facet_x = NULL, facet_y = NULL,
                     style = "bw", position = "dodge",
                     stat = "identity") {
  plot <- ggplot2::ggplot(df, ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ z }})) +
    ggplot2::geom_bar(ggplot2::aes(fill = {{ z }}),
                      position = position, stat = stat,
                      linewidth = line_width, color = "black") +
    .aelab_theme(style, base_size, text_color)
  .aelab_facet(plot, facet, facet_x, facet_y)
}

# ── Palette system ───────────────────────────────────────────────────────────────

#' @title aelab_palettes
#' @description Retrieve a named aelab colour palette as a character vector.
#' @details Available palette names: \code{"rainbow"}, \code{"two"},
#'   \code{"control"}, \code{"control2"}, \code{"control3"},
#'   \code{"period"}, \code{"ghg"}.
#' @param name Name of the palette (string).
#' @param n Number of colours to return. Defaults to the full palette length.
#' @param type \code{"discrete"} (default) or \code{"continuous"}. For
#'   \code{"continuous"}, colours are interpolated to length \code{n}.
#' @return A character vector of hex colour codes with class \code{"palette"}.
#' @examples
#' aelab_palettes("rainbow", 5)
#' aelab_palettes("ghg", type = "continuous", n = 20)
#' @importFrom grDevices colorRampPalette
#' @export
aelab_palettes <- function(name, n, type = c("discrete", "continuous")) {
  aelab_colours <- list(
    rainbow  = c("#C98686", "#f0a082", "#F2B880", "#eddea4",
                 "#8fcb9b", "#5b9279", "#9db4c0", "#71869a",
                 "#b8a1b4", "#eae6e5", "#a8a59a"),
    two      = c("#9db4c0", "#eddea4"),
    control  = c("#adb5bd", "#F2B880", "#f0a082", "#C98686"),
    control2 = c("#adb5bd", "#6c757d", "#F2B880", "#f0a082"),
    control3 = c("#adb5bd", "#F2B880", "#6c757d", "#f0a082"),
    period   = c("#adb5bd", "#e9c46a", "#6c757d"),
    ghg      = c("#e9c46a", "#adb5bd", "#6c757d")
  )

  palette <- aelab_colours[[name]]
  if (is.null(palette)) stop(paste("Unknown palette:", name))
  if (missing(n)) n <- length(palette)
  type <- match.arg(type)

  out <- switch(type,
    continuous = grDevices::colorRampPalette(palette)(n),
    discrete   = palette[seq_len(n)]
  )
  structure(out, name = name, class = "palette")
}


#' @title scale_colour_aelab_d
#' @description Discrete ggplot2 colour scale using an aelab palette.
#' @param name Palette name passed to \code{\link{aelab_palettes}}.
#' @param direction \code{1} (default) for normal order; \code{-1} to reverse.
#' @return A \code{ggplot2} scale.
#' @examples
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   ggplot2::geom_point() + scale_colour_aelab_d("rainbow")
#' }
#' @import ggplot2
#' @export
scale_colour_aelab_d <- function(name, direction = 1) {
  values <- aelab_palettes(name, type = "discrete")
  if (direction == -1) values <- rev(values)
  ggplot2::scale_colour_manual(values = values)
}

#' @rdname scale_colour_aelab_d
#' @export
scale_color_aelab_d <- scale_colour_aelab_d


#' @title scale_fill_aelab_d
#' @description Discrete ggplot2 fill scale using an aelab palette.
#' @param name Palette name passed to \code{\link{aelab_palettes}}.
#' @param direction \code{1} (default) for normal order; \code{-1} to reverse.
#' @return A \code{ggplot2} scale.
#' @examples
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = factor(cyl))) +
#'   ggplot2::geom_bar() + scale_fill_aelab_d("control")
#' }
#' @import ggplot2
#' @export
scale_fill_aelab_d <- function(name, direction = 1) {
  values <- aelab_palettes(name, type = "discrete")
  if (direction == -1) values <- rev(values)
  ggplot2::scale_fill_manual(values = values)
}


#' @title scale_colour_aelab_c
#' @description Continuous ggplot2 colour scale using an aelab palette.
#' @param name Palette name passed to \code{\link{aelab_palettes}}.
#' @param direction \code{1} (default) for normal order; \code{-1} to reverse.
#' @return A \code{ggplot2} scale.
#' @examples
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = mpg)) +
#'   ggplot2::geom_point() + scale_colour_aelab_c("rainbow")
#' }
#' @import ggplot2
#' @export
scale_colour_aelab_c <- function(name, direction = 1) {
  values <- aelab_palettes(name, type = "continuous")
  if (direction == -1) values <- rev(values)
  ggplot2::scale_colour_manual(values = values)
}

#' @rdname scale_colour_aelab_c
#' @export
scale_color_aelab_c <- scale_colour_aelab_c


#' @title scale_fill_aelab_c
#' @description Continuous ggplot2 fill scale using an aelab palette.
#' @param name Palette name passed to \code{\link{aelab_palettes}}.
#' @param direction \code{1} (default) for normal order; \code{-1} to reverse.
#' @return A \code{ggplot2} scale.
#' @examples
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), fill = mpg)) +
#'   ggplot2::geom_col() + scale_fill_aelab_c("ghg")
#' }
#' @import ggplot2
#' @export
scale_fill_aelab_c <- function(name, direction = 1) {
  values <- aelab_palettes(name, type = "continuous")
  if (direction == -1) values <- rev(values)
  ggplot2::scale_fill_manual(values = values)
}

# ── Map ──────────────────────────────────────────────────────────────────────────

#' @title plot_map_taiwan
#' @description Plot sampling sites on a map of Taiwan with a north arrow and
#'   scale bar.
#' @param long Numeric vector of longitudes.
#' @param lat Numeric vector of latitudes.
#' @param names Character vector of site labels (same length as \code{long}).
#' @param color Fill colour for site markers. Default \code{"darkgrey"}.
#' @param textsize Size for annotation and point labels. Default 5.
#' @param basesize Base font size for the map theme. Default 16.
#' @param shape_type ggplot2 point shape number. Default 22 (filled square).
#' @return A \code{ggplot} object (also printed to the active device).
#' @examples
#' \dontrun{
#' plot_map_taiwan(
#'   long = c(120.2, 121.5),
#'   lat  = c(22.9, 24.1),
#'   names = c("Site A", "Site B")
#' )
#' }
#' @import ggplot2
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf
#' @importFrom ggspatial annotation_north_arrow north_arrow_fancy_orienteering
#'   annotation_scale
#' @export
plot_map_taiwan <- function(long, lat, names, color = "darkgrey",
                            textsize = 5, basesize = 16, shape_type = 22) {
  if (!is.numeric(long) || !is.numeric(lat) || length(long) != length(lat)) {
    stop("Invalid latitude or longitude provided.")
  }

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  map <- ggplot2::ggplot(data = world) +
    ggplot2::geom_sf() +
    ggplot2::xlab(expression(bold("Longitude"))) +
    ggplot2::ylab(expression(bold("Latitude"))) +
    ggplot2::coord_sf(xlim = c(118.50, 123.50), ylim = c(21.00, 26.00),
                      expand = FALSE) +
    ggplot2::theme_bw(base_size = basesize)

  for (i in seq_along(long)) {
    site_df <- data.frame(long = long[i], lat = lat[i], names = names[i])
    map <- map +
      ggplot2::geom_point(data = site_df,
                          ggplot2::aes(x = long, y = lat),
                          size = textsize, shape = shape_type,
                          color = "black", fill = color, stroke = 1) +
      ggplot2::annotate(geom = "text", x = long[i], y = lat[i] + 0.2,
                        label = names[i],
                        fontface = "bold", size = textsize - 1, colour = "black")
  }

  map <- map +
    ggplot2::annotate(geom = "text", x = 121, y = 24, label = "Taiwan",
                      fontface = "italic", size = textsize) +
    ggspatial::annotation_north_arrow(
      location   = "bl", which_north = "true",
      pad_x      = ggplot2::unit(0.75, "in"),
      pad_y      = ggplot2::unit(0.5, "in"),
      style      = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

  print(map)
}
