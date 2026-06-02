df <- data.frame(x = 1:5, y = c(2, 4, 1, 5, 3), g = c("A", "A", "B", "B", "A"))

# ── plot_point ────────────────────────────────────────────────────────────────

test_that("plot_point returns a ggplot object", {
  p <- plot_point(df, x, y)
  expect_s3_class(p, "ggplot")
})

test_that("plot_point accepts z aesthetic", {
  p <- plot_point(df, x, y, g)
  expect_s3_class(p, "ggplot")
})

test_that("plot_point applies facet when facet = TRUE", {
  p <- plot_point(df, x, y, facet = TRUE, facet_y = "g")
  expect_s3_class(p, "ggplot")
})

# ── plot_line ─────────────────────────────────────────────────────────────────

test_that("plot_line returns a ggplot object", {
  p <- plot_line(df, x, y)
  expect_s3_class(p, "ggplot")
})

test_that("plot_line accepts z aesthetic", {
  p <- plot_line(df, x, y, g)
  expect_s3_class(p, "ggplot")
})

test_that("plot_line applies facet when facet = TRUE", {
  p <- plot_line(df, x, y, facet = TRUE, facet_x = "g")
  expect_s3_class(p, "ggplot")
})

# ── plot_box ──────────────────────────────────────────────────────────────────

test_that("plot_box returns a ggplot object", {
  p <- plot_box(df, g, y)
  expect_s3_class(p, "ggplot")
})

test_that("plot_box accepts z aesthetic", {
  p <- plot_box(df, g, y, g)
  expect_s3_class(p, "ggplot")
})

# ── plot_bar ──────────────────────────────────────────────────────────────────

test_that("plot_bar returns a ggplot object", {
  p <- plot_bar(df, g, y)
  expect_s3_class(p, "ggplot")
})

test_that("plot_bar accepts z aesthetic and stack position", {
  p <- plot_bar(df, g, y, g, position = "stack")
  expect_s3_class(p, "ggplot")
})

# ── aelab_palettes ────────────────────────────────────────────────────────────

test_that("aelab_palettes returns palette of requested length", {
  pal <- aelab_palettes("rainbow", 5)
  expect_length(pal, 5)
})

test_that("aelab_palettes returns class palette", {
  pal <- aelab_palettes("ghg", 3)
  expect_s3_class(pal, "palette")
})

test_that("aelab_palettes returns full palette when n is omitted", {
  pal <- aelab_palettes("two")
  expect_length(pal, 2)
})

test_that("aelab_palettes interpolates for continuous type", {
  pal <- aelab_palettes("ghg", n = 20, type = "continuous")
  expect_length(pal, 20)
  expect_s3_class(pal, "palette")
})

test_that("aelab_palettes errors on unknown palette name", {
  expect_snapshot(error = TRUE, aelab_palettes("nonexistent"))
})

# ── scale_colour_aelab_d ──────────────────────────────────────────────────────

test_that("scale_colour_aelab_d returns a Scale object", {
  s <- scale_colour_aelab_d("rainbow")
  expect_s3_class(s, "Scale")
})

test_that("scale_colour_aelab_d maps to colour aesthetic", {
  s <- scale_colour_aelab_d("rainbow")
  expect_equal(s$aesthetics, "colour")
})

test_that("scale_colour_aelab_d direction reverses palette values", {
  n <- length(aelab_palettes("control"))
  s_fwd <- scale_colour_aelab_d("control")
  s_rev <- scale_colour_aelab_d("control", direction = -1)
  expect_equal(
    as.character(s_fwd$palette(n)),
    rev(as.character(s_rev$palette(n)))
  )
})

# ── scale_fill_aelab_d ────────────────────────────────────────────────────────

test_that("scale_fill_aelab_d returns a Scale object", {
  s <- scale_fill_aelab_d("two")
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_aelab_d maps to fill aesthetic", {
  s <- scale_fill_aelab_d("two")
  expect_equal(s$aesthetics, "fill")
})

# ── scale_colour_aelab_c ──────────────────────────────────────────────────────

test_that("scale_colour_aelab_c returns a Scale object", {
  s <- scale_colour_aelab_c("ghg")
  expect_s3_class(s, "Scale")
})

test_that("scale_colour_aelab_c maps to colour aesthetic", {
  s <- scale_colour_aelab_c("ghg")
  expect_equal(s$aesthetics, "colour")
})

# ── scale_fill_aelab_c ────────────────────────────────────────────────────────

test_that("scale_fill_aelab_c returns a Scale object", {
  s <- scale_fill_aelab_c("period")
  expect_s3_class(s, "Scale")
})

test_that("scale_fill_aelab_c maps to fill aesthetic", {
  s <- scale_fill_aelab_c("period")
  expect_equal(s$aesthetics, "fill")
})
