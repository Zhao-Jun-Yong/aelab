# ch4_options with their defaults (see calculate_regression(bubble_detection = TRUE)), checked
.ch4_options <- function(ch4_options) {
  opt <- list(bubble_window = 10, bubble_align = "centre", bubble_prob = 0.75, merge_gap = 10,
              min_len = 5, early_s = 20, early_from = "window", total_from = "window",
              spar = 0.5, nknots_ratio = 1/3, trim_to_min = TRUE, neg_ebullition = "zero",
              min_rise = 0, bubble_pad = 0, min_diffusion_s = 20)
  if (!is.list(ch4_options)) stop("ch4_options must be a named list.")
  if (length(ch4_options) && (is.null(names(ch4_options)) || any(names(ch4_options) == "")))
    stop("Every ch4_options element needs a name.")
  unknown <- setdiff(names(ch4_options), names(opt))
  if (length(unknown)) stop("Unknown ch4_options: ", paste(unknown, collapse = ", "))
  opt[names(ch4_options)] <- ch4_options
  num <- function(x, lo, hi = Inf, whole = FALSE, lo_open = FALSE) {
    v <- opt[[x]]
    ok <- is.numeric(v) && length(v) == 1 && !is.na(v) && v <= hi && (if (lo_open) v > lo else v >= lo) &&
      (!whole || v %% 1 == 0)
    if (!ok) stop(x, " must be a ", if (whole) "whole " else "", "number ", if (lo_open) "> " else ">= ", lo,
                  if (is.finite(hi)) paste0(" and <= ", hi) else "", ".", call. = FALSE)
  }
  num("bubble_window", 2, whole = TRUE); num("bubble_prob", 0, 1, lo_open = TRUE)
  num("merge_gap", 0, whole = TRUE); num("min_len", 1, whole = TRUE); num("early_s", 0)
  num("spar", 0, lo_open = TRUE); num("nknots_ratio", 0, 1, lo_open = TRUE); num("min_rise", 0)
  num("bubble_pad", 0, whole = TRUE); num("min_diffusion_s", 0, lo_open = TRUE)
  if (!isTRUE(opt$trim_to_min) && !isFALSE(opt$trim_to_min))
    stop("trim_to_min must be TRUE or FALSE.", call. = FALSE)
  opt$bubble_align   <- match.arg(opt$bubble_align, c("centre", "start", "end"))
  opt$early_from     <- match.arg(opt$early_from, c("window", "placement"))
  opt$total_from     <- match.arg(opt$total_from, c("window", "segment"))
  opt$neg_ebullition <- match.arg(opt$neg_ebullition, c("zero", "keep", "na"))
  opt
}

# Bubble detection of Minaudo et al. (2026, p. 4): logical flag per reading; t in seconds from 0.
# Run on readings linearly interpolated to a 1 s grid, as the code archive (find_bubbles.R,
# lines 35-36), so window, merge_gap and min_len are seconds whatever the logging interval or gaps.
# min_thresh = 1e-4 (floor of the variance cut-off) is fixed, no published source.
.find_ch4_bubbles <- function(t, conc, window = 10, align = "centre", prob = 0.75, min_thresh = 1e-4,
                              merge_gap = 10, min_len = 5) {
  grid <- seq(0, floor(max(t)))
  at <- pmin(round(t), max(grid)) + 1  # grid point of each reading
  conc <- stats::approx(t, conc, grid)$y
  n <- length(conc)
  flag <- rep(FALSE, n)
  if (n < window + 1) return(flag[at])
  z <- conc / mean(conc)
  first <- seq_len(n - window + 1)  # first reading of each window
  v <- vapply(first, function(s) stats::var(z[s:(s + window - 1)]), numeric(1))
  owner <- first + switch(align, centre = window %/% 2, start = 0, end = window - 1)
  r <- rle(v > max(min_thresh, stats::quantile(v, prob)))
  # merge high runs split by fewer than merge_gap low windows
  inner <- seq_along(r$values) > 1 & seq_along(r$values) < length(r$values)
  r$values[!r$values & r$lengths < merge_gap & inner] <- TRUE
  r <- rle(inverse.rle(r))
  r$values[r$values & r$lengths < min_len] <- FALSE
  flag[owner] <- inverse.rle(r)
  flag[at]
}

# Unflag bubbles whose rise is <= min_rise. Rise as FluxSeparator's ebullitive_flux() (Sø et al.
# 2024 p. 5): last - lowest reading, from the reading before the segment to the one after; 0 if the
# segment falls or its lowest reading comes after its highest.
.drop_small_bubbles <- function(conc, flag, min_rise) {
  r <- rle(flag)
  e <- cumsum(r$lengths)
  b <- e - r$lengths + 1
  for (k in which(r$values)) {
    s <- conc[max(1, b[k] - 1):min(length(conc), e[k] + 1)]
    rise <- if (s[1] < s[length(s)] && which.min(s) < which.max(s)) s[length(s)] - min(s) else 0
    if (rise <= min_rise) flag[b[k]:e[k]] <- FALSE
  }
  flag
}

.lm_ch4 <- function(t, conc) {
  f <- suppressWarnings(summary(stats::lm(conc ~ t)))  # "essentially perfect fit" on flat stretches
  c(slope = f$coefficients[2, 1], r2 = f$adj.r.squared, p = f$coefficients[2, 4], se = f$coefficients[2, 2])
}

# First linear segment (>= min_s) of unflagged readings: within each unbroken run, in time order,
# grow a window from the run start; window ends passing adj. R2 >= 0.7, p < 0.05, slope > 0 (any sign before the first bubble)
# that are < 10 s apart form one block: one block -> best adj. R2, several -> last end of the
# first block (as the code archive; it reproduced 5 of 6 changed eye-accepted CarbonIMTA splits
# exactly, 2026-11-10). If none passes, the best-R2 window of the
# first long-enough run (ok = FALSE); fit = NULL when no run is long enough.
# The code archive's 100 s moving-window rising-slope check is not used: with trim_to_min on it
# changed no diffusion value (> 1%) in 97 bubbly CarbonIMTA closures (2026-11-10).
# Fixed, no published source: adj. R2 >= 0.7 and p < 0.05 (pass below); gap_s = 10 is the
# "< 10 s apart" block rule described above.
.first_linear_ch4 <- function(t, conc, flag, min_s = 20, gap_s = 10, t_bubble = Inf) {
  r <- rle(!flag)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  fallback <- NULL
  for (k in which(r$values)) {
    idx <- starts[k]:ends[k]
    tt <- t[idx]
    if (length(tt) < 3 || max(tt) - tt[1] < min_s) next
    js <- which(tt - tt[1] >= min_s & seq_along(tt) >= 3)  # a 2-point line has no R2 or p
    fits <- t(vapply(js, function(j) .lm_ch4(tt[1:j], conc[idx[1:j]]), numeric(4)))
    fits <- cbind(fits, start = tt[1], end = tt[js])
    # negative slopes (CH4 uptake) only before the first bubble: after one, a falling stretch is
    # bubble gas leaving the chamber, not uptake (5 of 160 CarbonIMTA closures otherwise)
    pass <- (fits[, "slope"] > 0 | tt[1] < t_bubble) & fits[, "r2"] >= 0.7 & fits[, "p"] < 0.05
    pass[is.na(pass)] <- FALSE
    if (any(pass)) {
      ok_i <- which(pass)
      brk <- which(diff(fits[ok_i, "end"]) > gap_s)
      best <- if (length(brk)) ok_i[brk[1]] else ok_i[which.max(fits[ok_i, "r2"])]
      return(list(fit = fits[best, ], ok = TRUE))
    }
    if (is.null(fallback)) fallback <- fits[which.max(fits[, "r2"]), ]
  }
  list(fit = fallback, ok = FALSE)
}

# Two-point total from the smoothed series (Minaudo et al. 2026, p. 3, Eq. 1)
# edge_s = 30 (width of the first and last stretch searched for c0 and c1) is fixed, no published source.
.total_ch4 <- function(tt, conc, spar, nknots_ratio, edge_s = 30) {
  tt <- tt - tt[1]
  sm <- stats::predict(stats::smooth.spline(tt, conc, nknots = round(nknots_ratio * length(tt)),
                                            spar = spar), tt)$y
  first <- tt < edge_s
  last <- tt > max(tt) - edge_s
  clamp <- function(x, ref) min(max(x, min(ref)), max(ref))
  c0 <- clamp(min(sm[first]), conc[first])
  c1 <- clamp(max(sm[last]), conc[last])
  c(slope = (c1 - c0) / max(tt), c0 = c0, c1 = c1)
}

# Total / diffusion / ebullition split of one CH4 closure (Minaudo et al. 2026, pp. 3-4).
# time in seconds; t_ref = placement time for early_from = "placement".
.separate_ch4 <- function(time, conc, opt, t_ref = NA_real_) {
  out <- data.frame(total = NA_real_, diffusion = NA_real_, ebullition = NA_real_,
                    n_bubbles = NA_integer_, ch4_flag = "too few readings",
                    diff_r2 = NA_real_, diff_cor = NA_real_, diff_se = NA_real_,
                    diff_r1 = NA_real_,
                    diff_start = NA_real_, diff_end = NA_real_, diff_n = NA_integer_, total_s = NA_real_,
                    total_t0 = NA_real_, total_c0 = NA_real_, total_c1 = NA_real_,
                    bubble_spans = NA_character_)
  o <- order(time)
  time <- time[o]; conc <- conc[o]
  keep <- !duplicated(time)
  time <- time[keep]; conc <- conc[keep]
  if (length(time) < 20) return(out)  # fixed, no published source
  tt <- time - time[1]

  flag <- .find_ch4_bubbles(tt, conc, opt$bubble_window, opt$bubble_align, opt$bubble_prob,
                            merge_gap = opt$merge_gap, min_len = opt$min_len)
  spans <- function(f) {
    r <- rle(f); e <- cumsum(r$lengths); s <- e - r$lengths + 1
    if (any(f)) paste(sprintf("%g-%g", round(tt[s[r$values]], 1), round(tt[e[r$values]], 1)),
                      collapse = "; ") else NA_character_
  }
  out$n_bubbles <- sum(rle(flag)$values)
  out$bubble_spans <- spans(flag)
  no_bubble <- function(label) {
    s <- .lm_ch4(tt, conc)[["slope"]]
    out[c("total", "diffusion", "ebullition", "ch4_flag")] <- list(s, s, 0, label)
    out
  }
  if (!any(flag)) return(no_bubble("no bubble"))

  tot <- .total_ch4(tt, conc, opt$spar, opt$nknots_ratio)
  window_total <- tot[["slope"]]
  out[c("total_t0", "total_c0", "total_c1")] <- list(time[1], tot[["c0"]], tot[["c1"]])
  out$total_s <- max(tt)
  t_early <- if (opt$early_from == "placement") time[which(flag)[1]] - t_ref else tt[which(flag)[1]]
  if (t_early <= opt$early_s) {
    out[c("total", "ch4_flag")] <- list(window_total, "early bubble: total only")
    return(out)
  }

  # min_rise and bubble_pad act on splits only, after the early-bubble rule (FluxSeparator's
  # concentration_diffusion_cutoff and IndexSpan, Sø et al. 2024 p. 5)
  if (opt$min_rise > 0) {
    flag <- .drop_small_bubbles(conc, flag, opt$min_rise)
    out$n_bubbles <- sum(rle(flag)$values)
    if (!any(flag)) {
      out[c("total_t0", "total_c0", "total_c1", "bubble_spans")] <- list(NA_real_, NA_real_, NA_real_, NA_character_)
      return(no_bubble("no bubble: rises <= min_rise"))
    }
  }
  if (opt$bubble_pad > 0) {
    ft <- tt[flag]  # pad in seconds
    flag <- flag | vapply(tt, function(x) any(abs(x - ft) <= opt$bubble_pad), logical(1))
  }
  out$bubble_spans <- spans(flag)

  excl <- flag
  # the lowest reading is searched in the first half of the closure: fixed, no published source
  if (isTRUE(opt$trim_to_min) && any(!flag & tt < max(tt) / 2)) {
    early <- which(!flag & tt < max(tt) / 2)
    # the lowest reading itself is dropped too: picked as the minimum, it is likely a noise low.
    # The code archive drops it from the final fit only, keeping it in the fits that choose the
    # end; here it is out of both (changes only unstable traces, e.g. one rejected by eye in
    # CarbonIMTA, 2026-11-10)
    excl <- flag | seq_along(tt) <= early[which.min(conc[early])]
  }
  seg <- .first_linear_ch4(tt, conc, excl, min_s = opt$min_diffusion_s, t_bubble = tt[which(flag)[1]])
  if (is.null(seg$fit)) {
    out[c("total", "ch4_flag")] <- list(window_total, "no linear diffusion segment")
    return(out)
  }
  d <- seg$fit[["slope"]]
  from <- tt >= seg$fit[["start"]]
  if (opt$total_from == "segment" && sum(from) >= 20) {  # 20 readings: fixed, no published source
    tot <- .total_ch4(tt[from], conc[from], opt$spar, opt$nknots_ratio)
    out$total <- tot[["slope"]]
    out[c("total_t0", "total_c0", "total_c1")] <- list(time[from][1], tot[["c0"]], tot[["c1"]])
    out$total_s <- max(tt) - seg$fit[["start"]]
  } else out$total <- window_total
  ebul <- out$total - d
  seg_i <- !excl & tt >= seg$fit[["start"]] & tt <= seg$fit[["end"]]
  # 1 Hz residuals are autocorrelated, which biases the OLS SE (Kutzbach et al. 2007, p. 1011): residual
  # variance with the effective sample size n_e = n (1 - r1) / (1 + r1) (Santer et al. 2000, Eq. 6,
  # p. 7343; AdjSE, p. 7344); NA when n_e <= 2
  e <- stats::residuals(stats::lm(conc[seg_i] ~ tt[seg_i]))
  out$diff_r1 <- if (length(e) > 2) stats::acf(e, lag.max = 1, plot = FALSE, na.action = stats::na.pass)$acf[2] else NA_real_
  ne <- length(e) * (1 - out$diff_r1) / (1 + out$diff_r1)
  out$diff_se <- if (isTRUE(ne > 2)) seg$fit[["se"]] * sqrt((length(e) - 2) / (ne - 2)) else NA_real_
  labels <- c("diffusion > total flux"[d > out$total],
              # no SE (n_e <= 2): ebullition cannot be shown to exceed diffusion + SE, so review
              "ebullition < diffusion + SE"[d <= out$total && !isFALSE(ebul < d + out$diff_se)],
              "no linear diffusion segment"[!seg$ok],
              # Sø et al. 2024 p. 6 and Minaudo et al. 2026 Guideline 4 (p. 10): diffusion after a bubble
              "diffusion after first bubble"[seg$fit[["start"]] > tt[which(flag)[1]]])
  out$ch4_flag <- if (length(labels)) paste(labels, collapse = "; ") else "ok"
  out$diffusion <- d
  out$diff_cor <- stats::cor(tt[seg_i], conc[seg_i])
  out$diff_r2 <- out$diff_cor^2
  out$diff_n <- sum(seg_i)
  out$diff_start <- time[1] + seg$fit[["start"]]
  out$diff_end <- time[1] + seg$fit[["end"]]
  out$ebullition <- if (ebul >= 0 || opt$neg_ebullition == "keep") ebul else
    if (opt$neg_ebullition == "zero") 0 else NA_real_
  out
}
