# Shared private helpers for the enrichment bar + lollipop renderers.
#
# Mirrors python/src/venn_diagram_lab/render/svg.py:760-960 (the v2.2.3
# enrichment-plot section). Constants, _PairStat-equivalent struct,
# nice-tick math, sig-marker thresholds, XML escaping.
#
# All helpers are private (leading dot, @noRd). Used by:
#   - r/R/render-enrichment-bar.R
#   - r/R/render-enrichment-lollipop.R

# ---- Constants (must match python/.../render/svg.py:770-799 byte-for-byte) ----

#' @noRd
.EP_FDR_FLOOR              <- 1e-300
#' @noRd
.EP_COLOR_AXIS             <- "#888888"
#' @noRd
.EP_COLOR_GRID             <- "#e8e8e8"
#' @noRd
.EP_COLOR_TEXT             <- "#222222"
#' @noRd
.EP_COLOR_TEXT_MUTED       <- "#555555"
#' @noRd
.EP_SIG_COLOR              <- "#2e7d32"
#' @noRd
.EP_NS_COLOR               <- "#888888"
#' @noRd
.EP_FONT_FAMILY            <- "Tahoma,sans-serif"
#' @noRd
.EP_SIG_THRESHOLD          <- 0.05
#' @noRd
.EP_SIG_TRIPLE             <- 0.001
#' @noRd
.EP_SIG_DOUBLE             <- 0.01
#' @noRd
.EP_METRIC_NEGLOG10FDR     <- "neglog10fdr"
#' @noRd
.EP_METRIC_FOLDENRICHMENT  <- "foldEnrichment"

#' @noRd
.EP_MARGIN_TOP             <- 24L
#' @noRd
.EP_MARGIN_RIGHT           <- 16L
#' @noRd
.EP_MARGIN_BOTTOM          <- 52L
#' @noRd
.EP_MARGIN_LEFT            <- 48L

#' @noRd
.EP_NICE_TICK_FRACTION_15  <- 1.5
#' @noRd
.EP_NICE_TICK_FRACTION_3   <- 3
#' @noRd
.EP_NICE_TICK_FRACTION_7   <- 7
#' @noRd
.EP_TICK_FORMAT_BIG        <- 100
#' @noRd
.EP_TICK_FORMAT_SMALL      <- 0.1

#' @noRd
.LOLLIPOP_MIN_DOT_R        <- 2.5
#' @noRd
.LOLLIPOP_MAX_DOT_R        <- 8.0
#' @noRd
.LOLLIPOP_BAR_W_MAX        <- 22L

# ---- Helpers ---------------------------------------------------------------

#' @noRd
# Return one of "***" / "**" / "*" / "" for the given FDR. Mirrors
# python `_sig_marker` (svg.py:815-823).
.sig_marker <- function(fdr) {
    if (fdr < .EP_SIG_TRIPLE) return("***")
    if (fdr < .EP_SIG_DOUBLE) return("**")
    if (fdr < .EP_SIG_THRESHOLD) return("*")
    ""
}

#' @noRd
# Human-readable Y-axis label for a given metric. Uses the same Unicode
# minus + subscript digits as the webtool / python for byte-for-byte parity.
.metric_label <- function(metric) {
    if (metric == .EP_METRIC_FOLDENRICHMENT) return("Fold Enrichment")
    "\u2212log\u2081\u2080(FDR)"
}

#' @noRd
# Port of niceTicks (TS) / _nice_ticks (Python). Returns a numeric vector of
# bracketing tick values from 0 to ceil(maxv) with ~`count` ticks.
.nice_ticks <- function(maxv, count = 4L) {
    if (!is.finite(maxv) || maxv <= 0) return(c(0, 1))
    raw <- maxv / count
    pow_ <- 10 ^ floor(log10(raw))
    normalized <- raw / pow_
    step_mult <- if (normalized < .EP_NICE_TICK_FRACTION_15) 1L
                 else if (normalized < .EP_NICE_TICK_FRACTION_3) 2L
                 else if (normalized < .EP_NICE_TICK_FRACTION_7) 5L
                 else 10L
    step <- step_mult * pow_
    ticks <- numeric()
    v <- 0
    # Bounded loop: defensively cap iterations even if floating-point drift
    # makes the < condition borderline (matches python `_nice_ticks`).
    safety_limit <- 100L
    while (v <= maxv + step * 0.0001 && length(ticks) < safety_limit) {
        ticks <- c(ticks, round(v, 10L))
        v <- v + step
    }
    ticks
}

#' @noRd
# Format a tick value following the TS / python `formatTick` rules.
.format_tick <- function(v) {
    if (v == 0) return("0")
    av <- abs(v)
    if (av >= .EP_TICK_FORMAT_BIG || av < .EP_TICK_FORMAT_SMALL) {
        # Mirror JS toExponential(1): "1.5e+2", "5.0e-3"
        exp_ <- floor(log10(av))
        mantissa <- v / (10 ^ exp_)
        sign_ <- if (exp_ >= 0) "+" else "-"
        return(sprintf("%.1fe%s%d", mantissa, sign_, abs(exp_)))
    }
    if (v == as.integer(v)) return(as.character(as.integer(v)))
    sprintf("%.1f", v)
}

#' @noRd
# Minimal XML escape (matches python `_esc` (svg.py:881-889)).
.xml_esc <- function(s) {
    s <- as.character(s)
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;",  s, fixed = TRUE)
    s <- gsub(">", "&gt;",  s, fixed = TRUE)
    s <- gsub('"', "&quot;", s, fixed = TRUE)
    s
}

#' @noRd
# Raise on an unsupported `metric` string.
.validate_metric <- function(metric) {
    if (!(metric %in% c(.EP_METRIC_NEGLOG10FDR, .EP_METRIC_FOLDENRICHMENT))) {
        stop(sprintf(
            "metric must be %s or %s, got %s",
            sQuote(.EP_METRIC_NEGLOG10FDR),
            sQuote(.EP_METRIC_FOLDENRICHMENT),
            sQuote(metric)
        ), call. = FALSE)
    }
}

#' @noRd
# Return the metric value for a single pair-stat list. Mirrors `_metric_value`.
.metric_value <- function(s, metric) {
    if (metric == .EP_METRIC_FOLDENRICHMENT) return(s$fold_enrichment)
    s$neglog10fdr
}

#' @noRd
# Assemble pairwise statistics in canonical (i<j) set-index order. Returns a
# list of one-row lists with fields a, b, label, intersection, fold_enrichment,
# fdr, neglog10fdr. Mirrors python `_collect_pair_stats` (svg.py:892-948).
.collect_pair_stats <- function(result) {
    stats_res <- statistics(result)
    set_names <- result@dataset@set_names
    n <- length(set_names)
    letters_chars <- strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]][seq_len(n)]
    name_to_letter <- setNames(letters_chars, set_names)

    hyp <- stats_res@hypergeometric
    by_pair <- list()
    if (nrow(hyp) > 0L) {
        for (i in seq_len(nrow(hyp))) {
            row <- hyp[i, , drop = FALSE]
            key <- paste(row$set_a, row$set_b, sep = "|")
            by_pair[[key]] <- list(
                intersection = as.integer(row$intersection),
                p_adjusted   = as.numeric(row$p_adjusted)
            )
        }
    }

    out <- list()
    fe_mat <- stats_res@fold_enrichment
    has_fe <- length(fe_mat) > 0L
    for (i in seq_len(n)) {
        if (i == n) break
        for (j in (i + 1L):n) {
            a_name <- set_names[i]
            b_name <- set_names[j]
            payload <- by_pair[[paste(a_name, b_name, sep = "|")]]
            if (is.null(payload)) payload <- by_pair[[paste(b_name, a_name, sep = "|")]]
            if (is.null(payload)) next
            fdr <- max(payload$p_adjusted, .EP_FDR_FLOOR)
            inter <- payload$intersection
            fe <- if (has_fe) as.numeric(fe_mat[i, j]) else 0
            neglog <- -log10(fdr)
            out[[length(out) + 1L]] <- list(
                a = name_to_letter[[a_name]],
                b = name_to_letter[[b_name]],
                label = paste0(name_to_letter[[a_name]], name_to_letter[[b_name]]),
                intersection = inter,
                fold_enrichment = fe,
                fdr = fdr,
                neglog10fdr = neglog
            )
        }
    }
    out
}
