# Bar chart of pairwise enrichment statistics.
#
# Mirrors python/src/venn_diagram_lab/render/svg.py:967-1112
# (render_enrichment_bar_svg). Pure string construction -- same pattern as
# render_share_distribution / render_cluster_heatmap. Constants and
# helper functions live in r/R/enrichment-plot-helpers.R.
#
# Public entry point: render_enrichment_bar.

#' Render the pairwise enrichment bar chart
#'
#' One bar per pairwise statistic, ordered by `(set_a_index, set_b_index)`.
#' Bar height is proportional to the chosen `metric`:
#' \itemize{
#'   \item `"neglog10fdr"` (default): \eqn{-\log_{10}}(BH-FDR), floor 1e-300.
#'   \item `"foldEnrichment"`: \eqn{(k \cdot N) / (K \cdot n)} from
#'     [StatisticsResult-class]'s `fold_enrichment` slot.
#' }
#'
#' Bars use \code{#2e7d32} for significant pairs (FDR < 0.05) and
#' \code{#888888} otherwise. Significance markers `***` (< 0.001),
#' `**` (< 0.01), and `*` (< 0.05) appear above each bar.
#'
#' Pure string construction -- no \pkg{xml2}.
#'
#' @param result A [`RegionResult-class`] from [analyze()].
#' @param metric Bar-height metric -- `"neglog10fdr"` or `"foldEnrichment"`.
#' @param width,height Output SVG dimensions in pixels.
#' @return An [`SvgImage-class`] with `content`, `width`, `height`.
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B"),
#'     items = list(A = c("x", "y"), B = c("y", "z")),
#'     item_order = c("x", "y", "z"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' res <- analyze(ds)
#' img <- render_enrichment_bar(res)
#' nchar(slot(img, "content")) > 0
#' @export
render_enrichment_bar <- function(result,
                                  metric = c("neglog10fdr", "foldEnrichment"),
                                  width = 560L,
                                  height = 240L) {
    # Validate first (produces the "metric must be" message tested below).
    # For the default missing() case, match.arg picks the first choice.
    if (missing(metric)) {
        metric <- match.arg(metric)
    } else {
        .validate_metric(metric)
    }
    pairs <- .collect_pair_stats(result)

    plot_x <- .EP_MARGIN_LEFT
    plot_y <- .EP_MARGIN_TOP
    plot_w <- width - .EP_MARGIN_LEFT - .EP_MARGIN_RIGHT
    plot_h <- height - .EP_MARGIN_TOP - .EP_MARGIN_BOTTOM
    ff <- .EP_FONT_FAMILY

    parts <- character()
    parts[length(parts) + 1L] <- sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d">',
        width, height, width, height)
    parts[length(parts) + 1L] <- sprintf(
        '<rect width="%d" height="%d" fill="#ffffff"/>', width, height)

    if (length(pairs) == 0L) {
        parts[length(parts) + 1L] <- sprintf(
            paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="11" ',
                   'text-anchor="middle">No pairs to plot</text>'),
            width / 2, height / 2, .EP_COLOR_TEXT_MUTED, ff)
        parts[length(parts) + 1L] <- "</svg>"
        return(methods::new("SvgImage",
            content = paste(parts, collapse = ""),
            width = as.integer(width),
            height = as.integer(height)))
    }

    values <- vapply(pairs, function(s) .metric_value(s, metric), numeric(1L))
    max_val <- max(c(0, values))
    ticks <- .nice_ticks(if (max_val > 0) max_val else 1)
    y_max <- max(c(max_val, if (length(ticks) > 0) ticks[length(ticks)] else 1))

    n <- length(pairs)
    slot_w <- plot_w / n
    bar_w <- min(.LOLLIPOP_BAR_W_MAX, slot_w * 0.7)

    # Gridlines + Y-axis tick labels.
    for (t in ticks) {
        y <- if (y_max > 0) plot_y + plot_h - (t / y_max) * plot_h else plot_y + plot_h
        parts[length(parts) + 1L] <- sprintf(
            '<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="1"/>',
            plot_x, y, plot_x + plot_w, y, .EP_COLOR_GRID)
        parts[length(parts) + 1L] <- sprintf(
            paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9" ',
                   'text-anchor="end">%s</text>'),
            plot_x - 4, y + 3, .EP_COLOR_TEXT_MUTED, ff, .xml_esc(.format_tick(t)))
    }

    # Y / X axis lines.
    parts[length(parts) + 1L] <- sprintf(
        '<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="1"/>',
        plot_x, plot_y, plot_x, plot_y + plot_h, .EP_COLOR_AXIS)
    parts[length(parts) + 1L] <- sprintf(
        '<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" stroke-width="1"/>',
        plot_x, plot_y + plot_h, plot_x + plot_w, plot_y + plot_h, .EP_COLOR_AXIS)

    # Per-pair bar + significance marker + x-axis rotated label.
    for (i in seq_along(pairs)) {
        s <- pairs[[i]]
        v <- values[i]
        cx <- plot_x + slot_w * (i - 1L) + slot_w / 2
        bar_h <- if (y_max > 0) max(0, (v / y_max) * plot_h) else 0
        y <- plot_y + plot_h - bar_h
        color <- if (s$fdr < .EP_SIG_THRESHOLD) .EP_SIG_COLOR else .EP_NS_COLOR
        parts[length(parts) + 1L] <- sprintf(
            paste0('<rect x="%g" y="%g" width="%g" height="%g" rx="1.5" ',
                   'fill="%s" opacity="0.85"/>'),
            cx - bar_w / 2, y, bar_w, bar_h, color)
        marker <- .sig_marker(s$fdr)
        if (nzchar(marker)) {
            parts[length(parts) + 1L] <- sprintf(
                paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9" ',
                       'text-anchor="middle" font-weight="bold">%s</text>'),
                cx, y - 3, .EP_COLOR_TEXT, ff, marker)
        }
        lx <- cx
        ly <- plot_y + plot_h + 10
        parts[length(parts) + 1L] <- sprintf(
            paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9" ',
                   'text-anchor="end" transform="rotate(-45 %g %g)">%s</text>'),
            lx, ly, .EP_COLOR_TEXT, ff, lx, ly, .xml_esc(s$label))
    }

    # Y-axis title (rotated 90 deg).
    y_label_x <- 14
    y_label_y <- plot_y + plot_h / 2
    parts[length(parts) + 1L] <- sprintf(
        paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="10" ',
               'font-weight="bold" text-anchor="middle" ',
               'transform="rotate(-90 %g %g)">%s</text>'),
        y_label_x, y_label_y, .EP_COLOR_TEXT, ff, y_label_x, y_label_y,
        .xml_esc(.metric_label(metric)))

    # Bottom legend: significant + non-significant swatches.
    legend_y <- height - 12
    parts[length(parts) + 1L] <- sprintf(
        '<rect x="%g" y="%g" width="8" height="8" fill="%s" opacity="0.85"/>',
        plot_x, legend_y - 6, .EP_SIG_COLOR)
    parts[length(parts) + 1L] <- sprintf(
        '<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9">FDR &lt; 0.05</text>',
        plot_x + 12, legend_y, .EP_COLOR_TEXT_MUTED, ff)
    parts[length(parts) + 1L] <- sprintf(
        '<rect x="%g" y="%g" width="8" height="8" fill="%s" opacity="0.85"/>',
        plot_x + 70, legend_y - 6, .EP_NS_COLOR)
    parts[length(parts) + 1L] <- sprintf(
        '<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9">not significant</text>',
        plot_x + 82, legend_y, .EP_COLOR_TEXT_MUTED, ff)

    parts[length(parts) + 1L] <- "</svg>"
    methods::new("SvgImage",
        content = paste(parts, collapse = ""),
        width = as.integer(width),
        height = as.integer(height))
}
