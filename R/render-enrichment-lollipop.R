# Lollipop chart of pairwise enrichment statistics.
#
# Mirrors python/src/venn_diagram_lab/render/svg.py:1115-1265
# (render_enrichment_lollipop_svg). Same data + significance scheme as
# render_enrichment_bar but as a stem-and-dot plot: a vertical line rises
# from the baseline to the metric value, capped by a filled circle whose
# radius scales with `intersection`.

#' Render the pairwise enrichment lollipop chart
#'
#' Same data and significance scheme as [render_enrichment_bar()] but as a
#' stem-and-dot plot: a vertical line rises from the baseline to the metric
#' value, capped by a filled circle whose radius scales with
#' \code{sqrt(intersection / max_intersection)} (range 2.5--8 px).
#'
#' Pure string construction -- no \pkg{xml2}.
#'
#' @param result A [`RegionResult-class`] from [analyze()].
#' @param metric Stem-height metric -- `"neglog10fdr"` or `"foldEnrichment"`.
#' @param width,height Output SVG dimensions in pixels.
#' @return An [`SvgImage-class`] with `content`, `width`, `height`.
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B"),
#'     items = list(A = c("x", "y"), B = c("y", "z")),
#'     item_order = c("x", "y", "z"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' res <- analyze(ds)
#' img <- render_enrichment_lollipop(res)
#' nchar(slot(img, "content")) > 0
#' @export
render_enrichment_lollipop <- function(result,
                                       metric = c("neglog10fdr", "foldEnrichment"),
                                       width = 560L,
                                       height = 240L) {
    if (missing(metric)) metric <- match.arg(metric) else .validate_metric(metric)
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
    max_inter <- max(c(1L, vapply(pairs, function(s) s$intersection, integer(1L))))

    n <- length(pairs)
    slot_w <- plot_w / n

    # Gridlines + Y-axis labels.
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

    # Per-pair stem + dot + significance marker + x-axis rotated label.
    for (i in seq_along(pairs)) {
        s <- pairs[[i]]
        v <- values[i]
        cx <- plot_x + slot_w * (i - 1L) + slot_w / 2
        dot_y <- if (y_max > 0) plot_y + plot_h - (v / y_max) * plot_h else plot_y + plot_h
        color <- if (s$fdr < .EP_SIG_THRESHOLD) .EP_SIG_COLOR else .EP_NS_COLOR
        t_size <- if (max_inter > 0) sqrt(s$intersection / max_inter) else 0
        r <- .LOLLIPOP_MIN_DOT_R + (.LOLLIPOP_MAX_DOT_R - .LOLLIPOP_MIN_DOT_R) * t_size

        parts[length(parts) + 1L] <- sprintf(
            paste0('<line x1="%g" y1="%g" x2="%g" y2="%g" stroke="%s" ',
                   'stroke-width="1.2" opacity="0.85"/>'),
            cx, plot_y + plot_h, cx, dot_y, color)
        parts[length(parts) + 1L] <- sprintf(
            '<circle cx="%g" cy="%g" r="%.2f" fill="%s" opacity="0.9"/>',
            cx, dot_y, r, color)
        marker <- .sig_marker(s$fdr)
        if (nzchar(marker)) {
            parts[length(parts) + 1L] <- sprintf(
                paste0('<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9" ',
                       'text-anchor="middle" font-weight="bold">%s</text>'),
                cx, dot_y - r - 2, .EP_COLOR_TEXT, ff, marker)
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

    # Bottom legend: small + large dot examples.
    legend_y <- height - 12
    parts[length(parts) + 1L] <- sprintf(
        '<circle cx="%g" cy="%g" r="%g" fill="%s"/>',
        plot_x + 4, legend_y - 2, .LOLLIPOP_MIN_DOT_R, .EP_COLOR_TEXT_MUTED)
    parts[length(parts) + 1L] <- sprintf(
        '<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9">small intersection</text>',
        plot_x + 12, legend_y, .EP_COLOR_TEXT_MUTED, ff)
    parts[length(parts) + 1L] <- sprintf(
        '<circle cx="%g" cy="%g" r="%g" fill="%s"/>',
        plot_x + 110, legend_y - 2, .LOLLIPOP_MAX_DOT_R, .EP_COLOR_TEXT_MUTED)
    parts[length(parts) + 1L] <- sprintf(
        '<text x="%g" y="%g" fill="%s" font-family="%s" font-size="9">large intersection</text>',
        plot_x + 122, legend_y, .EP_COLOR_TEXT_MUTED, ff)

    parts[length(parts) + 1L] <- "</svg>"
    methods::new("SvgImage",
        content = paste(parts, collapse = ""),
        width = as.integer(width),
        height = as.integer(height))
}
