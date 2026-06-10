# UpSet plot rendering via ComplexUpset (ggplot2-based). Data shaping mirrors
# python/src/venn_diagram_lab/render/upset.py; rendering uses ComplexUpset
# instead of matplotlib (idiomatic R; not a 1:1 port).
#
# All helpers private (leading dot, @noRd). Single public entry point:
# render_upset (Task A3).

#' @importFrom ggplot2 ggplot scale_fill_manual aes .data
#' @importFrom ComplexUpset upset upset_default_themes intersection_size
NULL

#' @noRd
# Emit a runtime warning when run on R < 4.6 about the ComplexUpset 1.3.3
# incompatibility with ggplot2 4.0+. The bug causes both render_upset() and
# to_pdf_report() (which embeds an UpSet panel) to fail with
# "axis.title.x must be element_text" during patchwork compose / print.
# Tracking upstream issue: https://github.com/krassowski/complex-upset/issues/213
.warn_if_oldrel_complex_upset <- function(r_version = getRversion()) {
    if (r_version < "4.6") {
        warning(
            "ComplexUpset 1.3.3 (current CRAN release) is broken with ggplot2 4.0+ ",
            "on R < 4.6 -- this rendering call may fail with ",
            "'axis.title.x must be element_text'. ",
            "Workarounds: (a) upgrade to R >= 4.6, OR (b) install dev ComplexUpset via ",
            "remotes::install_github('krassowski/complex-upset'). ",
            "Tracking: https://github.com/krassowski/complex-upset/issues/213",
            call. = FALSE
        )
    }
}

#' @noRd
.upset_data_from_region_result <- function(result) {
    n <- length(result@dataset@set_names)
    letters_chars <- strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]][seq_len(n)]
    intersections <- list()
    for (region in result@regions) {
        size <- length(region@exclusive_items)
        if (size == 0L) next
        members <- letters_chars[region@set_indices + 1L]
        intersections <- c(intersections, list(
            list(members = members, size = as.integer(size), label = region@label)
        ))
    }
    list(sets = letters_chars, intersections = intersections)
}

#' @noRd
.sort_by_size <- function(data) {
    sizes <- vapply(data$intersections, function(x) x$size, integer(1L))
    data$intersections[order(sizes, decreasing = TRUE)]
}

#' @noRd
.sort_by_degree <- function(data) {
    degrees <- vapply(data$intersections, function(x) length(x$members), integer(1L))
    labels  <- vapply(data$intersections, function(x) x$label, character(1L))
    data$intersections[order(degrees, labels)]
}

#' @noRd
# Per-bar colors keyed by mode. Mirrors python _bar_colors:
# "custom" -> user-supplied named map, fallback "#cccccc"
# "heatmap" -> grDevices Reds palette interpolated by size (uniform -> "#888888")
# "depth" -> viridis palette interpolated by membership degree (uniform -> "#444444")
.bar_colors <- function(intersections, mode, custom) {
    if (mode == "custom") {
        custom <- if (is.null(custom)) character(0) else custom
        return(vapply(intersections, function(i) {
            v <- custom[i$label]
            if (length(v) == 0L || is.na(v)) "#cccccc" else v
        }, character(1L)))
    }
    if (mode == "heatmap") {
        sizes <- vapply(intersections, function(x) x$size, integer(1L))
        if (length(sizes) == 0L || max(sizes) == min(sizes)) {
            return(rep("#888888", length(intersections)))
        }
        # Reds palette via grDevices::colorRampPalette on a 5-stop Reds gradient.
        ramp <- grDevices::colorRampPalette(c("#FFF5F0", "#FCBBA1", "#FB6A4A", "#CB181D", "#67000D"))
        n_steps <- 256L
        palette <- ramp(n_steps)
        idx <- round((sizes - min(sizes)) / (max(sizes) - min(sizes)) * (n_steps - 1L)) + 1L
        return(palette[idx])
    }
    # depth: viridis on degree
    degrees <- vapply(intersections, function(x) length(x$members), integer(1L))
    if (length(degrees) == 0L || max(degrees) == min(degrees)) {
        return(rep("#444444", length(intersections)))
    }
    ramp <- grDevices::colorRampPalette(c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725"))
    n_steps <- 256L
    palette <- ramp(n_steps)
    idx <- round((degrees - min(degrees)) / (max(degrees) - min(degrees)) * (n_steps - 1L)) + 1L
    palette[idx]
}

#' Render an UpSet plot from a RegionResult
#'
#' Builds a ComplexUpset ggplot showing intersection sizes (top bars), set
#' membership matrix (middle dot grid), and per-set sizes (left bars).
#' Idiomatic R port of Python `render_upset` -- same parameter contract, but
#' renders via ComplexUpset (ggplot2) instead of matplotlib (not a 1:1 port).
#'
#' @param result A [`RegionResult-class`].
#' @param max_columns Maximum number of intersections to display (default 20).
#'   Top-N by the active sort.
#' @param sort_by `"size"` (default -- descending) or `"degree"` (membership
#'   count ascending then alphabetical).
#' @param threshold Exclude intersections with size strictly below this value
#'   (default 0L = no filter).
#' @param color_mode `"depth"` (default -- viridis on degree), `"heatmap"`
#'   (Reds on size), or `"custom"` (use the `colors` mapping).
#' @param colors Named character vector mapping intersection LABELS (e.g.
#'   `"AB"`) to fill hex colors when `color_mode = "custom"`. Unspecified
#'   labels fall back to `"#cccccc"`.
#' @return A `ggplot` object (saveable via `ggplot2::ggsave()`).
#' @export
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B"),
#'     items = list(A = c("x", "y"), B = c("y", "z")),
#'     item_order = c("x", "y", "z"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' result <- analyze(ds)
#' if (getRversion() >= "4.6") {
#'   p <- render_upset(result)
#'   inherits(p, "ggplot")
#' }
#' \donttest{
#' if (getRversion() >= "4.6") {
#'   result <- analyze(load_sample("dataset_real_cancer_drivers_4"))
#'   p <- render_upset(result, sort_by = "degree", color_mode = "heatmap")
#'   ggplot2::ggsave(tempfile(fileext = ".png"), p, width = 8, height = 5)
#' }
#' }
render_upset <- function(result,
                          max_columns = 20L,
                          sort_by = c("size", "degree"),
                          threshold = 0L,
                          color_mode = c("depth", "heatmap", "custom"),
                          colors = NULL) {
    sort_by    <- match.arg(sort_by)
    color_mode <- match.arg(color_mode)
    .warn_if_oldrel_complex_upset()

    data <- .upset_data_from_region_result(result)
    sorter <- if (sort_by == "size") .sort_by_size else .sort_by_degree
    intersections <- sorter(data)
    if (threshold > 0L) {
        intersections <- intersections[
            vapply(intersections, function(x) x$size >= as.integer(threshold), logical(1L))
        ]
    }
    intersections <- head(intersections, max_columns)

    # Convert to ComplexUpset's expected long-form: one row per item, columns =
    # set names (boolean), plus a Region column for grouping. Since our items
    # are dedup'd per-region (exclusive_items), we expand each intersection
    # by its size: replicate the membership row `size` times.
    #
    # Column-name strategy: ComplexUpset renders the matrix-row labels from the
    # column names of the supplied data frame. We use "A -- Vogelstein"-style
    # labels (letter + en-dash + display name) so the UpSet panel matches the
    # Python matplotlib renderer instead of showing bare letters. The mapping
    # from short label -> display label is also applied to bar fill colours
    # and to the input intersection labels.
    sets <- data$sets
    set_display_names <- result@dataset@set_names
    set_labels <- vapply(
        seq_along(sets),
        function(i) sprintf("%s -- %s", sets[i], set_display_names[[i]]),
        character(1L)
    )
    letter_to_label <- setNames(set_labels, sets)

    # Build data frame column-wise so column names with "--" / spaces survive
    # (`rbind.data.frame()` runs the names through `make.names()`, which would
    # mangle them into things like "A....Vogelstein" and break the `intersect`
    # lookup downstream).
    n_total <- if (length(intersections) == 0L) 1L
               else sum(vapply(intersections, function(x) x$size, integer(1L)))
    df <- as.data.frame(
        matrix(FALSE, nrow = n_total, ncol = length(set_labels)),
        stringsAsFactors = FALSE
    )
    names(df) <- set_labels  # direct attribute set; does NOT pass through make.names
    df$Region <- character(n_total)

    if (length(intersections) == 0L) {
        df$Region[1L] <- "(empty)"
    } else {
        row_idx <- 1L
        for (inter in intersections) {
            size <- inter$size
            end_idx <- row_idx + size - 1L
            membership <- sets %in% inter$members
            for (j in seq_along(set_labels)) {
                df[row_idx:end_idx, j] <- membership[j]
            }
            df$Region[row_idx:end_idx] <- inter$label
            row_idx <- end_idx + 1L
        }
    }

    bar_colors <- .bar_colors(intersections, mode = color_mode, custom = colors)
    label_to_color <- setNames(bar_colors, vapply(intersections, function(x) x$label, character(1L)))

    # Wrap construction in withCallingHandlers to swallow the upstream
    # ComplexUpset 1.3.x `size` -> `linewidth` deprecation warning that fires
    # during `intersection_size()`. The warning is upstream-only and not
    # actionable for users of vennDiagramLab.
    plot <- withCallingHandlers(
        ComplexUpset::upset(
            df,
            intersect = set_labels,
            name = "Set membership",
            base_annotations = list(
                "Intersection size" = ComplexUpset::intersection_size(
                    mapping = ggplot2::aes(fill = .data$Region)
                ) + ggplot2::scale_fill_manual(values = label_to_color, guide = "none")
            ),
            # Rotate set-size x-axis labels 45 degrees so the 5-6 default
            # ticks don't pile up on a half-page-wide panel. Replacing the
            # underlying scale via `+ scale_x_reverse(breaks=...)` is not
            # honoured by ComplexUpset's internal scale builder, so this is
            # the pragmatic alternative.
            set_sizes = ComplexUpset::upset_set_size() +
                ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = 45, hjust = 1, vjust = 1, size = 7
                    )
                ),
            n_intersections = max_columns,
            sort_intersections_by = if (sort_by == "size") "cardinality" else "degree",
            # width_ratio: width of the set-size panel relative to the dot
            # matrix. Default 0.3 squeezes the size axis labels into a single
            # jumble at small page sizes; 0.25 keeps it readable while still
            # leaving most of the canvas for the matrix.
            width_ratio = 0.25,
            # height_ratio: height of the intersection-size panel relative
            # to the dot matrix. Default 0.5 leaves a tiny matrix below the
            # giant bars on a 4-set diagram; 0.7 evens it out.
            height_ratio = 0.7
        ),
        warning = function(w) {
            msg <- conditionMessage(w)
            if (grepl("Using `size` aesthetic for lines was deprecated",
                      msg, fixed = TRUE)) {
                invokeRestart("muffleWarning")
            }
        }
    )
    plot
}
