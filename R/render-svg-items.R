# Private engines for Phase 11 features A (show_items) and B (highlight)
# in render_venn_svg().
#
# The item-text engine (.render_items_in_regions) replaces a region's
# Count_<label> text node with a series of <tspan> elements (one per
# item, optionally column-paginated, optionally truncated when the
# region holds more items than `max_items_per_region`).
#
# The highlight engine (.apply_highlight) walks the Shape*/Shape*2 nodes
# and rewrites inline `fill:` styles: regions in the keep-set retain
# their original colour, all others get a desaturated grey fill.
#
# Both helpers operate on the same xml2-parsed root used by
# render_venn_svg(); they mutate in place and return invisibly.

#' @noRd
.ITEMS_DEFAULTS <- list(
    max_items_per_region = 20L,
    ncol_items = 1L,
    truncate_long_names = 12L,
    line_height = 10L,
    font_size = 8L,
    show_counts_with_items = FALSE,
    ellipsis = "..."
)

#' @noRd
# Merge user-supplied item_options with defaults; ignore unknown keys with a
# warning so typos are caught (matches the existing style = NULL pattern in
# render_share_distribution).
.resolve_item_options <- function(user_opts) {
    if (is.null(user_opts)) return(.ITEMS_DEFAULTS)
    if (!is.list(user_opts)) {
        stop("`item_options` must be a list or NULL.", call. = FALSE)
    }
    unknown <- setdiff(names(user_opts), names(.ITEMS_DEFAULTS))
    if (length(unknown) > 0L) {
        warning(sprintf("Ignoring unknown item_options: %s",
                         paste(sQuote(unknown), collapse = ", ")),
                call. = FALSE)
    }
    out <- .ITEMS_DEFAULTS
    for (k in intersect(names(user_opts), names(.ITEMS_DEFAULTS))) {
        out[[k]] <- user_opts[[k]]
    }
    out
}

#' @noRd
# Truncate a single item label to `max_chars`. If truncated, append the
# ellipsis. `max_chars = 0L` disables truncation entirely.
.truncate_item <- function(s, max_chars, ellipsis) {
    if (max_chars <= 0L || nchar(s) <= max_chars) return(s)
    paste0(substr(s, 1L, max_chars), ellipsis)
}

#' @noRd
# Lay items out into `ncol_items` columns by row-major order. Returns a
# list of rows where each row is a character vector of length `ncol_items`
# (padded with empty strings on the last row if needed).
.layout_items_columns <- function(items, ncol_items) {
    if (ncol_items <= 1L) return(lapply(items, function(x) x))
    n <- length(items)
    n_rows <- ceiling(n / ncol_items)
    rows <- vector("list", n_rows)
    for (r in seq_len(n_rows)) {
        start <- (r - 1L) * ncol_items + 1L
        end <- min(r * ncol_items, n)
        cells <- items[start:end]
        if (length(cells) < ncol_items) {
            cells <- c(cells, rep("", ncol_items - length(cells)))
        }
        rows[[r]] <- cells
    }
    rows
}

#' @noRd
# Build the SVG text content for one region: returns the inner content
# (one or more <tspan> children, one per visual row).
.build_region_text <- function(items, count, opts) {
    items_to_show <- items
    truncated_n <- 0L
    if (length(items_to_show) > opts$max_items_per_region) {
        truncated_n <- length(items_to_show) - opts$max_items_per_region
        items_to_show <- items_to_show[seq_len(opts$max_items_per_region)]
    }
    items_to_show <- vapply(items_to_show, .truncate_item,
                             max_chars = opts$truncate_long_names,
                             ellipsis = opts$ellipsis, character(1L))
    rows <- .layout_items_columns(items_to_show, opts$ncol_items)
    body <- character(0)
    if (isTRUE(opts$show_counts_with_items)) {
        body <- c(body, sprintf('<tspan x="0" dy="0" font-weight="bold">%d</tspan>',
                                 count))
        first_dy <- opts$line_height
    } else {
        first_dy <- 0L
    }
    for (i in seq_along(rows)) {
        row <- rows[[i]]
        dy <- if (i == 1L && length(body) == 0L) first_dy else opts$line_height
        body <- c(body, sprintf('<tspan x="0" dy="%d">%s</tspan>',
                                 dy, paste(row, collapse = "  ")))
    }
    if (truncated_n > 0L) {
        body <- c(body, sprintf('<tspan x="0" dy="%d" font-style="italic">+%d more</tspan>',
                                 opts$line_height, truncated_n))
    }
    paste(body, collapse = "")
}

#' @noRd
# Replace a single Count_<label> text node's content with the multi-tspan
# rendering for the given region. The Count_* node retains its position
# attributes (x, y); only the children change.
.replace_count_with_items <- function(root, count_id, items, count, opts, font_size) {
    el <- .find_by_id(root, count_id)
    if (is.null(el)) return(invisible(NULL))
    # Replace inner content. We rebuild the text node's children via the
    # xml_set_text + xml_add_child pattern: drop existing children, then
    # parse a wrapper <g> with our tspan list, transfer its children in.
    xml2::xml_set_text(el, "")
    # Remove all existing children (kids may include a single text node).
    kids <- xml2::xml_children(el)
    for (k in kids) xml2::xml_remove(k)
    # Build a wrapper document so xml2 parses the tspans.
    wrapper <- sprintf(
        '<text font-size="%d" xmlns="http://www.w3.org/2000/svg">%s</text>',
        font_size, .build_region_text(items, count, opts)
    )
    parsed <- xml2::read_xml(wrapper)
    for (child in xml2::xml_children(parsed)) {
        # xml_add_child copies, so we can safely add from a different doc.
        xml2::xml_add_child(el, child)
    }
    # Mirror the font-size onto the original Count_* element too so the
    # text scales correctly.
    xml2::xml_set_attr(el, "font-size", as.character(font_size))
    invisible(NULL)
}

#' @noRd
# Public-private boundary: called from render_venn_svg() when show_items=TRUE.
# Iterates over every non-empty region and writes its items into the
# corresponding Count_<label> node.
.render_items_in_regions <- function(root, result, item_options) {
    opts <- .resolve_item_options(item_options)
    for (region in result@regions) {
        items <- region@exclusive_items
        # Even empty regions stay blank (Count_* already got "0" or "" from
        # .apply_counts; show_items overrides with a blank tspan path).
        if (length(items) == 0L) {
            count_id <- sprintf("Count_%s", region@label)
            el <- .find_by_id(root, count_id)
            if (!is.null(el)) xml2::xml_set_text(el, "")
            next
        }
        .replace_count_with_items(
            root,
            sprintf("Count_%s", region@label),
            items,
            length(items),
            opts,
            font_size = opts$font_size
        )
    }
    invisible(NULL)
}

# ---- Feature B: highlight engine -------------------------------------------

#' @noRd
.HIGHLIGHT_GREY <- "#cccccc"
#' @noRd
.HIGHLIGHT_OPACITY <- "0.25"

#' @noRd
# Convert a user-supplied highlight argument into a character vector of
# region labels ("AB", "ABC"). Accepts:
#   - character vector of labels: passed through after a validity check
#   - integer vector of bitmasks: converted via region@label lookup
.resolve_highlight_labels <- function(highlight, result) {
    if (is.null(highlight)) return(character(0))
    n <- length(result@dataset@set_names)
    letters_chars <- strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]][seq_len(n)]
    max_mask <- bitwShiftL(1L, n) - 1L
    mask_to_label <- function(m) {
        bits <- which(vapply(seq_len(n),
                              function(i) bitwAnd(m, bitwShiftL(1L, i - 1L)) != 0L,
                              logical(1L)))
        paste(letters_chars[bits], collapse = "")
    }
    if (is.numeric(highlight)) {
        masks <- as.integer(highlight)
        labels <- character(length(masks))
        for (i in seq_along(masks)) {
            m <- masks[i]
            if (is.na(m) || m < 1L || m > max_mask) {
                stop(sprintf("highlight: no region with mask %d", m),
                     call. = FALSE)
            }
            labels[i] <- mask_to_label(m)
        }
        return(labels)
    }
    if (is.character(highlight)) {
        # Validate by reconstructing valid labels from all bitmasks
        # 1..max_mask (covers empty and non-empty regions alike).
        all_labels <- vapply(seq_len(max_mask), mask_to_label, character(1L))
        unknown <- setdiff(highlight, all_labels)
        if (length(unknown) > 0L) {
            stop(sprintf("highlight: unknown region label(s): %s",
                          paste(sQuote(unknown), collapse = ", ")),
                 call. = FALSE)
        }
        return(highlight)
    }
    stop("`highlight` must be NULL, a character vector of region labels, or an integer vector of bitmasks.",
         call. = FALSE)
}

#' @noRd
# Walk all letter-named Shape* nodes; for sets NOT contributing to any
# highlighted region, rewrite the fill colour to grey and reduce opacity.
# A set "contributes" if at least one of its bits is set in at least one
# highlighted region's mask.
.apply_highlight <- function(root, result, highlight) {
    labels <- .resolve_highlight_labels(highlight, result)
    if (length(labels) == 0L) return(invisible(NULL))
    n <- length(result@dataset@set_names)
    letters_chars <- strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]][seq_len(n)]

    # Compute the union mask of all highlighted regions.
    label_to_mask <- function(lbl) {
        chars <- strsplit(lbl, "", fixed = TRUE)[[1L]]
        Reduce(bitwOr,
               vapply(chars,
                       function(ch) bitwShiftL(1L, match(ch, letters_chars) - 1L),
                       integer(1L)),
               0L)
    }
    union_mask <- Reduce(bitwOr,
                          vapply(labels, label_to_mask, integer(1L)),
                          0L)

    # A set whose bit is NOT in union_mask gets desaturated.
    for (i in seq_len(n)) {
        bit <- bitwShiftL(1L, i - 1L)
        if (bitwAnd(union_mask, bit) != 0L) next
        letter <- letters_chars[i]
        for (id in c(sprintf("Shape%s", letter), sprintf("Shape%s2", letter))) {
            el <- .find_by_id(root, id)
            if (is.null(el)) next
            style <- xml2::xml_attr(el, "style")
            if (is.na(style) || nchar(style) == 0L) next
            new_style <- sub(.FILL_RE, sprintf("fill:%s", .HIGHLIGHT_GREY),
                              style, perl = FALSE)
            # Also reduce opacity, replacing the existing opacity:X.XX if present.
            if (grepl("opacity:", new_style, fixed = TRUE)) {
                new_style <- sub("opacity:\\s*[0-9.]+",
                                  sprintf("opacity:%s", .HIGHLIGHT_OPACITY),
                                  new_style)
            } else {
                new_style <- paste0(new_style, ";opacity:", .HIGHLIGHT_OPACITY)
            }
            xml2::xml_set_attr(el, "style", new_style)
        }
    }
    invisible(NULL)
}
