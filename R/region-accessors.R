# Region accessor sugar (Phase 11 — Feature C).
#
# Three exported helpers over `RegionResult` for the most common questions:
# "which items appear in every set in this group?" (intersection_items),
# "which items appear ONLY in this exact combination?" (exclusive_items),
# "which items appear in any of these sets?" (union_items).
#
# All three accept either set letters ("A", "B", ...) or display names
# (the values of `result@dataset@set_names`). Sets argument is required
# and must be non-empty.

#' @noRd
# Resolve a sets-vector (mix of letters and display names) into the
# corresponding 0-based set indices.
.resolve_set_indices <- function(result, sets) {
    if (length(sets) == 0L) {
        stop("`sets` must contain at least one set identifier.", call. = FALSE)
    }
    set_names <- result@dataset@set_names
    n <- length(set_names)
    letters_chars <- strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]][seq_len(n)]
    name_to_index <- setNames(seq_along(set_names) - 1L, set_names)
    letter_to_index <- setNames(seq_along(letters_chars) - 1L, letters_chars)
    indices <- integer(length(sets))
    for (i in seq_along(sets)) {
        s <- sets[[i]]
        if (s %in% names(letter_to_index)) {
            indices[i] <- letter_to_index[[s]]
        } else if (s %in% names(name_to_index)) {
            indices[i] <- name_to_index[[s]]
        } else {
            stop(sprintf("Unknown set identifier: %s", sQuote(s)), call. = FALSE)
        }
    }
    indices
}

#' @noRd
# Convert a 0-based-index vector into the canonical bitmask.
.indices_to_mask <- function(indices) {
    Reduce(bitwOr, bitwShiftL(1L, indices), 0L)
}

#' Items in the intersection of the named sets
#'
#' Returns items that appear in every set listed in `sets`, regardless of
#' whether they also appear in other (unlisted) sets. For "items in this
#' specific combination only", see [exclusive_items()].
#'
#' @param result A [`RegionResult-class`] from [analyze()].
#' @param sets Character vector of set letters (`"A"`, `"B"`, ...) or display
#'   names (values from `result@dataset@set_names`). May mix both.
#' @return A character vector of item IDs. Empty character if none.
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B", "C"),
#'     items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
#'     item_order = c("x", "y", "z", "w"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' res <- analyze(ds)
#' intersection_items(res, c("A", "B"))   # "y"
#' @export
intersection_items <- function(result, sets) {
    indices <- .resolve_set_indices(result, sets)
    set_names <- result@dataset@set_names
    chosen_names <- set_names[indices + 1L]
    # Items in every chosen set = items in the FIRST chosen set
    # that also appear in every other chosen set.
    first_items <- result@dataset@items[[chosen_names[1L]]]
    if (length(chosen_names) == 1L) return(first_items)
    keep <- vapply(first_items, function(it) {
        all(vapply(chosen_names[-1L],
                    function(nm) it %in% result@dataset@items[[nm]],
                    logical(1L)))
    }, logical(1L))
    first_items[keep]
}

#' Items exclusive to a specific set combination
#'
#' Returns items in EXACTLY the combination of sets listed in `sets` — that
#' is, in every set in `sets` AND in none of the other sets in the dataset.
#'
#' @inheritParams intersection_items
#' @return A character vector of item IDs. Empty character if none.
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B", "C"),
#'     items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
#'     item_order = c("x", "y", "z", "w"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' res <- analyze(ds)
#' exclusive_items(res, c("A", "B"))   # "y"  (in A and B, not in C)
#' @export
exclusive_items <- function(result, sets) {
    indices <- .resolve_set_indices(result, sets)
    mask <- .indices_to_mask(indices)
    region <- result@regions[[as.character(mask)]]
    if (is.null(region)) character(0) else region@exclusive_items
}

#' Items in the union of the named sets
#'
#' Returns items that appear in ANY of the sets listed in `sets`. Equivalent
#' to `unique(c(items_in_A, items_in_B, ...))`.
#'
#' @inheritParams intersection_items
#' @return A character vector of item IDs (deduplicated, first-seen order).
#' @examples
#' ds <- methods::new("VennDataset",
#'     set_names = c("A", "B", "C"),
#'     items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
#'     item_order = c("x", "y", "z", "w"),
#'     universe_size = 10L, source_path = NULL, format = "csv")
#' res <- analyze(ds)
#' union_items(res, c("A", "B"))   # "x", "y", "z"
#' @export
union_items <- function(result, sets) {
    indices <- .resolve_set_indices(result, sets)
    set_names <- result@dataset@set_names
    chosen_names <- set_names[indices + 1L]
    out <- character(0)
    for (nm in chosen_names) {
        out <- c(out, result@dataset@items[[nm]])
    }
    unique(out)
}
