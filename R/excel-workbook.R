# 3-sheet Excel workbook matching the webtool's ZIP-bundle statistics file.
#
# Mirrors python/src/venn_diagram_lab/report/excel.py:1-144.
# Sheets:
#   - "Jaccard"       -- NxN matrix of Jaccard indices.
#   - "Sorensen-Dice" -- NxN matrix of Dice coefficients.
#   - "Enrichment"    -- long-form (9 columns, N*(N-1)/2 rows).

#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
NULL

# ---- Internal constants (must match python excel.py:22-27) -----------------

#' @noRd
.XL_SHEET_JACCARD    <- "Jaccard"
#' @noRd
.XL_SHEET_DICE       <- "S\u00f8rensen-Dice"
#' @noRd
.XL_SHEET_ENRICHMENT <- "Enrichment"
#' @noRd
.XL_HEADER_FILL      <- "DDDDDD"

# ---- Internal helpers ------------------------------------------------------

#' @noRd
.xl_header_style <- function() {
    openxlsx::createStyle(textDecoration = "bold",
                           fgFill = paste0("#", .XL_HEADER_FILL))
}

#' @noRd
# Write an NxN pairwise metric matrix to a worksheet.
# Layout: A1 = "", B1..N+1 = set names, A2..N+1 = set names, body = 4-decimal floats.
.xl_write_square_matrix <- function(wb, sheet, set_names, matrix) {
    n <- length(set_names)
    # Column headers (row 1, cols 2..N+1).
    openxlsx::writeData(wb, sheet, x = matrix(set_names, nrow = 1L),
                         startRow = 1L, startCol = 2L, colNames = FALSE)
    # Row headers (col 1, rows 2..N+1).
    openxlsx::writeData(wb, sheet, x = matrix(set_names, ncol = 1L),
                         startRow = 2L, startCol = 1L, colNames = FALSE)
    # Body (4-decimal numerics).
    body <- as.matrix(matrix)
    storage.mode(body) <- "double"
    openxlsx::writeData(wb, sheet, x = body,
                         startRow = 2L, startCol = 2L, colNames = FALSE)

    header_style <- .xl_header_style()
    body_style <- openxlsx::createStyle(numFmt = "0.0000")
    # Apply header style to row 1 (cols 1..N+1) and col 1 (rows 1..N+1).
    openxlsx::addStyle(wb, sheet, style = header_style,
                        rows = 1L, cols = seq_len(n + 1L), gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, style = header_style,
                        rows = seq_len(n + 1L), cols = 1L, gridExpand = TRUE)
    # Apply 4-decimal numeric format to body cells.
    openxlsx::addStyle(wb, sheet, style = body_style,
                        rows = seq(2L, n + 1L), cols = seq(2L, n + 1L),
                        gridExpand = TRUE)
}

#' @noRd
# Build the long-form enrichment data.frame for the third sheet.
.xl_enrichment_df <- function(result) {
    stats_res <- statistics(result)
    hyp <- stats_res@hypergeometric
    set_sizes <- result@set_sizes
    fe_mat <- stats_res@fold_enrichment

    rows <- list()
    for (i in seq_len(nrow(hyp))) {
        row <- hyp[i, , drop = FALSE]
        a <- row$set_a; b <- row$set_b
        inter <- as.integer(row$intersection)
        union_size <- set_sizes[[a]] + set_sizes[[b]] - inter
        fe <- as.numeric(fe_mat[a, b])
        rows[[length(rows) + 1L]] <- data.frame(
            set_a = a, set_b = b,
            intersection = inter, union = union_size,
            expected = as.numeric(row$expected),
            fold_enrichment = fe,
            p_value = as.numeric(row$p_value),
            fdr = as.numeric(row$p_adjusted),
            significant = as.logical(row$significant),
            stringsAsFactors = FALSE
        )
    }
    if (length(rows) == 0L) {
        return(data.frame(
            set_a = character(), set_b = character(),
            intersection = integer(), union = integer(),
            expected = numeric(), fold_enrichment = numeric(),
            p_value = numeric(), fdr = numeric(),
            significant = logical()
        ))
    }
    do.call(rbind, rows)
}

#' Write a 3-sheet Excel workbook matching the webtool's ZIP bundle
#'
#' Sheets:
#' \itemize{
#'   \item \code{Jaccard} -- NxN matrix of Jaccard indices.
#'   \item \code{Sorensen-Dice} (the actual sheet title uses the o-with-stroke
#'     character) -- NxN matrix of Dice coefficients.
#'   \item \code{Enrichment} -- long-form (\code{set_a}, \code{set_b},
#'     \code{intersection}, \code{union}, \code{expected},
#'     \code{fold_enrichment}, \code{p_value}, \code{fdr},
#'     \code{significant}).
#' }
#'
#' Mirrors the Python `to_excel_workbook()` byte-for-byte in sheet titles,
#' column order, and 4-decimal numeric formatting. Uses \pkg{openxlsx}
#' (pure R, no Java dependency).
#'
#' @param result A [`RegionResult-class`] from [analyze()].
#' @param path Output xlsx file path.
#' @return Invisibly returns `NULL`.
#' @examples
#' \donttest{
#' ds <- load_sample("dataset_real_cancer_drivers_4")
#' res <- analyze(ds)
#' to_excel_workbook(res, tempfile(fileext = ".xlsx"))
#' }
#' @export
to_excel_workbook <- function(result, path) {
    set_names <- result@dataset@set_names
    stats_res <- statistics(result)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, .XL_SHEET_JACCARD)
    openxlsx::addWorksheet(wb, .XL_SHEET_DICE)
    openxlsx::addWorksheet(wb, .XL_SHEET_ENRICHMENT)

    .xl_write_square_matrix(wb, .XL_SHEET_JACCARD,
                              set_names, stats_res@jaccard)
    .xl_write_square_matrix(wb, .XL_SHEET_DICE,
                              set_names, stats_res@dice)

    # Enrichment sheet: long-form 9 columns.
    enr_df <- .xl_enrichment_df(result)
    openxlsx::writeData(wb, .XL_SHEET_ENRICHMENT, x = enr_df,
                         startRow = 1L, startCol = 1L, colNames = TRUE)
    n_rows <- nrow(enr_df)
    header_style <- .xl_header_style()
    openxlsx::addStyle(wb, .XL_SHEET_ENRICHMENT, style = header_style,
                        rows = 1L, cols = seq_len(9L), gridExpand = TRUE)
    if (n_rows > 0L) {
        # Column-specific numeric formats:
        # expected (5) -> "0.00", fold_enrichment (6) -> "0.0000".
        openxlsx::addStyle(wb, .XL_SHEET_ENRICHMENT,
                            style = openxlsx::createStyle(numFmt = "0.00"),
                            rows = seq(2L, n_rows + 1L), cols = 5L,
                            gridExpand = TRUE)
        openxlsx::addStyle(wb, .XL_SHEET_ENRICHMENT,
                            style = openxlsx::createStyle(numFmt = "0.0000"),
                            rows = seq(2L, n_rows + 1L), cols = 6L,
                            gridExpand = TRUE)
    }

    openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
    invisible(NULL)
}
