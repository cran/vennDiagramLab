test_that("to_excel_workbook writes a 3-sheet xlsx", {
    skip_if_not_installed("openxlsx")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    out <- tempfile(fileext = ".xlsx")
    to_excel_workbook(res, out)
    expect_true(file.exists(out))
    sheets <- openxlsx::getSheetNames(out)
    expect_equal(sheets, c("Jaccard", "Sørensen-Dice", "Enrichment"))
})

test_that("Jaccard sheet has 1 header row + 1 header col + NxN body", {
    skip_if_not_installed("openxlsx")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    out <- tempfile(fileext = ".xlsx")
    to_excel_workbook(res, out)
    df <- openxlsx::read.xlsx(out, sheet = "Jaccard", colNames = TRUE, rowNames = TRUE)
    expect_equal(rownames(df), c("A", "B", "C"))
    expect_equal(colnames(df), c("A", "B", "C"))
    # Diagonal is self-Jaccard = 1 (set vs itself), since
    # statistics(result)@jaccard[i,i] is 1 by definition.
    expect_equal(unname(diag(as.matrix(df))), c(1, 1, 1))
})

test_that("Enrichment sheet has 9 columns + N(N-1)/2 rows", {
    skip_if_not_installed("openxlsx")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    out <- tempfile(fileext = ".xlsx")
    to_excel_workbook(res, out)
    df <- openxlsx::read.xlsx(out, sheet = "Enrichment", colNames = TRUE)
    expect_equal(colnames(df),
                 c("set_a", "set_b", "intersection", "union",
                   "expected", "fold_enrichment", "p_value", "fdr", "significant"))
    expect_equal(nrow(df), 3L)  # 3-set -> 3 pairs
})
