test_that(".split_line splits CSV with quoted fields", {
    expect_equal(.split_line("a,b,c", ","), c("a", "b", "c"))
    expect_equal(.split_line('a,"b,c",d', ","), c("a", "b,c", "d"))
    expect_equal(.split_line('a,"b ""inner"" c",d', ","), c("a", 'b "inner" c', "d"))
    expect_equal(.split_line("  a , b ", ","), c("a", "b"))   # trims whitespace
})

test_that(".split_line works with tab delimiter", {
    expect_equal(.split_line("a\tb\tc", "\t"), c("a", "b", "c"))
})

test_that(".split_line handles trailing delimiter and empty input", {
    expect_equal(.split_line("a,", ","), c("a", ""))
    expect_equal(.split_line("", ","), "")
})

test_that(".detect_delimiter picks comma for CSV", {
    expect_equal(.detect_delimiter("a,b,c\n1,2,3\n"), ",")
})

test_that(".detect_delimiter picks tab for TSV", {
    expect_equal(.detect_delimiter("a\tb\tc\n1\t2\t3\n"), "\t")
})

test_that(".detect_delimiter falls back to comma for ambiguous input", {
    expect_equal(.detect_delimiter("singletoken\nanother\n"), ",")
})

test_that(".detect_delimiter falls back to comma on empty input", {
    expect_equal(.detect_delimiter(""), ",")
})

test_that(".parse_table returns headers + rows", {
    res <- .parse_table("a,b,c\n1,2,3\n4,5,6", ",")
    expect_equal(res$headers, c("a", "b", "c"))
    expect_equal(length(res$rows), 2L)
    expect_equal(res$rows[[1L]], c("1", "2", "3"))
})

test_that(".parse_table errors on too-few lines", {
    expect_error(.parse_table("just one line", ","), class = "InvalidDatasetError")
})

test_that(".binary_columns_to_dataset builds a Dataset with universe_size", {
    headers <- c("item", "SetA", "SetB")
    rows <- list(
        c("g1", "1", "0"),
        c("g2", "1", "1"),
        c("g3", "0", "1")
    )
    ds <- .binary_columns_to_dataset(headers, rows, "test.csv", "csv", prefix_cols = 1L)
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@set_names, c("SetA", "SetB"))
    expect_setequal(ds@items$SetA, c("g1", "g2"))
    expect_setequal(ds@items$SetB, c("g2", "g3"))
    expect_equal(ds@item_order, c("g1", "g2", "g3"))
    expect_equal(ds@universe_size, 3L)
    expect_equal(ds@format, "csv")
})

test_that(".binary_columns_to_dataset accepts true/false/yes/no values", {
    headers <- c("item", "SetA", "SetB")
    rows <- list(
        c("g1", "true", "no"),
        c("g2", "yes", "false")
    )
    ds <- .binary_columns_to_dataset(headers, rows, "test.csv", "csv", prefix_cols = 1L)
    expect_setequal(ds@items$SetA, c("g1", "g2"))
    expect_equal(length(ds@items$SetB), 0L)
})

test_that(".binary_columns_to_dataset rejects invalid values", {
    headers <- c("item", "SetA", "SetB")
    rows <- list(c("g1", "maybe", "0"))
    expect_error(
        .binary_columns_to_dataset(headers, rows, "test.csv", "csv", prefix_cols = 1L),
        class = "InvalidDatasetError"
    )
})

test_that(".binary_columns_to_dataset rejects too-few set columns", {
    headers <- c("item", "SetA")
    rows <- list(c("g1", "1"))
    expect_error(
        .binary_columns_to_dataset(headers, rows, "test.csv", "csv", prefix_cols = 1L),
        class = "InvalidDatasetError"
    )
})

test_that(".binary_columns_to_dataset respects prefix_cols = 2L", {
    headers <- c("title", "type", "SetA", "SetB")
    rows <- list(
        c("Movie1", "film",   "1", "0"),
        c("Movie2", "series", "0", "1")
    )
    ds <- .binary_columns_to_dataset(headers, rows, "t.csv", "csv", prefix_cols = 2L)
    expect_equal(ds@set_names, c("SetA", "SetB"))
    expect_setequal(ds@items$SetA, "Movie1")
    expect_setequal(ds@items$SetB, "Movie2")
    expect_equal(ds@universe_size, 2L)
})

test_that(".binary_columns_to_dataset counts all-zero-membership rows in universe_size", {
    headers <- c("item", "SetA", "SetB")
    rows <- list(
        c("in_a",       "1", "0"),
        c("in_b",       "0", "1"),
        c("in_neither", "0", "0"),
        c("in_both",    "1", "1")
    )
    ds <- .binary_columns_to_dataset(headers, rows, "t.csv", "csv", prefix_cols = 1L)
    expect_equal(ds@universe_size, 4L)
    expect_equal(ds@item_order, c("in_a", "in_b", "in_neither", "in_both"))
    expect_false("in_neither" %in% ds@items$SetA)
    expect_false("in_neither" %in% ds@items$SetB)
})

test_that(".aggregated_columns_to_dataset treats each column as a set", {
    headers <- c("Pathway1", "Pathway2", "Pathway3")
    rows <- list(
        c("geneA", "geneB", "geneA"),
        c("geneC", "geneB", ""),
        c("",      "geneD", "geneE")
    )
    ds <- .aggregated_columns_to_dataset(headers, rows, "test.csv", "csv")
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@set_names, c("Pathway1", "Pathway2", "Pathway3"))
    expect_setequal(ds@items$Pathway1, c("geneA", "geneC"))
    expect_setequal(ds@items$Pathway2, c("geneB", "geneD"))
    expect_setequal(ds@items$Pathway3, c("geneA", "geneE"))
    expect_null(ds@universe_size)   # aggregated mode leaves universe_size NULL
})

test_that(".aggregated_columns_to_dataset rejects all-empty input", {
    headers <- c("A", "B")
    rows <- list(c("", ""), c("", ""))
    expect_error(
        .aggregated_columns_to_dataset(headers, rows, "t.csv", "csv"),
        class = "InvalidDatasetError"
    )
})

test_that("load_csv loads a binary file with auto-detected delimiter", {
    tmp <- tempfile(fileext = ".csv")
    writeLines(c("item,SetA,SetB", "g1,1,0", "g2,1,1", "g3,0,1"), tmp)
    on.exit(unlink(tmp))
    ds <- load_csv(tmp, binary = TRUE)
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@format, "csv")
    expect_equal(ds@universe_size, 3L)
    expect_setequal(ds@items$SetA, c("g1", "g2"))
})

test_that("load_csv loads an aggregated file when binary = FALSE", {
    tmp <- tempfile(fileext = ".csv")
    writeLines(c("Path1,Path2", "geneA,geneB", "geneC,geneD"), tmp)
    on.exit(unlink(tmp))
    ds <- load_csv(tmp, binary = FALSE)
    expect_equal(ds@set_names, c("Path1", "Path2"))
    expect_setequal(ds@items$Path1, c("geneA", "geneC"))
    expect_null(ds@universe_size)
})

test_that("load_csv accepts explicit delimiter", {
    tmp <- tempfile(fileext = ".csv")
    writeLines(c("item;SetA;SetB", "g1;1;0", "g2;1;1"), tmp)
    on.exit(unlink(tmp))
    ds <- load_csv(tmp, binary = TRUE, delimiter = ";")
    expect_setequal(ds@items$SetA, c("g1", "g2"))
})

test_that("load_tsv forces tab delimiter", {
    tmp <- tempfile(fileext = ".tsv")
    writeLines(c("item\tSetA\tSetB", "g1\t1\t0", "g2\t0\t1"), tmp)
    on.exit(unlink(tmp))
    ds <- load_tsv(tmp, binary = TRUE)
    expect_equal(ds@format, "tsv")
    expect_setequal(ds@items$SetA, "g1")
    expect_setequal(ds@items$SetB, "g2")
})

test_that("load_gmt loads a Gene Matrix Transposed file", {
    tmp <- tempfile(fileext = ".gmt")
    writeLines(c(
        "Pathway1\tdesc1\tgeneA\tgeneB\tgeneC",
        "Pathway2\tdesc2\tgeneB\tgeneD\tgeneE"
    ), tmp)
    on.exit(unlink(tmp))
    ds <- load_gmt(tmp)
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@format, "gmt")
    expect_equal(ds@set_names, c("Pathway1", "Pathway2"))
    expect_setequal(ds@items$Pathway1, c("geneA", "geneB", "geneC"))
    expect_setequal(ds@items$Pathway2, c("geneB", "geneD", "geneE"))
    expect_null(ds@universe_size)
})

test_that("load_gmt skips lines without enough columns", {
    tmp <- tempfile(fileext = ".gmt")
    writeLines(c(
        "Pathway1\tdesc1\tgeneA\tgeneB",
        "Pathway2",                              # skip - too few columns
        "Pathway3\tdesc3\tgeneC\tgeneD"
    ), tmp)
    on.exit(unlink(tmp))
    ds <- load_gmt(tmp)
    expect_equal(ds@set_names, c("Pathway1", "Pathway3"))
})

test_that("load_gmt errors when fewer than 2 valid sets remain", {
    tmp <- tempfile(fileext = ".gmt")
    writeLines("Pathway1\tdesc1\tgeneA\tgeneB", tmp)
    on.exit(unlink(tmp))
    expect_error(load_gmt(tmp), class = "InvalidDatasetError")
})

test_that("load_gmx loads a transposed GMT file", {
    tmp <- tempfile(fileext = ".gmx")
    writeLines(c(
        "Pathway1\tPathway2",
        "desc1\tdesc2",
        "geneA\tgeneB",
        "geneB\tgeneD",
        "geneC\t"
    ), tmp)
    on.exit(unlink(tmp))
    ds <- load_gmx(tmp)
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@format, "gmx")
    expect_equal(ds@set_names, c("Pathway1", "Pathway2"))
    expect_setequal(ds@items$Pathway1, c("geneA", "geneB", "geneC"))
    expect_setequal(ds@items$Pathway2, c("geneB", "geneD"))
})
