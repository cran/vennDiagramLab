test_that("VennDataset can be constructed with required slots", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y"), B = c("y", "z")),
        item_order = c("x", "y", "z"),
        universe_size = 3L,
        source_path = "test.csv",
        format = "csv"
    )
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@set_names, c("A", "B"))
    expect_equal(length(ds@items), 2L)
})

test_that("VennDataset validity rejects out-of-range set counts", {
    expect_error(
        methods::new("VennDataset",
            set_names = c("A"),
            items = list(A = "x"),
            item_order = "x",
            universe_size = NULL,
            source_path = NULL,
            format = "csv"
        ),
        regexp = "between 2 and 9"
    )
})

test_that("VennDataset validity rejects bad format", {
    expect_error(
        methods::new("VennDataset",
            set_names = c("A", "B"),
            items = list(A = "x", B = "y"),
            item_order = c("x", "y"),
            universe_size = NULL,
            source_path = NULL,
            format = "xlsx"
        ),
        regexp = "csv/tsv/gmt/gmx"
    )
})

test_that("VennDataset validity rejects items names that don't match set_names", {
    expect_error(
        methods::new("VennDataset",
            set_names = c("A", "B"),
            items = list(A = "x", C = "y"),
            item_order = c("x", "y"),
            universe_size = NULL,
            source_path = NULL,
            format = "csv"
        ),
        regexp = "items names must match set_names"
    )
})

test_that("VennDataset validity rejects non-integer universe_size", {
    expect_error(
        methods::new("VennDataset",
            set_names = c("A", "B"),
            items = list(A = "x", B = "y"),
            item_order = c("x", "y"),
            universe_size = 3.5,
            source_path = NULL,
            format = "csv"
        ),
        regexp = "integer or NULL"
    )
})

test_that("RegionData stores bitmask + items", {
    rd <- methods::new("RegionData",
        bitmask = 5L,
        label = "AC",
        set_indices = c(0L, 2L),
        set_names = c("A", "C"),
        exclusive_items = c("x", "y"),
        inclusive_items = c("x", "y", "z")
    )
    expect_s4_class(rd, "RegionData")
    expect_equal(rd@bitmask, 5L)
    expect_equal(rd@label, "AC")
})

test_that("RegionResult stores dataset + model + regions + set_sizes", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = "x", B = "y"),
        item_order = c("x", "y"),
        universe_size = NULL,
        source_path = NULL,
        format = "csv"
    )
    rr <- methods::new("RegionResult",
        dataset = ds,
        model = "venn-2-set",
        regions = list(),
        set_sizes = c(A = 1L, B = 1L),
        is_approximate = FALSE
    )
    expect_s4_class(rr, "RegionResult")
    expect_equal(rr@model, "venn-2-set")
})
