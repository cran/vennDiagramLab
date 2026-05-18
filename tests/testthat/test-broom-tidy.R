test_that("tidy.RegionResult returns a single row for a 2-set dataset with all expected columns", {
    skip_if_not_installed("broom")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::tidy(res)

    expect_equal(nrow(out), 1L)
    expect_named(out, c(
        "set_a", "set_b", "intersection", "expected",
        "jaccard", "dice", "overlap_coefficient", "fold_enrichment",
        "p_value", "p_adjusted", "significant", "highly_significant"
    ))
    expect_equal(out$set_a, "A")
    expect_equal(out$set_b, "B")
    expect_equal(out$intersection, 1L)   # only g2 is shared
    # Inclusive sizes: A=2, B=2, intersection=1 -> jaccard = 1/3
    expect_equal(out$jaccard, 1/3, tolerance = 1e-9)
    # Dice = 2*1/(2+2) = 0.5
    expect_equal(out$dice, 0.5, tolerance = 1e-9)
    # OC = 1/min(2,2) = 0.5
    expect_equal(out$overlap_coefficient, 0.5, tolerance = 1e-9)
})

test_that("tidy.RegionResult returns 3 rows for a 3-set dataset", {
    skip_if_not_installed("broom")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::tidy(res)
    expect_equal(nrow(out), 3L)   # 3 choose 2 = 3 pairs
    expect_setequal(paste0(out$set_a, out$set_b), c("AB", "BC", "AC"))
})

test_that("tidy.RegionResult returns a tibble when tibble is installed", {
    skip_if_not_installed("broom")
    skip_if_not_installed("tibble")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::tidy(res)
    expect_s3_class(out, "tbl_df")
})

test_that(".as_tibble_safe always returns a data.frame-compatible object", {
    df  <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
    out <- vennDiagramLab:::.as_tibble_safe(df)
    expect_true(is.data.frame(out))
    expect_equal(nrow(out), 1L)
    expect_named(out, c("a", "b"))
})

test_that("glance.RegionResult returns a 1-row summary with expected columns", {
    skip_if_not_installed("broom")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::glance(res)

    expect_equal(nrow(out), 1L)
    expect_named(out, c(
        "n_sets", "n_regions", "n_items", "universe_size",
        "model", "is_approximate", "n_significant", "n_highly_significant"
    ))
    expect_equal(out$n_sets, 3L)
    expect_equal(out$n_regions, length(res@regions))
    expect_equal(out$n_items, 4L)              # x, y, z, w
    expect_equal(out$universe_size, 100L)      # from VennDataset@universe_size
    expect_equal(out$model, res@model)
    expect_false(out$is_approximate)
    expect_type(out$n_significant, "integer")
    expect_type(out$n_highly_significant, "integer")
})

test_that("augment.RegionResult returns one row per item with boolean set columns", {
    skip_if_not_installed("broom")
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::augment(res)

    expect_equal(nrow(out), 3L)
    expect_named(out, c("item", "A", "B", "region_label"))
    expect_equal(out$item, c("g1", "g2", "g3"))
    expect_equal(out$A, c(TRUE, TRUE, FALSE))
    expect_equal(out$B, c(FALSE, TRUE, TRUE))
    expect_equal(out$region_label, c("A", "AB", "B"))
})

test_that("augment.RegionResult preserves item_order even when items appear in multiple sets", {
    skip_if_not_installed("broom")
    ds <- methods::new("VennDataset",
        set_names = c("X", "Y", "Z"),
        items = list(X = c("a", "b", "c"), Y = c("b", "c", "d"), Z = c("c", "d", "e")),
        item_order = c("a", "b", "c", "d", "e"),
        universe_size = 50L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    out <- broom::augment(res)

    expect_equal(nrow(out), 5L)
    expect_equal(out$item, c("a", "b", "c", "d", "e"))
    # 'c' is in all three sets -> region_label = "ABC" (uses A/B/C letters by position)
    expect_equal(out$region_label[out$item == "c"], "ABC")
    # 'a' only in first set -> region_label = "A"
    expect_equal(out$region_label[out$item == "a"], "A")
})
