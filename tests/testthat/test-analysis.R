test_that("list_models returns a data.frame with all 44 bundled models", {
    models <- list_models()
    expect_s3_class(models, "data.frame")
    expect_setequal(colnames(models), c("name", "set_count", "display_name"))
    # 44 venn-* models, sorted by (set_count, name).
    expect_gte(nrow(models), 44L)
    expect_true(all(models$set_count >= 2L & models$set_count <= 9L))
    # Sorted ascending by set_count then name.
    expect_equal(models$set_count, sort(models$set_count))
})

test_that("list_models output includes venn-2-set as the first 2-set model", {
    models <- list_models()
    two_set <- models[models$set_count == 2L, ]
    expect_true("venn-2-set" %in% two_set$name)
})

test_that(".label_for_bitmask converts bitmask to letter label", {
    expect_equal(.label_for_bitmask(1L), "A")
    expect_equal(.label_for_bitmask(2L), "B")
    expect_equal(.label_for_bitmask(3L), "AB")    # bits 0 + 1
    expect_equal(.label_for_bitmask(5L), "AC")    # bits 0 + 2
    expect_equal(.label_for_bitmask(7L), "ABC")   # bits 0 + 1 + 2
})

test_that(".enumerate_regions on a 2-set example", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y", "z"), B = c("y", "z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = NULL,
        source_path = NULL,
        format = "csv"
    )
    regions <- .enumerate_regions(ds)
    # Mask 1 (A only): {x}, mask 2 (B only): {w}, mask 3 (AB): {y, z}
    expect_setequal(regions[["1"]]@exclusive_items, "x")
    expect_setequal(regions[["2"]]@exclusive_items, "w")
    expect_setequal(regions[["3"]]@exclusive_items, c("y", "z"))
    # Inclusive: items in at least the named sets.
    expect_setequal(regions[["1"]]@inclusive_items, c("x", "y", "z"))   # A inclusive = all in A
    expect_setequal(regions[["3"]]@inclusive_items, c("y", "z"))        # AB inclusive = items in both
})

test_that(".enumerate_regions skips empty regions", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x"), B = c("y"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = NULL,
        source_path = NULL,
        format = "csv"
    )
    regions <- .enumerate_regions(ds)
    # Only the 3 single-set masks should be populated; AB/AC/BC/ABC are empty.
    expect_setequal(names(regions), c("1", "2", "4"))
})

test_that(".resolve_model auto-picks alphabetically-first model for set count", {
    expect_equal(.resolve_model("auto", 2L), "venn-2-set")    # alphabetical first 2-set
    expect_equal(.resolve_model("auto", 4L), "venn-4-set")
})

test_that(".resolve_model passes through proportional", {
    expect_equal(.resolve_model("proportional", 2L), "proportional")
    expect_equal(.resolve_model("proportional", 3L), "proportional")
})

test_that(".resolve_model errors on unknown explicit model", {
    expect_error(
        .resolve_model("not-a-real-model", 4L),
        class = "UnknownModelError"
    )
})

test_that("analyze returns a RegionResult on a 2-set toy dataset", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y"), B = c("y", "z")),
        item_order = c("x", "y", "z"),
        universe_size = NULL,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "auto")
    expect_s4_class(res, "RegionResult")
    expect_equal(res@model, "venn-2-set")
    expect_equal(res@set_sizes, c(A = 2L, B = 2L))
    expect_setequal(names(res@regions), c("1", "2", "3"))
})

test_that("analyze on bundled cancer drivers sample produces 4-set result", {
    skip_if_not(file.exists(system.file("extdata", "samples",
                                          "dataset_real_cancer_drivers_4.tsv",
                                          package = "vennDiagramLab")),
                "extdata/samples not synced")
    sample_path <- system.file("extdata", "samples",
                               "dataset_real_cancer_drivers_4.tsv",
                               package = "vennDiagramLab")
    ds <- load_tsv(sample_path, binary = TRUE)
    res <- analyze(ds, model = "auto")
    expect_equal(res@model, "venn-4-set")
    expect_equal(length(res@dataset@set_names), 4L)
    # Universe = csv row count (binary mode); fixture is 20000 rows.
    expect_equal(res@dataset@universe_size, 20000L)
})

test_that("effective_universe falls back to union for aggregated dataset", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y"), B = c("y", "z")),
        item_order = c("x", "y", "z"),
        universe_size = NULL,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "auto")
    expect_equal(effective_universe(res), 3L)   # |union of items in regions|
})

test_that("effective_universe prefers dataset@universe_size when set", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x"), B = c("y")),
        item_order = c("x", "y"),
        universe_size = 100L,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "auto")
    expect_equal(effective_universe(res), 100L)
})

test_that("statistics() returns a StatisticsResult for a 3-set example", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 100L,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "auto")
    stats <- statistics(res)
    expect_s4_class(stats, "StatisticsResult")
    expect_equal(dim(stats@jaccard), c(3, 3))
    expect_equal(nrow(stats@hypergeometric), 3L)   # n choose 2
})
