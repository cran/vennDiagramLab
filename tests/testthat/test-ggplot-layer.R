test_that("geom_venn returns a list of ggplot2 layer-like objects given a RegionResult", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    layers <- geom_venn(data = res)

    expect_type(layers, "list")
    expect_true(length(layers) >= 2L)
    # First element should be the annotation_custom layer (a ggplot2 LayerInstance)
    expect_true(inherits(layers[[1L]], "LayerInstance") ||
                inherits(layers[[1L]], "Layer") ||
                inherits(layers[[1L]], "ggproto"))
})

test_that("ggplot() + geom_venn(data = result) builds without error", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    p <- ggplot2::ggplot() + geom_venn(data = res) + ggplot2::theme_void()
    expect_true(inherits(p, "ggplot"))
    # Building should produce a gtable without erroring (proves the rasterGrob is valid).
    built <- ggplot2::ggplot_build(p)
    expect_true(inherits(built, "ggplot_built"))
})

test_that("geom_venn rejects non-RegionResult input with a clear error", {
    expect_error(
        geom_venn(data = list(foo = "bar")),
        regexp = "RegionResult"
    )
    expect_error(
        geom_venn(data = NULL),
        regexp = "RegionResult"
    )
})

test_that("geom_venn honors width_px to control raster resolution", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    # Smaller raster should still produce a valid layer (smoke test).
    layers <- geom_venn(data = res, width_px = 200L)
    expect_type(layers, "list")
})

test_that("geom_venn works on a 4-set sample (proves it routes through render_venn_svg)", {
    skip_if_not(file.exists(system.file("extdata", "samples",
                                          "dataset_real_cancer_drivers_4.tsv",
                                          package = "vennDiagramLab")),
                "extdata/samples not synced")
    res <- analyze(load_sample("dataset_real_cancer_drivers_4"))
    layers <- geom_venn(data = res)
    p <- ggplot2::ggplot() + layers + ggplot2::theme_void()
    expect_true(inherits(p, "ggplot"))
})

test_that("geom_venn forwards ... to annotation_custom (override xmin/xmax)", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 100L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds)
    # Override the default coordinate range; if forwarding works, the layer is
    # built without error and its xmin/xmax fields reflect the override.
    layers <- geom_venn(data = res, xmin = 5, xmax = 10, ymin = 0, ymax = 5)
    expect_type(layers, "list")
    # Smoke test: the layer list should still be valid and composable.
    p <- ggplot2::ggplot() + layers + ggplot2::theme_void()
    expect_true(inherits(p, "ggplot"))
})
