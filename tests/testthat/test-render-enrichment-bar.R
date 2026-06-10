test_that("render_enrichment_bar returns an SvgImage with expected viewBox", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_bar(res)
    expect_true(inherits(img, "SvgImage"))
    expect_match(slot(img, "content"), '^<svg xmlns="http://www.w3.org/2000/svg"')
    expect_equal(slot(img, "width"), 560L)
    expect_equal(slot(img, "height"), 240L)
    expect_match(slot(img, "content"), 'viewBox="0 0 560 240"', fixed = TRUE)
})

test_that("render_enrichment_bar emits one <rect> bar per pair", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_bar(res)
    # Background rect + 3 pairwise bars + at least 1 legend swatch.
    rect_n <- length(gregexpr('<rect', slot(img, "content"), fixed = TRUE)[[1L]])
    # 1 (background) + 3 (pairs AB / AC / BC) + 2 (legend swatches) = 6
    expect_equal(rect_n, 6L)
})

test_that("render_enrichment_bar honors a custom width / height", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y"), B = c("y", "z")),
        item_order = c("x", "y", "z"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_bar(res, width = 800L, height = 320L)
    expect_equal(slot(img, "width"), 800L)
    expect_equal(slot(img, "height"), 320L)
    expect_match(slot(img, "content"), 'viewBox="0 0 800 320"', fixed = TRUE)
})

test_that("render_enrichment_bar rejects an unknown metric", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x"), B = c("x")),
        item_order = c("x"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    expect_error(render_enrichment_bar(res, metric = "bogus"),
                 "metric must be")
})
