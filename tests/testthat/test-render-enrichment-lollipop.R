test_that("render_enrichment_lollipop returns an SvgImage with expected viewBox", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_lollipop(res)
    expect_true(inherits(img, "SvgImage"))
    expect_equal(slot(img, "width"), 560L)
    expect_equal(slot(img, "height"), 240L)
    expect_match(slot(img, "content"), 'viewBox="0 0 560 240"', fixed = TRUE)
})

test_that("render_enrichment_lollipop emits one stem + one dot per pair", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_lollipop(res)
    # 3 pairs -> 3 dots
    dot_n <- length(gregexpr('<circle ', slot(img, "content"), fixed = TRUE)[[1L]])
    # 3 lollipop dots + 2 legend dots = 5
    expect_equal(dot_n, 5L)
})

test_that("render_enrichment_lollipop honors custom width / height", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x", "y"), B = c("y", "z")),
        item_order = c("x", "y", "z"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    img <- render_enrichment_lollipop(res, width = 720L, height = 280L)
    expect_equal(slot(img, "width"), 720L)
    expect_equal(slot(img, "height"), 280L)
})

test_that("render_enrichment_lollipop rejects an unknown metric", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("x"), B = c("x")),
        item_order = c("x"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    expect_error(render_enrichment_lollipop(res, metric = "bogus"),
                 "metric must be")
})
