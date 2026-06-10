.toy_3set_for_items <- function() {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 10L, source_path = NULL, format = "csv")
    analyze(ds)
}

test_that("render_venn_svg with show_items=FALSE behaves identically to the v2.2.3 default", {
    res <- .toy_3set_for_items()
    svg_default  <- render_venn_svg(res)
    svg_explicit <- render_venn_svg(res, show_items = FALSE)
    expect_equal(svg_default, svg_explicit)
})

test_that("render_venn_svg with show_items=TRUE replaces counts with item names", {
    res <- .toy_3set_for_items()
    svg <- render_venn_svg(res, show_items = TRUE)
    # The toy's region 1 (A-only) is "x"; region 3 (AB) is "y"; region 6 (BC) is "z"; region 4 (C-only) is "w".
    expect_match(svg, "<tspan[^>]*>x</tspan>")
    expect_match(svg, "<tspan[^>]*>y</tspan>")
    expect_match(svg, "<tspan[^>]*>z</tspan>")
    expect_match(svg, "<tspan[^>]*>w</tspan>")
})

test_that("render_venn_svg honors item_options$truncate_long_names", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("verylongitemname_aaaaaaa"), B = c("verylongitemname_aaaaaaa")),
        item_order = c("verylongitemname_aaaaaaa"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    svg <- render_venn_svg(res, show_items = TRUE,
                            item_options = list(truncate_long_names = 8L))
    # Truncated label should appear with an ellipsis.
    expect_match(svg, "verylong\\.\\.\\.")
})

test_that("render_venn_svg show_items truncates regions exceeding max_items_per_region", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = paste0("g", 1:30), B = paste0("g", 1:30)),
        item_order = paste0("g", 1:30),
        universe_size = 100L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    svg <- render_venn_svg(res, show_items = TRUE,
                            item_options = list(max_items_per_region = 5L))
    # Each item appears AT MOST inside one Count_ block; "+N more" tail expected.
    expect_match(svg, "\\+25 more")
})

test_that("render_venn_svg show_items rejects an unknown item_options key with a warning", {
    res <- .toy_3set_for_items()
    expect_warning(
        render_venn_svg(res, show_items = TRUE,
                         item_options = list(bogus_key = 99L)),
        "Ignoring unknown item_options"
    )
})
