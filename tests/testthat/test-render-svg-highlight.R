.toy_3set_for_highlight <- function() {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 10L, source_path = NULL, format = "csv")
    analyze(ds)
}

test_that("render_venn_svg with highlight=NULL is byte-identical to no-highlight default", {
    res <- .toy_3set_for_highlight()
    svg_default <- render_venn_svg(res)
    svg_null    <- render_venn_svg(res, highlight = NULL)
    expect_equal(svg_default, svg_null)
})

test_that("render_venn_svg highlight=AB desaturates shape C (set NOT in any highlighted region)", {
    res <- .toy_3set_for_highlight()
    svg <- render_venn_svg(res, highlight = "AB")
    # ShapeC fill should be rewritten to the highlight grey.
    expect_match(svg, 'id="ShapeC"[^>]*style="[^"]*fill:#cccccc')
    # ShapeA / ShapeB fills should NOT be rewritten (they are in the
    # highlighted region AB).
    expect_no_match(svg, 'id="ShapeA"[^>]*style="[^"]*fill:#cccccc')
    expect_no_match(svg, 'id="ShapeB"[^>]*style="[^"]*fill:#cccccc')
})

test_that("render_venn_svg highlight accepts an integer bitmask vector", {
    res <- .toy_3set_for_highlight()
    # Bitmask 3 = AB, bitmask 5 = AC. So A is in both, B in mask 3, C in mask 5.
    # Highlighted union mask = 7 (A|B|C). No set should be desaturated.
    svg <- render_venn_svg(res, highlight = c(3L, 5L))
    expect_no_match(svg, 'id="Shape[ABC]"[^>]*style="[^"]*fill:#cccccc')
})

test_that("render_venn_svg highlight composes with parse_region_expression", {
    res <- .toy_3set_for_highlight()
    masks <- parse_region_expression("A & B", n_sets = 3L)   # c(3L, 7L)
    svg <- render_venn_svg(res, highlight = masks)
    # Union of regions 3 and 7 covers A, B, C -> no set desaturated.
    expect_no_match(svg, 'id="Shape[ABC]"[^>]*style="[^"]*fill:#cccccc')

    # But A & B with C excluded:
    masks2 <- parse_region_expression("A & B & ~C", n_sets = 3L)  # 3L only
    svg2 <- render_venn_svg(res, highlight = masks2)
    # Only A and B contribute -> C is desaturated.
    expect_match(svg2, 'id="ShapeC"[^>]*style="[^"]*fill:#cccccc')
})

test_that("render_venn_svg highlight rejects unknown region labels", {
    res <- .toy_3set_for_highlight()
    expect_error(render_venn_svg(res, highlight = "XYZ"),
                 "unknown region label")
})

test_that("render_venn_svg highlight rejects unknown bitmasks", {
    res <- .toy_3set_for_highlight()
    expect_error(render_venn_svg(res, highlight = 99L),
                 "no region with mask")
})

test_that("render_venn_svg highlight rejects type-mismatched inputs", {
    res <- .toy_3set_for_highlight()
    expect_error(render_venn_svg(res, highlight = TRUE),
                 "must be NULL")
    expect_error(render_venn_svg(res, highlight = list(1L, 2L)),
                 "must be NULL")
})
