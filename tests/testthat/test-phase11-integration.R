.toy_4set_for_integration <- function() {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C", "D"),
        items = list(
            A = c("g1", "g2", "g3"),
            B = c("g2", "g3", "g4"),
            C = c("g3", "g4", "g5"),
            D = c("g4", "g5", "g6")
        ),
        item_order = c("g1", "g2", "g3", "g4", "g5", "g6"),
        universe_size = 100L, source_path = NULL, format = "csv")
    analyze(ds)
}

test_that("Boolean DSL output composes with render_venn_svg highlight=", {
    res <- .toy_4set_for_integration()
    masks <- parse_region_expression("A & B & C & ~D", n_sets = 4L)
    expect_true(length(masks) > 0L)
    svg <- render_venn_svg(res, highlight = masks)
    # ShapeD should be desaturated (set D never appears in the highlighted regions).
    expect_match(svg, 'id="ShapeD"[^>]*style="[^"]*fill:#cccccc')
    expect_no_match(svg, 'id="ShapeA"[^>]*style="[^"]*fill:#cccccc')
})

test_that("region accessors agree with parse_region_expression on toy data", {
    res <- .toy_4set_for_integration()
    # exclusive_items(c("A", "B")) returns items in A & B EXCLUSIVELY (mask 3).
    excl_ab <- exclusive_items(res, c("A", "B"))
    # The toy: g2 is in A and B (not C, not D). So g2 is exclusive to {A,B}.
    expect_equal(excl_ab, "g2")

    # parse_region_expression("A & B & ~C & ~D") should resolve to mask 3
    # (A & B, none of C or D).
    masks <- parse_region_expression("A & B & ~C & ~D", n_sets = 4L)
    expect_equal(masks, 3L)

    # The region at that mask should have exclusive_items == excl_ab.
    expect_equal(res@regions[["3"]]@exclusive_items, excl_ab)
})

test_that("show_items + highlight together render correctly", {
    res <- .toy_4set_for_integration()
    svg <- render_venn_svg(res,
                            show_items = TRUE,
                            highlight = parse_region_expression("A & B & ~C & ~D", n_sets = 4L))
    # Should contain at least one item tspan AND a desaturated shape.
    expect_match(svg, "<tspan[^>]*>g[1-6]")
    expect_match(svg, 'id="Shape[CD]"[^>]*style="[^"]*fill:#cccccc')
})
