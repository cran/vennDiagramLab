test_that("circle_intersection_area returns 0 when circles don't touch", {
    expect_equal(circle_intersection_area(1, 1, 3), 0)
})

test_that("circle_intersection_area returns the smaller circle when fully contained", {
    expect_equal(circle_intersection_area(2, 1, 0.5), pi * 1 ^ 2, tolerance = 1e-12)
    # r1 fully inside r2: d + r1 <= r2 -> area = pi * r1^2
    expect_equal(circle_intersection_area(1, 2, 0.5), pi * 1 ^ 2, tolerance = 1e-12)
})

test_that("circle_intersection_area for two equal circles touching at center", {
    # Two unit circles, centers at distance 1: intersection = 2*(pi/3 - sqrt(3)/4) per circle pair formula
    r <- 1; d <- 1
    a1 <- r^2 * acos((d^2 + r^2 - r^2) / (2 * d * r))
    a2 <- r^2 * acos((d^2 + r^2 - r^2) / (2 * d * r))
    triangle <- 0.5 * sqrt((-d + r + r) * (d + r - r) * (d - r + r) * (d + r + r))
    expected <- a1 + a2 - triangle
    expect_equal(circle_intersection_area(1, 1, 1), expected, tolerance = 1e-12)
})

test_that("solve_2set with no overlap places circles tangent (d = r_a + r_b)", {
    layout <- solve_2set(a_only = 10L, b_only = 10L, ab = 0L)
    expect_type(layout, "list")
    expect_named(layout, c("circles", "error", "is_approximate"))
    expect_false(layout$is_approximate)
    expect_length(layout$circles, 2L)
    # Both circles have radius sqrt(10/pi); distance = 2 * radius
    r <- sqrt(10 / pi)
    d_expected <- 2 * r
    actual_d <- abs(layout$circles[[1L]]$cx - layout$circles[[2L]]$cx)
    expect_equal(actual_d, d_expected, tolerance = 1e-9)
})

test_that("solve_2set with full containment gives concentric-ish circles", {
    # ab = pi * min(r)^2 means full containment of B in A
    layout <- solve_2set(a_only = 100L, b_only = 0L, ab = 10L)
    # size_b = 10, r_b = sqrt(10/pi); ab >= max overlap so d = abs(r_a - r_b)
    r_a <- sqrt(110 / pi)
    r_b <- sqrt(10 / pi)
    actual_d <- abs(layout$circles[[1L]]$cx - layout$circles[[2L]]$cx)
    expect_equal(actual_d, abs(r_a - r_b), tolerance = 1e-9)
})

test_that("solve_2set with empty set returns default fallback", {
    layout <- solve_2set(a_only = 0L, b_only = 5L, ab = 0L)
    expect_equal(layout$error, 0)
    # Fallback: both circles unit radius, centers at (-1, 0) and (1, 0)
    expect_equal(layout$circles[[1L]]$cx, -1)
    expect_equal(layout$circles[[2L]]$cx,  1)
    expect_equal(layout$circles[[1L]]$r, 1)
})

test_that("solve_2set partial overlap converges within tolerance", {
    layout <- solve_2set(a_only = 30L, b_only = 30L, ab = 10L)
    r_a <- sqrt(40 / pi); r_b <- sqrt(40 / pi)   # inclusive sizes
    d <- abs(layout$circles[[1L]]$cx - layout$circles[[2L]]$cx)
    achieved <- circle_intersection_area(r_a, r_b, d)
    expect_equal(achieved, 10, tolerance = 1e-3)
    expect_true(layout$error < 1e-3)
})

test_that("solve_3set with empty set returns default fallback", {
    layout <- solve_3set(list("1" = 0L, "2" = 5L, "3" = 0L, "4" = 0L,
                              "5" = 0L, "6" = 0L, "7" = 0L))
    expect_true(layout$is_approximate)
    expect_length(layout$circles, 3L)
    # Fallback: 3 unit circles in a horizontal row
    expect_equal(layout$circles[[1L]]$cx, -2)
    expect_equal(layout$circles[[2L]]$cx,  0)
    expect_equal(layout$circles[[3L]]$cx,  2)
    expect_true(all(vapply(layout$circles, function(c) c$r, numeric(1L)) == 1))
})

test_that("solve_3set with non-empty sets places 3 circles + flags as approximate", {
    # 10 in each exclusive region, 5 in each pair, 2 in triple
    regions <- list(
        "1" = 10L, "2" = 10L, "3" = 5L,
        "4" = 10L, "5" = 5L,  "6" = 5L, "7" = 2L
    )
    layout <- solve_3set(regions)
    expect_true(layout$is_approximate)
    expect_length(layout$circles, 3L)
    # All circles should have positive radius
    for (c in layout$circles) {
        expect_gt(c$r, 0)
    }
    # error is intentionally NaN for 3-set in v0.1
    expect_true(is.nan(layout$error))
})

test_that("solve_3set keeps centers finite (no NaN/Inf in coords)", {
    # Wikipedia-style 3-set example
    regions <- list(
        "1" = 100L, "2" = 80L, "3" = 30L,
        "4" = 60L,  "5" = 20L, "6" = 15L, "7" = 5L
    )
    layout <- solve_3set(regions)
    for (c in layout$circles) {
        expect_true(is.finite(c$cx) && is.finite(c$cy) && is.finite(c$r))
    }
})

test_that(".compute_transform centers a 2-circle layout in a 600x600 canvas", {
    circles <- list(
        list(cx = -1, cy = 0, r = 1),
        list(cx =  1, cy = 0, r = 1)
    )
    transform <- .compute_transform(circles, 600, 600)
    expect_named(transform, c("scale", "offset_x", "offset_y"))
    # bbox is x in [-2, 2] (span 4), y in [-1, 1] (span 2). usable = 540.
    # scale = min(540/4, 540/2) = 135
    expect_equal(transform$scale, 135)
})

test_that("generate_proportional_svg returns a parseable SVG with expected structure", {
    ds <- methods::new("VennDataset",
        set_names = c("Alpha", "Beta"),
        items = list(Alpha = c("g1", "g2"), Beta = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 3L,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "proportional")
    svg <- generate_proportional_svg(res)
    expect_type(svg, "character")
    expect_length(svg, 1L)

    # Round-trip through xml2 (well-formed XML check)
    doc <- xml2::read_xml(svg)
    expect_s3_class(doc, "xml_document")
    # Strip namespace so plain element-name XPath works
    xml2::xml_ns_strip(doc)

    # Has both Shape* circles (circle elements only, not the <g id="Shapes"> group)
    shapes <- xml2::xml_find_all(doc, ".//circle[starts-with(@id, 'Shape')]")
    expect_length(shapes, 2L)

    # Has both Name* texts with the right content (NameA, NameB -- letter-based ids)
    name_a <- xml2::xml_find_first(doc, ".//*[@id='NameA']")
    expect_equal(xml2::xml_text(name_a), "Alpha")
    name_b <- xml2::xml_find_first(doc, ".//*[@id='NameB']")
    expect_equal(xml2::xml_text(name_b), "Beta")

    # Has Count_* texts: A=1 (g1), B=1 (g3), AB=1 (g2)
    expect_equal(xml2::xml_text(xml2::xml_find_first(doc, ".//*[@id='Count_A']")),  "1")
    expect_equal(xml2::xml_text(xml2::xml_find_first(doc, ".//*[@id='Count_B']")),  "1")
    expect_equal(xml2::xml_text(xml2::xml_find_first(doc, ".//*[@id='Count_AB']")), "1")

    # 2-set is NOT approximate; should NOT have "approximate" annotation
    expect_false(grepl(">approximate<", svg, fixed = TRUE))
})

test_that("generate_proportional_svg with 3 sets emits 'approximate' annotation", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 4L,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "proportional")
    svg <- generate_proportional_svg(res)
    expect_match(svg, ">approximate<", fixed = TRUE)
})

test_that("generate_proportional_svg errors for n > 3", {
    # Construct a 4-set dataset and request proportional rendering
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C", "D"),
        items = list(A = "1", B = "2", C = "3", D = "4"),
        item_order = c("1", "2", "3", "4"),
        universe_size = 4L,
        source_path = NULL,
        format = "csv"
    )
    res <- analyze(ds, model = "proportional")
    expect_error(generate_proportional_svg(res), class = "IncompatibleModelError")
})
