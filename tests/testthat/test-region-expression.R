test_that("parse_region_expression handles single-letter atoms", {
    # For n=3, set A is bit 0 → atom A matches regions that include bit 0:
    # masks 1 (A), 3 (AB), 5 (AC), 7 (ABC).
    expect_equal(parse_region_expression("A", n_sets = 3L),
                 c(1L, 3L, 5L, 7L))
    # Set B is bit 1 → masks 2 (B), 3 (AB), 6 (BC), 7 (ABC).
    expect_equal(parse_region_expression("B", n_sets = 3L),
                 c(2L, 3L, 6L, 7L))
})

test_that("parse_region_expression handles `&` intersection", {
    # A & B → regions with bits 0 AND 1 → masks 3 (AB), 7 (ABC).
    expect_equal(parse_region_expression("A & B", n_sets = 3L),
                 c(3L, 7L))
    # A & B & C → mask 7 only.
    expect_equal(parse_region_expression("A & B & C", n_sets = 3L), 7L)
})

test_that("parse_region_expression handles `+` and `|` union", {
    # A + B → all masks that have either bit 0 OR bit 1 set.
    expect_equal(parse_region_expression("A + B", n_sets = 3L),
                 c(1L, 2L, 3L, 5L, 6L, 7L))
    expect_equal(parse_region_expression("A | B", n_sets = 3L),
                 c(1L, 2L, 3L, 5L, 6L, 7L))
})

test_that("parse_region_expression handles `~` and `!` complement", {
    # ~A → masks where bit 0 is NOT set → 2 (B), 4 (C), 6 (BC).
    expect_equal(parse_region_expression("~A", n_sets = 3L),
                 c(2L, 4L, 6L))
    expect_equal(parse_region_expression("!A", n_sets = 3L),
                 c(2L, 4L, 6L))
})

test_that("parse_region_expression honors parentheses", {
    # A & (B + C) = (A & B) + (A & C)
    expect_equal(parse_region_expression("A & (B + C)", n_sets = 3L),
                 sort(unique(c(
                     parse_region_expression("A & B", n_sets = 3L),
                     parse_region_expression("A & C", n_sets = 3L)
                 ))))
})

test_that("parse_region_expression combines & and ~ for spotlight syntax", {
    # A & ~B = "in A but not in B"
    # → masks with bit 0 set AND bit 1 NOT set = 1 (A) and 5 (AC).
    expect_equal(parse_region_expression("A & ~B", n_sets = 3L),
                 c(1L, 5L))
})

test_that("parse_region_expression rejects malformed input", {
    expect_error(parse_region_expression("", n_sets = 3L),
                 "empty")
    expect_error(parse_region_expression("A & B &", n_sets = 3L),
                 "[Mm]alformed")
    expect_error(parse_region_expression("(A & B", n_sets = 3L),
                 "[Pp]arenthes")
})

test_that("parse_region_expression rejects atoms out of range", {
    expect_error(parse_region_expression("D", n_sets = 3L),
                 "out of range")
})

test_that("parse_region_expression returns the empty integer vector for unsatisfiable expressions", {
    # A & ~A in n=3 sets has no matching regions
    expect_equal(parse_region_expression("A & ~A", n_sets = 3L),
                 integer(0))
})
