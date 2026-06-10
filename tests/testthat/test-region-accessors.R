.toy_3set <- function() {
    # 3-set toy: A = {x, y}, B = {y, z}, C = {z, w}.
    # Expected memberships:
    #   x in A only
    #   y in A and B
    #   z in B and C
    #   w in C only
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z", "w")),
        item_order = c("x", "y", "z", "w"),
        universe_size = 10L, source_path = NULL, format = "csv")
    analyze(ds)
}

test_that("intersection_items returns items present in every named set", {
    res <- .toy_3set()
    expect_equal(sort(intersection_items(res, c("A", "B"))), "y")
    expect_equal(sort(intersection_items(res, c("B", "C"))), "z")
    expect_equal(length(intersection_items(res, c("A", "C"))), 0L)
})

test_that("intersection_items accepts set NAMES (not only letters)", {
    res <- .toy_3set()  # set_names are "A", "B", "C" in this toy
    expect_equal(intersection_items(res, "A"), c("x", "y"))
})

test_that("exclusive_items returns items in EXACTLY this combination", {
    res <- .toy_3set()
    # x is in A only -> exclusive to {A}
    expect_equal(exclusive_items(res, "A"), "x")
    # y is in A and B (not C) -> exclusive to {A, B}
    expect_equal(exclusive_items(res, c("A", "B")), "y")
    # z is in B and C (not A) -> exclusive to {B, C}
    expect_equal(exclusive_items(res, c("B", "C")), "z")
    # No item is exclusively in {A, C} (because no item is in A&C only — only y is in A and only z is in C, neither is shared between A and C without B)
    expect_equal(length(exclusive_items(res, c("A", "C"))), 0L)
})

test_that("union_items returns items in any of the named sets", {
    res <- .toy_3set()
    expect_equal(sort(union_items(res, c("A", "B"))), c("x", "y", "z"))
    expect_equal(sort(union_items(res, c("A", "B", "C"))), c("w", "x", "y", "z"))
    expect_equal(sort(union_items(res, "A")), c("x", "y"))
})

test_that("accessors reject unknown set identifiers", {
    res <- .toy_3set()
    expect_error(intersection_items(res, c("A", "Z")), "Unknown set")
    expect_error(exclusive_items(res, c("Q")), "Unknown set")
    expect_error(union_items(res, c("A", "B", "X")), "Unknown set")
})

test_that("accessors raise on empty `sets`", {
    res <- .toy_3set()
    expect_error(intersection_items(res, character(0)), "at least one set")
    expect_error(exclusive_items(res, character(0)), "at least one set")
    expect_error(union_items(res, character(0)), "at least one set")
})
