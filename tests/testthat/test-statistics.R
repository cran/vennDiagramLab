test_that("jaccard returns intersection / union", {
    expect_equal(jaccard(10, 10, 5), 5 / 15)
    expect_equal(jaccard(10, 0, 0), 0)
    expect_equal(jaccard(5, 5, 5), 1.0)         # identical sets
    expect_equal(jaccard(0, 0, 0), 0)            # empty/empty -> 0 (web tool convention)
})

test_that("dice returns 2*intersection / (size_a + size_b)", {
    expect_equal(dice(10, 10, 5), 10 / 20)
    expect_equal(dice(5, 5, 5), 1.0)
    expect_equal(dice(0, 0, 0), 0)
    expect_equal(dice(10, 0, 0), 0)
})

test_that("overlap_coefficient returns intersection / min(size_a, size_b)", {
    expect_equal(overlap_coefficient(10, 5, 3), 3 / 5)
    expect_equal(overlap_coefficient(5, 10, 3), 3 / 5)   # symmetric
    expect_equal(overlap_coefficient(5, 5, 5), 1.0)       # identical
    expect_equal(overlap_coefficient(0, 5, 0), 0)         # one empty
})

test_that("hypergeometric_p_value computes P(X >= k) correctly", {
    # 4 of 50 success, drew 10, observed 3 -> upper-tail p
    # Hand-verified against R: phyper(2, 4, 46, 10, lower.tail = FALSE)
    expect_equal(
        hypergeometric_p_value(50, 4, 10, 3),
        phyper(2, 4, 50 - 4, 10, lower.tail = FALSE),
        tolerance = 1e-12
    )
    # Strong overlap: full population = full success
    expect_equal(hypergeometric_p_value(100, 100, 100, 100), 1.0, tolerance = 1e-12)
    # No overlap possible: k > min(K, n) -> 1.0
    expect_equal(hypergeometric_p_value(100, 5, 5, 6), 1.0)
    # Invalid inputs return 1.0 (safe for downstream BH-FDR)
    expect_equal(hypergeometric_p_value(0, 10, 10, 5), 1.0)
    expect_equal(hypergeometric_p_value(100, -1, 10, 5), 1.0)
})

test_that("fold_enrichment returns observed / expected ratio", {
    # k * N / (K * n) = 5 * 100 / (10 * 10) = 5.0
    expect_equal(fold_enrichment(100, 10, 10, 5), 5.0)
    # Underrepresented: k=1, K*n/N = 1.0 -> FE = 1.0
    expect_equal(fold_enrichment(100, 10, 10, 1), 1.0)
    # Edge cases return 0
    expect_equal(fold_enrichment(0, 10, 10, 5), 0)
    expect_equal(fold_enrichment(100, 0, 10, 5), 0)
    expect_equal(fold_enrichment(100, 10, 0, 5), 0)
})

test_that("bh_fdr matches stats::p.adjust(method='BH')", {
    p <- c(0.001, 0.008, 0.01, 0.02, 0.5)
    expect_equal(bh_fdr(p), stats::p.adjust(p, method = "BH"))
    # Empty input -> empty output
    expect_equal(bh_fdr(numeric(0)), numeric(0))
    # Single element passes through
    expect_equal(bh_fdr(0.05), 0.05)
})

test_that("compute_pairwise produces all 5 metric tables for 3 sets", {
    set_names <- c("A", "B", "C")
    inclusive_sizes <- c(A = 10L, B = 8L, C = 6L)
    pairwise_inter <- list("A|B" = 5L, "A|C" = 3L, "B|C" = 2L)
    # Helper to convert list-keyed pairs to the (a, b) -> int format used internally.
    # We use "A|B" (pipe-separated) as the key encoding for tests.
    res <- compute_pairwise(
        set_names = set_names,
        inclusive_sizes = inclusive_sizes,
        pairwise_intersections = pairwise_inter,
        universe_size = 100L
    )

    expect_s4_class(res, "StatisticsResult")
    # Square metric tables: 3x3 named matrix
    expect_equal(dim(res@jaccard), c(3, 3))
    expect_equal(rownames(res@jaccard), set_names)
    expect_equal(colnames(res@jaccard), set_names)
    expect_equal(res@jaccard["A", "A"], 1.0)   # diagonal
    expect_equal(res@jaccard["A", "B"], jaccard(10, 8, 5), tolerance = 1e-12)
    # Hypergeometric long-form table: 3 pairs (n choose 2)
    expect_equal(nrow(res@hypergeometric), 3L)
    expect_setequal(colnames(res@hypergeometric),
                    c("set_a", "set_b", "intersection", "expected",
                      "p_value", "p_adjusted", "significant", "highly_significant"))
})

test_that("compute_pairwise handles n=2 (1 pair)", {
    res <- compute_pairwise(
        set_names = c("X", "Y"),
        inclusive_sizes = c(X = 100L, Y = 50L),
        pairwise_intersections = list("X|Y" = 25L),
        universe_size = 1000L
    )
    expect_equal(dim(res@jaccard), c(2, 2))
    expect_equal(nrow(res@hypergeometric), 1L)
    # fold_enrichment diagonal is NA_real_ (a set is not "enriched" against itself).
    expect_true(is.na(res@fold_enrichment["X", "X"]))
    expect_true(is.na(res@fold_enrichment["Y", "Y"]))
    # Symmetry of off-diagonal entries.
    expect_equal(res@jaccard["X", "Y"], res@jaccard["Y", "X"])
})
