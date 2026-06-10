test_that(".nice_ticks produces 0..maxv with rounded step (byte-for-byte Python parity)", {
    # Port-of-_nice_ticks from python/.../render/svg.py:835.
    # For maxv=1, count=4: raw=0.25, pow=0.1, normalized=2.5 -> step_mult=2 (2.5 < 3),
    # step=0.2, so ticks = 0, 0.2, 0.4, 0.6, 0.8, 1.0.
    expect_equal(vennDiagramLab:::.nice_ticks(1.0, count = 4),
                 c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
    # For maxv=10: raw=2.5, pow=1, normalized=2.5 -> step_mult=2, step=2.
    expect_equal(vennDiagramLab:::.nice_ticks(10.0, count = 4),
                 c(0, 2, 4, 6, 8, 10))
    # 0 / negative input returns a fallback c(0, 1).
    expect_equal(vennDiagramLab:::.nice_ticks(0, count = 4), c(0, 1))
})

test_that(".format_tick rounds + formats per JS toExponential rules", {
    expect_equal(vennDiagramLab:::.format_tick(0), "0")
    expect_equal(vennDiagramLab:::.format_tick(5), "5")
    expect_equal(vennDiagramLab:::.format_tick(2.5), "2.5")
    expect_equal(vennDiagramLab:::.format_tick(150), "1.5e+2")
    expect_equal(vennDiagramLab:::.format_tick(0.05), "5.0e-2")
})

test_that(".sig_marker returns ***/**/* per FDR threshold", {
    expect_equal(vennDiagramLab:::.sig_marker(1e-10), "***")
    expect_equal(vennDiagramLab:::.sig_marker(0.005), "**")
    expect_equal(vennDiagramLab:::.sig_marker(0.04), "*")
    expect_equal(vennDiagramLab:::.sig_marker(0.1),  "")
})

test_that(".metric_label returns the correct Y-axis label per metric", {
    expect_equal(vennDiagramLab:::.metric_label("neglog10fdr"), "−log₁₀(FDR)")
    expect_equal(vennDiagramLab:::.metric_label("foldEnrichment"), "Fold Enrichment")
})

test_that(".xml_esc escapes XML-significant characters", {
    expect_equal(vennDiagramLab:::.xml_esc("A&B"), "A&amp;B")
    expect_equal(vennDiagramLab:::.xml_esc("<x>"), "&lt;x&gt;")
    expect_equal(vennDiagramLab:::.xml_esc('a"b'), "a&quot;b")
})

test_that(".collect_pair_stats returns one entry per ordered (i<j) pair", {
    ds <- methods::new("VennDataset",
        set_names = c("A", "B", "C"),
        items = list(A = c("x", "y"), B = c("y", "z"), C = c("z")),
        item_order = c("x", "y", "z"),
        universe_size = 10L, source_path = NULL, format = "csv")
    res <- analyze(ds)
    pairs <- vennDiagramLab:::.collect_pair_stats(res)
    expect_length(pairs, 3L)
    # First pair labels match (A,B), (A,C), (B,C) in canonical i<j order
    expect_equal(vapply(pairs, function(p) p$label, character(1L)),
                 c("AB", "AC", "BC"))
})
