test_that("to_zip_report writes a ZIP containing the expected 10 files", {
    skip_on_cran()
    skip_if_not_installed("zip")
    skip_if_not_installed("openxlsx")
    skip_if(getRversion() < "4.6", "ZIP bundle tests require R >= 4.6 (renders PDF that includes ComplexUpset path; breaks on older R)")
    ds <- load_sample("dataset_real_cancer_drivers_4")
    res <- analyze(ds)
    out <- tempfile(fileext = ".zip")
    suppressWarnings(to_zip_report(res, out))
    expect_true(file.exists(out))
    contents <- zip::zip_list(out)$filename
    expect_setequal(contents, c(
        "venn.svg", "upset.svg", "network.svg", "share-dist.svg",
        "regions_summary.tsv", "items_matrix.tsv", "statistics.tsv",
        "enrichment_statistics_4-sets.xlsx",
        "report.pdf",
        "README.txt"
    ))
})

test_that("to_zip_report README contains provenance + About text", {
    skip_on_cran()
    skip_if_not_installed("zip")
    skip_if_not_installed("openxlsx")
    skip_if(getRversion() < "4.6", "ZIP bundle tests require R >= 4.6 (renders PDF that includes ComplexUpset path; breaks on older R)")
    ds <- load_sample("dataset_real_cancer_drivers_4")
    res <- analyze(ds)
    out <- tempfile(fileext = ".zip")
    suppressWarnings(to_zip_report(res, out))
    td <- tempfile(); dir.create(td)
    zip::unzip(out, files = "README.txt", exdir = td)
    txt <- readLines(file.path(td, "README.txt"))
    full <- paste(txt, collapse = "\n")
    expect_match(full, "Venn Diagram Lab", fixed = TRUE)
    expect_match(full, "Generated:", fixed = TRUE)
    expect_match(full, "About This Report", fixed = TRUE)
    expect_match(full, "share-dist.svg", fixed = TRUE)
})
