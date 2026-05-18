test_that("list_samples returns 5 bundled sample names sorted", {
    samples <- list_samples()
    expect_type(samples, "character")
    expect_length(samples, 5L)
    expect_setequal(samples, c(
        "dataset_mock_gene_sets",
        "dataset_mock_streaming_platforms",
        "dataset_real_cancer_drivers_4",
        "dataset_real_msigdb_cancer_pathways",
        "dataset_real_msigdb_immune_pathways"
    ))
})

test_that("load_sample returns a VennDataset for cancer drivers", {
    skip_if_not(file.exists(system.file("extdata", "samples",
                                          "dataset_real_cancer_drivers_4.tsv",
                                          package = "vennDiagramLab")),
                "extdata/samples not synced")
    ds <- load_sample("dataset_real_cancer_drivers_4")
    expect_s4_class(ds, "VennDataset")
    expect_equal(ds@format, "tsv")
    expect_equal(length(ds@set_names), 4L)
    expect_equal(ds@universe_size, 20000L)
})

test_that("load_sample errors on unknown name", {
    expect_error(load_sample("not_a_real_sample"), regexp = "not a known sample")
})

test_that("load_sample loads aggregated mock_gene_sets correctly", {
    skip_if_not(file.exists(system.file("extdata", "samples",
                                          "dataset_mock_gene_sets.csv",
                                          package = "vennDiagramLab")),
                "extdata/samples not synced")
    ds <- load_sample("dataset_mock_gene_sets")
    expect_equal(ds@format, "csv")
    expect_null(ds@universe_size)   # aggregated mode
    expect_equal(length(ds@set_names), 6L)
})
