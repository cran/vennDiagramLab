## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# Skip evaluation of all chunks on CRAN's auto-check farm to fit the
# 10-minute build budget. Locally, on CI, and under devtools::check(),
# NOT_CRAN=true and all chunks evaluate normally. The vignette source
# (which CRAN users see in browseVignettes() / vignette()) is unchanged.
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(eval = NOT_CRAN)

## ----load---------------------------------------------------------------------
# library(vennDiagramLab)
# result <- analyze(load_sample("dataset_real_cancer_drivers_4"))

## ----broom--------------------------------------------------------------------
# broom::glance(result)
# head(broom::tidy(result))
# head(broom::augment(result))

## ----dplyr, eval = NOT_CRAN && requireNamespace("dplyr", quietly = TRUE)------
# broom::tidy(result) |>
#     dplyr::filter(highly_significant) |>
#     dplyr::arrange(dplyr::desc(jaccard)) |>
#     dplyr::select(set_a, set_b, intersection, jaccard, p_adjusted)

## ----dplyr-augment, eval = NOT_CRAN && requireNamespace("dplyr", quietly = TRUE)----
# broom::augment(result) |>
#     dplyr::count(region_label, sort = TRUE)

## ----targets-pipeline, eval = FALSE-------------------------------------------
# library(targets)
# 
# list(
#     tar_target(ds,        load_sample("dataset_real_cancer_drivers_4")),
#     tar_target(result,    analyze(ds)),
#     tar_target(stats_df,  broom::tidy(result)),
#     tar_target(genes_df,  broom::augment(result)),
#     tar_target(venn_svg,  render_venn_svg(result)),
#     tar_target(venn_path,
#                { writeLines(venn_svg, "venn.svg"); "venn.svg" },
#                format = "file")
# )

## ----cache--------------------------------------------------------------------
# stats <- statistics(result)
# str(stats@jaccard, max.level = 1)

