## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
# Skip evaluation of all chunks on CRAN's auto-check farm to fit the
# 10-minute build budget. Locally, on CI, and under devtools::check(),
# NOT_CRAN=true and all chunks evaluate normally. The vignette source
# (which CRAN users see in browseVignettes() / vignette()) is unchanged.
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(eval = NOT_CRAN)

## ----setup-data---------------------------------------------------------------
# library(vennDiagramLab)
# ds <- load_sample("dataset_real_cancer_drivers_4")
# ds@set_names

## ----sizes--------------------------------------------------------------------
# sapply(ds@items, length)

## ----universe-----------------------------------------------------------------
# ds@universe_size

## ----analyze------------------------------------------------------------------
# result <- analyze(ds)
# result@model
# length(result@regions)

## ----set-sizes-table----------------------------------------------------------
# result@set_sizes

## ----glance-------------------------------------------------------------------
# broom::glance(result)

## ----render-custom------------------------------------------------------------
# svg <- render_venn_svg(
#     result,
#     set_names = c(A = "Vogelstein", B = "COSMIC", C = "OncoKB", D = "IntOGen"),
#     title = "Cancer driver overlap (4 sources)"
# )
# nchar(svg)

## ----upset, eval = NOT_CRAN && (getRversion() >= "4.6")-----------------------
# upset_plot <- render_upset(result, sort_by = "size")
# upset_plot

## ----tidy---------------------------------------------------------------------
# top_pairs <- broom::tidy(result)
# top_pairs[order(top_pairs$p_adjusted), c("set_a", "set_b", "intersection",
#                                           "jaccard", "p_adjusted",
#                                           "significant")]

## ----augment------------------------------------------------------------------
# gene_table <- broom::augment(result)
# head(gene_table)
# nrow(gene_table)        # total unique genes across all four sets
# table(gene_table$region_label)   # how many genes in each region

## ----save-summary, eval = FALSE-----------------------------------------------
# to_region_summary_tsv(result, "cancer_drivers_regions.tsv")

