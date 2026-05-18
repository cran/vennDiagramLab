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
# length(result@dataset@set_names)

## ----venn---------------------------------------------------------------------
# svg <- render_venn_svg(result, title = "4-set Venn (cancer drivers)")
# nchar(svg)

## ----upset, eval = NOT_CRAN && (getRversion() >= "4.6")-----------------------
# render_upset(result, sort_by = "size", color_mode = "depth")

## ----network, eval = NOT_CRAN-------------------------------------------------
# render_network(result, edge_metric = "intersection")

