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

## ----samples------------------------------------------------------------------
# list_samples()

## ----load-sample--------------------------------------------------------------
# ds <- load_sample("dataset_real_cancer_drivers_4")
# ds@set_names
# vapply(ds@items, length, integer(1L))   # set sizes

## ----analyze------------------------------------------------------------------
# result <- analyze(ds)
# result@model
# length(result@regions)   # number of non-empty regions

## ----render-------------------------------------------------------------------
# svg <- render_venn_svg(result)
# nchar(svg)        # SVG length in bytes
# substr(svg, 1, 80)

## ----save, eval = FALSE-------------------------------------------------------
# writeLines(svg, "cancer_drivers.svg")

