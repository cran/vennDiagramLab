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

## ----generate, eval = NOT_CRAN && (getRversion() >= "4.6")--------------------
# out <- tempfile(fileext = ".pdf")
# to_pdf_report(result, path = out, title = "Cancer driver overlap")
# file.exists(out)
# file.size(out)   # bytes

## ----minimal, eval = FALSE----------------------------------------------------
# to_pdf_report(result, "venn_only.pdf",
#               include_network = FALSE,
#               include_about   = FALSE)

