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

## ----names--------------------------------------------------------------------
# svg <- render_venn_svg(
#     result,
#     set_names = c(A = "Vogelstein\n(2013)",
#                    B = "COSMIC CGC",
#                    C = "OncoKB",
#                    D = "IntOGen"),
#     title = "Pan-source cancer driver agreement"
# )
# substr(svg, 1, 60)

## ----colors-------------------------------------------------------------------
# svg <- render_venn_svg(
#     result,
#     colors = c(A = "#E69F00", B = "#56B4E9", C = "#009E73", D = "#CC79A7")
# )
# nchar(svg)

## ----hide-counts--------------------------------------------------------------
# svg_clean <- render_venn_svg(result, show_counts = FALSE)
# nchar(svg_clean)

## ----xml2, eval = NOT_CRAN && requireNamespace("xml2", quietly = TRUE)--------
# svg <- render_venn_svg(result)
# doc <- xml2::read_xml(svg)
# xml2::xml_attr(doc, "viewBox")

## ----ggplot, eval = NOT_CRAN--------------------------------------------------
# library(ggplot2)
# ggplot() +
#     geom_venn(data = result) +
#     theme_void() +
#     labs(title = "4 cancer-driver sources",
#           subtitle = "Vogelstein, COSMIC CGC, OncoKB, IntOGen") +
#     theme(plot.title = element_text(size = 14, face = "bold"),
#           plot.subtitle = element_text(size = 10, colour = "grey40"))

## ----export, eval = NOT_CRAN--------------------------------------------------
# svg <- render_venn_svg(result)
# png_path <- tempfile(fileext = ".png")
# rsvg::rsvg_png(charToRaw(svg), png_path, width = 1200)
# file.size(png_path)
# 
# pdf_path <- tempfile(fileext = ".pdf")
# rsvg::rsvg_pdf(charToRaw(svg), pdf_path)
# file.size(pdf_path)

