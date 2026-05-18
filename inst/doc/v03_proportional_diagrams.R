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

## ----build-2set---------------------------------------------------------------
# ds_2 <- methods::new("VennDataset",
#     set_names     = c("A", "B"),
#     items         = list(A = paste0("g", 1:30), B = paste0("g", 21:50)),
#     item_order    = paste0("g", 1:50),
#     universe_size = 100L,
#     source_path   = NULL,
#     format        = "csv"
# )
# 
# result_2 <- analyze(ds_2, model = "proportional")
# result_2@model
# result_2@is_approximate

## ----render-2set--------------------------------------------------------------
# svg_2 <- render_venn_svg(result_2)
# nchar(svg_2)

## ----build-3set---------------------------------------------------------------
# ds_3 <- methods::new("VennDataset",
#     set_names     = c("A", "B", "C"),
#     items         = list(
#         A = paste0("g", 1:40),
#         B = paste0("g", 21:60),
#         C = paste0("g", 41:80)
#     ),
#     item_order    = paste0("g", 1:80),
#     universe_size = 100L,
#     source_path   = NULL,
#     format        = "csv"
# )
# 
# result_3 <- analyze(ds_3, model = "proportional")
# result_3@model
# result_3@is_approximate

## ----render-3set--------------------------------------------------------------
# svg_3 <- render_venn_svg(result_3)
# nchar(svg_3)

## ----circle-area--------------------------------------------------------------
# circle_intersection_area(r1 = 1, r2 = 1, d = 1)         # ≈ 1.228
# circle_intersection_area(r1 = 1, r2 = 1, d = 0)         # 1 circle inside the other -> pi*r^2
# circle_intersection_area(r1 = 1, r2 = 1, d = 2)         # touching at one point -> 0

## ----solve-2set---------------------------------------------------------------
# geom_2 <- solve_2set(a_only = 20, b_only = 20, ab = 10)
# geom_2

## ----solve-3set---------------------------------------------------------------
# geom_3 <- solve_3set(list("1" = 20L, "2" = 20L, "4" = 20L,
#                            "3" = 5L,  "5" = 5L,  "6" = 5L, "7" = 2L))
# str(geom_3)

