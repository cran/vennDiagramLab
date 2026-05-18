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
# stats <- statistics(result)

## ----helpers------------------------------------------------------------------
# jaccard(size_a = 138, size_b = 581, intersection = 100)
# dice(size_a = 138, size_b = 581, intersection = 100)
# overlap_coefficient(size_a = 138, size_b = 581, intersection = 100)
# hypergeometric_p_value(N = 20000, K = 138, n = 581, k = 100)
# fold_enrichment(N = 20000, K = 138, n = 581, k = 100)

## ----stats-tables-------------------------------------------------------------
# stats@jaccard

## ----stats-hyp----------------------------------------------------------------
# head(stats@hypergeometric)

## ----bh-fdr-------------------------------------------------------------------
# raw_p <- stats@hypergeometric$p_value
# adjusted <- bh_fdr(raw_p)
# all.equal(adjusted, stats@hypergeometric$p_adjusted)

## ----bh-fdr-toy---------------------------------------------------------------
# toy_p <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 0.9)
# data.frame(
#     raw            = toy_p,
#     bonferroni     = pmin(toy_p * length(toy_p), 1),
#     bh_fdr         = bh_fdr(toy_p)
# )

## ----tidy---------------------------------------------------------------------
# broom::tidy(result)

## ----reproduce----------------------------------------------------------------
# sig_table <- broom::tidy(result)
# sig_table$colour <- ifelse(sig_table$highly_significant, "red",
#                     ifelse(sig_table$significant,        "orange",
#                                                           "grey"))
# sig_table[, c("set_a", "set_b", "p_adjusted", "colour")]

