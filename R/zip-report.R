# Full Report ZIP bundle: PDF + 4 SVGs + 3 TSVs + xlsx + README.txt.
#
# Mirrors python/src/venn_diagram_lab/cli/_report.py:95-202 (cmd_zip) plus
# python/.../cli/_report.py:170-202 (_build_readme_text). All 10 files
# are rendered into a temp directory, then bundled via `zip::zip` (pure-R
# zip writer, no system zip(1) dependency).

#' @importFrom zip zip
NULL

#' @noRd
# Compose the README.txt body: provenance header + file list + About body.
# Mirrors python `_build_readme_text` byte-for-byte for the headings.
.zip_readme_text <- function(resolved_source, result, xlsx_name) {
    timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    header <- paste0(
        "Venn Diagram Lab -- Full Report (ZIP)\n\n",
        sprintf("Generated: %s\n", timestamp),
        sprintf("Tool version: vennDiagramLab %s\n", vdl_version()),
        sprintf("Dataset: %s\n", resolved_source),
        sprintf("Model: %s\n\n", result@model),
        "Files in this bundle:\n",
        "  venn.svg                            Multi-set Venn diagram\n",
        "  upset.svg                           UpSet plot\n",
        "  network.svg                         Set-relationship network\n",
        "  share-dist.svg                      Item Share Distribution histogram\n",
        "  regions_summary.tsv                 Per-region exclusive + inclusive counts\n",
        "  items_matrix.tsv                    Binary item x set matrix\n",
        "  statistics.tsv                      Pairwise statistics\n",
        sprintf("  %-35s Excel workbook: Jaccard / Sorensen-Dice / Enrichment\n", xlsx_name),
        "  report.pdf                          Multi-page PDF report\n",
        "  README.txt                          This file\n\n"
    )
    # Re-use the .ABOUT_SECTIONS body (titles + bodies) the PDF appendix
    # ships, so the README methodology stays single-sourced.
    about_lines <- character()
    for (section in .ABOUT_SECTIONS) {
        about_lines <- c(about_lines, section$title)
        if (nzchar(section$body)) {
            about_lines <- c(about_lines,
                              strwrap(section$body, width = 80L), "")
        }
    }
    paste0(header, "About This Report\n\n", paste(about_lines, collapse = "\n"))
}

#' @noRd
# Save a ggplot as SVG. We try a chain of strategies because the ZIP
# bundle has to work on systems with very different graphics-device
# capabilities:
#
#   1. `svglite` package -- the cleanest path; produces native, editable
#      SVG. Listed in neither Imports nor Suggests, so optional.
#   2. `grDevices::svg()` -- cairo-backed, ships with R but fails on
#      systems where the cairo DLL/dylib cannot resolve its dependencies
#      (e.g. broken X11 install on macOS, headless Linux without cairo).
#   3. Fallback: render to PNG via base `grDevices::png()` and embed
#      the bytes as a base64 `<image>` inside a minimal SVG wrapper.
#      Always works; the result is a valid SVG file, just raster-based.
#
# The fallback is what makes the ZIP bundle robust on locked-down
# rendering environments.
.ggplot_to_svg_file <- function(plot, path, width, height) {
    if (requireNamespace("svglite", quietly = TRUE)) {
        svglite::svglite(filename = path, width = width, height = height)
        on.exit(grDevices::dev.off(), add = TRUE)
        print(plot)
        return(invisible(NULL))
    }
    # Try the base cairo svg device; fall back to PNG-in-SVG if cairo
    # can't load (we suppress its loud warnings + check the result).
    ok <- tryCatch({
        grDevices::svg(filename = path, width = width, height = height)
        print(plot)
        grDevices::dev.off()
        file.exists(path) && file.size(path) > 0L
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (isTRUE(ok)) return(invisible(NULL))
    # Fallback: render to PNG via the base `png()` device (ships with
    # R; doesn't need cairo for its bitmap backend on macOS/Linux/win),
    # then embed inside a minimal SVG wrapper.
    png_path <- tempfile(fileext = ".png")
    on.exit(unlink(png_path), add = TRUE)
    px_per_in <- 100L
    grDevices::png(filename = png_path,
                   width = width * px_per_in,
                   height = height * px_per_in,
                   res = px_per_in, bg = "white")
    print(plot)
    grDevices::dev.off()
    raw_bytes <- readBin(png_path, what = "raw",
                          n = file.size(png_path))
    b64 <- jsonlite::base64_enc(raw_bytes)
    w_px <- width * px_per_in
    h_px <- height * px_per_in
    svg_lines <- c(
        sprintf(paste0('<svg xmlns="http://www.w3.org/2000/svg" ',
                       'xmlns:xlink="http://www.w3.org/1999/xlink" ',
                       'width="%d" height="%d" viewBox="0 0 %d %d">'),
                w_px, h_px, w_px, h_px),
        sprintf(paste0('  <image x="0" y="0" width="%d" height="%d" ',
                       'xlink:href="data:image/png;base64,%s"/>'),
                w_px, h_px, b64),
        "</svg>"
    )
    writeLines(svg_lines, path)
    invisible(NULL)
}

#' @noRd
.upset_svg_to_path <- function(result, path) {
    suppressWarnings(suppressMessages(
        .ggplot_to_svg_file(render_upset(result, max_columns = 20L),
                            path, width = 8, height = 5)
    ))
}

#' @noRd
.network_svg_to_path <- function(result, path) {
    suppressWarnings(suppressMessages(
        .ggplot_to_svg_file(render_network(result),
                            path, width = 7, height = 7)
    ))
}

#' Write a Full Report ZIP bundle
#'
#' Bundles the multi-page PDF report alongside the supporting SVGs (Venn,
#' UpSet, Network, Share Distribution), TSVs (Region Summary, Item Matrix,
#' Statistics), the 3-sheet xlsx workbook, and a README.txt provenance
#' header into a single ZIP. Mirrors the webtool's *Download Everything*
#' button and Python's \verb{vdl report zip}.
#'
#' @param result A [`RegionResult-class`] from [analyze()].
#' @param path Output `.zip` file path.
#' @param include_share Passed through to [to_pdf_report()] (default `TRUE`).
#' @param include_cluster Passed through to [to_pdf_report()] (default `FALSE`).
#' @return Invisibly returns `NULL`. The ZIP is written to `path`.
#' @examples
#' \donttest{
#' if (getRversion() >= "4.6") {
#'   ds <- load_sample("dataset_real_cancer_drivers_4")
#'   res <- analyze(ds)
#'   to_zip_report(res, tempfile(fileext = ".zip"))
#' }
#' }
#' @export
to_zip_report <- function(result, path,
                          include_share = TRUE, include_cluster = FALSE) {
    td <- tempfile("vdl-zip-")
    dir.create(td, recursive = TRUE)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    n_sets <- length(result@dataset@set_names)
    xlsx_name <- sprintf("enrichment_statistics_%d-sets.xlsx", n_sets)
    resolved_source <- if (is.null(result@dataset@source_path)) {
        "(in-memory)"
    } else {
        basename(result@dataset@source_path)
    }

    # 4 SVGs.
    venn_svg <- render_venn_svg(result)
    writeLines(venn_svg, file.path(td, "venn.svg"))
    .upset_svg_to_path(result, file.path(td, "upset.svg"))
    .network_svg_to_path(result, file.path(td, "network.svg"))
    sd_img <- render_share_distribution(result@dataset)
    writeLines(slot(sd_img, "content"), file.path(td, "share-dist.svg"))

    # 3 TSVs.
    to_region_summary_tsv(result, file.path(td, "regions_summary.tsv"))
    to_matrix_tsv(result, file.path(td, "items_matrix.tsv"))
    to_statistics_tsv(result, file.path(td, "statistics.tsv"))

    # Excel workbook.
    to_excel_workbook(result, file.path(td, xlsx_name))

    # PDF.
    suppressWarnings(to_pdf_report(
        result, path = file.path(td, "report.pdf"),
        include_share = include_share, include_cluster = include_cluster
    ))

    # README.txt.
    writeLines(.zip_readme_text(resolved_source, result, xlsx_name),
               file.path(td, "README.txt"))

    # Bundle. zip::zip takes a working dir argument so the archive holds
    # only basenames (no temp-dir prefix).
    files <- list.files(td, full.names = FALSE)
    zip::zip(zipfile = path, files = files, root = td,
              recurse = FALSE, include_directories = FALSE,
              mode = "cherry-pick")

    invisible(NULL)
}
