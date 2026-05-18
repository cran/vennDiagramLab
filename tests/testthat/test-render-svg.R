test_that(".is_venn_model returns TRUE for venn-* names and FALSE for others", {
    skip_on_cran()
    expect_true(.is_venn_model("venn-2-set"))
    expect_true(.is_venn_model("venn-5-set-grunbaum"))
    expect_false(.is_venn_model("names-bar"))
    expect_false(.is_venn_model("not-a-venn"))
})

test_that(".load_template returns the bundled SVG text for a known model", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    expect_type(text, "character")
    expect_length(text, 1L)
    expect_match(text, "<svg", fixed = TRUE)
    expect_match(text, "ShapeA",  fixed = TRUE)
})

test_that(".load_template errors on unknown model", {
    skip_on_cran()
    expect_error(.load_template("not-a-real-model"), class = "UnknownModelError")
    expect_error(.load_template("names-bar"),         class = "UnknownModelError")
})

test_that(".find_by_id locates an element regardless of namespace", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    el <- .find_by_id(doc, "ShapeA")
    expect_false(is.null(el))
    expect_equal(xml2::xml_attr(el, "id"), "ShapeA")
})

test_that(".find_by_id returns NULL when id is absent", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    expect_null(.find_by_id(doc, "NoSuchId"))
})

test_that(".set_text overwrites the text of the element with the given id", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    .set_text(doc, "NameA", "MyAlphaSet")
    el <- .find_by_id(doc, "NameA")
    expect_equal(xml2::xml_text(el), "MyAlphaSet")
})

test_that(".set_text is a no-op when the id is absent", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    expect_silent(.set_text(doc, "NoSuchId", "irrelevant"))
})

test_that(".replace_fill_color updates the inline style fill attribute", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    .replace_fill_color(doc, "ShapeA", "#FF00FF")
    el <- .find_by_id(doc, "ShapeA")
    style <- xml2::xml_attr(el, "style")
    expect_match(style, "fill:#FF00FF", fixed = TRUE)
})

test_that(".replace_fill_color is a no-op when the id is absent", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    expect_silent(.replace_fill_color(doc, "NoSuchId", "#FF00FF"))
})

test_that(".count_ids_for_set_count enumerates Count_<label> ids for n=2", {
    skip_on_cran()
    ids <- .count_ids_for_set_count(2L)
    expect_setequal(ids, c("Count_A", "Count_B", "Count_AB"))
})

test_that(".count_ids_for_set_count counts grow as 2^n - 1 for n in 4..9", {
    skip_on_cran()
    for (n in 4L:9L) {
        ids <- .count_ids_for_set_count(n)
        expect_length(ids, 2L ^ n - 1L)
    }
})

test_that("all 44 bundled venn templates load and round-trip through xml2", {
    skip_on_cran()
    skip_if_not(dir.exists(.models_svg_dir()), "extdata/models/svg not synced")
    venn_models <- list_models()
    venn_models <- venn_models[startsWith(venn_models$name, "venn"), ]
    expect_gte(nrow(venn_models), 44L)
    for (nm in venn_models$name) {
        text <- .load_template(nm)
        doc  <- xml2::read_xml(text)
        expect_s3_class(doc, "xml_document")
        # Every venn template has at least one ShapeA element
        expect_false(is.null(.find_by_id(doc, "ShapeA")), info = nm)
    }
})

test_that(".count_ids_for_set_count enumerates 7 ids for n=3", {
    skip_on_cran()
    ids <- .count_ids_for_set_count(3L)
    # Single-letter (3) + 2-letter combos (3) + 3-letter (1) = 7
    expect_length(ids, 7L)
    expect_true(all(c("Count_A", "Count_AB", "Count_AC", "Count_BC", "Count_ABC") %in% ids))
})

test_that(".apply_counts writes the right counts onto a 2-set template", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 3L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    .apply_counts(doc, res, show = TRUE)
    # exclusive counts: A only = 1, B only = 1, AB = 1
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_A")),  "1")
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_B")),  "1")
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_AB")), "1")
    # Inclusive set totals: A = 2, B = 2
    expect_equal(xml2::xml_text(.find_by_id(doc, "CountSUM_A")), "2")
    expect_equal(xml2::xml_text(.find_by_id(doc, "CountSUM_B")), "2")
})

test_that(".apply_counts blanks all counts when show = FALSE", {
    skip_on_cran()
    text <- .load_template("venn-2-set")
    doc <- xml2::read_xml(text)
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 3L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    .apply_counts(doc, res, show = FALSE)
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_A")),  "")
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_AB")), "")
    expect_equal(xml2::xml_text(.find_by_id(doc, "CountSUM_A")), "")
})

test_that("render_venn_svg returns a valid SVG with counts and names from a 2-set result", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("Alpha", "Beta"),
        items = list(Alpha = c("g1", "g2"), Beta = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 3L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    svg <- render_venn_svg(res)
    expect_type(svg, "character")
    expect_length(svg, 1L)

    doc <- xml2::read_xml(svg)
    expect_s3_class(doc, "xml_document")

    # Names overwritten from result@dataset@set_names
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameA")), "Alpha")
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameB")), "Beta")

    # Counts written
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_A")),  "1")
    expect_equal(xml2::xml_text(.find_by_id(doc, "Count_AB")), "1")

    # CountSUM written
    expect_equal(xml2::xml_text(.find_by_id(doc, "CountSUM_A")), "2")
})

test_that("render_venn_svg honors set_names override (per-letter mapping)", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("Alpha", "Beta"),
        items = list(Alpha = c("g1"), Beta = c("g2")),
        item_order = c("g1", "g2"),
        universe_size = 2L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    svg <- render_venn_svg(res, set_names = c(A = "OverrideA"))
    doc <- xml2::read_xml(svg)
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameA")), "OverrideA")
    # Letter B not overridden -> falls back to dataset name
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameB")), "Beta")
})

test_that("render_venn_svg honors colors override", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1"), B = c("g2")),
        item_order = c("g1", "g2"),
        universe_size = 2L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    svg <- render_venn_svg(res, colors = c(A = "#FF00FF"))
    doc <- xml2::read_xml(svg)
    style <- xml2::xml_attr(.find_by_id(doc, "ShapeA"), "style")
    expect_match(style, "fill:#FF00FF", fixed = TRUE)
    bullet_style <- xml2::xml_attr(.find_by_id(doc, "BulletA"), "style")
    expect_match(bullet_style, "fill:#FF00FF", fixed = TRUE)
})

test_that("render_venn_svg honors title override", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1"), B = c("g2")),
        item_order = c("g1", "g2"),
        universe_size = 2L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    svg <- render_venn_svg(res, title = "My Custom Title")
    doc <- xml2::read_xml(svg)
    expect_equal(xml2::xml_text(.find_by_id(doc, "Title")), "My Custom Title")
})

test_that("render_venn_svg honors show_names = FALSE (blanks Name*)", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1"), B = c("g2")),
        item_order = c("g1", "g2"),
        universe_size = 2L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "venn-2-set")
    svg <- render_venn_svg(res, show_names = FALSE)
    doc <- xml2::read_xml(svg)
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameA")), "")
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameB")), "")
})

test_that("render_venn_svg with model = 'proportional' delegates to generate_proportional_svg", {
    skip_on_cran()
    ds <- methods::new("VennDataset",
        set_names = c("A", "B"),
        items = list(A = c("g1", "g2"), B = c("g2", "g3")),
        item_order = c("g1", "g2", "g3"),
        universe_size = 3L, source_path = NULL, format = "csv"
    )
    res <- analyze(ds, model = "proportional")
    svg <- render_venn_svg(res)
    # Proportional SVG has the "Proportional Venn diagram" default title text
    expect_match(svg, "Proportional Venn diagram", fixed = TRUE)
})

test_that("render_venn_svg works on bundled cancer drivers sample", {
    skip_on_cran()
    skip_if_not(file.exists(system.file("extdata", "samples",
                                          "dataset_real_cancer_drivers_4.tsv",
                                          package = "vennDiagramLab")),
                "extdata/samples not synced")
    res <- analyze(load_sample("dataset_real_cancer_drivers_4"))
    svg <- render_venn_svg(res)
    doc <- xml2::read_xml(svg)
    # Names from cancer drivers TSV header: Vogelstein, COSMIC_CGC, OncoKB, IntOGen
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameA")), "Vogelstein")
    expect_equal(xml2::xml_text(.find_by_id(doc, "NameD")), "IntOGen")
    # Count_ABCD (all 4 sets) -- should be a non-negative integer string
    count_abcd <- xml2::xml_text(.find_by_id(doc, "Count_ABCD"))
    expect_match(count_abcd, "^[0-9]+$", perl = TRUE)
})
