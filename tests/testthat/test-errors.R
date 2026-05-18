test_that(".stop_invalid_dataset raises a typed condition", {
    expect_error(
        .stop_invalid_dataset("test message"),
        regexp = "test message",
        class = "InvalidDatasetError"
    )
})

test_that(".stop_unknown_model raises a typed condition", {
    expect_error(
        .stop_unknown_model("model 'xyz' not found"),
        regexp = "model 'xyz' not found",
        class = "UnknownModelError"
    )
})

test_that(".stop_incompatible_model raises a typed condition", {
    expect_error(
        .stop_incompatible_model("proportional only supports 2-3 sets"),
        regexp = "proportional only supports 2-3 sets",
        class = "IncompatibleModelError"
    )
})

test_that("all custom error classes inherit from VennDiagramError", {
    err <- tryCatch(.stop_invalid_dataset("x"), error = function(e) e)
    expect_true(inherits(err, "VennDiagramError"))
    expect_true(inherits(err, "error"))
})
