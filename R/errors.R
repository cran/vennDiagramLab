# Typed error helpers. R lacks Python's class-based exception system; we
# attach class attributes to a simpleCondition so testthat can match via
# `expect_error(..., class = "InvalidDatasetError")`. Inheritance is
# implemented by listing parent classes in the `class` vector.
#
# Helpers are private (leading dot) and not exported.

#' @noRd
.stop_typed <- function(message, class_name) {
    cond <- structure(
        class   = c(class_name, "VennDiagramError", "error", "condition"),
        list(message = message, call = sys.call(-1L))
    )
    stop(cond)
}

#' @noRd
.stop_invalid_dataset <- function(message) {
    .stop_typed(message, "InvalidDatasetError")
}

#' @noRd
.stop_unknown_model <- function(message) {
    .stop_typed(message, "UnknownModelError")
}

#' @noRd
.stop_incompatible_model <- function(message) {
    .stop_typed(message, "IncompatibleModelError")
}
