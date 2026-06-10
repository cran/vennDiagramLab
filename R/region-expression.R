# Boolean region-expression parser (Phase 11 — Feature D).
#
# Translates an expression like "A & B + !C" into a sorted integer vector of
# region bitmasks. Designed to compose with:
#   - render_venn_svg(..., highlight = parse_region_expression(...))
#   - exclusive_items() / intersection_items() / union_items() in user code.
#
# Grammar (precedence: ~ / ! > & > | / +; left-associative):
#   expr   := term ((`|` | `+`) term)*
#   term   := factor (`&` factor)*
#   factor := (`~` | `!`) factor | `(` expr `)` | letter
#   letter := A | B | ... | I (uppercase, ASCII)
#
# Implementation: hand-rolled tokenizer + shunting-yard to RPN + RPN eval
# over the bitmask space {1..2^n - 1}.

#' @noRd
# Tokenize an expression string into a list of tokens.
# Tokens: list(type = "letter"|"op"|"paren", value = string).
.tokenize_region_expr <- function(expr) {
    if (!nzchar(trimws(expr))) {
        stop("region expression is empty", call. = FALSE)
    }
    chars <- strsplit(expr, "", fixed = TRUE)[[1L]]
    tokens <- list()
    for (ch in chars) {
        if (grepl("^[ \t\n]$", ch)) next
        if (ch %in% c("&", "|", "+", "~", "!")) {
            tokens[[length(tokens) + 1L]] <- list(type = "op", value = ch)
        } else if (ch %in% c("(", ")")) {
            tokens[[length(tokens) + 1L]] <- list(type = "paren", value = ch)
        } else if (grepl("^[A-I]$", ch)) {
            tokens[[length(tokens) + 1L]] <- list(type = "letter", value = ch)
        } else {
            stop(sprintf("Unrecognised character in region expression: %s",
                          sQuote(ch)), call. = FALSE)
        }
    }
    tokens
}

#' @noRd
# Operator precedence + associativity for shunting-yard.
# Higher number = higher precedence. All are left-associative except `~`/`!`.
.op_precedence <- function(op) {
    switch(op,
        "~" = 4L, "!" = 4L,
        "&" = 3L,
        "|" = 2L, "+" = 2L,
        0L)
}

#' @noRd
.is_unary <- function(op) op %in% c("~", "!")

#' @noRd
# Convert infix token list to RPN (Reverse Polish Notation) token list via
# Dijkstra's shunting-yard. Throws on unbalanced parens or malformed input.
.shunting_yard <- function(tokens) {
    output <- list()
    op_stack <- list()
    expect_operand <- TRUE
    for (i in seq_along(tokens)) {
        t <- tokens[[i]]
        if (t$type == "letter") {
            if (!expect_operand) {
                stop("Malformed region expression: unexpected letter", call. = FALSE)
            }
            output[[length(output) + 1L]] <- t
            expect_operand <- FALSE
        } else if (t$type == "op") {
            if (.is_unary(t$value)) {
                if (!expect_operand) {
                    stop("Malformed region expression: unexpected unary op", call. = FALSE)
                }
                op_stack[[length(op_stack) + 1L]] <- t
                # still expecting operand after unary
            } else {
                if (expect_operand) {
                    stop("Malformed region expression: binary op without left operand",
                         call. = FALSE)
                }
                while (length(op_stack) > 0L) {
                    top <- op_stack[[length(op_stack)]]
                    if (top$type == "op" &&
                        .op_precedence(top$value) >= .op_precedence(t$value)) {
                        output[[length(output) + 1L]] <- top
                        op_stack[[length(op_stack)]] <- NULL
                    } else {
                        break
                    }
                }
                op_stack[[length(op_stack) + 1L]] <- t
                expect_operand <- TRUE
            }
        } else if (t$type == "paren" && t$value == "(") {
            op_stack[[length(op_stack) + 1L]] <- t
            expect_operand <- TRUE
        } else if (t$type == "paren" && t$value == ")") {
            matched <- FALSE
            while (length(op_stack) > 0L) {
                top <- op_stack[[length(op_stack)]]
                op_stack[[length(op_stack)]] <- NULL
                if (top$type == "paren" && top$value == "(") {
                    matched <- TRUE
                    break
                }
                output[[length(output) + 1L]] <- top
            }
            if (!matched) {
                stop("Parenthesis mismatch in region expression", call. = FALSE)
            }
            expect_operand <- FALSE
        }
    }
    while (length(op_stack) > 0L) {
        top <- op_stack[[length(op_stack)]]
        op_stack[[length(op_stack)]] <- NULL
        if (top$type == "paren") {
            stop("Parenthesis mismatch in region expression", call. = FALSE)
        }
        output[[length(output) + 1L]] <- top
    }
    if (expect_operand && length(output) > 0L) {
        stop("Malformed region expression: trailing operator", call. = FALSE)
    }
    output
}

#' @noRd
# Evaluate an RPN token list against the candidate bitmask universe
# {1..2^n - 1}. Each token is interpreted as a set-of-masks; binary ops are
# set operations (intersection / union); unary ~ / ! is complement against
# the candidate universe.
.eval_rpn <- function(rpn, n_sets) {
    universe <- seq_len(bitwShiftL(1L, n_sets) - 1L)
    stack <- list()
    for (i in seq_along(rpn)) {
        t <- rpn[[i]]
        if (t$type == "letter") {
            letter <- t$value
            bit_pos <- match(letter, strsplit(.LETTERS_VDL, "", fixed = TRUE)[[1L]]) - 1L
            if (is.na(bit_pos) || bit_pos >= n_sets) {
                stop(sprintf("Letter %s is out of range for n_sets = %d",
                              sQuote(letter), n_sets), call. = FALSE)
            }
            bit <- bitwShiftL(1L, bit_pos)
            stack[[length(stack) + 1L]] <- universe[bitwAnd(universe, bit) != 0L]
        } else if (t$type == "op") {
            op <- t$value
            if (.is_unary(op)) {
                if (length(stack) < 1L) stop("Malformed expression: unary op with empty stack", call. = FALSE)
                a <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
                stack[[length(stack) + 1L]] <- setdiff(universe, a)
            } else {
                if (length(stack) < 2L) stop("Malformed expression: binary op with insufficient operands", call. = FALSE)
                b <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
                a <- stack[[length(stack)]]; stack[[length(stack)]] <- NULL
                result <- switch(op,
                    "&" = intersect(a, b),
                    "|" = union(a, b),
                    "+" = union(a, b),
                    stop(sprintf("Unknown operator %s", sQuote(op)), call. = FALSE))
                stack[[length(stack) + 1L]] <- result
            }
        }
    }
    if (length(stack) != 1L) {
        stop("Malformed region expression: stack imbalance after evaluation",
             call. = FALSE)
    }
    sort(as.integer(stack[[1L]]))
}

#' Parse a Boolean region expression into bitmasks
#'
#' Translates an expression like `"A & B + !C"` into a sorted integer vector
#' of region bitmasks (each `1..2^n_sets - 1`). The output is intended for
#' composition with [render_venn_svg()] (`highlight = ...`) and with the
#' region-accessor family in user code.
#'
#' Grammar:
#' \itemize{
#'   \item `A`, `B`, ..., `I` — set atoms (uppercase ASCII).
#'   \item `&` — intersection.
#'   \item `|` or `+` — union.
#'   \item `~` or `!` — complement (unary, against `1..2^n_sets - 1`).
#'   \item `(`, `)` — grouping.
#' }
#' Precedence (highest first): `~ !`, `&`, `| +`. Binary operators are
#' left-associative.
#'
#' @param expr Character scalar; a Boolean expression in the grammar above.
#' @param n_sets Integer; number of sets in the diagram (2..9).
#' @return Sorted integer vector of region bitmasks. Empty integer vector
#'   when the expression is satisfiable by no region (e.g. `"A & ~A"`).
#' @examples
#' parse_region_expression("A & B", n_sets = 3L)        # c(3L, 7L)
#' parse_region_expression("A + B", n_sets = 3L)        # c(1, 2, 3, 5, 6, 7)
#' parse_region_expression("A & ~B", n_sets = 3L)       # c(1L, 5L)
#' parse_region_expression("(A | B) & C", n_sets = 3L)  # c(5L, 6L, 7L)
#' @export
parse_region_expression <- function(expr, n_sets) {
    if (!is.character(expr) || length(expr) != 1L) {
        stop("`expr` must be a character scalar.", call. = FALSE)
    }
    if (!is.numeric(n_sets) || length(n_sets) != 1L ||
        n_sets < 2L || n_sets > 9L) {
        stop("`n_sets` must be an integer in 2..9.", call. = FALSE)
    }
    n_sets <- as.integer(n_sets)
    tokens <- .tokenize_region_expr(expr)
    rpn <- .shunting_yard(tokens)
    .eval_rpn(rpn, n_sets)
}
