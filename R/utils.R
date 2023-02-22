#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

left_padded_sequence <- function(x) {

  original <- withr::with_options(
    c(scipen = 999),
    as.character(x)
  )

  max_digits <- max(vapply(original, nchar, integer(1)))
  formatC(x, width = max_digits, format = "d", flag = "0")
}

# return +1 when skew positive, -1 when skew negative
skew_sign <- function(x) {
  sign(sum((x - mean(x))^3))
}

stop_if_not_installed <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(glue("Must install {package} for this functionality.", call. = FALSE))
  }
}

#' Safe L2 row normalization
#'
#' Helper function for Kaiser normalization to handle rows with zero (or
#' numerically zero) norm, which results in a divide by zero error
#' in the `stats::varimax()` implementation.
#'
#' @param x A matrix to row normalize.
#' @param eps Tolerance to use when assessing if squared L2 row norm is
#'   numerically larger or smaller than zero.
#'
#' @keywords internal
#'
#' @return The row-rescaled matrix
#'
safe_row_l2_normalize <- function(x, eps = 1e-10) {
  sc <- drop(apply(x, 1L, function(y) sum(y^2)))
  sc[sc < eps] <- 1
  x / sqrt(sc)
}

utils::globalVariables(
  c(
    ".",
    "activate",
    "arrange",
    "desc",
    "element",
    "gather",
    "group_by",
    "id",
    "importance",
    "leverage",
    "loading",
    "mutate",
    "node",
    "nodes",
    "pivot_longer",
    "pivot_wider",
    "row_number",
    "sample_n",
    "select",
    "top_n",
    "ungroup",
    "value",
    "word"
  )
)

