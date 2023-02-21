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

#' Make factors columnwise skew positive
#'
#' Given a factor analysis like object, flip
#' signs so that columns of `Z` and `Y` are
#' skew positive. Note that this also causes
#' corresponding sign flips in `B`. This
#' helps with interpretability of factors.
#'
#' @param fa A [fa_like()] object.
#'
#' @return A new [fa_like()] object where the columns
#'   of `Z` and `Y` has positive skew, that is otherwise
#'   equivalent to the original object.
#'
#' @keywords internal
make_skew_positive <- function(fa) {

  if (!inherits(fa, "fa_like"))
    stop("`make_skew_positive` is only intended for `fa_like` objects.")

  Z_column_skew_signs <- apply(fa$Z, 2, skew_sign)
  Y_column_skew_signs <- apply(fa$Y, 2, skew_sign)

  # use rowScale and dimScale instead of matrix multiplication
  # to preserve column names following update to Matrix package

  fa$Z <- colScale(fa$Z, Z_column_skew_signs)
  fa$B <- dimScale(fa$B, Z_column_skew_signs, Y_column_skew_signs)
  fa$Y <- colScale(fa$Y, Y_column_skew_signs)

  # update the rotation matrices so that we still have
  # Z = sqrt(n) * U %*% R_U, etc
  fa$R_U <- colScale(fa$R_U, Z_column_skew_signs)
  fa$R_V <- colScale(fa$R_V, Y_column_skew_signs)

  # in some cases (i.e. columns of Y or Z are constant) the skew
  # is zero

  stopifnot(all(apply(fa$Z, 2, skew_sign) >= 0))
  stopifnot(all(apply(fa$Y, 2, skew_sign) >= 0))

  fa
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

