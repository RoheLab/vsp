
#' Title
#'
#' @param x A vector of integers, small enough the scientific
#'
#' @return TODO
#' @keywords internal
#'
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
#' @export
#' @keywords internal
make_skew_positive <- function(fa) {

  if (!inherits(fa, "fa_like"))
    stop("`make_skew_positive` is only intended for `fa_like` objects.")

  Z_column_skew_signs <- apply(fa$Z, 2, skew_sign)
  Y_column_skew_signs <- apply(fa$Y, 2, skew_sign)

  S_Z <- Diagonal(n = ncol(fa$Z), x = Z_column_skew_signs)
  S_Y <- Diagonal(n = ncol(fa$Y), x = Y_column_skew_signs)

  # note that S_Z and S_Y are their own inverses

  fa$Z <- fa$Z %*% S_Z
  fa$B <- S_Z %*% fa$B %*% S_Y
  fa$Y <- fa$Y %*% S_Y

  # update the rotation matrices so that we still have
  # Z = sqrt(n) * U %*% R_U, etc
  fa$R_U <- fa$R_U %*% S_Z
  fa$R_V <- fa$R_V %*% S_Y

  stopifnot(all(apply(fa$Z, 2, skew_sign) > 0))
  stopifnot(all(apply(fa$Y, 2, skew_sign) > 0))

  fa
}

stop_if_not_installed <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(glue("Must install {package} for this functionality.", call. = FALSE))
  }
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

