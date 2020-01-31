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

# return +1 when skew positive, -1 when skew negative
skew_sign <- function(x) {
  sign(sum((x - mean(x))^3))
}

# X is a matrix
make_columnwise_skew_positive <- function(X) {
  apply(X, 2, function(x) return(x * skew_sign(x)))
}

