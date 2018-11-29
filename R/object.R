new_vsp <- function(Z, B, Y, scree, U, V) {
  # input validation: only type checks
  object <- list(Z = Z, B = B, Y = Y, scree = scree, U = U, V = V)
  class(object) <- "vsp"
  object
}

validate_vsp <- function(x) {
  # input validation: values are actually good
}

#' @export
print.vsp <- function(x, ...) {
  cat("Vintage Sparse PCA Factor Analysis\n\n")

  dim_or_null <- function(x) if (is.null(x)) NULL else dim(x)

  cat("Z:", dim_or_null(x$Z), "\n")
  cat("B:", dim_or_null(x$B), "\n")
  cat("Y:", dim_or_null(x$Y), "\n")
  cat("U:", dim_or_null(x$U), "\n")
  cat("V:", dim_or_null(x$V), "\n\n")

  cat("scree:", x$scree, "\n")
}
