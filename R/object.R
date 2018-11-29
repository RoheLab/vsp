new_vsp <- function(U, d, V, Z, B, Y, center, normalize) {
  # input validation: only type checks
  object <- list(
    U = U,
    d = d,
    V = V,
    Z = Z,
    B = B,
    Y = Y,
    center = center,
    normalize = normalize
  )

  class(object) <- "vsp"
  object
}

validate_vsp <- function(x) {
  # input validation: values are actually good
}

#' @export
print.vsp <- function(x, ...) {
  cat("Vintage Sparse PCA Factor Analysis\n\n")

  cat("Pre-processing Options")

  cat("Components: \n")

  dim_and_class <- function(x) {
    if (is.vector(x))
      paste0(length(x), "      [", class(x)[1], "]")
    else
      # is a matrix
      paste0(nrow(x), " x ", ncol(x), " [", class(x)[1], "]")
  }
    paste0(nrow(x), " x ", ncol(x), " [", class(x)[1], "]")

  # get the class printing to line up

  cat("U:", dim_and_class(x$U), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("V:", dim_and_class(x$V), "\n")
  cat("Z:", dim_and_class(x$Z), "\n")
  cat("B:", dim_and_class(x$B), "\n")
  cat("Y:", dim_and_class(x$Y), "\n\n")
}
