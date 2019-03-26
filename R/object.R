new_vsp <- function(U, d, V, Z, B, Y, center, normalize, k, tau_list) {
  # input validation: only type checks

  # tau_list: tau_row, tau_col, default_row = TRUE/FALSE, default_col = T/F

  object <- list(
    U = U,
    d = d,
    V = V,
    Z = Z,
    B = B,
    Y = Y,
    center = center,
    normalize = normalize,
    k = k,
    tau_list = tau_list
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

  cat(glue("Nodes (n):   {nrow(x$U)}"), sep = "\n")
  cat(glue("Factors (k): {x$k}"), sep = "\n")
  cat(glue("Lambda[k]:   {round(x$d[x$k], 4)}"), sep = "\n")

  cat("\nPre-Processing Options\n\n")

  tau_details <- function(default, tau) {
    if (!x$normalize)
      "None"
    else if (default)
      paste0(round(tau, 2), " [Default: Mean Degree]")
    else
      paste0(round(tau, 2), " [User-Specified]")
  }

  cat(" - Centering:    ", x$center, "\n")
  cat(" - Normalization:", x$normalize, "\n")
  cat(
    "   - Tau (rows): ",
    tau_details(x$tau_list$default_row, x$tau_list$tau_row), "\n"
  )
  cat(
    "   - Tau (cols): ",
    tau_details(x$tau_list$default_col, x$tau_list$tau_col), "\n\n"
  )

  cat("Components\n\n")

  dim_and_class <- function(x) {
    if (is.vector(x))
      paste0(length(x), "      [", class(x)[1], "]")
    else
      # is a matrix
      paste0(nrow(x), " x ", ncol(x), " [", class(x)[1], "]")
  }

  # get the class printing to line up

  cat("U:", dim_and_class(x$U), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("V:", dim_and_class(x$V), "\n")
  cat("Z:", dim_and_class(x$Z), "\n")
  cat("B:", dim_and_class(x$B), "\n")
  cat("Y:", dim_and_class(x$Y), "\n\n")
}


#' @export
#' @import ggplot2
screeplot.vsp <- function(x, ...) {
  ggplot(data = NULL, aes(1:x$k, x$d)) +
    geom_point() +
    labs(
      title = "Singular values of adjacency matrix",
      caption = "If `normalize = TRUE`, singular values of graph Laplacian",
      x = "Singular value",
      y = "Value"
    ) +
    ylim(0, 1) +
    theme_bw()
}
