#' Create a vintage sparse factor analysis object
#'
#' `adaptive_imputation` objects are a subclass of
#' [LRMF3::svd_like()], with an additional field `alpha`.
#'
#' @param Z TODO
#' @param B TODO
#' @param Y TODO
#'
#' @param u A *matrix* "left singular-ish" vectors.
#'
#' @param d A *vector* of "singular-ish" values.
#'
#' @param v A *matrix* of "right singular-ish" vectors.
#'
#' @param transformers
#'
#' @return An `adaptive_imputation` object.
#'
#' @export
vsp_fa <- function(u, d, v, Z, B, Y, transformers) {

  fa <- new_vsp_fa(
    Z = Z,
    B = B,
    Y = Y,
    subclasses = "vsp",
    u = u,
    d = d,
    v = v,
    transformers = transformers,
    ...
  )

  validate_vsp_fa(fa)
}


new_vsp_fa <- function(u, d, v, Z, B, Y, transformers) {
  fa_like(
    Z = Z,
    B = B,
    Y = Y,
    subclasses = "vsp_fa",
    u = u,
    d = d,
    v = v,
    transformers = transformers,
    ...
  )
}

validate_vsp_fa <- function(x) {

  # TODO

  # if (is.null(ai$alpha)) {
  #   stop(
  #     "Must have `alpha` field in adaptive imputation object.",
  #     call. = FALSE
  #   )
  # }
  #
  # if (!is.numeric(ai$alpha) || length(ai$alpha) != 1) {
  #   stop(
  #     "`alpha` must be a numeric vector of length 1.",
  #     call. = FALSE
  #   )
  # }
  x
}


#' @importFrom LRMF3 dim_and_class
#' @method print vsp_fa
#' @export
print.vsp_fa <- function(x, ...) {
  cat("Vintage Sparse PCA Factor Analysis\n\n")

  cat(glue("Nodes (n):   {nrow(x$U)}"), sep = "\n")
  cat(glue("Factors (rank): {x$rank}"), sep = "\n")
  cat(glue("Lambda[rank]:   {round(x$d[x$rank], 4)}"), sep = "\n")

  cat("\nPre-Processing Options (TODO) \n\n")

  cat("Components\n\n")

  # get the class printing to line up

  cat("Z:", dim_and_class(x$Z), "\n")
  cat("B:", dim_and_class(x$B), "\n")
  cat("Y:", dim_and_class(x$Y), "\n")

  cat("u:", dim_and_class(x$U), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("v:", dim_and_class(x$V), "\n\n")
}

