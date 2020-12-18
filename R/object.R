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
#' @param transformers TODO
#'
#' @return An `adaptive_imputation` object.
#'
#' @export
vsp_fa <- function(
  u, d, v,
  Z, B, Y,
  transformers,
  R_U, R_V,
  rownames = NULL, colnames = NULL) {

  fa <- new_vsp_fa(
    Z = as.matrix(Z),
    B = as.matrix(B),
    Y = as.matrix(Y),
    u = as.matrix(u),
    d = d,
    v = as.matrix(v),
    transformers = transformers,
    R_U = as.matrix(R_U),
    R_V = as.matrix(R_V)
  )

  if (is.null(rownames)) {
    rownames <- paste0("row", left_padded_sequence(1:nrow(fa$u)))
  }

  if (is.null(colnames)) {
    colnames <- paste0("col", left_padded_sequence(1:nrow(fa$v)))
  }

  rownames(fa$Z) <- rownames
  rownames(fa$u) <- rownames

  colnames(fa$Z) <- paste0("z", left_padded_sequence(1:fa$rank))
  colnames(fa$u) <- paste0("u", left_padded_sequence(1:fa$rank))

  rownames(fa$B) <- paste0("z", left_padded_sequence(1:fa$rank))
  colnames(fa$B) <- paste0("y", left_padded_sequence(1:fa$rank))

  rownames(fa$Y) <- colnames
  rownames(fa$v) <- colnames

  colnames(fa$Y) <- paste0("y", left_padded_sequence(1:fa$rank))
  colnames(fa$v) <- paste0("v", left_padded_sequence(1:fa$rank))

  validate_vsp_fa(fa)
}

#' TODO
#'
#' @param fa TODO
#'
#' @param names TODO
#'
#' @return TODO
#'
#' @export
set_z_factor_names <- function(fa, names) {

  if (length(unique(names)) != fa$rank) {
    stop("New Z factor names must be unique.", call. = FALSE)
  }

  if (length(names) != fa$rank) {
    stop(
      glue(
        "Incorrect number of Z factor names. Got {length(names)} but needed {fa$rank}."
      ),
      call. = FALSE
    )
  }

  colnames(fa$Z) <- names
  rownames(fa$B) <- names

  fa
}

#' TODO
#'
#' @param fa TODO
#'
#' @param names TODO
#'
#' @return TODO
#'
#' @export
set_y_factor_names <- function(fa, names) {

  if (length(unique(names)) != fa$rank) {
    stop("New Y factor names must be unique.", call. = FALSE)
  }

  if (length(names) != fa$rank) {
    stop(
      glue(
        "Incorrect number of Y factor names. Got {length(names)} but needed {fa$rank}."
      ),
      call. = FALSE
    )
  }

  colnames(fa$B) <- names
  colnames(fa$Y) <- names

  fa
}

new_vsp_fa <- function(u, d, v, Z, B, Y, transformers, R_U, R_V) {
  fa_like(
    Z = Z,
    B = B,
    Y = Y,
    subclasses = "vsp_fa",
    u = u,
    d = d,
    v = v,
    transformers = transformers,
    R_U = R_U,
    R_V = R_V
  )
}

validate_vsp_fa <- function(x) {

  # LMRF3::validate_fa_like(x)

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

  cat(glue("Rows (n):   {nrow(x$u)}"), sep = "\n")
  cat(glue("Cols (d):   {nrow(x$v)}"), sep = "\n")
  cat(glue("Factors (rank): {x$rank}"), sep = "\n")
  cat(glue("Lambda[rank]:   {round(x$d[x$rank], 4)}"), sep = "\n")

  cat("\nPre-Processing Options (TODO) \n\n")

  cat("Components\n\n")

  # get the class printing to line up

  cat("Z:", dim_and_class(x$Z), "\n")
  cat("B:", dim_and_class(x$B), "\n")
  cat("Y:", dim_and_class(x$Y), "\n")

  cat("u:", dim_and_class(x$u), "\n")
  cat("d:", dim_and_class(x$d), "\n")
  cat("v:", dim_and_class(x$v), "\n\n")
}