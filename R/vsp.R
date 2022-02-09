#' Semi-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements TODO.
#'
#' @param x Either a graph adjacency matrix, [igraph::igraph] or
#'  [tidygraph::tbl_graph]. If `x` is a [matrix] or [Matrix::Matrix]
#'  then `x[i, j]` should correspond to the edge going from node `i`
#'  to node `j`.
#'
#' @param edge_weights When `x` is an [igraph::igraph], an edge attribute
#'  to use to form a weighted adjacency matrix.
#'
#' @param rank The number of factors to calculate.
#'
#' @param center Should the adjacency matrix be row *and* column centered?
#'  Defaults to `FALSE`.
#'
#' @param recenter Should the varimax factors be re-centered around the
#'  original factor means? Only used when `center = TRUE`, defaults to `FALSE`.
#'
#' @param degree_normalize Should the regularized graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
#'  first be centered and then normalized.
#'
#' @param renormalize Should the regularized graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
#'  first be centered and then normalized.
#'
#' @param tau_row Row regularization term. Default is `NULL`, in which case
#'  we use the row degree. Ignored when `degree_normalize = FALSE`.
#'
#' @param tau_col Column regularization term. Default is `NULL`, in which case
#'  we use the column degree. Ignored when `degree_normalize = FALSE`.
#'
#' @param kaiser_normalize_u Whether or not to use Kaiser normalization
#'  when rotating the left singular vectors `U`. Defaults to `FALSE`.
#'
#' @param kaiser_normalize_v Whether or not to use Kaiser normalization
#'  when rotating the right singular vectors `V`. Defaults to `FALSE`.
#'
#' @param rownames Character vector of row names of `x`. These row names
#'  are propagated into the row names of the `U` and `Z`. Defaults
#'  to `NULL`.
#'
#' @param colnames Character vector of column names of `x`. These column names
#'  are propagated into the row names of the `V` and `Y`. Defaults
#'  to `NULL`.
#'
#' @param ... Ignored.
#'
#' @details Sparse SVDs use `RSpectra` for performance.
#'
#' @return An object of class `vsp`. TODO: Details
#'
#' @export
#'
#' @examples
#'
#' library(LRMF3)
#'
#' vsp(
#'   ml100k,
#'   rank = 2,
#'   degree_normalize = TRUE,
#'   renormalize = FALSE,
#'   center = FALSE,
#'   recenter = FALSE,
#'   rownames = rownames(ml100k),
#'   colnames = colnames(ml100k)
#' )
#'
vsp <- function(x, rank, ...) {
  rlang::check_dots_used()
  UseMethod("vsp")
}

#' @rdname vsp
#' @export
vsp.default <- function(x, rank, ...) {
  stop(glue("No `vsp` method for objects of class {class(x)}. "))
}

#' @importFrom invertiforms DoubleCenter RegularizedLaplacian
#' @importFrom invertiforms transform inverse_transform
#' @rdname vsp
#' @export
vsp.matrix <- function(x, rank, ..., center = FALSE, recenter = FALSE,
                       degree_normalize = TRUE, renormalize = FALSE,
                       tau_row = NULL, tau_col = NULL,
                       kaiser_normalize_u = FALSE,
                       kaiser_normalize_v = FALSE,
                       rownames = NULL, colnames = NULL) {

  if (!is.integer(rank))
    rank <- round(rank)

  if (rank < 2)
    stop("`rank` must be at least two.", call. = FALSE)

  if (recenter && !center)
    stop("`recenter` must be FALSE when `center` is FALSE.", call. = FALSE)

  if (renormalize && !degree_normalize)
    stop("`renormalize` must be FALSE when `degree_normalize` is FALSE.", call. = FALSE)

  n <- nrow(x)
  d <- ncol(x)

  transformers <- list()

  if (center) {
    centerer <- DoubleCenter(x)
    transformers <- append(transformers, centerer)
    L <- transform(centerer, x)
  } else{
    L <- x
  }

  if (degree_normalize) {
    scaler <- RegularizedLaplacian(L, tau_row = tau_row, tau_col = tau_col)
    transformers <- append(transformers, scaler)
    L <- transform(scaler, L)
  }

  # this includes a call to isSymmetric that we might be able to skip out on
  s <- svds(L, k = rank, nu = rank, nv = rank)

  # do kaiser normalization by hand so we can deal with rows of all zeros

  if (kaiser_normalize_u) {
    s$u <- safe_row_l2_normalize(s$u)
  }

  if (kaiser_normalize_v) {
    s$v <- safe_row_l2_normalize(s$v)
  }

  R_U <- stats::varimax(s$u, normalize = FALSE)$rotmat
  R_V <- stats::varimax(s$v, normalize = FALSE)$rotmat

  Z <- sqrt(n) * s$u %*% R_U
  Y <- sqrt(d) * s$v %*% R_V

  B <- t(R_U) %*% Diagonal(n = rank, x = s$d) %*% R_V / (sqrt(n) * sqrt(d))

  fa <- vsp_fa(
    u = s$u, d = s$d, v = s$v,
    Z = Z, B = B, Y = Y,
    R_U = R_U, R_V = R_V,
    transformers = transformers,
    rownames = rownames, colnames = colnames
  )

  if (renormalize) {
    fa <- inverse_transform(scaler, fa)
  }

  if (recenter) {
    fa <- inverse_transform(centerer, fa)
  }

  fa <- make_skew_positive(fa)
  fa
}

#' Perform varimax rotation on a low rank matrix factorization
#'
#' @inheritParams vsp
#'
#' @param centerer TODO
#' @param scaler TODO
#'
#' @export
#'
#' @examples
#'
#' library(LRMF3)
#' library(RSpectra)
#'
#' s <- svds(ml100k, k = 2)
#' mf <- as_svd_like(s)
#' fa <- vsp(mf, rank = 2)
#'
vsp.svd_like <- function(x, rank, ...,
                         centerer = NULL, scaler = NULL,
                         recenter = FALSE, renormalize = FALSE,
                         kaiser_normalize_u = FALSE,
                         kaiser_normalize_v = FALSE,
                         rownames = NULL, colnames = NULL) {

  n <- nrow(x$u)
  d <- nrow(x$v)

  # do kaiser normalization by hand so we can deal with rows of all zeros

  if (kaiser_normalize_u) {
    x$u <- safe_row_l2_normalize(x$u)
  }

  if (kaiser_normalize_v) {
    x$v <- safe_row_l2_normalize(x$v)
  }

  R_U <- stats::varimax(x$u, normalize = FALSE)$rotmat
  R_V <- stats::varimax(x$v, normalize = FALSE)$rotmat

  Z <- sqrt(n) * x$u %*% R_U
  Y <- sqrt(d) * x$v %*% R_V

  B <- t(R_U) %*% Diagonal(n = rank, x = x$d) %*% R_V / (sqrt(n) * sqrt(d))

  fa <- vsp_fa(
    u = x$u, d = x$d, v = x$v,
    Z = Z, B = B, Y = Y,
    R_U = R_U, R_V = R_V,
    transformers = list(centerer, scaler),
    rownames = rownames, colnames = colnames
  )

  if (!is.null(scaler) && renormalize) {
    fa <- inverse_transform(scaler, fa)
  }

  if (!is.null(centerer) && recenter) {
    fa <- inverse_transform(centerer, fa)
  }

  fa <- make_skew_positive(fa)
  fa
}

#' @rdname vsp
#' @export
vsp.Matrix <- vsp.matrix

#' @rdname vsp
#' @export
vsp.dgCMatrix <- vsp.matrix

#' @rdname vsp
#' @export
vsp.igraph <- function(x, rank, ..., edge_weights = NULL) {
  x <- igraph::get.adjacency(x, sparse = TRUE, attr = edge_weights)
  vsp.matrix(x, rank, ...)
}
