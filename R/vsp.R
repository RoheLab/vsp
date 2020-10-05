#' Non-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements TODO.
#'
#' @param x Either a graph adjacency matrix, [igraph::igraph] or
#'  [tidygraph::tbl_graph]. If `x` is a [matrix] or [Matrix::Matrix]
#'  then `x[i, j]` should correspond to the edge going from node `i`
#'  to node `j`.
#'
#' @param k The number of factors to calculate.
#'
#' @param center Should the adjacency matrix be row *and* column centered?
#'  Defaults to `TRUE`.
#'
#' @param normalize Should the graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
#'  first be centered and then normalized.
#'
#' @param tau_row Row regularization term. Default is `NULL`, in which case
#'  we use the row degree. Ignored when `normalize = FALSE`.
#'
#' @param tau_col Column regularization term. Default is `NULL`, in which case
#'  we use the column degree. Ignored when `normalize = FALSE`.
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
#' library(jiandjindata)
#' library(igraph)
#' library(invertiforms)
#'
#' A <- get.adjacency(citation_graph)
#'
#'
vsp <- function(x, k, ..., center = FALSE, normalize = TRUE,
                tau_row = NULL, tau_col = NULL) {
  ellipsis::check_dots_empty()
  UseMethod("vsp")
}


#' @rdname vsp
#' @export
vsp.default <- function(x, k, ..., center = FALSE, normalize = TRUE,
                        tau_row = NULL, tau_col = NULL) {

  ### Vintage Sparse PCA Reference Implementation

  ## INPUT VALIDATION

  if (k < 2)
    stop("`k` must be at least two.", call. = FALSE)

  n <- nrow(x)
  d <- ncol(x)

  if (center) {
    centerer <- DoubleCenter(x)
    L <- transform(centerer, x)
  } else{
    L <- x
  }

  if (normalize) {
    normalizer <- RegularizedLaplacian(A, tau_row = tau_row, tau_col = tau_col)
    L <- transform(normalizer, L)
  }

  # this includes a call to isSymmetric that we might be able to skip out on
  s <- RSpectra::svds(L, k = k, nu = k, nv = k)
  U <- s$u
  V <- s$v

  R_U <- varimax(U[rsA > 1, ], normalize = FALSE)$rotmat
  R_V <- varimax(V[csA > 1, ], normalize = FALSE)$rotmat

  Z <- sqrt(n) * U %*% R_U
  Y <- sqrt(d) * V %*% R_V

  B <- t(R_U) %*% Diagonal(n = k, x = s$d) %*% R_V / (sqrt(n) * sqrt(d))

  # TODO: make fa skew positive, invert the transformationations

  fa <- LRMF3::fa_like()

  ### STEP 5: MAKE Z, Y SKEW POSITIVE (REMARK 1.3)

  # TODO: not sure I trust this fully just yet
  Z <- make_columnwise_skew_positive(Z)
  Y <- make_columnwise_skew_positive(Y)

  ### STEP 6: RESCALE IF NORMALIZED, RETURN OUTPUT

  if (normalize) {
    # proper re-normalization is like this:
    Dnorm_row <- Diagonal(n = n, x = sqrt(rsA))
    Dnorm_col <- Diagonal(n = d, x = sqrt(csA))

    Z <- Dnorm_row %*% Z
    Y <- Dnorm_col %*% Y
   }

  new_vsp(
    U = as.matrix(U),
    d = s$d,
    V = as.matrix(V),
    Z = as.matrix(Z),
    B = as.matrix(B),
    Y = as.matrix(Y),
    center = center,
    normalize = normalize,
    k = k,
    tau_list = list(
      tau_row = if (exists("tau_r")) tau_r else NULL,
      tau_col = if (exists("tau_c")) tau_c else NULL,
      default_row = default_row,
      default_col = default_col
    )
  )
}

#' @rdname vsp
#' @export
vsp.igraph <- function(x, k, ..., center = FALSE, normalize = TRUE,
                       weights = NULL, tau_row = NULL, tau_col = NULL) {
  x <- igraph::get.adjacency(x, sparse = TRUE, attr = weights)
  NextMethod()
}
