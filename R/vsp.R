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

  A <- x

  if (k < 2)
    stop("`k` must be at least two.", call. = FALSE)

  n <- nrow(A)
  d <- ncol(A)

  default_row <- is.null(tau_row)
  default_col <- is.null(tau_col)

  # needed both for normalization and subsetting rows for varimax
  # note that we use absolute value of edge weights in case elements
  # of A are negative to avoid divide by zero issues

  rsA <- Matrix::rowSums(A * sign(A))  # out-degree
  csA <- Matrix::colSums(A * sign(A))  # in-degree

  ### STEP 1: OPTIONAL NORMALIZATION

  # normalization corresponds to the optional scaling step defined
  # defined in Remark 1.1

  if (normalize) {

    tau_r <- if (default_row) mean(rsA) else tau_row
    tau_c <- if (default_col) mean(csA) else tau_col

    D_row <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_r))
    D_col <- Diagonal(n = d, x = 1 / sqrt(csA + tau_c))

    # note: no identity matrix in the graph Laplacian here
    L <- D_row %*% A %*% D_col
  } else {
    L <- A
  }

  ### STEP 2: OPTIONAL CENTERING

  # here we center explicitly for clarity, but this should be done
  # implicitly for performance, as discussed in Remark 1.2

  if (center) {
    L <- double_center(L)
  }

  ### STEP 3: SVD

  # this doesn't differentiate between the symmetric and asymmetric cases
  # so there's opportunity for speed gains here

  # this includes a call to isSymmetric that we might be able to skip out on
  s <- RSpectra::svds(L, k = k, nu = k, nv = k)
  U <- s$u
  V <- s$v

  ### STEP 4: VARIMAX ROTATION

  # subset to only nodes with degree greater than 1. huge time saver.
  # some of karl's code uses rsL and csL here, which I think is a mistake
  # because it drops the computation time by an order of magnitude. is that
  # throwing out nonsense that doesn't affect results, or does it affect
  # results?

  # TODO: use some quantile of the degree distribution here instead?
  # if we first rotate both together, then B tends to have a strong diagonal.
  #  without this R_both, B tends to have a permutation matrix applied to row/column,
  #  making the j_th column of Z not match the j_th column of Y.
  #R_both <- varimax(rbind(U[rsA > 1, ],U[rsA > 1, ]), normalize = FALSE)$rotmat

  #R_U <- varimax(U[rsA > 1, ]%*%R_both, normalize = FALSE)$rotmat
  #R_V <- varimax(V[csA > 1, ]%*%R_both, normalize = FALSE)$rotmat


  R_U <- varimax(U[rsA > 1, ], normalize = FALSE)$rotmat
  R_V <- varimax(V[csA > 1, ], normalize = FALSE)$rotmat

  Z <- sqrt(n) * U %*% R_U
  Y <- sqrt(d) * V %*% R_V

  # TODO: check the paper and see if we should divide B by sqrt(n * d) here?  Yes, we should... because we are scaling Z and Y.
  # B <- t(R_U) %*% Diagonal(n = k, x = s$d) %*% R_V/sqrt(n*d)  # n*d causes integer overflow
  B <- t(R_U) %*% Diagonal(n = k, x = s$d) %*% R_V/sqrt(n)
  B <- B/sqrt(d)

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

double_center <- function(L) {
  warning(
    "Implicit centering has not yet been implemented.\n\n",
    call. = FALSE
  )

  L <- sweep(L, 1, Matrix::rowMeans(L))
  L <- sweep(L, 2, Matrix::colMeans(L))
  L + Matrix::mean(L)
}
