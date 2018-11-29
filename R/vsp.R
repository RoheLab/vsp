#' Non-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements regularized spectral clustering with varimax.
#' allows for symmetric, directed, and bipartite (i.e. rectangular A).
#'
#' @param A A [matrix] or [Matrix::Matrix] object.
#' @param k The number of factors to calculate.
#' @param tau_row Row regularization term. Defaults is `NULL`, in which case
#'  we use the row degree.
#' @param tau_col Column regularization term. Defaults is `NULL`, in which case
#'  we use the column degree.
#' @param normalize Should the graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`.
#'
#' @details Uses `RSpectra` for matrix computations. These are best-in-class
#'  implementations of sparse matrix eigendecompositions. The implementations
#'  are not parallel, however.
#'
#'  TODO: how the normalization is done, how the centering is done
#'
#' @return An object of class `vsp`, which is a list with elements:
#'
#'   - `U`: TODO
#'   - `d`: TODO
#'   - `V`: TODO
#'   - `Z`: TODO
#'   - `B`: TODO
#'   - `Y`: TODO
#'
#' @export
vsp <- function(A, k = 5, tau_row = NULL, tau_col = NULL, normalize = TRUE, center = FALSE) {

  ### Vintage Sparse PCA Reference Implementation

  n <- nrow(A)
  d <- ncol(A)

  ### STEP 1: OPTIONAL NORMALIZATION

  # normalization corresponds to the optional scaling step defined
  # defined in Remark 1.1

  if (normalize) {

    rsA <- rowSums(A)
    csA <- colSums(A)

    if (is.null(tau_row))
      tau_row <- mean(rsA)

    if (is.null(tau_col))
      tau_col <- mean(csA)

    D_row <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_row))
    D_col <- Diagonal(n = d, x = 1 / sqrt(csA + tau_col))

    # note: no identity matrix in the graph Laplacian here
    L <- D_row %*% A %*% D_col
  } else {
    L <- A
  }

  ### STEP 2: OPTIONAL CENTERING

  # here we center explicitly for clarity, but this should be done
  # implicitly for performance, as discussed in Remark 1.2

  if (center) {
    L <- sweep(L, 1, Matrix::rowMeans(L))
    L <- sweep(L, 2, Matrix::colMeans(L))
    L <- L + Matrix::mean(L)
  }

  ### STEP 3: SVD

  # this doesn't differentiate between the symmetric and asymmetric cases
  # so there's opportunity for speed gains here

  s <- RSpectra::svds(L, k = k, nu = k, nv = k)
  U <- s$u
  V <- s$v

  ### STEP 4: VARIMAX ROTATION

  R_U <- varimax(U, normalize = F)$rotmat
  R_V <- varimax(V, normalize = F)$rotmat

  Z <- sqrt(n) * U %*% R_U
  Y <- sqrt(d) * V %*% R_V

  B <- t(R_U) %*% Diagonal(n = k, x = s$d) %*% R_V

  ### STEP 5: RESCALE IF NORMALIZED, RETURN OUTPUT

  if (normalize) {
    Z <- D_row %*% Z
    Y <- D_col %*% Y
  }

  new_vsp(
    U = U,
    d = s$d,
    V = V,
    Z = Z,
    B = B,
    Y = Y,
    center = center,
    normalize = normalize
  )
}
