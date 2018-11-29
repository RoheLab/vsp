# TODO:
# 1) For symmetric calculations, it would be faster to find eigenvectors.
# 2) Currently has adjMat as input.  Should could allow igraph object.
# 3) should we find k+1 eigenvalues to inspect the eigengap?

#' Non-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements regularized spectral clustering with varimax.
#' allows for symmetric, directed, and bipartite (i.e. rectangular A).
#'
#' @param A A [matrix] or [Matrix::Matrix] object.
#' @param k is the number of clusters.  must be same for rows and columns.
#' @param tau is regularization parameter.  it can be a 2 vectors.
#'   Default is pretty good.
#' @param normalize Should the adjacency matrix be normalized?
#' @param center Should the adjacency matrix `A` be row and column centered?
#' @param sym Set to `FALSE` for a two-way analysis.
#'
#' @details Uses `RSpectra` for matrix computations. These are best-in-class
#'  implementations of sparse matrix eigendecompositions. The implementations
#'  are not parallel, however.
#'
#'  TODO: how the normalization is done, how the centering is done
#'
#' @return An object of class `vsp`, which is a list with elements:
#'
#'   - `V`: only returned in the rectangular case
#'   - `U`: TODO
#'   - `Z`: TODO
#'   - `B`: TODO
#'   - `scree`: TODO
#'   - `Y`: TODO
#'
#' @export
vsp <- function(A, k = 5, tau = NULL, normalize = TRUE, center = TRUE, symmetric = NULL) {

  ### STEPS
  # 1. input validation
  # 2. construct the graph laplacian
  # 3. do PCA on the graph laplacian
  # 4. varimax rotation

  ### STEP 1: INPUT VALIDATION

  # - can check for symmetry with `base::isSymmetric(A)`

  # TODO: allow user to set this?
  if (is.null(symmetric))
    symmetric <- nrow(A) == ncol(A)

  rsA <- rowSums(A)  # rowSums are the (in/out?) degrees of each node
  csA <- colSums(A)

  # warning: vector recycling
  if (symmetric && sd(rsA - csA) > 10^(-10)) {
    # TODO: understand why this is?
    symmetric <- FALSE
    print("The input matrix is not symmetric.  Performing a two-way analysis.")
  }

  ### STEP 2: CONSTRUCT GRAPH LAPLACIAN L

  n <- nrow(A)
  d <- ncol(A)

  if (normalize) {

    # TODO: allow user to specify these. currently tau gets ignored
    tau_row <- mean(rsA)
    tau_col <- mean(csA)

    # normalization. this does *not* support vector valued tau
    # i.e. tau_row and tau_col must be the same

    D1 <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_row))

    if (symmetric) {
      D2 <- D1
    } else {
      D2 <- Diagonal(n = d, x = 1 / sqrt(csA + tau_col))
    }

    # TODO: subtract this from the identity!
    L <- D1 %*% A %*% D2  # joke
  } else {
    L <- A
  }

  ### STEP 3: PCA

  # here we do the PCA differently if the user has/has not centered the data

  # this is a fancy way of doing the eigendecomposition for the centered
  #  version of L... that is, the matrix that is like L - 1 d^T - d 1^T + 11^T
  #  the reason it is fancy is that multiplying by that matrix can be fast, but can't
  #  define the full nxn (dense) matrix...
  if (center) {
    s <- centered_svd(L, k, symmetric)

    U <- s$u
    V <- s$v
    d <- s$d
  } else{
    # uncentered PCA

    # only get the right singular vectors in the two-way case
    # how do we get V otherwise?
    nv <- if (symmetric) 0 else k
    s <- RSpectra::svds(L, nu = k, nv = nv)
    U <- s$u
    d <- s$d

    # how do you get V in the symmetric
    V <- if (tw) s$v else NULL
  }

  ### STEP 4: VARIMAX ROTATION

  # rsL and csL not rsA and csA right?
  rsL <- rowSums(L)
  csL <- colSums(L)

  rotHatU <- varimax(U[rsL > 1, ], normalize = F)$rot
  Zhat <- U %*% rotHatU

  # switch signs to ensure each column of Zhat has positive third moment.
  signss <- sign(colSums(Zhat^3))
  rotHatU <- rotHatU %*% diag(signss)
  Zhat <- U %*% rotHatU

  rotHatV <- rotHatU

  if (!symmetric) {
    rotHatV <- varimax(V[csL > 1, ], normalize = F)$rot
    Yhat <- V %*% rotHatV

    signss <- sign(colSums(Yhat^3))
    rotHatV <- rotHatV %*% diag(signss)
    Yhat <- V %*% rotHatV
  }

  Bhat <- t(rotHatU) %*% diag(d) %*% rotHatV

  # need to get this back now too
  # scree <- if (center) ei$values else s$d

  ### STEP 5: CREATE A VSP OBJECT

  Yhat <- if (!symmetric) Yhat else NULL

  new_vsp(Z = Zhat, B = Bhat, Y = Yhat, scree = NULL, U = U, V = V)
}

# L is the graph Laplacian
centered_svd <- function(L, k, symmetric) {

  rsL <- rowSums(L)
  csL <- colSums(L)

  n <- nrow(L)
  d <- ncol(L)

  args <- list(
    A = L,
    At = t(L),
    rs = rsL,
    cs = csL,
    n = n,
    d = d,
    meanA = mean(L),
    onesR = rep(1, n),
    onesC = rep(1, d)
  )

  # this implicitly does both symmetric and asymmetric cases,
  # make this explicit
  ei <- RSpectra::eigs_sym(fcent, n = n, k = k, args = args, which = "LA")

  U <- ei$vectors

  V <- if (!symmetric) apply(t(U) %*% L, 1, function(x) return(x / sqrt(sum(x^2))))

  d <- if(symmetric) ei$values else sqrt(ei$values)

  list(u = U, v = V, d = d)
}

# multiplies centered(args$A)%*%x, quickly for sparse A.
# args$ contains: A, rs, n, meanA, onesR, At
# if problem is symmetric is_null(At) is true
# if problem is not symmetric, then must also contain cs, d, onesC
#  A is dgcMatrix
#  rs is rowSums(A)
#  n is nrow(A)
#  meanA = mean(A)
#  onesR = rep(1, n) # R stands for row (length n)
#  At = t(A) (or At = NULL)
#  cs = colSums(A)
#  d = ncol(A)
#  onesC = rep(1,d)  # C stands for column (length d)
# this code stores both A and At because we presume that At%*%x is faster than t(A)%*%x.
#   this is the case when A is of the type dgCMatrix and At is also dgCMatrix.
fcent <- function(x, args) {
  # if tA is not stored, then the problem is symmetric.
  # otherwise, the problem is asymmetric.
  symmetric <- is.null(args$At)
  if (symmetric) Ax(x, args) else Ax(Atx(x, args), args)
}

Ax <- function(x, args) {
  res <- with(
    args,
    A %*% x - onesR * (as.numeric((t(cs) %*% x) / n) - sum(x) * meanA) - rs * mean(x)
  )
  as.vector(res)
}

Atx <- function(x, args) {
  res <- with(
    args,
    At %*% x - onesC * (as.numeric((t(rs) %*% x) / d) - sum(x) * meanA) -
      cs * mean(x)
  )
  as.vector(res)
}

