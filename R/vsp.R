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
#' @details The `irlba` package is used for non-centered calculations, which
#'  requires calculating a partial SVD. Calculations for centered problems
#'  use `rARPACK`.
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
vsp <- function(A, k = 5, tau = NULL, normalize = TRUE, center = TRUE, sym = NULL) {

  ### STEPS
  # 1. input validation
  # 2. construct the graph laplacian
  # 3. do PCA on the graph laplacian
  # 4. varimax rotation

  ## questions
  # - what is a two-way analysis?
  # - is -1 being used a flag to say "do some reasonable default?"

  ### STEP 1: INPUT VALIDATION

  # need to know:
  # - is the adjacency matrix A symmetric?
  # - should we do a "one-way" or "two-way" calculation?
  #   - one-way calculations use eigen (?)
  #   - two-way calculations use svd (?)
  # - can check for symmetry with `base::isSymmetric(A)`

  # TODO: allow user to set this?
  if (is.null(sym))
    sym <- nrow(A) == ncol(A)

  # two way analysis is what happens when A is not symmetric?
  tw <- !sym

  rsA <- rowSums(A)  # rowSums are the (in/out?) degrees of each node
  csA <- colSums(A)

  if (!tw) {
    if (sd(rsA - csA) > 10^(-10)) {
      tw <- T
      print("The input matrix is not symmetric.  Performing a two-way analysis.")
      # return(NA)
    }
  }

  ### STEP 2: CONSTRUCT GRAPH LAPLACIAN L

  n <- nrow(A)
  d <- ncol(A)

  # normalize if requested

  if (normalize) {

    # if the user hasn't specified a regulatization parameter tau,
    # use the average (in/out?) node degree
    if (is.null(tau)) {
      tau <- mean(rsA)
    }

    # normalization. this does *not* support vector valued tau
    # i.e. tau_row and tau_col must be the same

    D <- Diagonal(n = n, x = 1 / sqrt(rsA + tau))
    tmp <- D %*% A
    if (tw) D <- Diagonal(n = d, x = 1 / sqrt(csA + mean(csA)))
    L <- tmp %*% D
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

    rsL <- rowSums(L)
    csL <- colSums(L)

    args <- list(
      A = L,
      At = t(L),
      rs = rsL,
      cs = csL,
      n = n,
      d = d,
      meanA = mean(L),
      onesR = rep(1, nrow(L)),
      onesC = rep(1, ncol(L))
    )

    ei <- rARPACK::eigs_sym(fcent, n = n, k = k, args = args, which = "LA")

    U <- ei$vectors

    V <- if (tw) apply(t(U) %*% L, 1, function(x) return(x / sqrt(sum(x^2))))

    d <- if(tw) sqrt(ei$values) else ei$values
  } else{
    # uncentered PCA

    # only get the right singular vectors in the two-way case
    nv <- if (tw) k else 0
    s <- irlba::irlba(L, nu = k, nv = nv)
    U <- s$u
    d <- s$d
    V <- if (tw) s$v else NULL
  }

  ### STEP 4: VARIMAX ROTATION

  # should rsA and csA be rsL and csL??

  rotHatU <- varimax(U[rsA > 1, ], normalize = F)$rot
  Zhat <- U %*% rotHatU

  # switch signs to ensure each column of Zhat has positive third moment.
  signss <- sign(colSums(Zhat^3))
  rotHatU <- rotHatU %*% diag(signss)
  Zhat <- U %*% rotHatU

  rotHatV <- rotHatU

  if (tw) {
    rotHatV <- varimax(V[csA > 1, ], normalize = F)$rot
    Yhat <- V %*% rotHatV

    signss <- sign(colSums(Yhat^3))
    rotHatV <- rotHatV %*% diag(signss)
    Yhat <- V %*% rotHatV
  }

  Bhat <- t(rotHatU) %*% diag(d) %*% rotHatV
  scree <- if (center) ei$values else s$d

  ### STEP 5: CREATE A VSP OBJECT

  Yhat <- if (tw) Yhat else NULL

  new_vsp(Z = Zhat, B = Bhat, Y = Yhat, scree = scree, U = U, V = V)
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

