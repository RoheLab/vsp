#' # TODO:
#' # 1) For symmetric calculations, it would be faster to find eigenvectors.
#' # 2) Currently has adjMat as input.  Should could allow igraph object.
#' # 3) should we find k+1 eigenvalues to inspect the eigengap?
#'
#' # things that can be made fancy
#' # - varimax: don't use low degree nodes
#' # - more efficient computation for symmetric A
#' # - implicitly power-svd calculation for centering
#'
#' #' Non-Parametric Factor Analysis via Vintage Sparse PCA
#' #'
#' #' This code implements regularized spectral clustering with varimax.
#' #' allows for symmetric, directed, and bipartite (i.e. rectangular A).
#' #'
#' #' @param A A [matrix] or [Matrix::Matrix] object.
#' #' @param k The number of factors to calculate.
#' #' @param tau_row Row regularization term. Defaults is `NULL`, in which case
#' #'  we use the row degree.
#' #' @param tau_col Column regularization term. Defaults is `NULL`, in which case
#' #'  we use the column degree.
#' #' @param normalize Should the graph laplacian be used instead of the
#' #'  raw adjacency matrix? Defaults to `TRUE`.
#' #'
#' #' @details Uses `RSpectra` for matrix computations. These are best-in-class
#' #'  implementations of sparse matrix eigendecompositions. The implementations
#' #'  are not parallel, however.
#' #'
#' #'  TODO: how the normalization is done, how the centering is done
#' #'
#' #' @return An object of class `vsp`, which is a list with elements:
#' #'
#' #'   - `V`: only returned in the rectangular case
#' #'   - `U`: TODO
#' #'   - `Z`: TODO
#' #'   - `B`: TODO
#' #'   - `scree`: TODO
#' #'   - `Y`: TODO
#' #'
#' #' @export
#' vsp <- function(A, k = 5, tau_row = NULL, tau_col = NULL, normalize = TRUE, center = FALSE) {
#'
#'   ### Vintage Sparse PCA Reference Implementation
#'   # 1.
#'
#'
#'
#'   ### STEPS
#'   # 1. input validation
#'   # 2. centering (not yet implemented)
#'   # 2. construct the graph laplacian
#'   # 3. do PCA on the graph laplacian
#'   # 4. varimax rotation
#'
#'   ### CONSTRUCT GRAPH LAPLACIAN L
#'
#'   n <- nrow(A)
#'   d <- ncol(A)
#'
#'   # normalization corresponds to the optional scaling step defined
#'   # defined in Remark 1.1
#'   if (normalize) {
#'
#'     # called "scaling" in the paper. don't forget to rescale output
#'
#'     if (is.null(tau_row))
#'       tau_row <- mean(rowSums(A))
#'
#'     if (is.null(tau_col))
#'       tau_col <- mean(colSums(A))
#'
#'     D_row <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_row))
#'     D_col <- Diagonal(n = d, x = 1 / sqrt(csA + tau_col))
#'
#'     # note: no identity matrix in the graph Laplacian here
#'     L <- D_row %*% A %*% D_col
#'   } else {
#'     L <- A
#'   }
#'
#'   # here we center explicitly for clarity, but this should be done
#'   # implicitly for performance, as discussed in Remark 1.2
#'
#'   if (center) {
#'     L <- sweep(L, 1, Matrix::rowMeans(L))
#'     L <- sweep(L, 2, Matrix::colMeans(L))
#'     L <- L + Matrix::mean(L)
#'   }
#'
#'   ### STEP 3: PCA
#'
#'   ## some benchmarking showed that the remark matrix-vector product trick
#'   # was 5 times slower than just using RSpectra, so I just use RSpectra here
#'
#'   # is this doing too much computation in the symmetric case?
#'   s <- RSpectra::svds(L, k = k, nu = k, nv = k)
#'   U <- s$u
#'   V <- s$v
#'
#'   ### STEP 4: VARIMAX ROTATION
#'
#'   # NOTE: eps may not be small enough here, see comment at
#'   # https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r
#'   # https://stackoverflow.com/questions/32351780/why-are-there-differences-in-psychprincipal-between-varimax-and-varimax/37322261#37322261
#'
#'   # does not take advantage of symmetric calculations here
#'
#'   # so this rotation is only identified up a sign change
#'   # karl's code tries to make it positive
#'   # can through away nodes with low degree via subsetting as well
#'
#'
#'   # sqrt(*) scaling terms?
#'   R_U <- varimax(U, normalize = F)$rotmat
#'   R_V <- varimax(V, normalize = F)$rotmat
#'
#'   Z <- sqrt(n) * U %*% R_U
#'   Y <- sqrt(d) * V %*% R_V
#'
#'   # TODO: understand why Karl does this instead?
#'
#'   # csL <- colSums(L)
#'   # R_V <- varimax(V[csL > 1, ], normalize = F)$rotmat
#'   # Y <- V %*% R_V
#'   #
#'   # signss <- sign(colSums(Y^3))
#'   # R_V <- R_V %*% diag(signss)
#'   # Y <- V %*% R_V
#'
#'   # TODO: does this get used, or is it of interest in and of itself?
#'
#'   # TODO: understand why Karl does this instead?
#'   # inverse of orthonormal matrices is itself?
#'   B <- t(R_U) %*% Diagonal(n = d, x = s$d) %*% R_V
#'
#'   # rescale output in the normalized case as suggested in the paper
#'   if (normalize) {
#'     Z <- D_row %*% Z
#'     Y <- D_col %*% Y
#'   }
#'
#'   ### STEP 5: CREATE A VSP OBJECT
#'   new_vsp(Z = Z, B = B, Y = Y, scree = d, U = U, V = V)
#' }
#'
#' # TODO:
#' # 1) For symmetric calculations, it would be faster to find eigenvectors.
#' # 2) Currently has adjMat as input.  Should could allow igraph object.
#' # 3) should we find k+1 eigenvalues to inspect the eigengap?
#'
#' # things that can be made fancy
#' # - varimax: don't use low degree nodes
#' # - more efficient computation for symmetric A
#' # - implicitly power-svd calculation for centering
#'

