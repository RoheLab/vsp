#' Non-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements TODO.
#'
#' @param x Either a graph adjacency matrix, [igraph::igraph] or
#'  [tidygraph::tbl_graph]. If `x` is a [matrix] or [Matrix::Matrix]
#'  then `x[i, j]` should correspond to the edge going from node `i`
#'  to node `j`.
#' @param k The number of factors to calculate.
#' @param center Should the adjacency matrix be row *and* column centered?
#'  Defaults to `TRUE`.
#' @param normalize Should the graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
#'  first be centered and then normalized.
#' @param tau_row Row regularization term. Default is `NULL`, in which case
#'  we use the row degree. Ignored when `normalize = FALSE`.
#' @param tau_col Column regularization term. Default is `NULL`, in which case
#'  we use the column degree. Ignored when `normalize = FALSE`.
#' @param ... Ignored.
#'
#' @details Sparse SVDs use `RSpectra` for performance.
#'
#' @return An object of class `vsp`. TODO: Details
#'
#' @export
vsp <- function(x, k = 5, center = TRUE, normalize = TRUE,
                tau_row = NULL, tau_col = NULL, ...) {
  UseMethod("vsp")
}

#' @rdname vsp
#' @export
vsp.default <- function(x, k = 5, center = TRUE, normalize = TRUE,
                        tau_row = NULL, tau_col = NULL, ...) {

  ### Vintage Sparse PCA Reference Implementation

  ## INPUT VALIDATION

  if (k < 2)
    stop("`k` must be at least two.", call. = FALSE)

  n <- nrow(x)
  d <- ncol(x)

  default_row <- is.null(tau_row)
  default_col <- is.null(tau_col)

  # needed both for normalization and subsetting rows for varimax
  rsx <- Matrix::rowSums(x)  # out-degree
  csx <- Matrix::colSums(x)  # in-degree

  ### STEP 1: OPTIONAL NORMALIZATION

  # normalization corresponds to the optional scaling step defined
  # defined in Remark 1.1

  if (normalize) {

    tau_r <- if (default_row) mean(rsx) else tau_row
    tau_c <- if (default_col) mean(csx) else tau_col

    D_row <- Diagonal(n = n, x = 1 / sqrt(rsx + tau_r))
    D_col <- Diagonal(n = d, x = 1 / sqrt(csx + tau_c))

    # note: no identity matrix in the graph Laplacian here
    L <- D_row %*% x %*% D_col
  } else {
    L <- x
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
  R_U <- varimax(U[rsx > 1, ], normalize = FALSE, eps = 1e-5)$rotmat
  R_V <- varimax(V[csx > 1, ], normalize = FALSE, eps = 1e-5)$rotmat

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
vsp.igraph <- function(x, k = 5, center = TRUE, normalize = TRUE,
                       tau_row = NULL, tau_col = NULL, ...) {
  x <- igraph::get.adjacency(x, sparse = TRUE)
  NextMethod()
}

graph_laplacian <- function(x, rs, cs, tau_row, tau_col) {
  D_row <- Diagonal(n = nrow(x), x = 1 / sqrt(rs + tau_row))
  D_col <- Diagonal(n = ncol(x), x = 1 / sqrt(cs + tau_col))
  D_row %*% x %*% D_col
}

double_center <- function(L) {
  L <- sweep(L, 1, Matrix::rowMeans(L))
  L <- sweep(L, 2, Matrix::colMeans(L))
  L + Matrix::mean(L)
}

#' Calculate new factors and add them to an existing factor analysis
#'
#' @param object A [vsp][vsp-object] object. TODO: better link
#' @param graph Either a graph adjacency matrix, [igraph::igraph] or
#'  [tidygraph::tbl_graph]. If `x` is a [matrix] or [Matrix::Matrix]
#'  then `x[i, j]` should correspond to the edge going from node `i`
#'  to node `j`.
#' @param ... Ignored.
#' @param k A new number of desired total factors. Must be larger than
#'  the number of factors that have currently been calculated.
#' @param update_varimax Should the varimax rotations be updated as well?
#'  Defaults to `FALSE`.
#'
#' @details First `object` and `graph` are used to preprocess the graph
#'  as in the original computations. Then we construct a rank `object$k`
#'  approximation to the pre-processed graph, and subtract this
#'  approximation from the graph. Then we do a new partial SVD on this
#'  deflated graph, calculating `k - object$k` new singular vectors and
#'  values. The results from this new SVD are bound to the results from
#'  the original SVD. If the `update_varimax = TRUE`, then varimax
#'  rotations are recomputed from scratch.
#'
#'  Note that the original SVD loadings (i.e. the `U`, `d`, and `V`
#'  elements) will agree with new first `object$k` SVD loadings.
#'  However, the original varimax loadings (the `Z`, `B` and `Y` elements)
#'  generally *will not* agree with the updated varimax loadings.
#'
#'  The `update_varimax` step can potentially be sped up by using a warm
#'  start based on `object$R_U` and `object$R_V`, but these will require
#'  a custom varimax implementation. We'll probably do this anyway, so
#'  no reason not to build in a warm start.
#'
#' @return A [vsp][vsp-object] factor analysis based on `graph` with `k`
#'  factors.
#' @export
update.vsp <- function(object, graph, ..., k, update_varimax = FALSE) {

  # TODO: reduce the insane amount of code duplication here

  if (inherits(graph, "igraph"))
    x <- igraph::get.adjacency(x, sparse = TRUE)
  else
    x <- graph

  # should rsx, csx, n, d just be parts of the vsp object?
  # don't save the graph itself though, that's gross
  rsx <- Matrix::rowSums(x)
  csx <- Matrix::colSums(x)

  n <- nrow(x)
  d <- ncol(x)

  if (object$normalize) {

    tau_r <- object$tau_list$tau_row
    tau_c <- object$tau_list$tau_col

    D_row <- Diagonal(n = n, x = 1 / sqrt(rsx + tau_r))
    D_col <- Diagonal(n = d, x = 1 / sqrt(csx + tau_c))

    L <- D_row %*% x %*% D_col
  } else {
    L <- x
  }

  if (object$center) {
    L <- double_center(L)
  }

  # give some informative error if we've already done the work
  new_k <- k - object$k
  stopifnot(new_k > 0)

  # deflate based on original rank k approximation
  L <- L - object$U %*% diag(object$d) %*% t(object$V)

  s <- RSpectra::svds(L, new_k)

  object$U <- cbind(object$U, s$u)
  object$d <- c(object$d, s$d)
  object$V <- cbind(object$V, s$v)
  object$k <- k

  if (update_varimax) {

    # come up with a warm-startable varimax where TT initializes to
    # the old R_U plus some additional "identity" columns
    # instead of the full identity?
    R_U <- varimax(object$U[rsx > 1, ], normalize = FALSE, eps = 1e-5)$rotmat
    R_V <- varimax(object$V[csx > 1, ], normalize = FALSE, eps = 1e-5)$rotmat

    Z <- sqrt(n) * object$U %*% R_U
    Y <- sqrt(d) * object$V %*% R_V

    object$B <- t(R_U) %*% Diagonal(n = k, x = object$d) %*% R_V

    if (object$normalize) {
      object$Z <- D_row %*% Z
      object$Y <- D_col %*% Y
    }
  }

  # probably should just create a new object at the end with the new
  # stuff instead of littering object$ throughout

  object
}



