#' Get the regularized graph laplacian of a graph
#'
#' @param graph Either a graph adjacency matrix, [igraph::igraph] or
#'  [tidygraph::tbl_graph]. If `x` is a [matrix] or [Matrix::Matrix]
#'  then `x[i, j]` should correspond to the edge going from node `i`
#'  to node `j`.
#'
#' @param tau_row Row regularization term. Default is `NULL`, in which case
#'  we use the mean row degree. Ignored when `normalize = FALSE`.
#'
#' @param tau_col Column regularization term. Default is `NULL`, in which case
#'  we use the mean column degree. Ignored when `normalize = FALSE`.
#'
#' @param ... Ignored.
#'
#' @return The regularized graph Laplacian as a [Matrix::Matrix()].
#'
#' @export
regularized_laplacian <- function(graph, ...) {
  UseMethod("regularized_laplacian")
}

#' @rdname regularized_laplacian
#' @export
regularized_laplacian.igraph <- function(graph, ..., tau_row = NULL,
                                         tau_col = NULL, sparse = TRUE) {

  A <- igraph::get.adjacency(graph, sparse = sparse)

  n <- nrow(A)
  d <- ncol(A)

  default_row <- is.null(tau_row)
  default_col <- is.null(tau_col)

  # needed both for normalization and subsetting rows for varimax
  rsA <- Matrix::rowSums(A)  # out-degree
  csA <- Matrix::colSums(A)  # in-degree

  tau_r <- if (default_row) mean(rsA) else tau_row
  tau_c <- if (default_col) mean(csA) else tau_col

  D_row <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_r))
  D_col <- Diagonal(n = d, x = 1 / sqrt(csA + tau_c))

  # note: no identity matrix in the graph Laplacian here
  D_row %*% A %*% D_col
}

