#' Add varimax factor loadings to node table of tidygraph
#'
#' @param graph A [tidygraph::tbl_graph] object.
#' @param fa Optionally, a [vsp] object to extract varimax loadings from. If
#'  you do not passed a [vsp] object, one will be created.
#' @inheritDotParams vsp
#'
#' @return The same `graph` object with columns `factor1`, ..., `factor{k}`
#'  in the table of node information.
#'
#' @export
bind_varimax_factors <- function(graph, fa = NULL, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  if (is.null(fa)) {
    A <- igraph::get.adjacency(graph, sparse = TRUE)
    fa <- vsp(A, ...)
  }

  # else: TODO: check that dimensions line up correctly

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!project_varimax(fa))

  graph
}

#' Add PCA factor loadings to node table of tidygraph
#'
#' @param graph A [tidygraph::tbl_graph] object.
#' @param fa Optionally, a [vsp] object to extract varimax loadings from. If
#'  you do not passed a [vsp] object, one will be created.
#' @inheritDotParams vsp
#'
#' @return The same `graph` object with columns `factor1`, ..., `factor{k}`
#'  in the table of node information.
#'
#' @export
bind_pca_factors <- function(graph, fa = NULL, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  if (is.null(fa)) {
    A <- igraph::get.adjacency(graph, sparse = TRUE)
    fa <- vsp(A, ...)
  }

  # else: TODO: check that dimensions line up correctly

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!project_pca(fa))

  graph
}

# spectral clustering: project into pca space, run kmeans++ on the node table,
# bind clusters to

# issue: now you have information about the kmeans which is more than what is
# in the node table

# what if you want to spectrally cluster the nodes
