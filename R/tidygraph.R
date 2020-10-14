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
bind_varimax_z <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  # else: TODO: check that dimensions line up correctly

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_varimax_z(fa))

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
bind_svd_u <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  # else: TODO: check that dimensions line up correctly

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_svd_u(fa))

  graph
}

