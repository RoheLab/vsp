#' Get left singular vectors in a tibble
#'
#' @param x A [vsp_fa()] object.
#' @param factors The specific columns to index into. The most reliable
#'   option here is to index with an integer vector of column indices,
#'   but you could also use a character vector if columns have been named.
#'   By default returns all factors/singular vectors.
#'
#' @return A [tibble::tibble()] with one row for each node, and one column
#'   containing each of the requested factor or singular vector, plus
#'   an additional `id` column.
#'
#' @export
#'
get_svd_u <- function(x, factors = 1:x$rank) {
  as_tibble(as.matrix(x$u[, factors, drop = FALSE]), rownames = "id")
}

#' @export
#' @describeIn get_svd_u Get right singular vectors in a tibble
get_svd_v <- function(x, factors = 1:x$rank) {
  as_tibble(as.matrix(x$v[, factors, drop = FALSE]), rownames = "id")
}

#' @export
#' @describeIn get_svd_u Get varimax Y factors in a tibble
get_varimax_z <- function(x, factors = 1:x$rank) {
  as_tibble(as.matrix(x$Z[, factors, drop = FALSE]), rownames = "id")
}

#' @export
#' @describeIn get_svd_u Get varimax Z factors in a tibble
get_varimax_y <- function(x, factors = 1:x$rank) {
  as_tibble(as.matrix(x$Y[, factors, drop = FALSE]), rownames = "id")
}


#' Get most important hubs for each Z factor
#'
#' @param hubs_per_factor The number of important nodes to get per
#'   latent factor. Defaults to `10`.
#'
#' @inheritParams get_svd_u
#'
#' @return A [tibble::tibble()] where each row corresponds to a single
#'   hub, and three columns:
#'
#'   - `id`: Node id of hub node
#'   - `factor`: Which factor that node is a hub for. Nodes can be hubs
#'     of multiple factors.
#'   - `loading`: The actual value of the hubs factor loading for that factor.
#'
#' @export
#'
get_z_hubs <- function(fa, hubs_per_factor = 10, factors = 1:fa$rank) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_z(factors) %>%
    tidyr::gather(factor, loading, dplyr::contains("z"), -id) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(x = ., n = hubs_per_factor, wt = abs(loading))
}

#' @export
#' @describeIn get_z_hubs Get most important hubs for each Y factor
get_y_hubs <- function(fa, hubs_per_factor = 10, factors = 1:fa$rank) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_y(factors) %>%
    tidyr::gather(factor, loading, dplyr::contains("y"), -id) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(x = ., n = hubs_per_factor, wt = abs(loading))
}

#' Add Z factor loadings to node table of tidygraph
#'
#' @param graph A [tidygraph::tbl_graph] object.
#' @param fa Optionally, a [vsp] object to extract varimax loadings from. If
#'  you do not passed a [vsp] object, one will be created.
#' @inheritDotParams vsp
#'
#' @return The same `graph` object with columns `factor1`, ..., `factor{rank}`
#'  in the table of node information.
#'
#' @export
bind_varimax_z <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_varimax_z(fa))

  graph
}

#' @export
#' @describeIn bind_varimax_z Add Y factor loadings to node table of tidygraph
bind_varimax_y <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_varimax_y(fa))

  graph
}

#' @export
#' @describeIn bind_varimax_z Add left singular vectors to node table of tidygraph
bind_svd_u <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_svd_u(fa))

  graph
}

#' @export
#' @describeIn bind_varimax_z Add right singular vectors to node table of tidygraph
bind_svd_v <- function(graph, fa, ...) {
  stopifnot(inherits(graph, "tbl_graph"))

  graph <- graph %>%
    activate(nodes) %>%
    mutate(!!!get_svd_v(fa))

  graph
}
