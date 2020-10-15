#' Assign nodes to clusters via a rough heuristic
#'
#' This is a heuristic that assigns nodes clusters based on highest
#' absolute value of loadings. Each node is assigned to a single cluster.#'
#'
#' @param x A factor analysis object.
#' @param ... Ignored.
#'
#' @export
get_y_clusters <- function(x, ...) {

  stop_if_not_installed("dplyr")

  x %>%
    get_varimax_y() %>%
    dplyr::mutate_all(abs) %>%
    apply(1, which.max)
}

#' Assign nodes to clusters via a rough heuristic
#'
#' This is a heuristic that assigns nodes clusters based on highest
#' absolute value of loadings. Each node is assigned to a single cluster.#'
#'
#' @param x A factor analysis object.
#' @param ... Ignored.
#'
#' @export
get_z_clusters <- function(x, ...) {

  stop_if_not_installed("dplyr")

  x %>%
    get_varimax_z() %>%
    dplyr::mutate_all(abs) %>%
    apply(1, which.max)
}

#' @export
get_svd_u <- function(x, ...) {
  colnames(x$u) <- paste0("u", 1:x$rank)
  as_tibble(x$u)
}

#' @export
get_svd_v <- function(x, ...) {
  colnames(x$v) <- paste0("v", 1:x$rank)
  as_tibble(x$v)
}

#' @export
get_varimax_z <- function(x, ...) {
  as_tibble(as.matrix(x$Z[, factors, drop = FALSE]))
}

#' @export
get_varimax_y <- function(x, factors = 1:x$rank, ...) {
  # TODO: eventually this type coercion shouldn't be necessary
  as_tibble(as.matrix(x$Y[, factors, drop = FALSE]))
}

#' @export
get_z_hubs <- function(fa, hubs_per_factor = 10, factors = 1:fa$rank) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_z(factors) %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    tidyr::gather(factor, loading, dplyr::contains("z"), -index) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(hubs_per_factor, wt = abs(loading))
}

#' @export
get_y_hubs <- function(fa, hubs_per_factor = 10) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_y() %>%
    dplyr::mutate(index = dplyr::row_number()) %>%
    tidyr::gather(factor, loading, dplyr::contains("y"), -index) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(hubs_per_factor, wt = abs(loading))
}