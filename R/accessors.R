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
    select(-id) %>%
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
    select(-id) %>%
    dplyr::mutate_all(abs) %>%
    apply(1, which.max)
}

#' Title
#'
#' @param x TODO
#' @param factors TODO
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
get_svd_u <- function(x, factors, ...) {
  as_tibble(as.matrix(x$u[, factors, drop = FALSE]), rownames = "id")
}

#' Title
#'
#' @param x TODO
#' @param factors TODO
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
get_svd_v <- function(x, factors, ...) {
  as_tibble(as.matrix(x$v[, factors, drop = FALSE]), rownames = "id")
}

#' Title
#'
#' @param x TODO
#' @param factors TODO
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
get_varimax_z <- function(x, factors = 1:x$rank, ...) {
  as_tibble(as.matrix(x$Z[, factors, drop = FALSE]), rownames = "id")
}

#' Title
#'
#' @param x TODO
#' @param factors TODO
#' @param ... TODO
#'
#' @return TODO
#' @export
#'
get_varimax_y <- function(x, factors = 1:x$rank, ...) {
  as_tibble(as.matrix(x$Y[, factors, drop = FALSE]), rownames = "id")
}


#' Title
#'
#' @param fa TODO
#' @param hubs_per_factor TODO
#' @param factors TODO
#'
#' @return TODO
#' @export
#'
get_z_hubs <- function(fa, hubs_per_factor = 10, factors = 1:fa$rank) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_z(factors) %>%
    tidyr::gather(factor, loading, dplyr::contains("z"), -id) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(hubs_per_factor, wt = abs(loading))
}

#' Title
#'
#' @param fa TODO
#' @param hubs_per_factor TODO
#' @param factors TODO
#'
#' @return TODO
#' @export
#'
get_y_hubs <- function(fa, hubs_per_factor = 10, factors = 1:fa$rank) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("tidyr")

  fa %>%
    get_varimax_y(factors) %>%
    tidyr::gather(factor, loading, dplyr::contains("y"), -id) %>%
    dplyr::group_by(factor) %>%
    dplyr::top_n(hubs_per_factor, wt = abs(loading))
}