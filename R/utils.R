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
  x %>%
    get_varimax_y() %>%
    mutate_all(abs) %>%
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
  x %>%
    get_varimax_z() %>%
    mutate_all(abs) %>%
    apply(1, which.max)
}

#' @export
#' @importFrom tibble as_tibble
get_svd_u <- function(x, ...) {
  colnames(x$U) <- paste0("u", 1:x$k)
  as_tibble(x$U)
}

#' @export
#' @importFrom tibble as_tibble
get_svd_v <- function(x, ...) {
  colnames(x$V) <- paste0("v", 1:x$k)
  as_tibble(x$V)
}

#' @export
#' @importFrom tibble as_tibble
get_varimax_z <- function(x, ...) {
  z <- as.matrix(x$Z)
  colnames(z) <- paste0("z", 1:x$k)
  as_tibble(z)
}

#' @export
#' @importFrom tibble as_tibble
get_varimax_y <- function(x, ...) {
  y <- as.matrix(x$Y)
  colnames(y) <- paste0("y", 1:x$k)
  as_tibble(y)
}

