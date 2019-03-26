
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
  colnames(x$Z) <- paste0("z", 1:x$k)
  as_tibble(x$Z)
}

#' @export
#' @importFrom tibble as_tibble
get_varimax_y <- function(x, ...) {
  colnames(x$Y) <- paste0("y", 1:x$k)
  as_tibble(x$Y)
}

