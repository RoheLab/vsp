#' @export
plot_varimax_z_pairs <- function(fa, factors = 1:min(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_z(factors) %>%
    select(-id) %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...) +
    theme_minimal()
}

#' @export
plot_varimax_y_pairs <- function(fa, factors = 1:min(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_y(factors) %>%
    select(-id) %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...) +
    theme_minimal()
}

#' @export
plot_svd_u <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("ggplot2")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_u(factors) %>%
    mutate(element = row_number()) %>%
    gather(eigen, value, -element) %>%
    ggplot(aes(element, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @export
plot_svd_v <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("scales")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_v(factors) %>%
    mutate(element = row_number()) %>%
    gather(eigen, value, -element) %>%
    ggplot(aes(element, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @method screeplot vsp_fa
#' @export
#' @import ggplot2
#' @importFrom stats screeplot
screeplot.vsp_fa <- function(x, ...) {

  ggplot(data = NULL, aes(1:x$rank, x$d)) +
    geom_point() +
    labs(
      title = "Screeplot of graph spectrum",
      x = "Index",
      y = "Singular value"
    ) +
    expand_limits(x = 0, y = 0) +
    theme_minimal()
}

#' @export
#' @import ggplot2
plot_mixing_matrix <- function(fa, ...) {
  as_tibble(as.matrix(fa$B), rownames = "row") %>%
    tidyr::gather(col, value, -row) %>%
    dplyr::arrange(row, col) %>%
    ggplot(aes(x = col, y = row, fill = value)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_minimal()
}

#' @export
#' @import ggplot2
plot_ipr_pairs <- function(fa, ...) {

  ipr <- function(x) sum(x^4)

  ipr_u <- apply(fa$u, 2, ipr)
  ipr_v <- apply(fa$v, 2, ipr)

  ggplot(data = NULL) +
    aes(x = ipr_u, y = ipr_v) +
    geom_point(alpha = 0.5) +
    expand_limits(x = 0, y = 0) +
    labs(
      title = "Inverse participation ratios of singular vectors",
      x = "U (left singular vectors)",
      y = "V (right singular vectors)"
    ) +
    theme_minimal()
}
