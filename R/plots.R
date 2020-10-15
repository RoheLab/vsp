#' @export
plot_varimax_z_pairs <- function(fa, factors = 1:max(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_z() %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...)
}

#' @export
plot_varimax_y_pairs <- function(fa, factors = 1:max(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_y() %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...)
}

#' @export
plot_svd_u <- function(fa) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("ggplot2")
  stop_if_not_installed("tidyr")

  get_svd_u(fa) %>%
    mutate(element = row_number()) %>%
    gather(eigen, value, -element) %>%
    ggplot(aes(element, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @export
plot_svd_v <- function(fa) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("scales")
  stop_if_not_installed("tidyr")

  get_svd_v(fa) %>%
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
