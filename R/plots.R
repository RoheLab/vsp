
#' Create a pairs plot of select Y factors
#'
#' To avoid overplotting, plots data for a maximum of 1000 nodes. If there
#' are more than 1000 nodes, samples 1000 nodes randomly proportional to
#' row norms (i.e. nodes with embeddings larger in magniture are more likely
#' to be sampled).
#'
#' @inheritParams get_svd_u
#' @inheritDotParams GGally::ggpairs
#'
#' @return A [ggplot2::ggplot()] plot or [GGally::ggpairs()] plot.
#'
#' @export
#'
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
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...) +
    theme_minimal()
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select Z factors
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
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    GGally::ggpairs(aes(alpha = 0.001), ...) +
    theme_minimal()
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select left singular vectors
#' @export
plot_svd_u <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("ggplot2")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_u(factors) %>%
    select(-id) %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    mutate(node = row_number()) %>%
    gather(eigen, value, -node) %>%
    ggplot(aes(node, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select right singular vectors
#' @export
plot_svd_v <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("scales")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_v(factors) %>%
    select(-id) %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    mutate(node = row_number()) %>%
    gather(eigen, value, -node) %>%
    ggplot(aes(node, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' Create a screeplot from a factor analysis object
#'
#' @inherit get_svd_u params return
#'
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
    expand_limits(x = 1, y = 0) +
    theme_minimal()
}

#' Plot the mixing matrix B
#'
#' @inherit get_svd_u params return
#'
#' @export
plot_mixing_matrix <- function(fa) {
  as_tibble(as.matrix(fa$B), rownames = "row") %>%
    tidyr::gather(col, value, -row) %>%
    ggplot(aes(x = col, y = row, fill = value)) +
    geom_tile() +
    scale_fill_gradient2() +
    theme_minimal()
}

#' Plot pairs of inverse participation ratios for singular vectors
#'
#' When IPR for a given singular vector is O(1) rather than O(1 / sqrt(n)),
#' this can indicate that the singular vector is localizing on a small
#' subset of nodes. Oftentimes this localization indicates overfitting.
#' If you see IPR values that are not close to zero (where "close to zero"
#' is something you sort of have to pick up over time), then you need
#' to some further investigation to see if you have localization and that
#' localization corresponds to overfitting. Note, however, that not all
#' localization is overfitting.
#'
#' @inherit get_svd_u params return
#'
#' @export
plot_ipr_pairs <- function(fa) {

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
