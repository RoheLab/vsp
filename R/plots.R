
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
#' @import ggplot2
#'
#' @return A [ggplot2::ggplot()] plot or [GGally::ggpairs()] plot.
#'
#' @export
#'
#' @examples
#'
#' data(enron, package = "igraphdata")
#'
#' fa <- vsp(enron, rank = 3)
#'
#' plot_varimax_z_pairs(fa)
#' plot_varimax_y_pairs(fa)
#'
#' plot_svd_u(fa)
#' plot_svd_v(fa)
#'
#' screeplot(fa)
#'
#' plot_mixing_matrix(fa)
#'
#' plot_ipr_pairs(fa)
#'
plot_varimax_z_pairs <- function(fa, factors = 1:min(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_z(factors) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    dplyr::sample_n(min(nrow(.), 1000), weight = leverage^2 + 1e-10) %>%
    dplyr::select(-leverage) %>%
    GGally::ggpairs(ggplot2::aes(alpha = 0.001), ...) +
    ggplot2::theme_minimal()
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select Z factors
#' @export
plot_varimax_y_pairs <- function(fa, factors = 1:min(5, fa$rank), ...) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("GGally")
  stop_if_not_installed("purrr")

  fa %>%
    get_varimax_y(factors) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    dplyr::sample_n(min(nrow(.), 1000), weight = leverage^2 + 1e-10) %>%
    dplyr::select(-leverage) %>%
    GGally::ggpairs(ggplot2::aes(alpha = 0.001), ...) +
    ggplot2::theme_minimal()
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select left singular vectors
#' @export
plot_svd_u <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("ggplot2")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_u(factors) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    dplyr::sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    dplyr::mutate(node = dplyr::row_number()) %>%
    tidyr::gather(eigen, value, -node) %>%
    ggplot2::ggplot(ggplot2::aes(node, value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~eigen) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @describeIn plot_varimax_z_pairs Create a pairs plot of select right singular vectors
#' @export
plot_svd_v <- function(fa, factors = 1:min(5, fa$rank)) {

  stop_if_not_installed("dplyr")
  stop_if_not_installed("scales")
  stop_if_not_installed("tidyr")

  fa %>%
    get_svd_v(factors) %>%
    dplyr::select(-id) %>%
    dplyr::mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    dplyr::sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    dplyr::mutate(node = dplyr::row_number()) %>%
    tidyr::gather(eigen, value, -node) %>%
    ggplot2::ggplot(ggplot2::aes(node, value)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~eigen) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())
}

#' Create a screeplot from a factor analysis object
#'
#' @param x A [vsp_fa()] object.
#' @inherit get_svd_u return
#' @param ... Ignored, included only for consistency with S3 generic.
#'
#' @method screeplot vsp_fa
#' @export
#' @import ggplot2
#' @importFrom stats screeplot
screeplot.vsp_fa <- function(x, ...) {

  ggplot2::ggplot(data = NULL, ggplot2::aes(1:x$rank, x$d)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Screeplot of graph spectrum",
      x = "Index",
      y = "Singular value"
    ) +
    ggplot2::expand_limits(x = 1, y = 0) +
    ggplot2::theme_minimal()
}

#' Plot the mixing matrix B
#'
#' @inherit get_svd_u params return
#'
#' @export
plot_mixing_matrix <- function(fa) {
  as_tibble(as.matrix(fa$B), rownames = "row") %>%
    tidyr::gather(col, value, -row) %>%
    ggplot2::ggplot(ggplot2::aes(x = col, y = row, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2() +
    ggplot2::theme_minimal()
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

  ggplot2::ggplot(data = NULL) +
    ggplot2::aes(x = ipr_u, y = ipr_v) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::labs(
      title = "Inverse participation ratios of singular vectors",
      x = "U (left singular vectors)",
      y = "V (right singular vectors)"
    ) +
    ggplot2::theme_minimal()
}
