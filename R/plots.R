#' @importFrom GGally ggpairs
#' @export
plot_varimax_z_pairs <- function(fa, factors = 1:max(5, fa$k), ...) {
  fa %>%
    get_varimax_z() %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    ggpairs(aes(alpha = 0.001), ...)
}

#' @importFrom GGally ggpairs
#' @export
plot_varimax_y_pairs <- function(fa, factors = 1:max(5, fa$k), ...) {
  fa %>%
    get_varimax_y() %>%
    mutate(
      leverage = purrr::pmap_dbl(., sum)
    ) %>%
    select(!!factors, leverage) %>%
    sample_n(min(nrow(.), 1000), weight = leverage^2) %>%
    select(-leverage) %>%
    ggpairs(aes(alpha = 0.001), ...)
}

#' @export
plot_svd_u <- function(fa) {
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
  get_svd_v(fa) %>%
    mutate(element = row_number()) %>%
    gather(eigen, value, -element) %>%
    ggplot(aes(element, value)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_minimal() +
    scale_x_continuous(breaks = scales::pretty_breaks())
}

#' @export
#' @import ggplot2
screeplot.vsp <- function(x, ...) {
  ggplot(data = NULL, aes(1:x$k, x$d)) +
    geom_point() +
    labs(
      title = "Singular values of adjacency matrix",
      caption = "If `normalize = TRUE`, singular values of graph Laplacian",
      x = "Singular value",
      y = "Value"
    ) +
    ylim(0, 1) +
    theme_bw()
}

# normalized_ipr_table <- get_ipr(L, k)
#
# ggplot(normalized_ipr_table, aes(u_ipr, v_ipr)) +
#   geom_point() +
#   labs(
#     title = "Supreme Court citation IPR Pair Plot",
#     subtitle = "singular vectors of regularized graph Laplacian (optimal tau)",
#     x = "Inverse participation ratio (U)",
#     y = "Inverse participation ratio (V)",
#     caption = "Each point represents one U-V singular vector pair"
#   ) +
#   theme_minimal()
