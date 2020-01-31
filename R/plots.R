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