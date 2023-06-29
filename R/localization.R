cumulative_participation <- function(U) {
  sum(rowSums(U^2)^2)
}

ipr <- function(x) sum(x^4)

iprs <- function(s) {
  tibble(
    ipr_u = apply(s$u, 2, ipr),
    ipr_v = apply(s$v, 2, ipr),
    i = 1:ncol(s$u)
  )
}

#' Title
#'
#' @param graph
#' @param max_rank
#' @param ...
#' @param tau_min
#' @param tau_max
#' @param num_tau
#'
#' @return
#' @export
#' @include utils.R
#'
#' @examples
#'
#' library(igraphdata)
#' library(furrr)
#'
#' data(karate, package = "igraphdata")
#'
#' plan(multicore, workers = 10)
#'
#' # karate is undirected, enron is directed
#'
#' stats <- localization_statistics(karate, max_rank = 15, num_tau = 200)
#'
#' plot_cumulative_curves(stats)
#' plot_ipr_curves(stats)
#'
localization_statistics <- function(graph, max_rank, ..., tau_min = 10^-2, tau_max = 10^4, num_tau = 50) {

  stop_if_not_installed("invertiforms")
  stop_if_not_installed("furrr")

  A <- as_csparse(graph, ...)

  avg_row_sum <- mean(rowSums(A)) # U / left
  avg_col_sum <- mean(colSums(A)) # V / right

  tau <- 10^seq(-2, 4, length.out = num_tau)

  laplacians <- dplyr::tibble(tau = tau) %>%
    dplyr::mutate(
      scaler = furrr::future_map(tau, ~ invertiforms::RegularizedLaplacian(A, .x, .x)),
      L_tau = furrr::future_map(scaler, ~ invertiforms::transform(.x, A))
    )

  localization <- laplacians %>%
    dplyr::mutate(
      s = furrr::future_map(L_tau, RSpectra::svds, max_rank, .options = furrr_options(seed = TRUE)),
      ipr = furrr::future_map(s, iprs),
      cum_u = furrr::future_map_dbl(s, ~ cumulative_participation(.x$u)),
      cum_v = furrr::future_map_dbl(s, ~ cumulative_participation(.x$v))
    ) |>
    # dplyr::select(tau, ipr, cum_u, cum_v) |>
    tidyr::unnest(ipr)

  localization <- list(
    stats = localization,
    avg_row_sum = avg_row_sum,
    avg_col_sum = avg_col_sum
  )

  class(localization) <- c("localization_stats")

  localization
}

#' Title
#'
#' @param stats
#'
#' @return
#' @export
#'
#' @examples
plot_cumulative_curves <- function(localization) {
  localization$stats %>%
    tidyr::pivot_longer(contains("cum")) %>%
    dplyr::mutate(
      name = dplyr::if_else(name == "cum_u", "U (left)", "V (right)")
    ) |>
    ggplot() +
    aes(x = tau, y = value, color = name) +
    geom_line() +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    labs(
      y = "Cumulative participation",
      x = "Regularization parameter (tau)",
      color = "Singular vectors"
    )
}

#' Title
#'
#' @param stats
#' @param indices
#'
#' @return
#' @export
#'
#' @examples
plot_ipr_curves <- function(localization, indices = NULL) {

  if (is.null(indices)) {
    max_index <- min(max(stats$i), 10)
    indices <- 1:max_index
  }

  localization$stats %>%
    dplyr::filter(i %in% indices) |>
    tidyr::pivot_longer(contains("ipr")) %>%
    dplyr::mutate(
      name = dplyr::if_else(name == "ipr_u", "U (left)", "V (right)")
    ) |>
    ggplot() +
    aes(x = tau, y = value, color = name) +
    geom_line() +
    scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    labs(
      y = "Inverse participation ratio",
      x = "Regularization parameter (tau)",
      color = "Singular vectors"
    ) +
    facet_grid(rows = vars(i))
}


