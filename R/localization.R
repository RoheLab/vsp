#' Calculate cumulative participation of a set of singular vectors.
#'
#' @param U A matrix of singular vectors.
#'
#' @return A scalar numeric value representing the cumulative participation.
#' @export
#'
cumulative_participation <- function(U) {
  sum(rowSums(U^2)^2)
}

#' Calculate the inverse participation ratio (IPR) for a vector.
#'
#' @param x A numeric vector.
#'
#' @return A scalar numeric value representing the IPR.
#' @export
#'
ipr <- function(x) {
  sum(x^4)
}

#' Calculate IPR for all singular vectors in a list.
#'
#' @param s A list containing `u` and `v` matrices of singular vectors.
#'
#' @return A tibble with IPR values for each singular vector in `u` and `v`.
#' @export
#'
iprs <- function(s) {
  tibble(
    ipr_u = apply(s$u, 2, ipr),
    ipr_v = apply(s$v, 2, ipr),
    i = 1:ncol(s$u)
  )
}

#' Compute localization statistics across various regularization parameters.
#'
#' @param graph An igraph object or a sparse matrix.
#' @param max_rank The maximum number of singular vectors to compute.
#' @param ... Additional arguments passed to as_csparse.
#' @param tau_min The minimum value for the regularization parameter tau.
#' @param tau_max The maximum value for the regularization parameter tau.
#' @param num_tau The number of values of tau to test.
#'
#' @return A list of class `localization_stats` containing the results.
#' @export
#' @include utils.R
#'
#' @examples
#' \dontrun{
#' library(igraphdata)
#' library(furrr)
#'
#' data(karate, package = "igraphdata")
#'
#' plan(multisession, workers = 2)
#'
#' # karate is undirected, enron is directed
#'
#' stats <- localization_statistics(karate, max_rank = 15, num_tau = 200)
#'
#' plot_cumulative_curves(stats)
#' plot_ipr_curves(stats)
#' }
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
      s = furrr::future_map(L_tau, RSpectra::svds, max_rank, .options = furrr::furrr_options(seed = TRUE)),
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

#' Plot cumulative participation curves.
#'
#' @param localization A `localization_stats` object.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' # See localization_statistics for examples
plot_cumulative_curves <- function(localization) {
  localization$stats %>%
    tidyr::pivot_longer(tidyselect::contains("cum")) %>%
    dplyr::mutate(
      name = dplyr::if_else(name == "cum_u", "U (left)", "V (right)")
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = tau, y = value, color = name) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::labs(
      y = "Cumulative participation",
      x = "Regularization parameter (tau)",
      color = "Singular vectors"
    )
}

#' Plot IPR curves.
#'
#' @param localization A `localization_stats` object.
#' @param indices A vector of integers specifying which singular vectors to plot.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' # See localization_statistics for examples
plot_ipr_curves <- function(localization, indices = NULL) {

  if (is.null(indices)) {
    max_index <- min(max(localization$stats$i), 10)
    indices <- 1:max_index
  }

  localization$stats %>%
    dplyr::filter(i %in% indices) %>%
    tidyr::pivot_longer(tidyselect::contains("ipr")) %>%
    dplyr::mutate(
      name = dplyr::if_else(name == "ipr_u", "U (left)", "V (right)")
    ) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = tau, y = value, color = name) +
    ggplot2::facet_wrap(~i) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    ggplot2::labs(
      y = "Inverse participation ratio (IPR)",
      x = "Regularization parameter (tau)",
      color = "Singular vectors"
    )
}


