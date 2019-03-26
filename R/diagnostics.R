# call on an eigen/singular-vector
localization_factor <- function(x) {
  -log(median(abs((x - mean(x)) / sd(x))))
}

#' Get the localization measure for each factor
#'
#' @param fa A [vsp][vsp-object] object created by a call to [vsp()].
#'
#' @return TODO
#'
#' @export
#' @family diagnostics
#'
#' @examples
#'
#' set.seed(27)
#' M <- sign(rsparsematrix(12, 12, nnz = 40))^2
#'
#' fa <- vsp(M, k = 4)
#'
#' get_localization(fa)
#'
get_localization <- function(fa) {

  u_loc <- get_svd_u(fa) %>%
    dplyr::summarize_all(localization_factor) %>%
    tidyr::gather(factor, localization)

  v_loc <- get_svd_v(fa) %>%
    dplyr::summarize_all(localization_factor) %>%
    tidyr::gather(factor, localization)

  tibble::tibble(
    eigenvector = 1:fa$k,
    u_loc = u_loc$localization,
    v_loc = v_loc$localization
  )
}

#' Get the localization measure for each factor
#'
#' @param fa A [vsp][vsp-object] object created by a call to [vsp()].
#'
#' @return TODO
#'
#' @export
#' @family diagnostics
#'
#' @examples
#'
#' set.seed(27)
#' M <- sign(rsparsematrix(12, 12, nnz = 40))^2
#'
#' fa <- vsp(M, k = 4)
#'
#' plot_localization(fa)
#'
plot_localization <- function(fa) {
  get_localization(fa) %>%
    ggplot(aes(u_loc, v_loc)) +
    geom_rect(
      xmin = -2, xmax = Inf, ymin = -2, ymax = Inf,
      fill = "green",
      alpha = 0.02
    ) +
    geom_rect(
      xmin = -Inf, xmax = -2, ymin = -Inf, ymax = -2,
      fill = "red",
      alpha = 0.02
    ) +
    geom_rect(
      xmin = -Inf, xmax = -2, ymin = -2, ymax = Inf,
      fill = "yellow",
      alpha = 0.02
    ) +
    geom_rect(
      xmin = -2, xmax = Inf, ymin = -Inf, ymax = -2,
      fill = "yellow",
      alpha = 0.02
    ) +
    geom_hline(
      yintercept = -2,
      size = 1,
      linetype = "dashed",
      color = "steelblue"
    ) +
    geom_vline(
      xintercept = -2,
      size = 1,
      linetype = "dashed",
      color = "steelblue"
    ) +
    geom_point(size = 2) +
    labs(
      title = "Localization measures for each eigenvector",
      caption = "Values less than -2 indicate localization",
      x = expression(paste("Localization for ", U[i])),
      y = expression(paste("Localization for ", V[i]))
    ) +
    theme_minimal()
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
