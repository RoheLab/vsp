# factor localization plot

- want to factors that are not localized
- locationization diagnostic: get the varimax factors
- histogram of loadings
- log scale not useful, want to meaningful distribution on uniform scale for loadings
- stableTopics github: vsp code has diagnostics
- center, scale, absolute value, median, log, smaller than -2 localized

localization_factor <- function(x) {
  -log(median(abs((x - mean(x)) / sd(x))))
}


#' Get the localization measure for each factor
#'
#' TODO: why this is a good measure of localization / link to the
#'  diagnostics vignette
#'
#' @param fa A [vsp][vsp-object] object created by a call to [vsp()].
#'
#' @return A [tibble::tibble()] with two columns:
#'
#'  - **factor** (character): `factor1`, `factor2`, ..., `factor{k}` where `k` is
#'   number of factors used in `fa`.
#'  - **localization** (numeric): The localization factor
#'
#' @export
#' @family diagnostics
#'
#' @examples
#'
#' # TODO: better example data
#'
#' set.seed(27)
#' M <- sign(rsparsematrix(12, 12, nnz = 40))^2
#'
#' fa <- vsp(M, k = 4)
#'
#' get_localization(fa)
#'
#'
get_localization <- function(fa) {
  fa %>%
    project_varimax() %>%
    summarize_all(localization_factor) %>%
    gather(factor, localization)
}

#' Plot the localization measure for each factor
#'
#' TODO: why this is a good measure of localization / link to the
#'  diagnostics vignette
#'
#' @param fa A [vsp][vsp-object] object created by a call to [vsp()].
#'
#' @return A [ggplot2::ggplot()] object.
#' @export
#' @family diagnostics
#' @family plots
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
    ggplot(aes(factor, localization)) +
    geom_hline(yintercept = -2, size = 1, linetype = "dashed", color = "steelblue") +
    geom_point(size = 2) +
    labs(
      title = "Localization measure for each factor",
      caption = "Values less than -2 indicate localization"
    ) +
    theme_bw() +
    theme(axis.title.x = element_blank())
}


plot_svd_u <- function(fa) {
  # visualize V matrix
  fa$V %>%
    as.data.frame() %>%
    as_tibble() %>%
    set_names(paste0("eigen", 1:fa$k)) %>%
    mutate(index = row_number()) %>%
    gather(eigen, coord, -index) %>%
    ggplot(aes(index, coord)) +
    geom_line() +
    facet_wrap(~eigen) +
    theme_bw()
}
