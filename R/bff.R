#' Find features most associated with cluster membership
#'
#' @param loadings An `n` by `k` matrix of weights that indicates how
#'   important that ith user is to the jth cluster, i.e., the `Z` or `Y`
#'   matrix calculated by [vsp()].
#'
#' @param features An `n` by `d` matrix of features measured for each
#'   node in the network.
#
#' @param num_best An integer indicating how many of the top features
#'   for differentiating between loadings you want.
#'
#' @return An `n` by `k` matrix whose `[i, j]` entry is the
#'   ith "most important" feature for cluster j.
#'
#' @details See `vignette("bff")`.
#'
#' @export
bff <- function(loadings, features, num_best) {
  l1_normalize <- function(x) x / sum(x)

  # Fan has this line in his code but I don't understand why?
  loadings[loadings < 0] <-  0
  k <- ncol(loadings)

  best_feat <- matrix("", ncol = k, nrow = num_best)

  ## normalize the cluster to a probability distribution (select one member at random)
  nc <- apply(loadings, 2, l1_normalize)
  nOutC <- apply(loadings == 0, 2, l1_normalize)

  inCluster <- sqrt(crossprod(features, nc))
  outCluster <- sqrt(crossprod(features, nOutC))

  # diff is d x K, element [i,j] indicates importance of i-th feature to j-th block
  # contrast: sqrt(A) - sqrt(B), variance stabilization
  diff <-  inCluster - outCluster

  diff %>%
    as.matrix() %>%
    as_tibble(rownames = "word") %>%
    pivot_longer(
      -word,
      names_to = "factor",
      values_to = "importance"
    ) %>%
    group_by(factor) %>%
    top_n(num_best, importance) %>%
    arrange(factor, desc(importance)) %>%
    mutate(
      rank = row_number()
    ) %>%
    ungroup() %>%
    pivot_wider(
      id_cols = factor,
      names_from = rank,
      names_prefix = "word",
      values_from = word
    )
}