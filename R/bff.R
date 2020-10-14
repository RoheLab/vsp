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
#' @export
bff <- function(loadings, features, num_best, ...) {
  UseMethod("bff")
}

l1_normalize <- function(x) x / sum(x)

#' @export
bff.default <-  function(loadings, features, num_best, ...) {

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

  for (j in 1:k) {
    bestIndex <-  order(-diff[,j])[1:num_best]
    best_feat[,j] <-  colnames(features[, bestIndex])
  }

  best_feat
}


#' @export
bff2 <-  function(loadings, features, num_best, ...) {

  # variance stabilization

  stabilized <- sqrt(features)

  # turn into probabilities that each node belongs
  # to a particular cluster
  y_tilde <- apply(abs(loadings), 1, l1_normalize)
  crossprod(y_tilde, stabilized)
}