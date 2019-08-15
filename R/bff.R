## FUN: best features function (BFF) after vsp

## INPUT:
#' @param clusters  n x K [matrix], cluster weights, i.e. Z or Y in the SMD.
#' @param features - n x d [matrix], colnames of features should be "meaningful", e.g. the dictionary in bag of words
#' @param nfeatures - number of features wanted

## OUTPUT:
#' @return [matrix] of nfeatures x K, where `[i,j]` is the ith "most important" feature for cluster j.

#' @export
bff <- function(clusters, features, nfeatures, ...) {
  UseMethod("vsp")
}

#' @rdname vsp
#' @export

bff.default = function(clusters, features, nfeatures){
  clusters[clusters < 0] = 0
  nclust = ncol(clusters)
  bestFeatures = matrix("", ncol = nclust, nrow = nfeatures)

  ## normalize the cluster to a probability distribution (select one member at random)
  nc = apply(clusters, 2, function(x) return(x/sum(x)))
  nOutC = apply(clusters == 0, 2, function(x) return(x/sum(x)))
  inCluster = t(features) %*% nc %>% sqrt
  outCluster = t(features) %*% nOutC %>% sqrt

  ## diff is d x K, element [i,j] indicates importance of i-th feature to j-th block
  diff = inCluster - outCluster ## contrast: sqrt(A) - sqrt(B), variance stabilization
  for (j in 1:nclust) {
    bestIndex = order(-diff[,j])[1:nfeatures]
    bestFeatures[,j] = colnames(features[, bestIndex])
  }

  return(bestFeatures)
}

