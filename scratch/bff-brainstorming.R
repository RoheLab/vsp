
identity <- function(x) x

# https://stackoverflow.com/questions/42599498/numercially-stable-softmax
softmax <- function(x) {
  z <- x - max(x)
  numerator <- exp(z)
  denominator <- sum(numerator)
  numerator / denominator
}

#' @export
bff3 <-  function(loadings, features, num_best, ...) {

  # Fan has this line in his code but I don't understand why?
  loadings <- apply(loadings, 2, softmax)
  print(dim(loadings))
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
    bestIndex <-  order(diff[, j, decreasing = TRUE])[1:num_best]
    best_feat[,j] <-  colnames(features[, bestIndex])
  }

  best_feat
}

View(bff3(fa$Y, important_words, num_best = 20))

#' @export
bff2 <-  function(loadings, features, num_best, ...) {

  # TODO: stabilization and normalization?

  # each cluster needs to count the same, thus column normalization of loadings

  loadings[loadings < 0] <- 0

  importance <- sqrt(crossprod(loadings, features))
  k <- ncol(loadings)

  avg_importance <- colMeans(importance)

  # easiest - difference of cluster important from avg importance
  # sweep(importance, 2, avg_importance)

  # slightly more interesting - difference of cluster importance from avg importance
  # for univariate X_1, ..., X_k you can work out that this out pretty quickly to find
  diff <- sweep((1 + 1 / (k - 1)) * importance, 2, avg_importance * (k / (k - 1)))

  best_feat <- matrix("", ncol = k, nrow = num_best)

  print(summary(as.matrix(diff)))

  for (j in 1:k) {
    bestIndex <-  order(-diff[j, ])[1:num_best]
    best_feat[,j] <- colnames(features[, bestIndex])
  }

  return(best_feat)

}

x <- bff2(fa$Z, important_words, num_best = 20)
View(x)