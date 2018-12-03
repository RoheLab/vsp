
#' @export
#' @importFrom tibble as_tibble
project_pca <- function(x, ...) {
  scores <- x$U %*% diag(x$d)
  colnames(scores) <- paste0("PC", 1:x$k)
  tibble::as_tibble(scores)
}

#' @export
#' @importFrom tibble as_tibble
project_varimax <- function(x, ...) {
  # do i need to multiply by B or sqrt(B) here??
  vrmx <- as.matrix(x$Z)
  colnames(vrmx) <- paste0("factor", 1:x$k)
  tibble::as_tibble(vrmx)
}

#' @export
lambda_2_null_dist <- function(A, k = 3, reps = 100) {

  # eventually the following should get written up into a package
  source("https://raw.githubusercontent.com/karlrohe/fastRG/master/fastRDPG.R")
  rs <- rowSums(A)
  cs <- colSums(A)

  rs <- cbind(rs, 0)
  cs <- cbind(cs, 0)

  rsn <- rs / sum(rs)
  csn <- cs / sum(cs)

  rs_sum <- sum(rs)
  rs_n <- length(rs)

  lambda <- numeric(reps)

  # use furrr for this / parallelize
  for (i in 1:reps){
    A <- fastRG(
      X = rsn,
      Y = csn,
      S = diag(c(1, 0)),
      avgDeg = rs_sum * 2 / length(rs)
    )
    lambda[i] <- vsp(A, k = k, center = FALSE)$d[2]
  }

  lambda
}

#' @export
#' @import ggplot2
plot_simulation_test <- function(A, k, reps = 100) {

  lambda_2_sim <- lambda_2_null_dist(A, k, reps)
  lambda_2_obs <- vsp(A, k, center = FALSE)$d[2]

  ggplot(NULL, aes(lambda_2_sim)) +
    geom_histogram() +
    geom_vline(xintercept = lambda_2_obs, color = "red", size = 1.5) +
    labs(
      title = "Distribution of second eigenvalue under null model",
      x = "Second eigenvalue",
      caption = "Red line is observed eigenvalue"
    ) +
    theme_bw()
}



