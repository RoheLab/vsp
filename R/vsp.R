#' Non-Parametric Factor Analysis via Vintage Sparse PCA
#'
#' This code implements regularized spectral clustering with varimax.
#' allows for symmetric, directed, and bipartite (i.e. rectangular A).
#'
#' @param A A [matrix] or [Matrix::Matrix] object representing an adjacency
#'  matrix. `A_ij` corresponds to the edge going from node `i` to node `j`.
#' @param k The number of factors to calculate.
#' @param center Should the adjacency matrix be row *and* column centered?
#'  Defaults to `TRUE`.
#' @param normalize Should the graph laplacian be used instead of the
#'  raw adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
#'  first be centered and then normalized.
#' @param tau_row Row regularization term. Default is `NULL`, in which case
#'  we use the row degree. Ignored when `normalize = FALSE`.
#' @param tau_col Column regularization term. Default is `NULL`, in which case
#'  we use the column degree. Ignored when `normalize = FALSE`.
#'
#' @details Sparse SVDs use `RSpectra` for performance.
#'
#' @return An object of class `vsp`. TODO: Details
#'
#' @export
vsp <- function(A, k = 5, center = TRUE, normalize = TRUE, tau_row = NULL, tau_col = NULL) {

  ### Vintage Sparse PCA Reference Implementation

  if (k < 2)
    stop("`k` must be at least two.", call. = FALSE)

  n <- nrow(A)
  d <- ncol(A)

  default_row <- is.null(tau_row)
  default_col <- is.null(tau_col)

  ### STEP 1: OPTIONAL NORMALIZATION

  # normalization corresponds to the optional scaling step defined
  # defined in Remark 1.1

  if (normalize) {

    rsA <- Matrix::rowSums(A)  # out-degree
    csA <- Matrix::colSums(A)  # in-degree

    tau_r <- if (default_row) mean(rsA) else tau_row
    tau_c <- if (default_col) mean(csA) else tau_col

    D_row <- Diagonal(n = n, x = 1 / sqrt(rsA + tau_r))
    D_col <- Diagonal(n = d, x = 1 / sqrt(csA + tau_c))

    # note: no identity matrix in the graph Laplacian here
    L <- D_row %*% A %*% D_col
  } else {
    L <- A
  }

  ### STEP 2: OPTIONAL CENTERING

  # here we center explicitly for clarity, but this should be done
  # implicitly for performance, as discussed in Remark 1.2

  if (center) {
    L <- double_center(L)
  }

  ### STEP 3: SVD

  # this doesn't differentiate between the symmetric and asymmetric cases
  # so there's opportunity for speed gains here

  # this includes a call to isSymmetric that we might be able to skip out on
  s <- RSpectra::svds(L, k = k, nu = k, nv = k)
  U <- s$u
  V <- s$v

  ### STEP 4: VARIMAX ROTATION

  # subset to only nodes with degree greater than. huge time saver.
  # some of karl's code uses rsL and csL here, which I think is a mistake
  # because it drops the computation time by an order of magnitude. is that
  # throwing out nonsense that doesn't affect results, or does it affect
  # results?
  R_U <- varimax(U[rsA > 1, ], normalize = FALSE, eps = 1e-5)$rotmat
  R_V <- varimax(V[csA > 1, ], normalize = FALSE, eps = 1e-5)$rotmat

  Z <- sqrt(n) * U %*% R_U
  Y <- sqrt(d) * V %*% R_V

  B <- t(R_U) %*% Diagonal(n = k, x = s$d) %*% R_V

  ### STEP 5: RESCALE IF NORMALIZED, RETURN OUTPUT

  if (normalize) {
    Z <- D_row %*% Z
    Y <- D_col %*% Y
  }

  new_vsp(
    U = U,
    d = s$d,
    V = V,
    Z = Z,
    B = B,
    Y = Y,
    center = center,
    normalize = normalize,
    k = k,
    tau_list = list(
      tau_row = if (exists("tau_r")) tau_r else NULL,
      tau_col = if (exists("tau_c")) tau_c else NULL,
      default_row = default_row,
      default_col = default_col
    )
  )
}

double_center <- function(L) {
  L <- sweep(L, 1, Matrix::rowMeans(L))
  L <- sweep(L, 2, Matrix::colMeans(L))
  L + Matrix::mean(L)
}

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

  lambda <- numeric(reps)

  for (i in 1:reps){
    A <- fastRG(
      X = rs / sum(rs),
      Y = cs / sum(cs),
      S = diag(c(1, 0)),
      avgDeg = sum(rs) * 2 / length(rs)
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



