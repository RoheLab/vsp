# subsample x to move from projected gradient descent to project sgd

varimax2 <- function (x, eps = 1e-05) {
  nc <- ncol(x)
  if (nc < 2)
    return(x)
  p <- nrow(x)
  TT <- diag(nc)
  d <- 0
  for (i in 1L:1000L) {
    z <- x %*% TT
    B <- t(x) %*% (z^3 - z %*% diag(drop(rep(1, p) %*% z^2))/p)

    # projection to recover orthonormality
    sB <- RSpectra::svds(B, 50)
    TT <- sB$u %*% t(sB$v)

    # convergence criteria
    dpast <- d
    d <- sum(sB$d)
    if (d < dpast * (1 + eps))
      break
  }
  TT
}

100^2

X <- matrix(rnorm(100^2), 100, 100)
varimax(X, normalize = F)$rotmat
varimax2(X)

bm <- bench::mark(
  varimax(X, normalize = FALSE)$rotmat,
  varimax2(X),
  min_iterations = 5,
  check = F
)

plot(bm)

c("response", "link", "precision", "variance", "quantile")
