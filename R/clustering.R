## ------------------------------------------------------------
## what: network clustering analysis
## who: fan chen (fan.chen@wisc.edu)
## when: 09/21/2018
## where: murmuration project
## ------------------------------------------------------------
suppressPackageStartupMessages({
  library(Matrix)
  library(irlba)
  library(tidyverse)
})

## FUN: find two-way spectrum + varimax
## INPUT:
##    A - matrix/Matirx
##    k - rank of spectrum
##    tauR, tauC - positive numeric, row/column regularizers
##    normalize - logical, normalize option for varimax
##    ... - options for irlba
## OUTPUT: Z, B, Y, and the input parameters
vsp <- function(A, k = 5,
                tauR = NULL, tauC = NULL,
                normalize = F, plot = F,
                ...) {
  A1 = 1 * A ## in case of binary input
  deg_row = rowSums(A1)
  deg_col = colSums(A1)
  tauR = ifelse(is.null(tauR), mean(deg_row), tauR)
  tauC = ifelse(is.null(tauC), mean(deg_col), tauC)
  Dr = Diagonal(nrow(A1), 1 / sqrt(deg_row + tauR))
  Dc = Diagonal(ncol(A1), 1 / sqrt(deg_col + tauC))
  L = Dr %*% A1 %*% Dc ## laplacian

  system.time({s <- irlba::irlba(L, nv = k, ...)}) ## svd
  if (plot) screeplot.vsp(list(k = k, d = s$d))

  # varimax
  Ru =  varimax(s$u, normalize = normalize)
  Z = s$u %*% Ru$rotmat
  Z = apply(Z, 2, function(x) return(x * sign(mean(x ^ 3))))

  Rv =  varimax(s$v, normalize = normalize)
  Y = s$v %*% Rv$rotmat
  Y = apply(Y, 2, function(x) return(x * sign(mean(x ^ 3))))

  B = t(Ru$rotmat) %*% diag(s$d) %*% Rv$rotmat

  list(Z = Z, B = B, Y = Y,
       normalize = normalize, tau = c(tauR, tauC), ...)
}
