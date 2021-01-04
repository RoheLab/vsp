# devtools::install_github("RoheLab/fastRG")
library(fastRG)
library(vsp)

n <- 1000
A <- erdos_renyi(n, avg_deg = 20) + 0

# these tests confirm that the matrix reconstructions based on (U, D, V)
# and (Z, B, Y) match

test_that("SVD and factor reconstructions match", {
  fa <- vsp(A, rank = 5)

  udv <- as.matrix(fa$u %*% diag(fa$d) %*% t(fa$v))
  zby <- as.matrix(fa$Z %*% fa$B %*% t(fa$Y))

  expect_true(isTRUE(all.equal(udv, zby)))
})


test_that("SVD and factor reconstructions match, scaling", {
  fa <- vsp(A, rank = 5, scale = TRUE)

  udv <- as.matrix(fa$u %*% diag(fa$d) %*% t(fa$v))
  zby <- as.matrix(fa$Z %*% fa$B %*% t(fa$Y))

  expect_true(isTRUE(all.equal(udv, zby)))
})

rm(A, n)