library(vsp)

n <- 1000
A <- rsparsematrix(10, 10, density = 0.1)

# these tests confirm that the matrix reconstructions based on (U, D, V)
# and (Z, B, Y) match

test_that("SVD and factor reconstructions match", {
  fa <- vsp(A, rank = 5)

  udv <- as.matrix(fa$u %*% diag(fa$d) %*% t(fa$v))
  zby <- as.matrix(fa$Z %*% fa$B %*% t(fa$Y))

  expect_true(isTRUE(all.equal(udv, zby)))
})


test_that("SVD and factor reconstructions match, scaling", {
  fa <- vsp(A, rank = 5, degree_normalize = TRUE)

  udv <- as.matrix(fa$u %*% diag(fa$d) %*% t(fa$v))
  zby <- as.matrix(fa$Z %*% fa$B %*% t(fa$Y))

  expect_true(isTRUE(all.equal(udv, zby)))
})

rm(A, n)