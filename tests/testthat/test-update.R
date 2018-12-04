context("test-update")

library(Matrix)

test_that("Updated SVD agrees with full original SVD", {

  set.seed(27)
  M <- rsparsematrix(12, 12, nnz = 40)
  M2 <- sign(M)^2
  fa <- vsp(M2, k = 7)

  fa2 <- vsp(M2, k = 4)
  fa3 <- update(fa2, graph = M2, k = 7)

  expect_equal(fa$U, fa3$U)
  expect_equal(fa$d, fa3$d)
  expect_equal(fa$V, fa3$V)
  expect_equal(fa$k, fa3$k)
})

test_that("Updated varimax agrees with full original varimax", {

  set.seed(27)
  M <- rsparsematrix(12, 12, nnz = 40)
  M2 <- sign(M)^2
  fa <- vsp(M2, k = 7)

  fa2 <- vsp(M2, k = 4)
  fa3 <- update(fa2, graph = M2, k = 7, update_varimax = TRUE)

  expect_equal(fa$Z, fa3$Z)
  expect_equal(fa$B, fa3$B)
  expect_equal(fa$Y, fa3$Y)
})

