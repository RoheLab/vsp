library(testthat)

skip_if_not_installed("fastadi")
library(fastadi)

test_that("fastadi, no transformations", {

  mf <- adaptive_impute(ml100k, rank = 3, max_iter = 5)

  expect_warning(
    fa <- vsp(mf),
    regexp = "Reached maximum allowed iterations. Returning early."
  )
})

test_that("fastadi, with scaling", {

  library(LRMF3)
  library(invertiforms)

  scaler <- RegularizedLaplacian(ml100k)
  M <- transform(scaler, ml100k)

  mf <- adaptive_impute(M, rank = 3, max_iter = 5)

  expect_warning(
    vsp(mf, scaler = scaler, rescale = TRUE),
    regexp = "Reached maximum allowed iterations. Returning early."
  )
})
