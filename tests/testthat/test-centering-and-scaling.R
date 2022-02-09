library(LRMF3)

# arguments that i haven't tested yet: k, tau_row, tau_col
# also x itself / different types of input

# other test: recover A with k = rank(A)
# recover A when using transformations

# four options: center, recenter, degree_normalize, renormalize

# 16 options total, but can only recenter or renormalize after centering
# or scaling in the first place, expect errors in those cases

test_that("vsp.Matrix defaults", {
  expect_silent(
    vsp(ml100k, rank = 5)
  )
})

test_that("vsp.Matrix FFFF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = FALSE,
      degree_normalize = FALSE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix FFFT", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = FALSE,
      degree_normalize = FALSE,
      renormalize = TRUE
    ),
    regexp = "`renormalize` must be FALSE when `degree_normalize` is FALSE."
  )
})

test_that("vsp.Matrix FFTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = FALSE,
      degree_normalize = TRUE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix FFTT", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = FALSE,
      degree_normalize = TRUE,
      renormalize = TRUE
    )
  )
})

test_that("vsp.Matrix FTFF", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = TRUE,
      degree_normalize = FALSE,
      renormalize = FALSE
    ),
    regexp = "`recenter` must be FALSE when `center` is FALSE."
  )
})

test_that("vsp.Matrix FTFT", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = TRUE,
      degree_normalize = FALSE,
      renormalize = TRUE
    ),
    regexp = "`recenter` must be FALSE when `center` is FALSE."
  )
})

test_that("vsp.Matrix FTTF", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = TRUE,
      degree_normalize = TRUE,
      renormalize = FALSE
    ),
    regexp = "`recenter` must be FALSE when `center` is FALSE."
  )
})

test_that("vsp.Matrix FTTT", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = TRUE,
      degree_normalize = TRUE,
      renormalize = TRUE
    ),
    regexp = "`recenter` must be FALSE when `center` is FALSE."
  )
})

test_that("vsp.Matrix TFFF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = FALSE,
      degree_normalize = FALSE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix TFFT", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = FALSE,
      degree_normalize = FALSE,
      renormalize = TRUE
    ),
    regexp = "`renormalize` must be FALSE when `degree_normalize` is FALSE."
  )
})

test_that("vsp.Matrix TFTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = FALSE,
      degree_normalize = TRUE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix TFTT", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = FALSE,
      degree_normalize = TRUE,
      renormalize = TRUE
    )
  )
})

test_that("vsp.Matrix TTFF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = TRUE,
      degree_normalize = FALSE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix TTFT", {
  expect_error(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = TRUE,
      degree_normalize = FALSE,
      renormalize = TRUE
    ),
    regexp = "`renormalize` must be FALSE when `degree_normalize` is FALSE."
  )
})

test_that("vsp.Matrix TTTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = TRUE,
      degree_normalize = TRUE,
      renormalize = FALSE
    )
  )
})

test_that("vsp.Matrix TTTT", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = TRUE,
      degree_normalize = TRUE,
      renormalize = TRUE
    )
  )
})
