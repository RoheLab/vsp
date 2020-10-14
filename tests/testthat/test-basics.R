library(LRMF3)

# arguments that i haven't tested yet: k, tau_row, tau_col
# also x itself / different types of input

# other test: recover A with k = rank(A)
# recover A when using transformations

# four options: center, recenter, scale, rescale

# 16 options total, but can only recenter or rescale after centering
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
      scale = FALSE,
      rescale = FALSE
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
      scale = FALSE,
      rescale = TRUE
    ),
    regexp = "`rescale` must be FALSE when `scale` is FALSE."
  )
})

test_that("vsp.Matrix FFTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = FALSE,
      recenter = FALSE,
      scale = TRUE,
      rescale = FALSE
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
      scale = TRUE,
      rescale = TRUE
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
      scale = FALSE,
      rescale = FALSE
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
      scale = FALSE,
      rescale = TRUE
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
      scale = TRUE,
      rescale = FALSE
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
      scale = TRUE,
      rescale = TRUE
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
      scale = FALSE,
      rescale = FALSE
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
      scale = FALSE,
      rescale = TRUE
    ),
    regexp = "`rescale` must be FALSE when `scale` is FALSE."
  )
})

test_that("vsp.Matrix TFTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = FALSE,
      scale = TRUE,
      rescale = FALSE
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
      scale = TRUE,
      rescale = TRUE
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
      scale = FALSE,
      rescale = FALSE
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
      scale = FALSE,
      rescale = TRUE
    ),
    regexp = "`rescale` must be FALSE when `scale` is FALSE."
  )
})

test_that("vsp.Matrix TTTF", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      center = TRUE,
      recenter = TRUE,
      scale = TRUE,
      rescale = FALSE
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
      scale = TRUE,
      rescale = TRUE
    )
  )
})
