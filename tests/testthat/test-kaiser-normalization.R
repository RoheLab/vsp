library(LRMF3)

test_that("vsp.Matrix normalize U", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      kaiser_normalize_u = TRUE
    )
  )
})

test_that("vsp.Matrix normalize V", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      kaiser_normalize_v = TRUE
    )
  )
})

test_that("vsp.Matrix normalize U & V", {
  expect_silent(
    vsp(
      ml100k,
      rank = 5,
      kaiser_normalize_u = TRUE,
      kaiser_normalize_v = TRUE
    )
  )
})