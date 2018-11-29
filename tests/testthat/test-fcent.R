context("test-fcent")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# dump of comments from vspec file

# here is test code for Ax and Atx which do left and right multiplication.
# A = matrix(rexp(1000*100, 10), nrow = 1000)
# n = nrow(A)
# d = ncol(A)
# rs = rowSums(A)
# cs = colSums(A)
# meanA = mean(A)
#
# Acent = A - outer(rep(1/n,n), cs) - outer(rs, rep(1/d,d)) + outer(rep(1,n), rep(1,d))*meanA
#
#
# x = rnorm(d)
# args = list(A = A, rs = rowSums(A), cs = colSums(A), n = nrow(A), d = ncol(A), meanA = mean(A), onesR = rep(1, nrow(A)), onesC = rep(1, ncol(A)))
# sd(Ax(x,args) - (Acent%*%x))
#
# x = rnorm(1000)
# args = list(A = A,At=t(A), rs = rowSums(A), cs = colSums(A), n = nrow(A), d = ncol(A), meanA = mean(A), onesR = rep(1, nrow(A)), onesC = rep(1, ncol(A)))
# sd(Atx(x,args) - (t(Acent)%*%x))