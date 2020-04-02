context("test-simple")

library(Matrix)

# works with square matrices

M <- rsparsematrix(12, 12, nnz = 40)  # this breaks things
M2 <- sign(M)^2                       # but this doesn't
fa <- vsp(M2, k = 7)
fa

screeplot(fa)

# works with rectangular matrices

M <- rsparsematrix(12, 9, nnz = 40)  # this breaks things
M2 <- sign(M)^2                       # but this doesn't
vsp(M2, k = 7)

# works with negative matrix elements

range(M)
M
vsp(M, k = 7)
