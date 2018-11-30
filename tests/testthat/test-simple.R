context("test-simple")

library(Matrix)

# symmetric

M <- rsparsematrix(12, 12, nnz = 40)  # this breaks things
M2 <- sign(M)^2                       # but this doesn't
fa <- vsp(M2, k = 7)
fa

screeplot(fa)

# asymmetric

M <- rsparsematrix(12, 9, nnz = 40)  # this breaks things
M2 <- sign(M)^2                       # but this doesn't
vsp(M2, k = 7)
