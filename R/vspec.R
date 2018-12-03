# # This code implements regularized spectral clustering with varimax.
# # allows for symmetric, directed, and bipartite (i.e. rectangular A).
# # It has the following implemented:
# # uses irlba package
# # checks if symmetric (uses irlba/svd either way)
# # uses regularization
# # returns rotated singular (eigen) vectors and kmeans++ clusters.
#
# # TODO:
# # 1) For symmetric calculations, it would be faster to find eigenvectors.
# # 2) Currently has adjMat as input.  Should could allow igraph object.
# # 3) should we find k+1 eigenvalues to inspect the eigengap?
#
# #  irlba uses default parameters
#
# # Version 0.1.  January 4, 2018  karlrohe@stat.wisc.edu
#
# # library(irlba)
# library(rARPACK)
# library(Matrix)
# library(tidyverse)
# library(irlba)
#
#
#
# projectSimplex = function(y){
#   # project vector onto simplex
#   d = length(y)
#   u = -sort(-y) # negatives for descending
#   rho = which(u + ((1:d)^(-1))*(1-cumsum(u)) >0) %>% max
#   lambda = (1/rho)*(1-sum(u[1:rho]))
#   tmp = y + lambda
#   tmp[tmp<0]=0
#   return(tmp)
#   # tmp = rep(NA, d)
#   # for(j in 1:d) tmp[j]  = u[j] + (1/j)*(1-sum(u[1:j]))
# }
#
#
# makePos = function(x){
#   s = x[which.max(abs(x))]
#   x = sign(s)*x
#   x[x<0]=0
#   return(x)
# }
# flipThresh = function(x, tune = 2){
#   # make varimax rotatated data "positive"
#   #  then, find smallest value (i.e. largest negative).  soft threshold with its absolute value/tune.
#   s = x[which.max(abs(x))]
#   x = sign(s)*x
#   x = projectSimplex(x/tune)
#   # threshold = min(x) %>% abs
#   # x = x - threshold/tune # soft-threshold
#   # x[x<0] = 0  # "hinge" or ReLu
#   return(x)
# }
#
#
# fcent = function(x, args){
#   # multiplies centered(args$A)%*%x, quickly for sparse A.
#
#   # args$ contains: A, rs, n, meanA, onesR, At
#   # if problem is symmetric is_null(At) is true
#   # if problem is not symmetric, then must also contain cs, d, onesC
#   #  A is dgcMatrix
#   #  rs is rowSums(A)
#   #  n is nrow(A)
#   #  meanA = mean(A)
#   #  onesR = rep(1, n) # R stands for row (length n)
#   #  At = t(A) (or At = NULL)
#   #  cs = colSums(A)
#   #  d = ncol(A)
#   #  onesC = rep(1,d)  # C stands for column (length d)
#
#   # this code stores both A and At because we presume that At%*%x is faster than t(A)%*%x.
#   #   this is the case when A is of the type dgCMatrix and At is also dgCMatrix.
#
#   # here is test code for Ax and Atx which do left and right multiplication.
#   # A = matrix(rexp(1000*100, 10), nrow = 1000)
#   # n = nrow(A)
#   # d = ncol(A)
#   # rs = rowSums(A)
#   # cs = colSums(A)
#   # meanA = mean(A)
#   #
#   # Acent = A - outer(rep(1/n,n), cs) - outer(rs, rep(1/d,d)) + outer(rep(1,n), rep(1,d))*meanA
#   #
#   #
#   # x = rnorm(d)
#   # args = list(A = A, rs = rowSums(A), cs = colSums(A), n = nrow(A), d = ncol(A), meanA = mean(A), onesR = rep(1, nrow(A)), onesC = rep(1, ncol(A)))
#   # sd(Ax(x,args) - (Acent%*%x))
#   #
#   # x = rnorm(1000)
#   # args = list(A = A,At=t(A), rs = rowSums(A), cs = colSums(A), n = nrow(A), d = ncol(A), meanA = mean(A), onesR = rep(1, nrow(A)), onesC = rep(1, ncol(A)))
#   # sd(Atx(x,args) - (t(Acent)%*%x))
#
#   if(is_null(args$At)){ # if tA is not stored, then the problem is symmetric.
#     # mx = mean(x)
#     return(
#       Ax(x, args)
#       # as.vector(args$A %*%x - args$onesR*(as.numeric((t(args$rs)%*%x)/args$n) - mx*args$meanA)  - args$rs*mx )
#     )
#   }
#   # otherwise, the problem is asymmetric.
#   return(Ax(Atx(x, args), args))
# }
#
# Ax = function(x, args){
#   # mx = mean(x)
#   return(
#     as.vector(args$A %*%x - args$onesR*(as.numeric((t(args$cs)%*%x)/args$n) - sum(x)*args$meanA)  - args$rs*mean(x) )
#   )
# }
#
# Atx = function(x, args){
#   # mx = mean(x)
#   return(
#     as.vector(args$At %*%x - args$onesC*(as.numeric((t(args$rs)%*%x)/args$d) - sum(x)*args$meanA)  - args$cs*mean(x) )
#   )
# }
#
#
#
# vspec = function(A, k = 5, tau = -1, normalization = T, centering = T, sym = -1){
#   # A is a matrix or a Matrix.
#   # k is the number of clusters.  must be same for rows and columns.
#   # tau is regularization parameter.  it can be a 2 vectors.  Default is pretty good.
#   # This code currently uses the irlba package for non-centered calculations
#   #  This finds a partial SVD.
#   # For centered problems, uses rARPACK and a custom matrix mult function
#   # set sym=F if you want to do two-way analysis.
#
#
#   #   ensure arguments to function are reasonable
#   # # this takes too long:
#   # sym = isSymmetric(A)
#   # if(!(sym %in% c(T,F))) if(nrow(A) = ncol(A)) sym = T
#   if(nrow(A) == ncol(A)) sym = T
#   tw = F # indicates whether two-way (hence svd), or one-way (hence eigen)
#   cs = F
#   if((sym %in% c(-1, F)) | (nrow(A) != ncol(A))){
#     tw = T
#     cs = colSums(A)
#     # print("Two Way analysis not yet supported.  Just ask Karl! ")
#     # return(NA)
#   }
#
#   rs = rowSums(A)
#   cs = colSums(A)
#   if(!tw) if(sd(rs - cs) > 10^(-10)) {
#     tw = T
#     print("The input matrix is not symmetric.  Performing a two-way analysis.")
#     # return(NA)
#   }
#   n = nrow(A)
#   d = ncol(A)
#   if(normalization){
#     if(tau <0)  tau = mean(rs)
#     D = Diagonal(n = n, x = 1/sqrt(rs + tau))
#     tmp = D%*%A
#     if(tw) D = Diagonal(n = d, x = 1/sqrt(cs + mean(cs)))
#     L = tmp %*% D
#
#   }
#   if(!normalization) L = A
#
#
#   if(centering){
#     # this is a fancy way of doing the eigendecomposition for the centered
#     #  version of L... that is, the matrix that is like L - 1 d^T - d 1^T + 11^T
#     #  the reason it is fancy is that multiplying by that matrix can be fast, but can't
#     #  define the full nxn (dense) matrix...
#     rs = rowSums(L)
#     cs = colSums(L)
#     args = list(A = L,At=t(L), rs = rs, cs = cs, n = nrow(L), d = ncol(L), meanA = mean(L), onesR = rep(1, nrow(L)), onesC = rep(1, ncol(L)))
#     ei = eigs_sym(fcent, n=nrow(L), k=k, args=args, which = "LA")
#     U = ei$vectors
#     if(tw) V = t(U)%*%L %>% apply(1, function(x) return(x/sqrt(sum(x^2))))
#     d = ei$values
#     if(tw) d =sqrt(d)
#   }
#   if(!centering){
#     nv= 0
#     if(tw) nv = k
#     s = irlba(L, nu = k, nv = nv)
#     U = s$u
#     if(tw) V = s$v
#     d = s$d
#   }
#
#
#   rotHatU = varimax(U[rs>1,], normalize=F)$rot
#   Zhat = U%*%rotHatU
#   # switch signs to ensure each column of Zhat has positive third moment.
#   signss = sign(colSums(Zhat^3))
#   rotHatU = rotHatU%*%diag(signss)
#   Zhat = U%*%rotHatU
#
#   rotHatV = rotHatU
#   if(tw){
#     rotHatV = varimax(V[cs>1,], normalize=F)$rot
#     Yhat = V%*%rotHatV
#
#     signss = sign(colSums(Yhat^3))
#     rotHatV = rotHatV%*%diag(signss)
#     Yhat = V%*%rotHatV
#
#   }
#
#   Bhat = t(rotHatU)%*%diag(d)%*%rotHatV
#
#   # rs%*%U%*%diag(1/d)%*%rotHatU
#
#
#   # if(centering){
#   #   if(tw) rs%*%V%*%diag(1/d)%*%rotHatU/(n*d)
#   #   if(!tw) rs%*%U%*%diag(1/d)%*%rotHatU/(n*d)
#   # }
#   if(centering) scree = ei$values
#   if(!centering) scree = s$d
#
#   if(!tw) return(list(Z = Zhat, B = Bhat, Y = NULL, scree = scree, U = U))
#   return(list(Z = Zhat, B = Bhat, Y = Yhat, scree = scree, U = U, V= V ))
#
#   # if(cleanUpStep){
#   #   align = Zhat %*% solve(t(Zhat)%*%Zhat)
#   #   B =  t(align) %*% A %*% align
#   #   B[B<0]= 0
#   #   Zhat= A%*% align %*% solve(B)
#   #   Zhat = apply(Zhat,2,makePos)
#   #
#   #   # align = Zhat %*% solve(t(Zhat)%*%Zhat)
#   #   # B =  t(align) %*% L %*% align
#   #   # Zhat= L%*% align %*% solve(B)
#   #   # Zhat = apply(Zhat,2,flipThresh)
#   #   #
#   # }
#   # return(list(zu = Zhat, d = ei$values))
#   #
#
#   #
#   # # compute the (regularized) graph Laplacian.
#   # nr = nrow(A); nc = ncol(A)
#   # rs = rowSums(A);  cs = colSums(A)
#   #
#   #
#   #
#   #
#   # if(tau <0)  tau = c(mean(rs), mean(cs))
#   # if(length(tau)==1) tau = c(tau,tau)
#   #
#   # Drow = Diagonal(n = nr, x = 1/sqrt(rs + tau[1]))
#   # Dcol = Diagonal(n = nc, x = 1/sqrt(cs + tau[2]))
#   # tmp = Drow%*%A
#   # L = tmp %*% Dcol
#   #
#   #
#   # s = irlba(L, nu = 0, nv = k)
#   # # find the singular vectors
#   # # s = svds(L, k = K)
#   # if(sym) s = irlba(L[deg > mean(deg),], nu = 0, nv = k)
#   #
#   #
#   # if(!sym){
#   #   X = rbind(s$u %*% diag(sqrt(s$d)), s$v %*% diag(sqrt(s$d)))
#   #   deg = c(rs, cs)
#   # }
#   # if(sym){
#   #   X = s$v %*% diag(sqrt(s$d))
#   #   deg = rs
#   # }
#   #
#   #
#   # rotHat = varimax(X[deg>1,], normalize=F)$rot
#   # Zhat = X%*%rotHat
#   #
#   #
#   #
#   #
#   #
#   # if(sym){
#   #   align = Zhat %*% solve(t(Zhat)%*%Zhat)
#   #   B =  t(align) %*% A %*% align
#   #   B[B<0]= 0
#   #   Zhat= A%*% align %*% solve(B)
#   #   Zhat = apply(Zhat,2,makePos)
#   #
#   #   # align = Zhat %*% solve(t(Zhat)%*%Zhat)
#   #   # B =  t(align) %*% L %*% align
#   #   # Zhat= L%*% align %*% solve(B)
#   #   # Zhat = apply(Zhat,2,flipThresh)
#   #   #
#   #   return(list(zu = Zhat, d = s$d))
#   # }
#   # if(!sym){
#   #   zu = Zhat[1:length(rs),]
#   #   zv = Zhat[-(1:length(rs)),]
#   #
#   #   alignLeft = zu %*% solve(t(zu)%*%zu)
#   #   alignRight = zv %*% solve(t(zv)%*%zv)
#   #   B =  t(alignLeft) %*% L %*% alignRight
#   #   Bi= solve(B)
#   #   zu= L%*% alignRight %*% Bi
#   #   zv=Bi %*% t(alignLeft) %*% L
#   #   zv = t(zv)
#   #
#   #   zu = apply(zu,2,flipThresh)
#   #   zv = apply(zv,2,flipThresh)
#   #
#   #   return(list(zu = zu, zv = zv, d = s$d))
#   # }
# }
