# ipr <- function(x) {
#   sum(x^4)
# }
#
# get_ipr <- function(A, k) {
#
#   s <- RSpectra::svds(A, k)
#
#   tibble(
#     u_ipr = apply(s$u, 2, ipr),
#     v_ipr = apply(s$v, 2, ipr),
#     id = 1:k
#   )
# }
#
