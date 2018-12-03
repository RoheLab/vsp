// // -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// #include "RcppArmadillo.h"
//
// // [[Rcpp::depends(RcppArmadillo)]]
//
// // there has to be a better way to do this
// arma::vec prox_l1(arma::vec y, double lambda) {
//   arma::vec z = arma::zeros<arma::vec>(y.n_elem);
//   arma::mat both = arma::join_horiz(arma::abs(y) - lambda, z);
//   return sign(y) % arma::max(both, 1);
// }
//
// // similarly, can this be done without the intermediate matrix?
// double op_norm(arma::vec & x, arma::mat & A) {
//   arma::mat quad_form = x.t() * A * x;
//   return std::sqrt(quad_form(0, 0));
// }
//
// // export this to R, but not to the user of the package
// // [[Rcpp::export]]
// Rcpp::List sfpca_arma(arma::mat & X, double lambda_u, double lambda_v,
//                       double alpha_u, double alpha_v,
//                       arma::mat Omega_u, arma::mat Omega_v) {
//
//   // following same notation conventions as in sfpca_r
//
//   int n = X.n_rows;
//   int p = X.n_cols;
//
//   // initialize u, v to rank 1 SVD solution
//
//   arma::mat U, V, I_n, I_p, S_u, S_v;
//   arma::vec s, u, v, old_u, old_v, tmp_u, tmp_v;
//   double L_u, L_v, tol, delta, delta_u, delta_v, u_norm, v_norm;
//
//   svd(U, s, V, X);
//
//   u = U.col(0);
//   v = V.col(0);
//
//   I_n.eye(n, n);
//   I_p.eye(p, p);
//
//   // multiply by n and p to agree with MATLAB implementation
//   S_u = I_n + alpha_u * Omega_u * n;
//   S_v = I_p + alpha_v * Omega_v * p;
//
//   // add fudge factor to the largest eigenval to match MATLAB code
//   L_u = std::real(max(eig_gen(S_u))) + 0.01;
//   L_v = std::real(max(eig_gen(S_v))) + 0.01;
//
//   tol = 1e-6;
//   delta = 1;
//
//   while (delta > tol) {
//
//     delta_u = delta_v = 1;
//     old_u = u;
//     old_v = v;
//
//     while (delta_u > tol) {
//       tmp_u = u;
//       u = prox_l1(u + (X * v - S_u * u) / L_u, lambda_u / L_u);
//       double u_norm = op_norm(u, S_u);
//       if (u_norm > 0) {
//         u = u / u_norm;
//       }
//       delta_u = norm(u - tmp_u) / norm(tmp_u);
//     }
//
//     while (delta_v > tol) {
//       tmp_v = v;
//       v = prox_l1(v + (X.t() * u - S_v * v) / L_v, lambda_v / L_v);
//       double v_norm = op_norm(v, S_v);
//       if (v_norm > 0) {
//         v = v / v_norm;
//       }
//       delta_v = norm(v - tmp_v) / norm(tmp_v);
//     }
//
//     delta = norm(old_u - u) / norm(old_u) + norm(old_v - v) / norm(old_v);
//   }
//
//   u_norm = norm(u);
//   v_norm = norm(v);
//
//   if (u_norm > 0) {
//     u = u / u_norm;
//   }
//
//   if (v_norm > 0) {
//     v = v / v_norm;
//   }
//
//   // again, can I avoid the intermediate matrix?
//   arma::mat sigma = u.t() * X * v;
//
//   return Rcpp::List::create(Rcpp::Named("u") = u,
//                             Rcpp::Named("d") = sigma(0, 0),
//                             Rcpp::Named("v") = v);
// }