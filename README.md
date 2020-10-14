
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/RoheLab/vsp/branch/master/graph/badge.svg)](https://codecov.io/gh/RoheLab/vsp?branch=master)
[![R build
status](https://github.com/RoheLab/vsp/workflows/R-CMD-check/badge.svg)](https://github.com/RoheLab/vsp/actions)
<!-- badges: end -->

# vsp

The goal of `vsp` is to enable fast, spectral estimation of latent
factors in random dot product graphs. Under mild assumptions, the `vsp`
estimator is consistent for (degree-corrected) stochastic blockmodels,
(degree-corrected) mixed-membership stochastic blockmodels, and
degree-corrected overlapping stochastic blockmodels.

More generally, the `vsp` estimator is consistent for random dot product
graphs that can be written in the form

    E(A) = Z B Y^T

where `Z` and `Y` satisfy the varimax assumptions of \[1\]. `vsp` works
on directed and undirected graphs, and on weighted and unweighted
graphs. Note that `vsp` is a semi-parametric estimator.

## Installation

You can install the development version of `vsp` with:

``` r
install.packages("devtools")
devtools::install_github("RoheLab/vsp")
```

## Example

Obtaining estimates from `vsp` is straightforward. We recommend
representing networks as [`igraph`](https://igraph.org/r/) objects or
(sparse) `Matrix` objects from the
[`Matrix`](https://cran.r-project.org/web/packages/Matrix/index.html)
package (see also `?Matrix`).

Once you have your network in one of these formats, you can get
estimates by calling the `vsp()` function. The result is an object of
class `"vsp_fa"`.

``` r
library(Matrix)
library(vsp)

library(igraph)
data(enron, package = "igraphdata")

enron_fa <- vsp(enron, rank = 30, scale = TRUE)
enron_fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Rows (n):   184
#> Cols (d):   184
#> Factors (rank): 30
#> Lambda[rank]:   0.2077
#> 
#> Pre-Processing Options (TODO) 
#> 
#> Components
#> 
#> Z: 184 x 30 [dgeMatrix] 
#> B: 30 x 30 [dgeMatrix] 
#> Y: 184 x 30 [dgeMatrix] 
#> u: 184 x 30 [matrix] 
#> d: 30      [numeric] 
#> v: 184 x 30 [matrix]
```

## References

1.  Rohe, K. & Zeng, M. *Vintage Factor Analysis with Varimax Performs
    Statistical Inference*. 2020+.

Code to reproduce the results in *Vintage Factor Analysis with Varimax
Performs Statistical Inference* is [available
here](https://github.com/RoheLab/vsp-paper).
