
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**Disclaimer:** This package is a work in progress. Some basic
functionality is available, but the API is subject to change, as are
some (minor) computational details. Code to reproduce the results in
*Vintage Factor Analysis with Varimax Performs Statistical Inference* is
[available here](https://github.com/RoheLab/vsp-paper).

# vsp

The goal of `vsp` to enable fast, spectral estimation of latent factors
in random dot product graphs. Under mild assumptions, the `vsp`
estimator is consistent for (degree-corrected) stochastic blockmodels,
(degree-corrected) mixed-membership stochastic blockmodels,
degree-corrected overlapping stochastic blockmodels.

More generally, the `vsp` estimator is consistent for random dot product
graphs that can be written in the form

    E(A) = Z B Y^T

where `Z` and `Y` satisfy the varimax assumptions of \[1\]. `vsp` works
on directed and undirected graphs, and on weighted and unweighted
graphs.

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
class `"vsp"`.

``` r
library(vsp)
library(Matrix)

# create a random sparse adjacency matrix
M <- sign(rsparsematrix(12, 12, nnz = 40))^2

# vintage sparse pca
fa <- vsp(M, k = 5)
fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   12
#> Factors (k): 5
#> Lambda[k]:   0.1895
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     TRUE 
#>  - Normalization: TRUE 
#>    - Tau (rows):  3.33 [Default: Mean Degree] 
#>    - Tau (cols):  3.33 [Default: Mean Degree] 
#> 
#> Components
#> 
#> U: 12 x 5 [matrix] 
#> d: 5      [numeric] 
#> V: 12 x 5 [matrix] 
#> Z: 12 x 5 [dgeMatrix] 
#> B: 5 x 5 [dgeMatrix] 
#> Y: 12 x 5 [dgeMatrix]
```

`vsp()` is an S3 generic, so can similarly just pass an `igraph` object.
This means that `vsp` also supports
[`tidygraph`](https://github.com/thomasp85/tidygraph), which is built on
top of `igraph`. We use `enron` network from `igraphdata` package to
demonstrate this functionality.

``` r
library(igraph)
data(enron, package = "igraphdata")

enron
#> IGRAPH 64ec693 D--- 184 125409 -- Enron email network
#> + attr: LDC_names (g/c), LDC_desc (g/c), name (g/c), Citation (g/c),
#> | Email (v/c), Name (v/c), Note (v/c), Time (e/c), Reciptype (e/c),
#> | Topic (e/n), LDC_topic (e/n)
#> + edges from 64ec693:
#>  [1]  25->154  25->154  30-> 30  30-> 30  30-> 30  30-> 30  39-> 39  52-> 67
#>  [9]  52-> 67  52-> 67  52-> 67  61->100  61->100  61->163  61->163  61->166
#> [17]  61->166  61->170  64-> 59  64-> 59  64-> 64  64-> 64  64->147  64->147
#> [25]  64->164  64->164  64->168  66-> 66  66-> 66  67->129  67->129  67->129
#> [33]  67->129  93-> 10  93-> 10  93-> 10  93-> 10  93-> 39  93-> 39  93-> 93
#> [41]  93-> 93  93-> 93  93-> 93  93->124  93->124 100-> 61 100-> 61 115->115
#> + ... omitted several edges
```

``` r
enron_fa <- vsp(enron, k = 30)
enron_fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   184
#> Factors (k): 30
#> Lambda[k]:   0.2039
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     TRUE 
#>  - Normalization: TRUE 
#>    - Tau (rows):  681.57 [Default: Mean Degree] 
#>    - Tau (cols):  681.57 [Default: Mean Degree] 
#> 
#> Components
#> 
#> U: 184 x 30 [matrix] 
#> d: 30      [numeric] 
#> V: 184 x 30 [matrix] 
#> Z: 184 x 30 [dgeMatrix] 
#> B: 30 x 30 [dgeMatrix] 
#> Y: 184 x 30 [dgeMatrix]
```

The default print method has a large amount of valuable information.
First, we learn that the graph has 184 nodes, and that we have used `k
= 30` as our guess of the rank of `A`, the network adjacency matrix. The
fifth singular value of `A` is 0.2077. This is of interest, as we expect
`lambda[k] ~ 0` to indicate that we have we have overestimated the rank
of `A`, in say, blockmodels.

The pre-processing section indicates that the matrix representation of
the graph was row- and column-centered before estimation (`center =
TRUE`), and that we used the graph Laplacian rather than the adjacency
matrix (`normalize = TRUE`).

By default, `normalize = TRUE` not only uses the graph Laplacian, but
also regularizes the graph Laplacian using mean in-degree and
out-degree. In particular, we form the graph Laplacian as follows:

    # TODO

In general, you do not need to tune `tau_row` and `tau_col`. To turn off
regularization (not recommended\!), see `tau_row = 0` and `tau_col = 0`.
This often leads to eigenvector localization and thus overfitting.

``` r
vsp(M, k = 5, tau_row = 0, tau_col = 0)
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   12
#> Factors (k): 5
#> Lambda[k]:   0.3813
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     TRUE 
#>  - Normalization: TRUE 
#>    - Tau (rows):  0 [User-Specified] 
#>    - Tau (cols):  0 [User-Specified] 
#> 
#> Components
#> 
#> U: 12 x 5 [matrix] 
#> d: 5      [numeric] 
#> V: 12 x 5 [matrix] 
#> Z: 12 x 5 [dgeMatrix] 
#> B: 5 x 5 [dgeMatrix] 
#> Y: 12 x 5 [dgeMatrix]
```

The components `U`, `d`, and `V` are the standard elements of the
singular value decomposition of your network.

At the moment, we are performing centering explicitly, which is
inefficient and converts adjacency matrices from sparse into dense
matrices. Setting `center = TRUE` produces a warning to remind you of
this. We plan to implement implicit centering shortly.

``` r
vsp(M, k = 5, center = TRUE)
#> Warning in double_center(L): Implicit centering has not yet been implemented.
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   12
#> Factors (k): 5
#> Lambda[k]:   0.1895
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     TRUE 
#>  - Normalization: TRUE 
#>    - Tau (rows):  3.33 [Default: Mean Degree] 
#>    - Tau (cols):  3.33 [Default: Mean Degree] 
#> 
#> Components
#> 
#> U: 12 x 5 [matrix] 
#> d: 5      [numeric] 
#> V: 12 x 5 [matrix] 
#> Z: 12 x 5 [dgeMatrix] 
#> B: 5 x 5 [dgeMatrix] 
#> Y: 12 x 5 [dgeMatrix]
```

Note that In particular, you can approximately reconstruct the matrix
representation of your network as follows:

``` r
fa2 <- vsp(M, k = 5, normalize = FALSE)

fa2$U %*% diag(fa2$d) %*% t(fa2$V)
#>              [,1]        [,2]        [,3]        [,4]        [,5]        [,6]
#>  [1,]  0.08395758  0.52874594 -0.18471932 -0.40346505 -0.36342981  0.49172545
#>  [2,] -0.34351273 -0.41829112  0.26200057 -0.18895489  0.07528951 -0.49563936
#>  [3,] -0.16692887 -0.01830199  0.09835462  0.05803358  0.08553863 -0.09415295
#>  [4,]  0.53205718 -0.37151399 -0.15036809 -0.20716106 -0.29998079 -0.47453640
#>  [5,]  0.39652037  0.28662413 -0.07506757  0.50638202 -0.10116961  0.27283751
#>  [6,] -0.01381713  0.36098110  0.42700058  0.28235501 -0.54316678 -0.70625195
#>  [7,] -0.48346166 -0.26566531 -0.03067872  0.57178049  1.03350575 -0.39433902
#>  [8,] -0.41437937 -0.03028295  0.02353064  0.31450829  0.60969998 -0.09412494
#>  [9,] -0.16692887 -0.01830199  0.09835462  0.05803358  0.08553863 -0.09415295
#> [10,] -0.26028961  0.59242705 -0.09801114 -0.33400990 -0.11597931  0.43322896
#> [11,]  0.56512603 -0.19713562 -0.12822063 -0.16528438 -0.40847838  0.57899113
#> [12,]  0.27165707 -0.44928524 -0.24217554 -0.49221770 -0.05736781  0.57641452
#>              [,7]        [,8]        [,9]        [,10]       [,11]       [,12]
#>  [1,] -0.22173982  0.61321647 -0.28693631 -0.455326136 -0.37742261  0.57539361
#>  [2,]  0.18465233 -0.09965227 -0.24321769  0.572204044  0.72827908 -0.03315747
#>  [3,]  0.02250366 -0.11146065 -0.09149344 -0.004104664  0.19638824  0.02562384
#>  [4,] -0.25339050  0.56161868  0.49512927  0.668955452 -0.21686289 -0.28394686
#>  [5,] -0.08885419 -0.34094924  0.58154722 -0.724287304 -0.46020122 -0.25338210
#>  [6,] -0.64023247  0.27102438  0.41514757 -0.677605524  0.54316725  0.28139796
#>  [7,] -0.15935243 -0.27633451 -0.19970063  0.607855219 -0.09650052 -0.30710867
#>  [8,] -0.04031135 -0.23308113 -0.27527295  0.163226397  0.01880806 -0.04232068
#>  [9,]  0.02250366 -0.11146065 -0.09149344 -0.004104664  0.19638824  0.02562384
#> [10,] -0.25720923  0.55269229 -0.58077813 -0.455698992 -0.20046589  0.72409391
#> [11,]  0.64790612 -0.51381236  0.39758166 -0.193551820 -0.18181044 -0.40131131
#> [12,]  0.78352422 -0.31180102 -0.12051313  0.502437992 -0.14976730 -0.31090606
```

Note that you don’t want to do this for large networks since the
reconstruction will be dense. Similarly, the varimax-rotated network
approximate reconstruction is

``` r
fa$Z %*% fa$B %*% t(fa$Y)
#> 12 x 12 Matrix of class "dgeMatrix"
#>               [,1]         [,2]          [,3]        [,4]         [,5]
#>  [1,] -0.007525320  0.013788208 -0.0446787737 -0.14739242 -0.095481324
#>  [2,]  0.124934712 -0.118842288 -0.1361631344 -0.07981819  0.065850432
#>  [3,]  0.021774092 -0.013657558 -0.0061254090  0.01147960 -0.006178269
#>  [4,] -0.086600958 -0.043907686  0.0006924182 -0.04660414  0.120856241
#>  [5,]  0.001209366  0.018595144  0.1002411790  0.08829795 -0.142328652
#>  [6,]  0.046617113 -0.100851076 -0.0093647434 -0.04016756 -0.093260408
#>  [7,] -0.103883071  0.019664967  0.0546303398  0.21500219  0.327187200
#>  [8,] -0.110888536  0.066767933  0.0572182678  0.19639288  0.280421085
#>  [9,]  0.021774092 -0.013657558 -0.0061254090  0.01147960 -0.006178269
#> [10,]  0.003322721  0.007152494 -0.0709570316 -0.16834003 -0.083826049
#> [11,]  0.037075545  0.088329297  0.0687424197  0.04477925 -0.182469201
#> [12,]  0.040859334  0.089444382 -0.0164389349 -0.01508955 -0.052566600
#>               [,6]        [,7]         [,8]        [,9]       [,10]
#>  [1,]  0.109324096  0.11581364  0.018393903 -0.11197529  0.02709630
#>  [2,] -0.016803554  0.02638064  0.138451990  0.12387904  0.16438666
#>  [3,]  0.008128220  0.02644816 -0.021284659  0.03166487 -0.00421887
#>  [4,] -0.099296968 -0.09366922  0.082997589 -0.11581371  0.16989723
#>  [5,] -0.045328868 -0.01534004 -0.099234629  0.07162963 -0.07355947
#>  [6,]  0.004434681  0.13809334 -0.080680152  0.04253063  0.11598937
#>  [7,] -0.050381393 -0.06079082 -0.150374256 -0.04113295 -0.08529287
#>  [8,]  0.019811013 -0.01378732 -0.180610731 -0.07798634 -0.16218918
#>  [9,]  0.008128220  0.02644816 -0.021284659  0.03166487 -0.00421887
#> [10,]  0.155109507  0.17523813 -0.002396813 -0.12480396  0.02402951
#> [11,] -0.066608477 -0.17063636  0.098943776  0.11365169 -0.12563281
#> [12,] -0.027974598 -0.18077794  0.196140950  0.06693505 -0.09351821
#>              [,11]        [,12]
#>  [1,]  0.033899663  0.075019497
#>  [2,] -0.274215893 -0.104231903
#>  [3,] -0.023268340 -0.031351379
#>  [4,]  0.103187125  0.045163135
#>  [5,]  0.166703859 -0.040128891
#>  [6,]  0.085009203 -0.108154128
#>  [7,] -0.004668562 -0.039776444
#>  [8,]  0.001340002  0.003200313
#>  [9,] -0.023268340 -0.031351379
#> [10,] -0.003644719  0.067857764
#> [11,]  0.012300063  0.053876992
#> [12,] -0.162831202  0.096030237
```

If you want to directly manipulate the estimates, you can access them as
we have above with `fa$B` and so on and so forth. Note that these are
`matrix` and `Matrix` objects, and you may need to perform type
coercions. We also provide some utility functions to extract estimates
into `tibbles`:

``` r
get_varimax_z(fa)
#> # A tibble: 12 x 5
#>         z1      z2      z3       z4       z5
#>      <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
#>  1  0.644  -0.187  -0.193  -0.0273   0.0266 
#>  2 -0.136  -0.0418  1.21    0.0571  -0.122  
#>  3 -0.0452  0.0249  0.103   0.0901  -0.190  
#>  4 -0.0356  0.0121 -0.0486 -0.00337  1.20   
#>  5 -0.408  -0.211  -0.520   0.332   -0.173  
#>  6 -0.0807 -0.157   0.111   0.699   -0.0488 
#>  7 -0.240   0.890  -0.0670 -0.0812  -0.00716
#>  8  0.0444  0.893  -0.297  -0.200   -0.308  
#>  9 -0.0452  0.0249  0.103   0.0901  -0.190  
#> 10  0.826  -0.103  -0.0956 -0.00323 -0.175  
#> 11 -0.447  -0.501  -0.296  -0.364   -0.129  
#> 12 -0.129  -0.299   0.179  -0.764   -0.0775
```

``` r
get_varimax_y(fa)
#> # A tibble: 12 x 5
#>         y1        y2      y3      y4       y5
#>      <dbl>     <dbl>   <dbl>   <dbl>    <dbl>
#>  1  0.0764  0.0857   -0.308   0.292  -0.346  
#>  2 -0.527  -0.178    -0.0756 -0.236  -0.00891
#>  3 -0.231  -0.121    -0.0592  0.164   0.439  
#>  4 -0.416  -0.0435    0.399   0.553   0.221  
#>  5  0.0822  0.000162  1.26   -0.0641 -0.194  
#>  6 -0.222   0.514    -0.123  -0.380  -0.308  
#>  7  0.192   0.952    -0.134  -0.138  -0.111  
#>  8  0.230  -0.724    -0.214  -0.255  -0.403  
#>  9 -0.0581 -0.0971   -0.275   0.784  -0.238  
#> 10  0.978  -0.0532    0.0776 -0.0309  0.100  
#> 11  0.121   0.0305   -0.279  -0.100   1.15   
#> 12 -0.264  -0.319    -0.0876 -0.611   0.00889
```

To visualize a screeplot of the singular value, use:

``` r
screeplot(fa)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

At the moment, we also enjoy using pairs plots of the factors as a
diagnostic measure:

``` r
plot_varimax_z_pairs(fa, 1:3)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r
plot_varimax_y_pairs(fa, 1:3)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

Similarly, an IPR pairs plot can be a good way to check for singular
vector localization (and thus overfitting\!).

``` r
# TODO
```

TODO: visualizing B

TODO: selecting k

## Tidygraph integration

``` r
library(tidygraph)

sbm <- play_blocks(25, 25, 0.1)
```

`vsp` also works on `igraph` and `tidygraph` objects:

``` r
fa2 <- vsp(sbm, k = 5)
```

You can then add the resulting factor loadings back into the node
information about the graph:

``` r
# if you already have a vsp object
sbm_fa2 <- sbm %>% 
  bind_varimax_z(fa2)

sbm_fa2
#> # A tbl_graph: 25 nodes and 56 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>        z1      z2      z3      z4      z5
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1  0.788  -0.183  -0.377  -0.160   0.386 
#> 2  0.152  -0.238  -0.631   0.516   0.716 
#> 3  0.219  -0.242  -0.603  -0.236  -0.846 
#> 4  0.110  -0.0367  0.0379  0.0278 -0.0910
#> 5  0.0334  0.740  -0.138  -0.147  -0.345 
#> 6 -0.755   0.0643  0.429  -0.309   0.446 
#> # ... with 19 more rows
#> #
#> # Edge Data: 56 x 2
#>    from    to
#>   <int> <int>
#> 1     6     1
#> 2    10     1
#> 3     5     2
#> # ... with 53 more rows
```

## References

1.  Rohe, K. & Zeng, M. *Vintage Factor Analysis with Varimax Performs
    Statistical Inference*. 2020+.
