
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
#> Lambda[k]:   0.2019
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
#> Lambda[k]:   0.4245
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
#> Lambda[k]:   0.2019
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
#>              [,1]        [,2]         [,3]        [,4]        [,5]        [,6]
#>  [1,]  0.12551303  0.11932844  0.006659208 -0.24126936  0.44367359 -0.08234905
#>  [2,]  0.08439468 -0.34433706 -0.380682978 -0.19607005 -0.24389048  0.52082725
#>  [3,] -0.05491455  0.55154965 -0.302527323 -0.26274434 -0.46446806 -0.55039221
#>  [4,] -0.01403590  0.51281026  0.522065394 -0.38564437  0.74306092 -0.37270373
#>  [5,] -0.18047962 -0.36753573 -0.041306959 -0.22966763 -0.42435306  0.66863863
#>  [6,] -0.30440754 -0.23916816 -0.054158354 -0.09321193  0.54157078 -0.43239923
#>  [7,]  0.07979592 -0.01708854 -0.049560946  0.07383041 -0.05503668  0.05802639
#>  [8,]  0.11842157 -0.22265905 -0.160727887  0.73397254 -0.22671450 -0.15153499
#>  [9,]  0.44106406  0.37568094  0.476481476  0.47056188  0.09574027  0.10353964
#> [10,] -0.35531952 -0.59367715  0.162683448  0.25514246  0.07308329  0.19544628
#> [11,] -0.01982807  0.24218496 -0.129364132 -0.19873003 -0.42762938 -0.01512537
#> [12,]  0.07979592 -0.01708854 -0.049560946  0.07383041 -0.05503668  0.05802639
#>             [,7]        [,8]        [,9]       [,10]       [,11]       [,12]
#>  [1,] -0.4319879  0.23896201 -0.39159648  0.69493279 -0.22565091 -0.25621534
#>  [2,] -0.4629647  0.48234775 -0.33394902  0.97201353 -0.27305914  0.17537019
#>  [3,]  0.5174915 -0.27505617  0.66229851 -0.17001588  0.54030385 -0.19152499
#>  [4,]  0.3834677 -0.33036115 -0.10112936 -0.26846777 -0.45160586 -0.23745608
#>  [5,]  0.4786965  0.03421609  0.04990733 -0.13708653 -0.47180395  0.62077495
#>  [6,] -0.4587633  0.60580615 -0.33710418  0.08072078  0.70569922 -0.01458423
#>  [7,] -0.1277952  0.01035964 -0.02990935  0.14247515 -0.03319136 -0.05190544
#>  [8,] -0.5172692  0.04603428  0.09420421 -0.19340635  0.67466006 -0.19498068
#>  [9,]  0.1582448 -0.78902045  0.11661314 -0.39259235 -0.69449215 -0.36182127
#> [10,]  0.0833050  0.22876282 -0.10737192 -0.72023653  0.28832295  0.48985888
#> [11,]  0.5053700 -0.26241060  0.40794650 -0.15081199 -0.02599136  0.07438944
#> [12,] -0.1277952  0.01035964 -0.02990935  0.14247515 -0.03319136 -0.05190544
```

Note that you don’t want to do this for large networks since the
reconstruction will be dense. Similarly, the varimax-rotated network
approximate reconstruction is

``` r
fa$Z %*% fa$B %*% t(fa$Y)
#> 12 x 12 Matrix of class "dgeMatrix"
#>               [,1]         [,2]         [,3]         [,4]         [,5]
#>  [1,]  0.128123475  0.092277209 -0.008783674  0.138952200 -0.104643368
#>  [2,]  0.099100436  0.059010524 -0.094506148  0.049052301 -0.051170541
#>  [3,]  0.013634315  0.101654973  0.105211773 -0.085199372  0.180609918
#>  [4,]  0.043693205 -0.062435123  0.101350965  0.152020051 -0.043883681
#>  [5,] -0.013130599 -0.159745859 -0.053908746  0.006442194 -0.003819166
#>  [6,] -0.051318502 -0.013729950 -0.015498440  0.084746737  0.061209793
#>  [7,] -0.001721112  0.004208073 -0.017217906 -0.012532253 -0.032736942
#>  [8,] -0.172689297  0.044658325 -0.086155194 -0.210025463 -0.027551363
#>  [9,] -0.002332104 -0.004005504  0.022176859 -0.030423714 -0.090237447
#> [10,] -0.069329014 -0.081430172 -0.035149610 -0.014063469  0.007397860
#> [11,]  0.051131825  0.064826295  0.079690537 -0.074554046  0.137145172
#> [12,] -0.001721112  0.004208073 -0.017217906 -0.012532253 -0.032736942
#>              [,6]        [,7]         [,8]        [,9]        [,10]
#>  [1,] -0.03876572  0.02333433 -0.185277329  0.13042358 -0.016495597
#>  [2,] -0.05484752 -0.06668509 -0.044092021  0.09775527  0.262853414
#>  [3,] -0.09544353 -0.02529707 -0.005149683 -0.07534360  0.054019717
#>  [4,]  0.10770306  0.02544525 -0.104987518 -0.07319908 -0.239471909
#>  [5,]  0.14376277 -0.10215999  0.129477750 -0.12673515  0.199810956
#>  [6,] -0.13069168 -0.07974247  0.086383400 -0.02651811 -0.093153531
#>  [7,]  0.01032451  0.02443099 -0.011835826  0.03274918 -0.003335965
#>  [8,] -0.09979722  0.13054433  0.112797869  0.13872856 -0.125649744
#>  [9,]  0.09432132  0.11400646 -0.080144441  0.04835747 -0.127895484
#> [10,]  0.02184313 -0.03065621  0.109367719 -0.04820947 -0.009517891
#> [11,] -0.01732472 -0.04136215 -0.016325078 -0.08345470  0.163128707
#> [12,]  0.01032451  0.02443099 -0.011835826  0.03274918 -0.003335965
#>              [,11]       [,12]
#>  [1,] -0.013947311 -0.13068458
#>  [2,]  0.003534606 -0.22118545
#>  [3,] -0.062764027 -0.11171624
#>  [4,] -0.063806979  0.16198183
#>  [5,] -0.061101442  0.08833424
#>  [6,]  0.228494661 -0.06983423
#>  [7,] -0.006305117  0.01110603
#>  [8,]  0.163993342  0.05627626
#>  [9,] -0.098363136  0.14070043
#> [10,]  0.072661330  0.07221603
#> [11,] -0.145031109 -0.09614474
#> [12,] -0.006305117  0.01110603
```

If you want to directly manipulate the estimates, you can access them as
we have above with `fa$B` and so on and so forth. Note that these are
`matrix` and `Matrix` objects, and you may need to perform type
coercions. We also provide some utility functions to extract estimates
into `tibbles`:

``` r
get_varimax_z(fa)
#> # A tibble: 12 x 5
#>         z1       z2      z3     z4       z5
#>      <dbl>    <dbl>   <dbl>  <dbl>    <dbl>
#>  1  0.382  -0.376   -0.716  -0.353 -0.0382 
#>  2  0.952  -0.190   -0.0800 -0.196 -0.00416
#>  3 -0.0901 -0.00972 -0.156   0.927  0.0780 
#>  4 -0.735  -0.568   -0.189  -0.221  0.00512
#>  5  0.129  -0.260    1.00   -0.162 -0.129  
#>  6 -0.151   0.0253  -0.170  -0.106  1.02   
#>  7  0.0604  0.0835  -0.0317 -0.103 -0.131  
#>  8 -0.0343  1.19    -0.161  -0.106 -0.0390 
#>  9 -0.273   0.111   -0.132  -0.187 -0.606  
#> 10 -0.143   0.146    0.381  -0.149  0.225  
#> 11  0.126  -0.191    0.124   0.798 -0.242  
#> 12  0.0604  0.0835  -0.0317 -0.103 -0.131
```

``` r
get_varimax_y(fa)
#> # A tibble: 12 x 5
#>          y1        y2       y3      y4      y5
#>       <dbl>     <dbl>    <dbl>   <dbl>   <dbl>
#>  1  0.146   -0.382    -0.242   -0.0722  0.468 
#>  2 -0.109   -0.192    -0.653   -0.0160 -0.211 
#>  3 -0.363   -0.364    -0.141    0.477   0.0733
#>  4 -0.123    0.0606    0.00336 -0.0638  1.07  
#>  5 -0.0322  -0.000785 -0.224    0.785  -0.191 
#>  6  0.106   -0.373     0.630   -0.159   0.106 
#>  7 -0.336   -0.204    -0.0537  -0.323  -0.429 
#>  8  0.252    0.565     0.403    0.255  -0.347 
#>  9 -0.00321  0.0848   -0.344   -0.713  -0.188 
#> 10  1.24    -0.138    -0.0419  -0.0351 -0.100 
#> 11 -0.178    0.951    -0.104   -0.0828  0.133 
#> 12 -0.422   -0.100     0.819   -0.0456 -0.208
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
#> # A tbl_graph: 25 nodes and 48 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>        z1      z2      z3      z4      z5
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1 -0.104   1.06   -0.552  -0.278   0.311 
#> 2  0.0223  0.0355  0.0157  0.0852  0.0979
#> 3 -0.441  -0.717  -0.339  -0.0832 -0.126 
#> 4  0.0223  0.0355  0.0157  0.0852  0.0979
#> 5 -0.205   0.0199  0.0916  0.0429 -0.169 
#> 6  0.0366 -0.855  -0.514  -0.523  -0.106 
#> # ... with 19 more rows
#> #
#> # Edge Data: 48 x 2
#>    from    to
#>   <int> <int>
#> 1    12     2
#> 2    17     2
#> 3    22     3
#> # ... with 45 more rows
```

## References

1.  Rohe, K. & Zeng, M. *Vintage Factor Analysis with Varimax Performs
    Statistical Inference*. 2020+.
