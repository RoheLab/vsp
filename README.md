
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

**Disclaimer:** This package is a work in progress. Some basic
functionality is available, but the API is subject to change. Code to
reproduce the results in *Vintage Factor Analysis with Varimax Performs
Statistical Inference* is [available
here](https://github.com/RoheLab/vsp-paper).

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
#> Lambda[k]:   0.1834
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
enron_fa <- vsp(enron, k = 30, center = FALSE)
enron_fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   184
#> Factors (k): 30
#> Lambda[k]:   0.2077
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     FALSE 
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
#> Lambda[k]:   0.3911
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
singular value decomposition of your network. In particular, you can
approximately reconstruct the matrix representation of your network as
follows:

``` r
fa2 <- vsp(M, k = 5, center = FALSE, normalize = FALSE)

fa2$U %*% diag(fa2$d) %*% t(fa2$V)
#>              [,1]         [,2]        [,3]          [,4]        [,5]
#>  [1,]  1.02129791  0.005730746  0.08407334  0.2386801454 -0.18164095
#>  [2,]  0.87352354  0.787998850  0.11955064  0.0003396054  1.11859233
#>  [3,]  0.16305641  1.287957243 -0.10939721  0.0089753571  0.92366055
#>  [4,]  1.05892898  0.027531099  0.01564212 -0.1176486462  0.06303591
#>  [5,] -0.08053916  0.870627214 -0.06538458  0.0246418710  0.82446198
#>  [6,]  0.85929679  0.914452122  1.21958818  0.1965919842  0.14493281
#>  [7,]  0.95411270 -0.072755110  0.81511023 -0.1466905793 -0.11698156
#>  [8,]  0.13232884  0.084309333  1.02010403 -0.0381168073 -0.16043488
#>  [9,]  0.13175920  0.288882321 -0.01739103 -0.1228768970  0.82762080
#> [10,]  1.05501309  0.030407843  0.90627482  0.1097832926  0.14878152
#> [11,] -0.05877267  0.557889198  0.01474443  0.1137063455  0.31273802
#> [12,]  1.05818940  1.121388757  0.89870148  0.7771335934 -0.08892006
#>              [,6]        [,7]        [,8]        [,9]       [,10]       [,11]
#>  [1,]  0.11869412  0.59845749  0.18319648  0.08472791  0.04103558 -0.25491138
#>  [2,]  1.03568835  0.09055829  0.22662895  0.10270089  0.10323227 -0.11340206
#>  [3,] -0.13875319 -0.07104222  0.70702813  0.74507896 -0.11813219  0.14078499
#>  [4,] -0.04934350  0.14556541  0.69314703  0.08637050 -0.26687336  0.17079206
#>  [5,]  0.29204114 -0.09537105  0.17110617  0.34542243  0.03307196 -0.04381293
#>  [6,] -0.04507758  0.12265733  0.23309461  0.81686865  0.98870218  0.95937862
#>  [7,]  0.11529610 -0.10365282  0.29125046  0.09293040  0.42962066  0.81924471
#>  [8,]  0.05317757 -0.26213129 -0.21276802  0.20214916  0.81723488  0.91154273
#>  [9,]  0.90601835 -0.19470603 -0.11806679 -0.21352897  0.02618734 -0.11605363
#> [10,]  0.89779235  0.21008787 -0.29665885 -0.08575669  0.76371872  0.35285177
#> [11,] -0.02385569  0.06450342  0.08630377  0.33102626  0.09829177 -0.02872687
#> [12,] -0.01275400  1.01581374 -0.10867329  0.98270093  1.04539206  0.01178460
#>       [,12]
#>  [1,]     0
#>  [2,]     0
#>  [3,]     0
#>  [4,]     0
#>  [5,]     0
#>  [6,]     0
#>  [7,]     0
#>  [8,]     0
#>  [9,]     0
#> [10,]     0
#> [11,]     0
#> [12,]     0
```

Note that you don’t want to do this for large networks since the
reconstruction will be dense. Similarly, the varimax-rotated network
approximate reconstruction is

``` r
fa$Z %*% fa$B %*% t(fa$Y)
#> 12 x 12 Matrix of class "dgeMatrix"
#>              [,1]         [,2]         [,3]         [,4]         [,5]
#>  [1,]  0.15180782 -0.020130811 -0.101865308  0.046268450  0.014648232
#>  [2,]  0.03779036 -0.115853352  0.066422796  0.031603448 -0.096910316
#>  [3,] -0.04451128 -0.005495416 -0.039922824 -0.019754667  0.055650187
#>  [4,]  0.10162991 -0.091212377 -0.084077069 -0.052573938 -0.025464430
#>  [5,] -0.02982160 -0.025349668  0.026376191 -0.008764749  0.078982315
#>  [6,] -0.06922781  0.105290170 -0.012574623 -0.037321686  0.097378190
#>  [7,]  0.04167743  0.001381582 -0.004712423 -0.130823798  0.143430150
#>  [8,] -0.07893444  0.089221990  0.039798262 -0.076985135  0.075810368
#>  [9,]  0.01907526 -0.164601470  0.129659097  0.047458710 -0.203905971
#> [10,] -0.01401785 -0.021269741  0.074372204  0.070847224 -0.219374009
#> [11,] -0.04490966  0.057114600 -0.022674684  0.013244692  0.091888949
#> [12,] -0.03284071  0.128617648 -0.064324980  0.096649022 -0.006608153
#>               [,6]         [,7]        [,8]          [,9]        [,10]
#>  [1,]  0.007871403  0.309940918 -0.06874060 -0.1187521691 -0.167045033
#>  [2,]  0.135476643 -0.000633111 -0.05242208 -0.0269927268  0.125628892
#>  [3,]  0.077419379 -0.124417129  0.15631233  0.0503913418  0.019493250
#>  [4,] -0.060510061 -0.001591583  0.37828754  0.0132142347 -0.111546338
#>  [5,]  0.220375610 -0.052189387 -0.16837446 -0.0778895294  0.058243016
#>  [6,] -0.070953723 -0.053680113 -0.05230020  0.0079142936 -0.059633201
#>  [7,]  0.017552318 -0.018542704 -0.06512147 -0.1657948415 -0.166077491
#>  [8,] -0.108727028 -0.120658664 -0.05299276 -0.0002580749 -0.040389133
#>  [9,]  0.111089979 -0.079269193  0.01405777  0.0420343056  0.240743159
#> [10,] -0.193768060 -0.011855176  0.10946625  0.1574389890  0.140268448
#> [11,]  0.088138996  0.005344177 -0.13183776 -0.0249191504 -0.006377012
#> [12,] -0.134693619  0.144784280 -0.06370767  0.0878531677 -0.032629023
#>             [,11]        [,12]
#>  [1,] -0.07049426  0.045661345
#>  [2,] -0.07718766 -0.032359547
#>  [3,] -0.10257684 -0.013369729
#>  [4,] -0.04725473  0.023632487
#>  [5,] -0.03960814 -0.003138480
#>  [6,]  0.10957731  0.022062234
#>  [7,]  0.24921414  0.081159957
#>  [8,]  0.22152614  0.026519763
#>  [9,] -0.09431617 -0.070487921
#> [10,] -0.02421753 -0.060778077
#> [11,] -0.03864076  0.003539651
#> [12,] -0.09500456 -0.014723277
```

If you want to directly manipulate the estimates, you can access them as
we have above with `fa$B` and so on and so forth. Note that these are
`matrix` and `Matrix` objects, and you may need to perform type
coercions. We also provide some utility functions to extract estimates
into `tibbles`:

``` r
get_varimax_z(fa)
#> # A tibble: 12 x 5
#>         z1      z2      z3      z4       z5
#>      <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#>  1  0.0389 -0.116   1.14    0.0890 -0.127  
#>  2 -0.140   0.565   0.159  -0.211  -0.00770
#>  3 -0.420  -0.0423 -0.520   0.441  -0.445  
#>  4  0.0216  0.0298  0.0702  1.20    0.0415 
#>  5 -0.140   0.295  -0.0994 -0.429  -0.611  
#>  6  0.211  -0.370  -0.292  -0.142  -0.0400 
#>  7  0.925   0.0586  0.172   0.0363 -0.163  
#>  8  0.573  -0.232  -0.434  -0.201   0.216  
#>  9 -0.224   0.799  -0.0929 -0.192   0.323  
#> 10 -0.135   0.0423 -0.126   0.0174  0.932  
#> 11 -0.208  -0.145  -0.0645 -0.323  -0.439  
#> 12 -0.435  -0.618   0.201  -0.213   0.146
```

``` r
get_varimax_y(fa)
#> # A tibble: 12 x 5
#>           y1      y2      y3      y4      y5
#>        <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -0.147     0.157   0.580   0.163   0.108 
#>  2  0.440    -0.471  -0.226  -0.277  -0.152 
#>  3 -0.445     0.0542 -0.192  -0.184   0.307 
#>  4 -0.180    -0.206   0.117  -0.237  -0.443 
#>  5  0.815     0.250  -0.203  -0.0474  0.0292
#>  6  0.107     1.04   -0.130  -0.145  -0.256 
#>  7 -0.000799 -0.272   0.958  -0.360  -0.303 
#>  8  0.0133   -0.0869 -0.0659  1.31   -0.0954
#>  9 -0.158    -0.474  -0.361   0.147  -0.367 
#> 10 -0.635     0.0999 -0.443  -0.202  -0.204 
#> 11  0.0221   -0.170  -0.0676 -0.0967  1.11  
#> 12  0.189     0.0462  0.128   0.0360  0.175
```

To visualize a screeplot of the singular value, use:

``` r
screeplot(fa)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

At the moment, we also enjoy using pairs plots of the factors as a
diagnostic measure:

``` r
plot_varimax_z_pairs(fa, 1:3)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
plot_varimax_y_pairs(fa, 1:3)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

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
#> # A tbl_graph: 25 nodes and 60 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>        z1      z2      z3      z4      z5
#>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1  1.19    0.0178 -0.0838 -0.0405 -0.0353
#> 2  0.383   0.833  -0.185   0.179  -0.0502
#> 3  0.575  -0.0186 -0.390  -0.508  -0.301 
#> 4 -0.212  -0.0966 -0.0981  0.878  -0.324 
#> 5 -0.0993 -0.593  -0.349   0.737  -0.282 
#> 6  0.0480 -0.0949  0.0796 -0.176   0.0266
#> # ... with 19 more rows
#> #
#> # Edge Data: 60 x 2
#>    from    to
#>   <int> <int>
#> 1    15     1
#> 2    19     2
#> 3    21     2
#> # ... with 57 more rows
```

## References

1.  Rohe, K. & Zeng, M. *Vintage Factor Analysis with Varimax Performs
    Statistical Inference*. 2020+.
