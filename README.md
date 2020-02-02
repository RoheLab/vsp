
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
#> Lambda[k]:   0.2553
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     FALSE 
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

Here we demonstrate `vsp` usage on an `igraph` object, using the `enron`
network from `igraphdata` package to demonstrate this functionality.
First we peak at the graph:

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

Now we estimate:

``` r
enron_fa <- vsp(enron, k = 30)
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
30th singular value of `A` is 0.2039. This is of interest, as we expect
`lambda[k] ~ 0` to indicate that we have we have overestimated the rank
of `A`, in say, blockmodels.

The pre-processing section indicates that the matrix representation of
the graph was *not* row- and column-centered before estimation (`center
= FALSE`), and that we used the graph Laplacian rather than the
adjacency matrix (`normalize = TRUE`).

By default, `normalize = TRUE` not only uses the graph Laplacian, but
also regularizes the graph Laplacian using mean in-degree and
out-degree. See \[1\] for details on how we form the regularized graph
Laplacian.

In general, you do not need to tune `tau_row` and `tau_col`. To turn off
regularization (not recommended\!), see `tau_row = 0` and `tau_col = 0`.
This often leads to eigenvector localization and thus overfitting.

``` r
vsp(M, k = 5, tau_row = 0, tau_col = 0)
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   12
#> Factors (k): 5
#> Lambda[k]:   0.5942
#> 
#> Pre-Processing Options
#> 
#>  - Centering:     FALSE 
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
#> Warning: Implicit centering has not yet been implemented.
#> Vintage Sparse PCA Factor Analysis
#> 
#> Nodes (n):   12
#> Factors (k): 5
#> Lambda[k]:   0.2137
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

You can approximately reconstruct the matrix representation of your
network as follows:

``` r
fa2 <- vsp(M, k = 5, normalize = FALSE)

fa2$U %*% diag(fa2$d) %*% t(fa2$V)
#>              [,1]         [,2]        [,3]        [,4]        [,5]         [,6]
#>  [1,] -0.03454720  0.904850238  0.91807119  0.04725985  0.27751472 -0.051516842
#>  [2,]  1.09186104 -0.109809383  0.17532352  0.81318256 -0.04027532  0.002691287
#>  [3,]  0.01284443  1.150375326  0.99732067  0.93566251  0.50257598 -0.223903702
#>  [4,]  0.72828808  0.005641928 -0.09170597  0.05387074 -0.17886598  0.459290681
#>  [5,]  0.05076201  0.045359673  0.03566514  1.00842100  0.15427288  0.227809888
#>  [6,]  0.01931569  0.848866125  0.08964146 -0.10558345  0.03121677  0.864468528
#>  [7,]  0.93970781 -0.048293273 -0.13839463  1.13519484  0.03204876 -0.074458413
#>  [8,] -0.13569054  0.880790750  0.01136774  0.12625529  0.06932065  1.077715968
#>  [9,]  0.35203063  0.435615759 -0.26107677 -0.19102621 -0.14557514  0.829194189
#> [10,]  0.90309085  0.836174139  0.26018474  0.14599522 -0.01729300  0.552773298
#> [11,]  1.04316587  0.060323689  0.95611585  0.03791287 -0.06981024  0.072269698
#> [12,]  1.04780000  1.068776134  0.90773959  0.95681906  0.28704021 -0.048622688
#>               [,7]        [,8]        [,9]       [,10]       [,11]       [,12]
#>  [1,]  0.080114059 -0.19940055  0.14088165  0.08517419  0.05280149 -0.04634909
#>  [2,]  0.359344013  0.27585700  0.18801350 -0.08238852  0.48932144  0.09856586
#>  [3,]  0.084462644  0.03204876  0.12417206 -0.06233072 -0.11008556  0.22359354
#>  [4,]  0.166459952  0.08309057  0.30308253  0.41885161  0.31239351  0.05824120
#>  [5,]  0.658751039  0.17376761 -0.40557078 -0.01291587  0.21190943  0.88071930
#>  [6,]  0.010421667 -0.09693510  0.36164075  0.94458588 -0.10323886  0.35541547
#>  [7,]  0.003908568  0.45470528  0.31522052 -0.06045117  0.10599796  0.09384721
#>  [8,]  0.253806323 -0.07992039  0.16910772  1.05482928 -0.08150550  0.76091137
#>  [9,] -0.130608356  0.01930651  0.49951380  0.90588521 -0.05268815  0.13095781
#> [10,] -0.262988938  0.09709781  0.85481109  0.78546101  0.03859057 -0.20537124
#> [11,]  0.887336682 -0.16985904 -0.04815891 -0.11645343  1.03477127  0.03183806
#> [12,] -0.060132393  0.19881619  0.68995276  0.19372714  0.15395217 -0.16204962
```

In the example above, this will *not* approximate `M`, but rather the
regularized graph Laplacian formed from `M`. Similarly, the
varimax-rotated network approximate reconstruction is

``` r
fa$Z %*% fa$B %*% t(fa$Y)
#> 12 x 12 Matrix of class "dgeMatrix"
#>              [,1]         [,2]        [,3]         [,4]        [,5]
#>  [1,] -0.14317125  0.163089692  0.22014195 -0.218483812  0.07050711
#>  [2,]  0.02749321  0.083047972  0.02834693  0.139120900  0.09237798
#>  [3,] -0.01675417  0.153347796  0.29788454 -0.088591274  0.08218680
#>  [4,]  0.07884215  0.015504585 -0.14250678  0.086821194 -0.02998231
#>  [5,] -0.04599909  0.065204618  0.05025482  0.191693867  0.08312272
#>  [6,]  0.04364964  0.040645635 -0.10903066 -0.044599988 -0.08356361
#>  [7,]  0.29695446 -0.006126842  0.03950231  0.294049769  0.01529530
#>  [8,]  0.02679886  0.038661906 -0.10912358  0.026887899 -0.06871035
#>  [9,]  0.10350063  0.002526301 -0.19662474  0.009426914 -0.10696654
#> [10,]  0.09908310  0.066360984 -0.05080113 -0.021252774 -0.04425062
#> [11,] -0.26719268  0.181982294  0.07657347 -0.071062335  0.16482902
#> [12,]  0.05286608  0.135493593  0.15428657 -0.025636312  0.05412715
#>               [,6]        [,7]         [,8]        [,9]         [,10]
#>  [1,] -0.005516588 -0.03254213 -0.266344803  0.02731352  0.0437769711
#>  [2,] -0.027071043 -0.09846210  0.123265370  0.04802208 -0.0005021226
#>  [3,] -0.037806541  0.10713018 -0.214244615  0.02014367 -0.0062228690
#>  [4,]  0.131436486 -0.09980209  0.154328198  0.09840441  0.1290433172
#>  [5,]  0.035442957  0.24365242  0.025359153 -0.10282009  0.0143495613
#>  [6,]  0.237784083  0.01320153 -0.002638471  0.09975768  0.2240098389
#>  [7,] -0.011178335  0.07795646  0.240200008  0.08649697 -0.0294480937
#>  [8,]  0.251080961  0.13236524  0.006193737  0.04268534  0.2204882442
#>  [9,]  0.248962093 -0.06418584  0.107047197  0.13708012  0.2313587959
#> [10,]  0.161607923 -0.10379222  0.045712651  0.15560390  0.1726621281
#> [11,] -0.044137966 -0.20150541 -0.083828929 -0.01521100  0.0241943391
#> [12,]  0.024245290 -0.05265889 -0.053875064  0.10744667  0.0596853839
#>              [,11]        [,12]
#>  [1,] -0.012996545 -0.107576141
#>  [2,] -0.101513279 -0.007332501
#>  [3,]  0.133717755 -0.058586535
#>  [4,] -0.090640193  0.045003016
#>  [5,]  0.015632598  0.337424161
#>  [6,] -0.008125137  0.111200700
#>  [7,]  0.158477984  0.014930657
#>  [8,]  0.014900462  0.258122271
#>  [9,] -0.045046543  0.077078416
#> [10,] -0.022927344 -0.046701797
#> [11,] -0.302280893 -0.014111484
#> [12,]  0.028213021 -0.104317304
```

In general, you won’t want to explicit reconstruct the network matrix
for large networks since the reconstruction will be dense.

If you want to directly manipulate the estimates, you can access them as
we have above with `fa$B` and so on and so forth. Note that these are
`matrix` and `Matrix` objects, and you may need to perform type
coercions. We also provide some utility functions to extract estimates
into `tibbles`:

``` r
get_varimax_z(fa)
#> # A tibble: 12 x 5
#>         z1      z2       z3      z4      z5
#>      <dbl>   <dbl>    <dbl>   <dbl>   <dbl>
#>  1 -0.336   0.0679  0.727   -0.135   0.204 
#>  2  0.550  -0.0237 -0.0966   0.0441  0.520 
#>  3  0.0531 -0.0645  0.905    0.137  -0.109 
#>  4  0.287   0.393  -0.293   -0.0970  0.248 
#>  5  0.121  -0.0111  0.0376   1.14    0.113 
#>  6 -0.157   0.662   0.0739   0.0524 -0.0768
#>  7  1.06   -0.0433  0.0354   0.118  -0.233 
#>  8 -0.183   0.638   0.0419   0.525  -0.131 
#>  9  0.0382  0.708  -0.194   -0.153  -0.0102
#> 10  0.214   0.519   0.125   -0.337   0.0926
#> 11 -0.142  -0.0248 -0.00461  0.0758  1.10  
#> 12  0.335   0.154   0.519   -0.202   0.157
```

``` r
get_varimax_y(fa)
#> # A tibble: 12 x 5
#>         y1      y2      y3      y4      y5
#>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1  0.828   0.0804 -0.0193 -0.0538  0.183 
#>  2 -0.162   0.329   0.657  -0.143   0.164 
#>  3  0.115  -0.206   0.905   0.117  -0.0517
#>  4  0.120  -0.0816  0.0393  0.0566  0.919 
#>  5 -0.272  -0.122   0.443  -0.122   0.340 
#>  6  0.0831  0.768  -0.114   0.128  -0.0916
#>  7  0.131  -0.0354  0.0938  0.998   0.0102
#>  8  0.198  -0.0170 -0.362  -0.328   0.709 
#>  9  0.359   0.377   0.173  -0.381  -0.0162
#> 10  0.0234  0.774   0.0374 -0.0218 -0.0808
#> 11  0.709  -0.149   0.126   0.546  -0.283 
#> 12 -0.435   0.374  -0.131   0.771   0.465
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
#> # A tbl_graph: 25 nodes and 59 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>          z1        z2        z3        z4        z5
#>       <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#> 1 -2.80e- 1 -7.86e- 2  3.57e- 1 -1.56e- 1  8.95e- 1
#> 2 -4.39e- 1  6.35e- 2  2.51e- 1  7.76e- 2 -1.23e- 1
#> 3  2.43e- 1 -6.82e- 2 -1.72e- 1  1.12e+ 0  1.37e- 1
#> 4  2.11e- 1 -1.04e- 1  1.18e+ 0 -9.89e- 2  4.81e- 2
#> 5  6.84e- 1 -2.41e- 3 -3.69e- 2  8.89e- 1 -1.26e- 1
#> 6 -2.15e-16  2.86e-16 -6.68e-16 -4.55e-16  1.87e-15
#> # ... with 19 more rows
#> #
#> # Edge Data: 59 x 2
#>    from    to
#>   <int> <int>
#> 1     1     2
#> 2    10     2
#> 3    16     2
#> # ... with 56 more rows
```

## References

1.  Rohe, K. & Zeng, M. *Vintage Factor Analysis with Varimax Performs
    Statistical Inference*. 2020+.
