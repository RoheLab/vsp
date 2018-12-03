
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vsp

The goal of vsp is to …

## Installation

`vsp` is experimental and nascent. If you are a collaborator on the
project, you can install the development version with:

``` r
install.packages("devtools")
devtools::install_github("alexpghayes/vsp", auth_token = NEED_TO_DO_THIS)
```

You’ll need to set up a Github auth token. See the documentation in
`?remotes::install_github()` for details on how to do this.

## Example

Fitting a `vsp` factor analysis:

``` r
library(vsp)
library(Matrix)

# make a random sparse graph
M <- rsparsematrix(12, 12, nnz = 40)
M2 <- sign(M)^2

# vintage sparse pca
fa <- vsp(M2, k = 7)
fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Factors:  7 
#> Lambda_2: 0.3385 
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
#> U: 12 x 7 [matrix] 
#> d: 7      [numeric] 
#> V: 12 x 7 [matrix] 
#> Z: 12 x 7 [dgeMatrix] 
#> B: 7 x 7 [dgeMatrix] 
#> Y: 12 x 7 [dgeMatrix]
```

The screeplot:

``` r
screeplot(fa)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
project_pca(fa)
#> # A tibble: 12 x 7
#>         PC1     PC2      PC3      PC4      PC5      PC6      PC7
#>       <dbl>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 -0.0867   0.0938  0.00637 -0.0120  -0.110   -0.0996  -0.0463 
#>  2 -0.0367  -0.101   0.0421   0.0915   0.0413   0.0814  -0.0449 
#>  3  0.104   -0.0141 -0.183    0.0710  -0.00900  0.0113   0.0744 
#>  4  0.151   -0.0878 -0.0893  -0.00887  0.0199  -0.0791  -0.0508 
#>  5  0.129    0.0590  0.167   -0.0351   0.0555   0.0216   0.00758
#>  6 -0.0510   0.0236  0.00512  0.0662   0.0999  -0.0345  -0.0764 
#>  7 -0.0639   0.154  -0.130   -0.109    0.0225   0.0962  -0.0379 
#>  8  0.00275 -0.187   0.0220  -0.103   -0.00253  0.00852  0.00781
#>  9 -0.0556   0.105   0.0375   0.0717   0.0910  -0.0469   0.0866 
#> 10 -0.160   -0.0795  0.0138  -0.115    0.0138  -0.0273   0.0595 
#> 11  0.159    0.0768  0.0764  -0.0376  -0.0950   0.0238   0.0127 
#> 12 -0.0917  -0.0427  0.0320   0.120   -0.127    0.0446   0.00774
```

``` r
project_varimax(fa)
#> # A tibble: 12 x 7
#>    factor1   factor2 factor3  factor4 factor5 factor6 factor7
#>      <dbl>     <dbl>   <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#>  1  0.0524 -0.0603    0.229   0.0272  -0.0892 -1.19   -0.0570
#>  2  0.278  -0.0674    0.253   0.413   -0.427   0.610  -0.242 
#>  3  0.103  -0.000637 -1.03   -0.0371   0.0224  0.168   0.101 
#>  4  0.253  -0.351    -0.290   0.202    0.707  -0.159  -0.505 
#>  5  0.214  -0.135     0.539  -0.581    0.295   0.400   0.249 
#>  6  0.431  -0.0153    0.372   0.691    0.244  -0.0323  0.129 
#>  7  0.0225  1.27     -0.0126  0.0186   0.0627  0.0192 -0.0852
#>  8 -0.590  -0.203     0.0869 -0.0291   0.131   0.282  -0.495 
#>  9 -0.0619 -0.190    -0.102   0.0949   0.0720  0.0363  1.07  
#> 10 -1.01    0.00489   0.105   0.121    0.0180 -0.0607  0.142 
#> 11  0.205  -0.0179    0.0350 -0.755   -0.0334 -0.0748 -0.117 
#> 12  0.105  -0.219    -0.100   0.00405 -1.01   -0.118  -0.135
```

``` r
pairs(project_pca(fa))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
pairs(project_varimax(fa))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

think about how else to visualize these matrices. distributions of
loadings? am i actually plotting the loadings here or did i just do some
nonsense?

``` r
plot_simulation_test(M2, k = 3)
#> Loading required package: igraph
#> Warning: package 'igraph' was built under R version 3.5.1
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
#> `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

## Tidygraph integration

``` r
library(tidygraph)
#> Warning: package 'tidygraph' was built under R version 3.5.1
#> 
#> Attaching package: 'tidygraph'
#> The following object is masked from 'package:igraph':
#> 
#>     groups
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(ggraph)
#> Warning: package 'ggraph' was built under R version 3.5.1
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 3.5.1

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
  bind_varimax_factors(fa2)

sbm_fa2
#> # A tbl_graph: 25 nodes and 67 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>   factor1 factor2 factor3 factor4  factor5
#>     <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#> 1  0.0643  -1.11   0.0452  0.0941  0.00242
#> 2 -0.0981   0.183 -0.118  -0.421   0.596  
#> 3  0.0200  -0.602  1.15   -0.0905 -0.0241 
#> 4 -0.147    0.200  1.22    0.198  -0.179  
#> 5 -0.711   -0.620 -0.566   0.101  -0.229  
#> 6  1.08    -0.200 -0.0697  0.201  -0.249  
#> # ... with 19 more rows
#> #
#> # Edge Data: 67 x 2
#>    from    to
#>   <int> <int>
#> 1    11     1
#> 2    19     1
#> 3    23     1
#> # ... with 64 more rows
```

If you haven’t already created a `vsp` object, that gets taken care of
for you as well:

``` r
sbm_fa3 <- sbm %>% 
  bind_varimax_factors()

sbm_fa3
#> # A tbl_graph: 25 nodes and 67 edges
#> #
#> # A directed simple graph with 1 component
#> #
#> # Node Data: 25 x 5 (active)
#>   factor1 factor2 factor3 factor4  factor5
#>     <dbl>   <dbl>   <dbl>   <dbl>    <dbl>
#> 1  0.0643  -1.11   0.0452  0.0941  0.00242
#> 2 -0.0981   0.183 -0.118  -0.421   0.596  
#> 3  0.0200  -0.602  1.15   -0.0905 -0.0241 
#> 4 -0.147    0.200  1.22    0.198  -0.179  
#> 5 -0.711   -0.620 -0.566   0.101  -0.229  
#> 6  1.08    -0.200 -0.0697  0.201  -0.249  
#> # ... with 19 more rows
#> #
#> # Edge Data: 67 x 2
#>    from    to
#>   <int> <int>
#> 1    11     1
#> 2    19     1
#> 3    23     1
#> # ... with 64 more rows
```

``` r
ggraph(sbm_fa2) + 
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
    geom_node_point(aes(size = factor1)) + 
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
#> Using `nicely` as default layout
#> Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
#> font family not found in Windows font database

#> Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
#> font family not found in Windows font database

#> Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
#> font family not found in Windows font database

#> Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
#> font family not found in Windows font database
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

## Messy scratch

``` r
library(tidyverse)

fa$B %>%
  as.matrix() %>%
  as_tibble() %>%
  mutate(row = row_number()) %>%
  gather(k, v, -row) %>%
  ggplot(aes(row, k, fill = v)) +
  geom_tile() +
  scale_fill_viridis_c()

image(fa$B)
image(fa$Z)
```
