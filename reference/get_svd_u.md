# Get left singular vectors in a tibble

Get left singular vectors in a tibble

## Usage

``` r
get_svd_u(fa, factors = 1:fa$rank)

get_svd_v(fa, factors = 1:fa$rank)

get_varimax_z(fa, factors = 1:fa$rank)

get_varimax_y(fa, factors = 1:fa$rank)
```

## Arguments

- fa:

  A [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
  object.

- factors:

  The specific columns to index into. The most reliable option here is
  to index with an integer vector of column indices, but you could also
  use a character vector if columns have been named. By default returns
  all factors/singular vectors.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row for each node, and one column containing each of the
requested factor or singular vector, plus an additional `id` column.

## Functions

- `get_svd_v()`: Get right singular vectors in a tibble

- `get_varimax_z()`: Get varimax Y factors in a tibble

- `get_varimax_y()`: Get varimax Z factors in a tibble

## Examples

``` r
data(enron, package = "igraphdata")

fa <- vsp(enron, rank = 30)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...
fa
#> Vintage Sparse PCA Factor Analysis
#> 
#> Rows (n):   184
#> Cols (d):   184
#> Factors (rank): 30
#> Lambda[rank]:   0.2077
#> Components
#> 
#> Z: 184 x 30 [matrix] 
#> B: 30 x 30 [matrix] 
#> Y: 184 x 30 [matrix] 
#> u: 184 x 30 [matrix] 
#> d: 30      [numeric] 
#> v: 184 x 30 [matrix] 
#> 

get_svd_u(fa)
#> # A tibble: 184 × 31
#>    id           u01     u02      u03     u04      u05      u06      u07      u08
#>    <chr>      <dbl>   <dbl>    <dbl>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 row001 0.0000162 2.80e-4  5.22e-4 4.71e-4  6.45e-4  1.33e-3  3.33e-3 -3.81e-2
#>  2 row002 0.000588  2.23e-3  6.97e-4 1.17e-2  7.25e-3  2.31e-3  7.09e-3  3.41e-3
#>  3 row003 0.0000942 6.95e-4  4.32e-4 4.50e-3  5.87e-3  1.57e-2  2.30e-4  2.53e-3
#>  4 row004 0.000105  6.05e-4 -2.63e-5 3.79e-3  2.12e-3 -1.04e-3 -1.42e-4  4.74e-4
#>  5 row005 0.00252   1.06e-2  1.99e-2 1.61e-2  9.37e-3  8.45e-3  5.80e-2  1.82e-2
#>  6 row006 0.00299   7.59e-3  1.45e-3 8.55e-3  5.97e-3  4.60e-3  5.44e-2  1.29e-2
#>  7 row007 0.00146   1.80e-2  6.87e-3 2.43e-2  8.48e-3  1.53e-4  4.58e-2 -2.76e-2
#>  8 row008 0.00193   3.97e-3 -2.91e-4 2.06e-3  1.08e-3  1.38e-3  1.12e-2  2.21e-3
#>  9 row009 0.00150   4.77e-3 -1.82e-4 3.61e-2 -7.74e-2  1.80e-2  1.06e-4 -1.90e-4
#> 10 row010 0.000329  4.10e-3  7.46e-3 1.05e-2  1.22e-2  2.37e-2  7.10e-2 -7.62e-1
#> # ℹ 174 more rows
#> # ℹ 22 more variables: u09 <dbl>, u10 <dbl>, u11 <dbl>, u12 <dbl>, u13 <dbl>,
#> #   u14 <dbl>, u15 <dbl>, u16 <dbl>, u17 <dbl>, u18 <dbl>, u19 <dbl>,
#> #   u20 <dbl>, u21 <dbl>, u22 <dbl>, u23 <dbl>, u24 <dbl>, u25 <dbl>,
#> #   u26 <dbl>, u27 <dbl>, u28 <dbl>, u29 <dbl>, u30 <dbl>
get_svd_v(fa)
#> # A tibble: 184 × 31
#>    id          v01     v02       v03     v04      v05      v06      v07      v08
#>    <chr>     <dbl>   <dbl>     <dbl>   <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 col001 0.000291 0.00122  0.00107  0.00402  2.65e-3  0.00242  1.49e-2 -5.75e-2
#>  2 col002 0.00209  0.00856  0.00679  0.0269   1.65e-2  0.00562  7.04e-2  1.19e-2
#>  3 col003 0.000888 0.00378  0.00238  0.0246   2.60e-2  0.0585   1.57e-2  1.11e-2
#>  4 col004 0.000317 0.00131  0.000631 0.00867  7.56e-3  0.0122   7.10e-3  4.11e-3
#>  5 col005 0.00564  0.0179   0.0376   0.0275   1.57e-2  0.0109   1.21e-1  2.64e-2
#>  6 col006 0.00222  0.00745  0.00147  0.0105   3.76e-3  0.00511  6.15e-2  1.65e-2
#>  7 col007 0.00383  0.0437   0.0129   0.122    5.38e-2 -0.0283   1.25e-1 -4.77e-2
#>  8 col008 0.00189  0.00150  0.000384 0.00392  9.75e-4  0.00143  1.33e-2  3.26e-3
#>  9 col009 0.000393 0.00153 -0.000196 0.0158  -3.37e-2  0.00766 -4.64e-4  3.77e-4
#> 10 col010 0.000450 0.00818  0.0147   0.00770  1.15e-2  0.0248   4.34e-2 -4.81e-1
#> # ℹ 174 more rows
#> # ℹ 22 more variables: v09 <dbl>, v10 <dbl>, v11 <dbl>, v12 <dbl>, v13 <dbl>,
#> #   v14 <dbl>, v15 <dbl>, v16 <dbl>, v17 <dbl>, v18 <dbl>, v19 <dbl>,
#> #   v20 <dbl>, v21 <dbl>, v22 <dbl>, v23 <dbl>, v24 <dbl>, v25 <dbl>,
#> #   v26 <dbl>, v27 <dbl>, v28 <dbl>, v29 <dbl>, v30 <dbl>

get_varimax_z(fa)
#> # A tibble: 184 × 31
#>    id         z01      z02      z03      z04      z05      z06      z07      z08
#>    <chr>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#>  1 row0…  2.42e-4 -0.00245 -2.99e-2  3.37e-4  9.96e-5 -0.0114  -0.00849  0.502  
#>  2 row0… -2.52e-3  0.00135  6.70e-4 -1.63e-1 -1.47e-2  0.0471   0.190    0.00181
#>  3 row0…  2.98e-4 -0.100    1.17e-4 -3.62e-3 -2.06e-2  0.187   -0.158    0.00303
#>  4 row0… -7.75e-5 -0.0183   1.17e-4  5.42e-2 -5.58e-3  0.00165 -0.0367  -0.00106
#>  5 row0… -2.31e-3  0.00150  2.57e-1 -1.42e-2 -4.38e-2  0.00629  1.18    -0.0179 
#>  6 row0… -3.46e-2 -0.0527  -2.61e-2 -1.26e-2 -1.83e-2  0.0282   0.408   -0.0286 
#>  7 row0… -1.08e-3 -0.327   -6.01e-1 -6.98e-2 -9.85e-2 -0.0709   0.509    0.0511 
#>  8 row0…  1.58e-2 -0.0518  -1.34e-2 -1.03e-2 -4.12e-3 -0.0139   0.225   -0.0244 
#>  9 row0…  2.22e-3  0.0752   3.30e-2 -6.50e-4 -5.00e-1 -0.0278  -0.0740  -0.00556
#> 10 row0…  7.13e-4 -0.0119   1.95e-2 -5.06e-3 -7.08e-3  0.00341 -0.00369 13.4    
#> # ℹ 174 more rows
#> # ℹ 22 more variables: z09 <dbl>, z10 <dbl>, z11 <dbl>, z12 <dbl>, z13 <dbl>,
#> #   z14 <dbl>, z15 <dbl>, z16 <dbl>, z17 <dbl>, z18 <dbl>, z19 <dbl>,
#> #   z20 <dbl>, z21 <dbl>, z22 <dbl>, z23 <dbl>, z24 <dbl>, z25 <dbl>,
#> #   z26 <dbl>, z27 <dbl>, z28 <dbl>, z29 <dbl>, z30 <dbl>
get_varimax_y(fa)
#> # A tibble: 184 × 31
#>    id          y01      y02      y03      y04      y05      y06     y07      y08
#>    <chr>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <dbl>
#>  1 col001 -0.00455 -1.72e-2  8.06e-3 -3.08e-3 -0.0100   0.0155   0.317   1.16   
#>  2 col002 -0.0282  -1.30e-1  1.46e-1  6.56e-2 -0.0243  -0.0199   2.15   -0.00828
#>  3 col003 -0.0274  -4.84e-1 -4.23e-2 -8.76e-2 -0.149    0.788   -0.319   0.00535
#>  4 col004 -0.0119  -1.87e-1 -1.93e-2 -6.55e-2 -0.0527   0.146   -0.227  -0.0128 
#>  5 col005 -0.0168   5.16e-2  1.33e-1  1.29e-1 -0.0170  -0.0563   2.32   -0.0376 
#>  6 col006 -0.0663  -1.07e-2  3.98e-2  2.43e-2 -0.0454   0.00226  1.12   -0.0182 
#>  7 col007 -0.0124   5.94e-1  1.90e-1  3.10e-2  0.00379 -0.0280   0.807   0.0115 
#>  8 col008  0.0142   5.29e-2 -1.00e-2 -8.83e-3 -0.0823  -0.0227  -0.0102 -0.00142
#>  9 col009 -0.00243  1.73e-3  5.17e-4  1.16e-3 -0.210   -0.00822 -0.0244 -0.00329
#> 10 col010  0.00404  5.07e-4 -1.94e-1 -9.31e-4  0.0119  -0.278   -0.181   5.51   
#> # ℹ 174 more rows
#> # ℹ 22 more variables: y09 <dbl>, y10 <dbl>, y11 <dbl>, y12 <dbl>, y13 <dbl>,
#> #   y14 <dbl>, y15 <dbl>, y16 <dbl>, y17 <dbl>, y18 <dbl>, y19 <dbl>,
#> #   y20 <dbl>, y21 <dbl>, y22 <dbl>, y23 <dbl>, y24 <dbl>, y25 <dbl>,
#> #   y26 <dbl>, y27 <dbl>, y28 <dbl>, y29 <dbl>, y30 <dbl>
```
