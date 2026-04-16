# Get most important hubs for each Z factor

Get most important hubs for each Z factor

## Usage

``` r
get_z_hubs(fa, hubs_per_factor = 10, factors = 1:fa$rank)

get_y_hubs(fa, hubs_per_factor = 10, factors = 1:fa$rank)
```

## Arguments

- fa:

  A [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
  object.

- hubs_per_factor:

  The number of important nodes to get per latent factor. Defaults to
  `10`.

- factors:

  The specific columns to index into. The most reliable option here is
  to index with an integer vector of column indices, but you could also
  use a character vector if columns have been named. By default returns
  all factors/singular vectors.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
where each row corresponds to a single hub, and three columns:

- `id`: Node id of hub node

- `factor`: Which factor that node is a hub for. Nodes can be hubs of
  multiple factors.

- `loading`: The actual value of the hubs factor loading for that
  factor.

## Functions

- `get_y_hubs()`: Get most important hubs for each Y factor

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

get_z_hubs(fa)
#> # A tibble: 300 × 3
#> # Groups:   factor [30]
#>    id     factor loading
#>    <chr>  <chr>    <dbl>
#>  1 row179 z01    13.6   
#>  2 row181 z01     0.159 
#>  3 row110 z01     0.103 
#>  4 row073 z01     0.0611
#>  5 row097 z01     0.0463
#>  6 row075 z01    -0.0416
#>  7 row106 z01    -0.0407
#>  8 row058 z01     0.0405
#>  9 row164 z01     0.0398
#> 10 row006 z01    -0.0346
#> # ℹ 290 more rows
get_y_hubs(fa)
#> # A tibble: 300 × 3
#> # Groups:   factor [30]
#>    id     factor loading
#>    <chr>  <chr>    <dbl>
#>  1 col179 y01    13.6   
#>  2 col110 y01     0.217 
#>  3 col052 y01     0.217 
#>  4 col148 y01     0.198 
#>  5 col099 y01     0.156 
#>  6 col068 y01     0.142 
#>  7 col181 y01     0.113 
#>  8 col114 y01    -0.0971
#>  9 col057 y01     0.0803
#> 10 col054 y01     0.0786
#> # ℹ 290 more rows
```
