# Semi-Parametric Factor Analysis via Vintage Sparse PCA

This code implements TODO.

## Usage

``` r
vsp(x, rank, ...)

# Default S3 method
vsp(x, rank, ...)

# S3 method for class 'matrix'
vsp(
  x,
  rank,
  ...,
  center = FALSE,
  recenter = FALSE,
  degree_normalize = TRUE,
  renormalize = FALSE,
  tau_row = NULL,
  tau_col = NULL,
  kaiser_normalize_u = FALSE,
  kaiser_normalize_v = FALSE,
  rownames = NULL,
  colnames = NULL,
  match_columns = TRUE
)

# S3 method for class 'Matrix'
vsp(
  x,
  rank,
  ...,
  center = FALSE,
  recenter = FALSE,
  degree_normalize = TRUE,
  renormalize = FALSE,
  tau_row = NULL,
  tau_col = NULL,
  kaiser_normalize_u = FALSE,
  kaiser_normalize_v = FALSE,
  rownames = NULL,
  colnames = NULL,
  match_columns = TRUE
)

# S3 method for class 'dgCMatrix'
vsp(
  x,
  rank,
  ...,
  center = FALSE,
  recenter = FALSE,
  degree_normalize = TRUE,
  renormalize = FALSE,
  tau_row = NULL,
  tau_col = NULL,
  kaiser_normalize_u = FALSE,
  kaiser_normalize_v = FALSE,
  rownames = NULL,
  colnames = NULL,
  match_columns = TRUE
)

# S3 method for class 'igraph'
vsp(x, rank, ..., edge_weights = NULL)
```

## Arguments

- x:

  Either a graph adjacency matrix, igraph::igraph or
  [tidygraph::tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).
  If `x` is a [matrix](https://rdrr.io/r/base/matrix.html) or
  [Matrix::Matrix](https://rdrr.io/pkg/Matrix/man/Matrix.html) then
  `x[i, j]` should correspond to the edge going from node `i` to node
  `j`.

- rank:

  The number of factors to calculate.

- ...:

  These dots are for future extensions and must be empty.

- center:

  Should the adjacency matrix be row *and* column centered? Defaults to
  `FALSE`.

- recenter:

  Should the varimax factors be re-centered around the original factor
  means? Only used when `center = TRUE`, defaults to `FALSE`.

- degree_normalize:

  Should the regularized graph laplacian be used instead of the raw
  adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
  first be centered and then normalized.

- renormalize:

  Should the regularized graph laplacian be used instead of the raw
  adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
  first be centered and then normalized.

- tau_row:

  Row regularization term. Default is `NULL`, in which case we use the
  row degree. Ignored when `degree_normalize = FALSE`.

- tau_col:

  Column regularization term. Default is `NULL`, in which case we use
  the column degree. Ignored when `degree_normalize = FALSE`.

- kaiser_normalize_u:

  Whether or not to use Kaiser normalization when rotating the left
  singular vectors `U`. Defaults to `FALSE`.

- kaiser_normalize_v:

  Whether or not to use Kaiser normalization when rotating the right
  singular vectors `V`. Defaults to `FALSE`.

- rownames:

  Character vector of row names of `x`. These row names are propagated
  into the row names of the `U` and `Z`. Defaults to `NULL`.

- colnames:

  Character vector of column names of `x`. These column names are
  propagated into the row names of the `V` and `Y`. Defaults to `NULL`.

- match_columns:

  Should the columns of `Y` be re-ordered such that `Y[, i]` corresponds
  to `Z[, i]` to the extent possible? Defaults to `TRUE`. Typically
  helps with interpretation, and often makes `B` more diagonally
  dominant.

- edge_weights:

  When `x` is an igraph::igraph, an edge attribute to use to form a
  weighted adjacency matrix.

## Value

An object of class `vsp`. TODO: Details

## Details

Sparse SVDs use `RSpectra` for performance.

## Examples

``` r
library(LRMF3)
#> Loading required package: Matrix

vsp(ml100k, rank = 2)
#> Vintage Sparse PCA Factor Analysis
#> 
#> Rows (n):   943
#> Cols (d):   1682
#> Factors (rank): 2
#> Lambda[rank]:   0.2994
#> Components
#> 
#> Z: 943 x 2 [matrix] 
#> B: 2 x 2 [matrix] 
#> Y: 1682 x 2 [matrix] 
#> u: 943 x 2 [matrix] 
#> d: 2      [numeric] 
#> v: 1682 x 2 [matrix] 
#> 
```
