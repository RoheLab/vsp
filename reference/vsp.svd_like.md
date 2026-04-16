# Perform varimax rotation on a low rank matrix factorization

Perform varimax rotation on a low rank matrix factorization

## Usage

``` r
# S3 method for class 'svd_like'
vsp(
  x,
  rank,
  ...,
  centerer = NULL,
  scaler = NULL,
  recenter = FALSE,
  renormalize = FALSE,
  kaiser_normalize_u = FALSE,
  kaiser_normalize_v = FALSE,
  rownames = NULL,
  colnames = NULL,
  match_columns = TRUE
)
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

- centerer:

  TODO

- scaler:

  TODO

- recenter:

  Should the varimax factors be re-centered around the original factor
  means? Only used when `center = TRUE`, defaults to `FALSE`.

- renormalize:

  Should the regularized graph laplacian be used instead of the raw
  adjacency matrix? Defaults to `TRUE`. If `center = TRUE`, `A` will
  first be centered and then normalized.

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

## Examples

``` r
library(LRMF3)
library(RSpectra)

s <- svds(ml100k, k = 2)
mf <- as_svd_like(s)
fa <- vsp(mf, rank = 2)
```
