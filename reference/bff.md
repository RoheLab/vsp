# Find features most associated with cluster membership

Find features most associated with cluster membership

## Usage

``` r
bff(loadings, features, num_best)
```

## Arguments

- loadings:

  An `n` by `k` matrix of weights that indicates how important that ith
  user is to the jth cluster, i.e., the `Z` or `Y` matrix calculated by
  [`vsp()`](https://rohelab.github.io/vsp/reference/vsp.md).

- features:

  An `n` by `d` matrix of features measured for each node in the
  network.

- num_best:

  An integer indicating how many of the top features for differentiating
  between loadings you want.

## Value

An `n` by `k` matrix whose `[i, j]` entry is the ith "most important"
feature for cluster j.

## Details

See [`vignette("bff")`](https://rohelab.github.io/vsp/articles/bff.md).
