# Compute localization statistics across various regularization parameters.

Compute localization statistics across various regularization
parameters.

## Usage

``` r
localization_statistics(
  graph,
  max_rank,
  ...,
  tau_min = 10^-2,
  tau_max = 10^4,
  num_tau = 50
)
```

## Arguments

- graph:

  An igraph object or a sparse matrix.

- max_rank:

  The maximum number of singular vectors to compute.

- ...:

  Additional arguments passed to as_csparse.

- tau_min:

  The minimum value for the regularization parameter tau.

- tau_max:

  The maximum value for the regularization parameter tau.

- num_tau:

  The number of values of tau to test.

## Value

A list of class `localization_stats` containing the results.

## Examples

``` r
if (FALSE) { # \dontrun{
library(igraphdata)
library(furrr)

data(karate, package = "igraphdata")

plan(multisession, workers = 2)

# karate is undirected, enron is directed

stats <- localization_statistics(karate, max_rank = 15, num_tau = 200)

plot_cumulative_curves(stats)
plot_ipr_curves(stats)
} # }
```
