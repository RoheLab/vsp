# Plot the mixing matrix B

Plot the mixing matrix B

## Usage

``` r
plot_mixing_matrix(fa)
```

## Arguments

- fa:

  A [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
  object.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row for each node, and one column containing each of the
requested factor or singular vector, plus an additional `id` column.
