# Plot pairs of inverse participation ratios for singular vectors

When IPR for a given singular vector is O(1) rather than O(1 / sqrt(n)),
this can indicate that the singular vector is localizing on a small
subset of nodes. Oftentimes this localization indicates overfitting. If
you see IPR values that are not close to zero (where "close to zero" is
something you sort of have to pick up over time), then you need to some
further investigation to see if you have localization and that
localization corresponds to overfitting. Note, however, that not all
localization is overfitting.

## Usage

``` r
plot_ipr_pairs(fa)
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
