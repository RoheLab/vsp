# Safe L2 row normalization

Helper function for Kaiser normalization to handle rows with zero (or
numerically zero) norm, which results in a divide by zero error in the
[`stats::varimax()`](https://rdrr.io/r/stats/varimax.html)
implementation.

## Usage

``` r
safe_row_l2_normalize(x, eps = 1e-10)
```

## Arguments

- x:

  A matrix to row normalize.

- eps:

  Tolerance to use when assessing if squared L2 row norm is numerically
  larger or smaller than zero.

## Value

The row-rescaled matrix
