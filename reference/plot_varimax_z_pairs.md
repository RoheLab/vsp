# Create a pairs plot of select Y factors

To avoid overplotting, plots data for a maximum of 1000 nodes. If there
are more than 1000 nodes, samples 1000 nodes randomly proportional to
row norms (i.e. nodes with embeddings larger in magniture are more
likely to be sampled).

## Usage

``` r
plot_varimax_z_pairs(fa, factors = 1:min(5, fa$rank), ...)

plot_varimax_y_pairs(fa, factors = 1:min(5, fa$rank), ...)

plot_svd_u(fa, factors = 1:min(5, fa$rank))

plot_svd_v(fa, factors = 1:min(5, fa$rank))
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

- ...:

  Arguments passed on to
  [`GGally::ggpairs`](https://ggobi.github.io/ggally/reference/ggpairs.html)

  `data`

  :   data set using. Can have both numerical and categorical data.

  `mapping`

  :   aesthetic mapping (besides `x` and `y`). See
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
      `mapping` is numeric, `columns` will be set to the `mapping` value
      and `mapping` will be set to `NULL`.

  `columns`

  :   which columns are used to make plots. Defaults to all columns.

  `title,xlab,ylab`

  :   title, x label, and y label for the graph

  `upper`

  :   see Details

  `lower`

  :   see Details

  `diag`

  :   see Details

  `params`

  :   **\[deprecated\]** see
      [`wrap_fn_with_param_arg`](https://ggobi.github.io/ggally/reference/wrap.html)

  `axisLabels`

  :   either "show" to display axisLabels, "internal" for labels in the
      diagonal plots, or "none" for no axis labels

  `columnLabels`

  :   label names to be displayed. Defaults to names of columns being
      used.

  `labeller`

  :   labeller for facets. See
      [`labellers`](https://ggplot2.tidyverse.org/reference/labellers.html).
      Common values are `"label_value"` (default) and `"label_parsed"`.

  `switch`

  :   switch parameter for facet_grid. See
      `ggplot2::`[`facet_grid`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
      By default, the labels are displayed on the top and right of the
      plot. If `"x"`, the top labels will be displayed to the bottom. If
      `"y"`, the right-hand side labels will be displayed to the left.
      Can also be set to `"both"`

  `showStrips`

  :   boolean to determine if each plot's strips should be displayed.
      `NULL` will default to the top and right side plots only. `TRUE`
      or `FALSE` will turn all strips on or off respectively.

  `legend`

  :   May be the two objects described below or the default `NULL`
      value. The legend position can be moved by using ggplot2's theme
      element `pm + theme(legend.position = "bottom")`

      a single numeric value

      :   provides the location of a plot according to the display
          order. Such as `legend = 3` in a plot matrix with 2 rows and 5
          columns displayed by column will return the plot in position
          `c(1,2)`

      a object from [`grab_legend()`](https://ggobi.github.io/ggally/reference/grab_legend.html)

      :   a predetermined plot legend that will be displayed directly

  `cardinality_threshold`

  :   maximum number of levels allowed in a character / factor column.
      Set this value to NULL to not check factor columns. Defaults to 15

  `progress`

  :   `NULL` (default) for a progress bar in interactive sessions with
      more than 15 plots, `TRUE` for a progress bar, `FALSE` for no
      progress bar, or a function that accepts at least a plot matrix
      and returns a new
      `progress::`[`progress_bar`](http://r-lib.github.io/progress/reference/progress_bar.md).
      See
      [`ggmatrix_progress`](https://ggobi.github.io/ggally/reference/ggmatrix_progress.html).

  `proportions`

  :   Value to change how much area is given for each plot. Either
      `NULL` (default), numeric value matching respective length,
      `grid::`[`unit`](https://rdrr.io/r/grid/unit.html) object with
      matching respective length or `"auto"` for automatic relative
      proportions based on the number of levels for categorical
      variables.

  `legends`

  :   **\[deprecated\]**

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
plot or
[`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html)
plot.

## Functions

- `plot_varimax_y_pairs()`: Create a pairs plot of select Z factors

- `plot_svd_u()`: Create a pairs plot of select left singular vectors

- `plot_svd_v()`: Create a pairs plot of select right singular vectors

## Examples

``` r
data(enron, package = "igraphdata")

fa <- vsp(enron, rank = 3)
#> This graph was created by an old(er) igraph version.
#> ℹ Call `igraph::upgrade_graph()` on it to use with the current igraph version.
#> For now we convert it on the fly...

plot_varimax_z_pairs(fa)

plot_varimax_y_pairs(fa)


plot_svd_u(fa)

plot_svd_v(fa)


screeplot(fa)


plot_mixing_matrix(fa)


plot_ipr_pairs(fa)

```
