# Add Z factor loadings to node table of tidygraph

Add Z factor loadings to node table of tidygraph

## Usage

``` r
bind_varimax_z(graph, fa, ...)

bind_varimax_y(graph, fa, ...)

bind_svd_u(graph, fa, ...)

bind_svd_v(graph, fa, ...)
```

## Arguments

- graph:

  A
  [tidygraph::tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
  object.

- fa:

  Optionally, a [vsp](https://rohelab.github.io/vsp/reference/vsp.md)
  object to extract varimax loadings from. If you do not passed a
  [vsp](https://rohelab.github.io/vsp/reference/vsp.md) object, one will
  be created.

- ...:

  Arguments passed on to
  [`vsp`](https://rohelab.github.io/vsp/reference/vsp.md)

  `x`

  :   Either a graph adjacency matrix, igraph::igraph or
      [tidygraph::tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html).
      If `x` is a [matrix](https://rdrr.io/r/base/matrix.html) or
      [Matrix::Matrix](https://rdrr.io/pkg/Matrix/man/Matrix.html) then
      `x[i, j]` should correspond to the edge going from node `i` to
      node `j`.

  `rank`

  :   The number of factors to calculate.

## Value

The same `graph` object with columns `factor1`, ..., `factor{rank}` in
the table of node information.

## Functions

- `bind_varimax_y()`: Add Y factor loadings to node table of tidygraph

- `bind_svd_u()`: Add left singular vectors to node table of tidygraph

- `bind_svd_v()`: Add right singular vectors to node table of tidygraph
