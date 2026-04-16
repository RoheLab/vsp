# Package index

## Estimation and estimation helpers

- [`screeplot(`*`<vsp_fa>`*`)`](https://rohelab.github.io/vsp/reference/screeplot.vsp_fa.md)
  : Create a screeplot from a factor analysis object
- [`vsp()`](https://rohelab.github.io/vsp/reference/vsp.md) :
  Semi-Parametric Factor Analysis via Vintage Sparse PCA
- [`vsp(`*`<svd_like>`*`)`](https://rohelab.github.io/vsp/reference/vsp.svd_like.md)
  : Perform varimax rotation on a low rank matrix factorization
- [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md) :
  Create a vintage sparse factor analysis object
- [`set_z_factor_names()`](https://rohelab.github.io/vsp/reference/set_z_factor_names.md)
  [`set_y_factor_names()`](https://rohelab.github.io/vsp/reference/set_z_factor_names.md)
  : Give the dimensions of Z factors informative names
- [`bff()`](https://rohelab.github.io/vsp/reference/bff.md) : Find
  features most associated with cluster membership

## Turn latent factors into nice tibbles

- [`get_svd_u()`](https://rohelab.github.io/vsp/reference/get_svd_u.md)
  [`get_svd_v()`](https://rohelab.github.io/vsp/reference/get_svd_u.md)
  [`get_varimax_z()`](https://rohelab.github.io/vsp/reference/get_svd_u.md)
  [`get_varimax_y()`](https://rohelab.github.io/vsp/reference/get_svd_u.md)
  : Get left singular vectors in a tibble
- [`get_z_hubs()`](https://rohelab.github.io/vsp/reference/get_z_hubs.md)
  [`get_y_hubs()`](https://rohelab.github.io/vsp/reference/get_z_hubs.md)
  : Get most important hubs for each Z factor
- [`bind_varimax_z()`](https://rohelab.github.io/vsp/reference/bind_varimax_z.md)
  [`bind_varimax_y()`](https://rohelab.github.io/vsp/reference/bind_varimax_z.md)
  [`bind_svd_u()`](https://rohelab.github.io/vsp/reference/bind_varimax_z.md)
  [`bind_svd_v()`](https://rohelab.github.io/vsp/reference/bind_varimax_z.md)
  : Add Z factor loadings to node table of tidygraph

## Plotting

- [`plot_cumulative_curves()`](https://rohelab.github.io/vsp/reference/plot_cumulative_curves.md)
  : Plot cumulative participation curves.
- [`plot_ipr_curves()`](https://rohelab.github.io/vsp/reference/plot_ipr_curves.md)
  : Plot IPR curves.
- [`plot_ipr_pairs()`](https://rohelab.github.io/vsp/reference/plot_ipr_pairs.md)
  : Plot pairs of inverse participation ratios for singular vectors
- [`plot_mixing_matrix()`](https://rohelab.github.io/vsp/reference/plot_mixing_matrix.md)
  : Plot the mixing matrix B
- [`plot_varimax_z_pairs()`](https://rohelab.github.io/vsp/reference/plot_varimax_z_pairs.md)
  [`plot_varimax_y_pairs()`](https://rohelab.github.io/vsp/reference/plot_varimax_z_pairs.md)
  [`plot_svd_u()`](https://rohelab.github.io/vsp/reference/plot_varimax_z_pairs.md)
  [`plot_svd_v()`](https://rohelab.github.io/vsp/reference/plot_varimax_z_pairs.md)
  : Create a pairs plot of select Y factors
- [`screeplot(`*`<vsp_fa>`*`)`](https://rohelab.github.io/vsp/reference/screeplot.vsp_fa.md)
  : Create a screeplot from a factor analysis object

## Localization heuristics

- [`ipr()`](https://rohelab.github.io/vsp/reference/ipr.md) : Calculate
  the inverse participation ratio (IPR) for a vector.
- [`iprs()`](https://rohelab.github.io/vsp/reference/iprs.md) :
  Calculate IPR for all singular vectors in a list.
- [`cumulative_participation()`](https://rohelab.github.io/vsp/reference/cumulative_participation.md)
  : Calculate cumulative participation of a set of singular vectors.
- [`localization_statistics()`](https://rohelab.github.io/vsp/reference/localization_statistics.md)
  : Compute localization statistics across various regularization
  parameters.
