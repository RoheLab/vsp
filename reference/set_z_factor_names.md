# Give the dimensions of Z factors informative names

Give the dimensions of Z factors informative names

## Usage

``` r
set_z_factor_names(fa, names)

set_y_factor_names(fa, names)
```

## Arguments

- fa:

  A [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
  object.

- names:

  Describe new names for Z/Y factors.

## Value

A new [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
object, but the columns names of `Z` and the row names of `B` have been
set to `names` (for `set_z_factor_names`), and the column names of `B`
and the column names of `Y` have been set to `names` (for
`set_y_factor_names`).

## Functions

- `set_y_factor_names()`: Give the dimensions of Y factors informative
  names
