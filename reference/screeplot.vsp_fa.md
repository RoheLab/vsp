# Create a screeplot from a factor analysis object

Create a screeplot from a factor analysis object

## Usage

``` r
# S3 method for class 'vsp_fa'
screeplot(x, ...)
```

## Arguments

- x:

  A [`vsp_fa()`](https://rohelab.github.io/vsp/reference/vsp_fa.md)
  object.

- ...:

  Ignored, included only for consistency with S3 generic.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row for each node, and one column containing each of the
requested factor or singular vector, plus an additional `id` column.
