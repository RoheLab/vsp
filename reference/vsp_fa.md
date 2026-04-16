# Create a vintage sparse factor analysis object

`vsp_fa` objects are a subclass of
[`LRMF3::fa_like()`](https://rdrr.io/pkg/LRMF3/man/fa_like.html), with
additional fields `u`, `d`, `v`, `transformers`, `R_U`, and `R_V`

## Usage

``` r
vsp_fa(
  u,
  d,
  v,
  Z,
  B,
  Y,
  transformers,
  R_U,
  R_V,
  rownames = NULL,
  colnames = NULL
)
```

## Arguments

- u:

  A [`matrix()`](https://rdrr.io/r/base/matrix.html) of "left
  singular-ish" vectors.

- d:

  A [`numeric()`](https://rdrr.io/r/base/numeric.html) vector of
  "singular-ish" values.

- v:

  A [`matrix()`](https://rdrr.io/r/base/matrix.html) of "right
  singular-ish" vectors.

- Z:

  A *matrix* of embeddings for each observation.

- B:

  A mixing *matrix* describing how observation embeddings and topics
  interact. Does not have to be diagonal!

- Y:

  A *matrix* describing the compositions of various topics or factors.

- transformers:

  A list of transformations from the `invertiforms` package.

- R_U:

  Varimax rotation matrix use to transform `u` into `Z`.

- R_V:

  Varimax rotation matrix use to transform `v` into `Y`.

- rownames:

  Identifying names for each row of the original data. Defaults to
  `NULL`, in which cases each row is given a row number left-padded with
  zeros as a name.

- colnames:

  Identifying names for each column of the original data. Defaults to
  `NULL`, in which cases each column is given a row column left-padded
  with zeros as a name.

## Value

A `svd_fa` object.
