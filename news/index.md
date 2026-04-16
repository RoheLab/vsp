# Changelog

## vsp 0.1.4

## vsp 0.1.3.9000

- Improve compatibility with `RSpectra`
  ([\#78](https://github.com/RoheLab/vsp/issues/78))
- Added localization heuristics to investigate localization of
  eigenvectors:
  [`iprs()`](https://rohelab.github.io/vsp/reference/iprs.md),
  [`cumulative_participation()`](https://rohelab.github.io/vsp/reference/cumulative_participation.md),
  [`localization_statistics()`](https://rohelab.github.io/vsp/reference/localization_statistics.md),
  `plot_localization_curves()`,
  [`plot_ipr_curves()`](https://rohelab.github.io/vsp/reference/plot_ipr_curves.md).
  These heuristics can be helpful when selecting regularization
  parameters, and roughly the idea is that minimizing various measures
  of localization is often helpful. That said, there is substantial
  nuance here, and these functions are intended to aid experienced users
  in model selection and interpretation.

## vsp 0.1.3

CRAN release: 2025-08-20

- Re-submission per CRAN request

## vsp 0.1.2

CRAN release: 2024-11-05

- Default to using `clue` to heuristically align `Z` and `Y` factors
  (use `match_columns = FALSE` for previous behavior)
- Compatibility updates for new Matrix release
- Add more usage examples to documentation (primarily for testing
  purposes)

## vsp 0.1.1

CRAN release: 2022-12-05

- Version bump for CRAN, not reflecting any new features

## vsp 0.1.0

CRAN release: 2022-02-10

- Added a `NEWS.md` file to track changes to the package.
