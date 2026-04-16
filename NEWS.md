# vsp 0.1.3.9000

- Improve compatibility with `RSpectra` (#78)
- Added localization heuristics to investigate localization of eigenvectors: `iprs()`, `cumulative_participation()`, `localization_statistics()`, `plot_localization_curves()`, `plot_ipr_curves()`. These heuristics can be helpful when selecting regularization parameters, and roughly the idea is that minimizing various measures of localization is often helpful. That said, there is substantial nuance here, and these functions are intended to aid experienced users in model selection and interpretation.

# vsp 0.1.3

- Re-submission per CRAN request

# vsp 0.1.2

- Default to using `clue` to heuristically align `Z` and `Y` factors (use `match_columns = FALSE` for previous behavior)
- Compatibility updates for new Matrix release
- Add more usage examples to documentation (primarily for testing purposes)

# vsp 0.1.1

- Version bump for CRAN, not reflecting any new features

# vsp 0.1.0

- Added a `NEWS.md` file to track changes to the package.
