# Changelog

## bayesian 1.0.1

CRAN release: 2024-04-28

- Built vignettes to fix CRAN warnings
- Replaced [`citEntry()`](https://rdrr.io/r/utils/citEntry.html) with
  [`bibentry()`](https://rdrr.io/r/utils/bibentry.html)
- Updated citation and invalid URLs

## bayesian 1.0.0

- Fixed predictions with intercept-only models
  ([\#17](https://github.com/hsbadr/bayesian/issues/17))

## bayesian 0.0.9

CRAN release: 2022-06-16

- Fixed handling of `extended.family` class
- Fixed multi-class probability (`type = "prob"`) predictions
- Fixed multi-class confidence-intervals (`type = "conf_int"`)
  predictions
- Fixed multi-class prediction-intervals (`type = "pred_int"`)
  predictions

## bayesian 0.0.9

CRAN release: 2022-06-16

- Depends on `parsnip >= 1.0.0`
- Using base R pipe and depends on R \>= 4.1.0
- Supported the use of case weights
- Supported threshold (inverse probability weights) for multi-class
  predictions
- Fixed inconsistent probability (`type = "prob"`) predictions for
  two-class models
- Updated package dependencies
- Used `init` instead of the deprecated `inits` (`brms >= 2.16.9`)

## bayesian 0.0.8

CRAN release: 2022-01-20

- Updated package website
- Updated package `CITATION`
- Updated package `DESCRIPTION` and `README`
- Updated package dependencies
- Added preliminary support for multivariate non-linear models
  ([\#7](https://github.com/hsbadr/bayesian/issues/7))
- Fixed possibly invalid URLs
- Fixed `type = "raw"` predictions for multivariate non-linear models
  ([\#7](https://github.com/hsbadr/bayesian/issues/7))
- Handled `mvbrmsformula` class in `bayesian_formula`
  ([\#7](https://github.com/hsbadr/bayesian/issues/7))
- Fixed threshold probability check for class predictions
- Set dependency by model’s mode with `parsnip > 0.1.7`

## bayesian 0.0.7

CRAN release: 2021-09-29

- Fixed `GetStarted.Rmd` vignette build
- Fixed class predictions for binary classification
- Added threshold probability option for class predictions
- Replaced deprecated
  [`pull_workflow_fit()`](https://workflows.tidymodels.org/reference/workflow-extractors.html)

## bayesian 0.0.6

CRAN release: 2021-07-12

- Added functionality to change to declare an engine in the model
  specification function.
- Updated package dependencies
- Updated `README` and citation

## bayesian 0.0.5

CRAN release: 2021-05-31

- Cleanly evaluating the fit call
- Simplified and exposed the family call
- Cleaned up `bayesian_fit` function
- Directly calling the `update` method
- Revised engine-specific encodings
- tests: Update Bayesian model specification
- Reformatted code and updated documentation
- Updated dependencies and documentation
- Updated GettingStarted vignette
- Updated `tidymodels` citation
- Updated the package logo

## bayesian 0.0.4

CRAN release: 2021-05-18

- Added GettingStarted vignette
- Added `rstan` & `future` to suggested packages
- Added `formula.override` argument to model specification
- Added `family` argument to model specification
- Added `threads` argument to model specification
- Added `stan_args` to pass extra arguments to `Stan`
- Implemented
  [`parsnip::set_model_arg`](https://parsnip.tidymodels.org/reference/set_new_model.html)
- Switched to engine-specific defaults
- Fixed NOTE: Undeclared packages `rstan`, `future` in Rd xrefs
- Fixed NOTE: Found (possibly) invalid URLs
- Fixed building vignettes
- Support updating more
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html)
  arguments in model specification
- Updated documentation and vignettes
- Updated `WORDLIST`
- Minor code cleanup

## bayesian 0.0.3

CRAN release: 2021-04-02

- Added `CITATION` and updated package `DESCRIPTION`
- Added fit wrapper and other helper functions
- Renamed the current engine from `stan` to `brms`
- Updated manual, logo, `WORDLIST`, and `NEWS`
- Fixed a CRAN NOTE regarding LazyData
- Updated lifecycle URL in the `README`

## bayesian 0.0.2

CRAN release: 2021-01-14

- Added `bayesian_read` and `bayesian_write` functions
- Cite tidymodels, brms, and Stan references
- Removed the redundant `verbose` argument
- Fixed CRAN notes and comments
- Fixed possibly invalid file URL
- Updated package `DESCRIPTION` and `WORDLIST`
- Updated `README` and added DOI badge
- Used CRAN template for MIT license

## bayesian 0.0.1

- Initial release
