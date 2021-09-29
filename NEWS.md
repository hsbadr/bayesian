# bayesian 0.0.7

- Fixed `GetStarted.Rmd` vignette build
- Fixed class predictions for binary classification
- Added threshold probability option for class predictions
- Replaced deprecated `pull_workflow_fit()`

# bayesian 0.0.6

- Added functionality to change to declare an engine in the model specification function.
- Updated package dependencies
- Updated README and citation

# bayesian 0.0.5

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

# bayesian 0.0.4

- Added GettingStarted vignette
- Added `rstan` & `future` to suggested packages
- Added `formula.override` argument to model specification
- Added `family` argument to model specification
- Added `threads` argument to model specification
- Added `stan_args` to pass extra arguments to `Stan`
- Implemented `parsnip::set_model_arg`
- Switched to engine-specific defaults
- Fixed NOTE: Undeclared packages `rstan`, `future` in Rd xrefs
- Fixed NOTE: Found (possibly) invalid URLs
- Fixed building vignettes
- Support updating more `brms::brm` arguments in model specification
- Updated documentation and vignettes
- Updated WORDLIST
- Minor code cleanup

# bayesian 0.0.3

- Added CITATION and updated package DESCRIPTION
- Added fit wrapper and other helper functions
- Renamed the current engine from `stan` to `brms`
- Updated manual, logo, WORDLIST, and NEWS
- Fixed a CRAN NOTE regarding LazyData
- Updated lifecycle URL in the README

# bayesian 0.0.2

- Added `bayesian_read` and `bayesian_write` functions
- Cite tidymodels, brms, and Stan references
- Removed the redundant `verbose` argument
- Fixed CRAN notes and comments
- Fixed possibly invalid file URL
- Updated package DESCRIPTION and WORDLIST
- Updated README and added DOI badge
- Used CRAN template for MIT license

# bayesian 0.0.1

- Initial release
