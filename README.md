<!-- README.md is generated from README.Rmd. Please edit that file -->

bayesian: Bindings for Bayesian TidyModels [<img src="man/figures/bayesian.png" align="right" width="160" alt="bayesian logo" />](https://hsbadr.github.io/bayesian/)
=====================================================================================================================================================================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last
commit](https://img.shields.io/github/last-commit/hsbadr/bayesian)](https://github.com/hsbadr/bayesian/commits/main)
[![Commits since
release](https://img.shields.io/github/commits-since/hsbadr/bayesian/0.0.1.svg?color=green)](https://GitHub.com/hsbadr/bayesian/commit/main/)
[![R-CMD-check](https://github.com/hsbadr/bayesian/workflows/R-CMD-check/badge.svg)](https://github.com/hsbadr/bayesian/actions)

[![CRAN
Status](https://www.r-pkg.org/badges/version/bayesian)](https://CRAN.R-project.org/package=bayesian)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/grand-total/bayesian)](https://cran.r-project.org/package=bayesian)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/hsbadr/bayesian/blob/main/LICENSE.md)
[![Codecov Test
Coverage](https://codecov.io/gh/hsbadr/bayesian/branch/main/graph/badge.svg)](https://codecov.io/gh/hsbadr/bayesian?branch=main)
[![DOI](https://zenodo.org/badge/327419436.svg)](https://zenodo.org/badge/latestdoi/327419436)
<!-- badges: end -->

[**`bayesian`**](https://hsbadr.github.io/bayesian/) supports Bayesian
modeling using
[`brms`](https://paul-buerkner.github.io/brms/)/[`Stan`](https://mc-stan.org/)
with
[`parsnip`](https://parsnip.tidymodels.org/)/[`tidymodels`](https://www.tidymodels.org/).

Installation
------------

The stable version of [`bayesian`](https://hsbadr.github.io/bayesian/)
can be installed from
[CRAN](https://CRAN.R-project.org/package=bayesian) using:

    install.packages("bayesian")

The development version of
[`bayesian`](https://hsbadr.github.io/bayesian/) can be installed from
[GitHub](https://github.com/hsbadr/bayesian) using:

    install.packages("remotes")
    remotes::install_github("hsbadr/bayesian")

Example
-------

    library(bayesian)

    bayesian_mod <-
      bayesian() %>%
      set_engine("stan") %>%
      fit(
        rating ~ treat + period + carry + (1 | subject),
        data = inhaler
      )

    summary(bayesian_mod$fit)

Contributing
------------

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

-   For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://community.rstudio.com/).

-   If you think you have encountered a bug, please [submit an
    issue](https://github.com/hsbadr/bayesian/issues).

-   Either way, learn how to create and share a
    [reprex](https://reprex.tidyverse.org) (a minimal, reproducible
    example), to clearly communicate about your code.

-   Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
