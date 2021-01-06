
# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines bayesian model function in the parsnip model database
  bayesian_make(modes = c("classification", "regression")) # nolint
}
