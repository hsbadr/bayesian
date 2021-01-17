if (requireNamespace("spelling", quietly = TRUE) &&
  !nzchar(Sys.getenv("CI")) &&
  !nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
  spelling::spell_check_test(
    vignettes = TRUE, error = FALSE,
    skip_on_cran = TRUE
  )
}
