context("bayesian model")

test_that("bayesian execution", {
  skip_on_cran()
  skip_on_os("mac")
  skip_on_os("windows")
  skip("Update tests to improve unit test coverage!")

  brms_cl <- call2("brm",
    .ns = "brms",
    rating ~ treat + period + carry + (1 | subject),
    data = inhaler, iter = 500, seed = 2020, refresh = 0
  )

  set.seed(2020)
  expect_warning(brms_mod <- eval_tidy(brms_cl), "ESS")

  # nolint start
  expect_warning(
    {
      set.seed(2020)
      expect_error(
        bayesian_mod <-
          bayesian(
            iter = 500,
            seed = 2020,
            stan_args = list(refresh = 0)
          ) %>%
          set_engine("brms") %>%
          fit(
            rating ~ treat + period + carry + (1 | subject),
            data = inhaler
          ),
        regex = NA
      )
    },
    "ESS"
  )
  # nolint end

  expect_equal(
    coef(bayesian_mod$fit)$subject,
    coef(brms_mod)$subject
  )

  pred_cl <- call2(
    "posterior_predict",
    .ns = "brms",
    brms_mod,
    head(inhaler)
  )

  set.seed(2020)
  brms_post <- eval_tidy(pred_cl)
  brms_pred <- unname(apply(brms_post, 2, mean))
  brms_pi_lower <- unname(apply(brms_post, 2, quantile, probs = 0.05))
  brms_pi_upper <- unname(apply(brms_post, 2, quantile, probs = 0.95))

  set.seed(2020)
  bayesian_pred <- predict(bayesian_mod, head(inhaler))

  expect_equal(bayesian_pred$.pred, brms_pred, tolerance = 0.1)

  set.seed(2020)
  bayesian_pred_int <- predict(
    bayesian_mod,
    head(inhaler),
    level = 0.90,
    type = "pred_int"
  )

  expect_equal(bayesian_pred_int$.pred_lower, brms_pi_lower, tolerance = 0.1)
  expect_equal(bayesian_pred_int$.pred_upper, brms_pi_upper, tolerance = 0.1)

  # Check the default engine
  expect_equal(bayesian()$engine, "brms")
})
