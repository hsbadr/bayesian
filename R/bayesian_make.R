# These functions are tested indirectly when the models are used.
# It is executed on package startup, and can't be executed for testing
# since they are already in the parsnip model database.

# coverage stats for this reason.

# nocov

bayesian_make <- function(modes = c("classification", "regression")) {
  model <- "bayesian"
  engine <- "brms"

  parsnip::set_new_model(model)

  for (mode in modes) {
    parsnip::set_model_mode("bayesian", mode)

    # -------------------------------------------------------------------------

    parsnip::set_model_engine(model = model, mode = mode, eng = engine)

    # -------------------------------------------------------------------------

    parsnip::set_dependency(model = model, eng = engine, pkg = "brms")
    parsnip::set_dependency(model = model, eng = engine, pkg = "bayesian")

    # -------------------------------------------------------------------------

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "formula.override",
      original = "formula.override",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "family",
      original = "family",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "prior",
      original = "prior",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "sample_prior",
      original = "sample_prior",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "knots",
      original = "knots",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stanvars",
      original = "stanvars",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "fit",
      original = "fit",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "inits",
      original = "inits",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "chains",
      original = "chains",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "iter",
      original = "iter",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "warmup",
      original = "warmup",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "thin",
      original = "thin",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "cores",
      original = "cores",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "threads",
      original = "threads",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "algorithm",
      original = "algorithm",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "backend",
      original = "backend",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stan_args",
      original = "stan_args",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "control",
      original = "control",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_pars",
      original = "save_pars",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_model",
      original = "save_model",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file",
      original = "file",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file_refit",
      original = "file_refit",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "normalize",
      original = "normalize",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "future",
      original = "future",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "seed",
      original = "seed",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "silent",
      original = "silent",
      func = c(pkg = "bayesian", fun = "bayesian_fit"),
      has_submodel = FALSE
    )

    # -------------------------------------------------------------------------

    parsnip::set_fit(
      model = model,
      eng = engine,
      mode = mode,
      value = list(
        interface = "formula",
        protect = c("formula", "data"),
        func = c(pkg = "bayesian", fun = "bayesian_fit"),
        defaults = list()
      )
    )

    # -------------------------------------------------------------------------

    parsnip::set_encoding(
      model = model,
      eng = engine,
      mode = mode,
      options = list(
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      )
    )

    # -------------------------------------------------------------------------

    if (mode == "classification") {
      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "class",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(object$lvl) == 2) {
              results <- ifelse(
                results[, 1] >= 0.5,
                object$lvl[2],
                object$lvl[1]
              )
            } else if (
              length(object$lvl) > 2 &
                length(object$lvl) == ncol(results)
            ) {
              results <- object$lvl[apply(results, 1, which.max)]
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            unname(results)
          },
          func = c(pkg = "bayesian", fun = "bayesian_predict"),
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )

      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "prob",
        value = list(
          pre = NULL,
          post = function(results, object) {
            if (length(object$lvl) == 2) {
              results <- tibble::tibble(
                v1 = 1 - results[, 1],
                v2 = results[, 1]
              )
              colnames(results) <- object$lvl
            } else if (
              length(object$lvl) > 2 &
                length(object$lvl) == ncol(results)
            ) {
              colnames(results) <- object$lvl
            } else {
              rlang::abort("Unexpected model predictions!")
            }
            results
          },
          func = c(pkg = "bayesian", fun = "bayesian_predict"),
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )
    } else {
      parsnip::set_pred(
        model = model,
        eng = engine,
        mode = mode,
        type = "numeric",
        value = list(
          pre = NULL,
          post = function(results, object) {
            tibble::tibble(.pred = results[, 1])
          },
          func = c(pkg = "bayesian", fun = "bayesian_predict"),
          args = list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
        )
      )
    }

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "conf_int",
      value = list(
        pre = NULL,
        post = function(results, object) {
          if (mode == "classification") {
            res_2 <-
              tibble::tibble(
                lo = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level
                ),
                hi = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level,
                  lower = FALSE
                ),
              )
            res_1 <- res_2
            res_1$lo <- 1 - res_2$hi
            res_1$hi <- 1 - res_2$lo
            res <- dplyr::bind_cols(res_1, res_2)
            lo_nms <- paste0(".pred_lower_", object$lvl)
            hi_nms <- paste0(".pred_upper_", object$lvl)
            colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])
          } else {
            res <-
              tibble::tibble(
                .pred_lower = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level
                ),
                .pred_upper = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$conf_int$extras$level,
                  lower = FALSE
                ),
              )
          }
          if (object$spec$method$pred$conf_int$extras$std_error) {
            res$.std_error <- apply(results, 2, stats::sd, na.rm = TRUE)
          }
          res
        },
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          summary = FALSE
        )
      )
    )

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "pred_int",
      value = list(
        pre = NULL,
        post = function(results, object) {
          if (mode == "classification") {
            res_2 <-
              tibble::tibble(
                lo = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level
                ),
                hi = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level,
                  lower = FALSE
                ),
              )
            res_1 <- res_2
            res_1$lo <- 1 - res_2$hi
            res_1$hi <- 1 - res_2$lo
            res <- dplyr::bind_cols(res_1, res_2)
            lo_nms <- paste0(".pred_lower_", object$lvl)
            hi_nms <- paste0(".pred_upper_", object$lvl)
            colnames(res) <- c(lo_nms[1], hi_nms[1], lo_nms[2], hi_nms[2])
          } else {
            res <-
              tibble::tibble(
                .pred_lower = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level
                ),
                .pred_upper = parsnip::convert_stan_interval(
                  results,
                  level = object$spec$method$pred$pred_int$extras$level,
                  lower = FALSE
                ),
              )
          }
          if (object$spec$method$pred$pred_int$extras$std_error) {
            res$.std_error <- apply(results, 2, stats::sd, na.rm = TRUE)
          }
          res
        },
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          summary = FALSE
        )
      )
    )

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "raw",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          summary = FALSE
        )
      )
    )

    parsnip::set_pred(
      model = model,
      eng = engine,
      mode = mode,
      type = "quantile",
      value = list(
        pre = NULL,
        post = function(results, object) {
          tibble::as_tibble(results)
        },
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args = list(
          object = rlang::expr(object$fit),
          newdata = rlang::expr(new_data),
          summary = TRUE
        )
      )
    )
  }
}

# nocov end
