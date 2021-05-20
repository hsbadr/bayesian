# These functions are tested indirectly when the models are used.
# It is executed on package startup, and can't be executed for testing
# since they are already in the parsnip model database.

# coverage stats for this reason.

# nocov

bayesian_make <- function(modes = c("classification", "regression")) {
  model <- "bayesian"
  engine <- "brms"

  fitfunc <- c(pkg = "bayesian", fun = "bayesian_fit")
  predfunc <- c(pkg = "bayesian", fun = "bayesian_predict")

  dependpkgs <- unique(c("brms", fitfunc["pkg"], predfunc["pkg"]))
  dependpkgs <- dependpkgs[!is.na(dependpkgs)]

  parsnip::set_new_model(model)

  for (mode in modes) {
    parsnip::set_model_mode(model = model, mode = mode)

    # -------------------------------------------------------------------------

    parsnip::set_model_engine(model = model, mode = mode, eng = engine)

    # -------------------------------------------------------------------------

    for (pkg in dependpkgs) {
      parsnip::set_dependency(model = model, eng = engine, pkg = pkg)
    }

    # -------------------------------------------------------------------------

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "formula.override",
      original = "formula.override",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "family",
      original = "family",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "prior",
      original = "prior",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "sample_prior",
      original = "sample_prior",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "knots",
      original = "knots",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stanvars",
      original = "stanvars",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "fit",
      original = "fit",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "inits",
      original = "inits",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "chains",
      original = "chains",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "iter",
      original = "iter",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "warmup",
      original = "warmup",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "thin",
      original = "thin",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "cores",
      original = "cores",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "threads",
      original = "threads",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "algorithm",
      original = "algorithm",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "backend",
      original = "backend",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "stan_args",
      original = "stan_args",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "control",
      original = "control",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_pars",
      original = "save_pars",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "save_model",
      original = "save_model",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file",
      original = "file",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "file_refit",
      original = "file_refit",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "normalize",
      original = "normalize",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "future",
      original = "future",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "seed",
      original = "seed",
      func = fitfunc,
      has_submodel = FALSE
    )

    parsnip::set_model_arg(
      model = model,
      eng = engine,
      parsnip = "silent",
      original = "silent",
      func = fitfunc,
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
        func = fitfunc,
        defaults = list()
      )
    )

    # -------------------------------------------------------------------------

    parsnip::set_encoding(
      model = model,
      eng = engine,
      mode = mode,
      options = list(
        predictor_indicators = "none",
        compute_intercept = FALSE,
        remove_intercept = FALSE,
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
          func = predfunc,
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
          func = predfunc,
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
          func = predfunc,
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
        func = predfunc,
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
        func = predfunc,
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
        func = predfunc,
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
        func = predfunc,
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
