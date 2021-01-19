# These functions are tested indirectly when the models are used.
# It is executed on package startup, and can't be executed for testing
# since they are already in the parsnip model database.

# coverage stats for this reason.

# nocov

bayesian_make <- function(modes = c("classification", "regression")) {
  parsnip::set_new_model("bayesian")

  for (mode in modes) {
    parsnip::set_model_mode("bayesian", mode)

    # -------------------------------------------------------------------------

    parsnip::set_model_engine("bayesian", mode, "brms")

    parsnip::set_dependency("bayesian", "brms", "brms")
    parsnip::set_dependency("bayesian", "brms", "bayesian")

    parsnip::set_fit(
      model = "bayesian",
      eng = "brms",
      mode = mode,
      value = list(
        interface = "formula",
        protect = c("formula", "data"),
        func = c(pkg = "bayesian", fun = "bayesian_fit"),
        defaults = list(
          family = if (mode == "classification") {
            rlang::expr(brms::brmsfamily("binomial"))
          } else {
            rlang::expr(brms::brmsfamily("gaussian"))
          }
        )
      )
    )

    parsnip::set_encoding(
      model = "bayesian",
      eng = "brms",
      mode = mode,
      options = list(
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
      )
    )

    if (mode == "classification") {
      parsnip::set_pred(
        model = "bayesian",
        eng = "brms",
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
          args =
            list(
              object = rlang::expr(object$fit),
              newdata = rlang::expr(new_data),
              summary = TRUE
            )
        )
      )

      parsnip::set_pred(
        model = "bayesian",
        eng = "brms",
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
          args =
            list(
              object = rlang::expr(object$fit),
              newdata = rlang::expr(new_data),
              summary = TRUE
            )
        )
      )
    } else {
      parsnip::set_pred(
        model = "bayesian",
        eng = "brms",
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
      model = "bayesian",
      eng = "brms",
      mode = mode,
      type = "conf_int",
      value = list(
        pre = NULL,
        post = function(results, object) {
          if (mode == "classification") {
            res_2 <-
              tibble::tibble(
                lo =
                  parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$conf_int$extras$level
                  ),
                hi =
                  parsnip::convert_stan_interval(
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
                .pred_lower =
                  parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$conf_int$extras$level
                  ),
                .pred_upper =
                  parsnip::convert_stan_interval(
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
        args =
          list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = FALSE
          )
      )
    )

    parsnip::set_pred(
      model = "bayesian",
      eng = "brms",
      mode = mode,
      type = "pred_int",
      value = list(
        pre = NULL,
        post = function(results, object) {
          if (mode == "classification") {
            res_2 <-
              tibble::tibble(
                lo =
                  parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$pred_int$extras$level
                  ),
                hi =
                  parsnip::convert_stan_interval(
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
                .pred_lower =
                  parsnip::convert_stan_interval(
                    results,
                    level = object$spec$method$pred$pred_int$extras$level
                  ),
                .pred_upper =
                  parsnip::convert_stan_interval(
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
        args =
          list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = FALSE
          )
      )
    )

    parsnip::set_pred(
      model = "bayesian",
      eng = "brms",
      mode = mode,
      type = "raw",
      value = list(
        pre = NULL,
        post = NULL,
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args =
          list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = FALSE
          )
      )
    )

    parsnip::set_pred(
      model = "bayesian",
      eng = "brms",
      mode = mode,
      type = "quantile",
      value = list(
        pre = NULL,
        post = function(results, object) {
          tibble::as_tibble(results)
        },
        func = c(pkg = "bayesian", fun = "bayesian_predict"),
        args =
          list(
            object = rlang::expr(object$fit),
            newdata = rlang::expr(new_data),
            summary = TRUE
          )
      )
    )
  }
}

# nocov end
