#' General Interface for Bayesian TidyModels
#'
#' `bayesian()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  \pkg{Stan} via \pkg{brms} package in \pkg{R}.
#'
#'  The arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be
#'  used in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#'
#' @param formula.override Overrides the formula; for details see
#'  \code{\link[brms]{brmsformula}}.
#'
#' @inheritParams brms::brm
#'
#' @param stan_args A list of extra arguments to \pkg{Stan}.
#'
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `bayesian()`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created by the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{brms}:  `"brms"`
#' }
#'
#' @includeRmd man/rmd/bayesian-engine.Rmd details
#'
#' For prediction, the `brms` engine can compute posterior
#'  intervals analogous to confidence and prediction intervals. In
#'  these instances, the units are the original outcome and when
#'  `std_error = TRUE`, the standard deviation of the posterior
#'  distribution (or posterior predictive distribution as
#'  appropriate) is returned.
#'
#' @examples
#'
#' bayesian()
#'
#' show_model_info("bayesian")
#'
#' bayesian(mode = "classification")
#' bayesian(mode = "regression")
#' \dontrun{
#' bayesian_mod <-
#'   bayesian() %>%
#'   set_engine("brms") %>%
#'   fit(
#'     rating ~ treat + period + carry + (1 | subject),
#'     data = inhaler
#'   )
#'
#' summary(bayesian_mod$fit)
#' }
#'
#' # -------------------------------------------------------------------------
#' @export
bayesian <-
  function(mode = "regression",
           formula.override = NULL,
           family = NULL,
           prior = NULL,
           sample_prior = NULL,
           knots = NULL,
           stanvars = NULL,
           fit = NULL,
           inits = NULL,
           chains = NULL,
           iter = NULL,
           warmup = NULL,
           thin = NULL,
           cores = NULL,
           threads = NULL,
           algorithm = NULL,
           backend = NULL,
           stan_args = NULL,
           control = NULL,
           save_pars = NULL,
           save_model = NULL,
           file = NULL,
           file_refit = NULL,
           normalize = NULL,
           future = NULL,
           seed = NULL,
           silent = NULL) {
    args <- list(
      formula.override = rlang::enquo(formula.override),
      family = rlang::enquo(family),
      prior = rlang::enquo(prior),
      sample_prior = rlang::enquo(sample_prior),
      knots = rlang::enquo(knots),
      stanvars = rlang::enquo(stanvars),
      fit = rlang::enquo(fit),
      inits = rlang::enquo(inits),
      chains = rlang::enquo(chains),
      iter = rlang::enquo(iter),
      warmup = rlang::enquo(warmup),
      thin = rlang::enquo(thin),
      cores = rlang::enquo(cores),
      threads = rlang::enquo(threads),
      algorithm = rlang::enquo(algorithm),
      backend = rlang::enquo(backend),
      stan_args = rlang::enquo(stan_args),
      control = rlang::enquo(control),
      save_pars = rlang::enquo(save_pars),
      save_model = rlang::enquo(save_model),
      file = rlang::enquo(file),
      file_refit = rlang::enquo(file_refit),
      normalize = rlang::enquo(normalize),
      future = rlang::enquo(future),
      seed = rlang::enquo(seed),
      silent = rlang::enquo(silent)
    )

    parsnip::new_model_spec(
      "bayesian",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.bayesian <- function(x, ...) {
  cat("Bayesian Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}


#' @export
translate.bayesian <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'brms'` for translation.")
    engine <- "brms"
  }

  x <- parsnip::translate.default(x, engine, ...)

  x
}

# -------------------------------------------------------------------------

#' @param object A Bayesian model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Other arguments passed to internal functions.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#'
#' @return An updated model specification.
#'
#' @examples
#' model <- bayesian(inits = "random")
#' model
#' update(model, inits = "0")
#' update(model, inits = "0", fresh = TRUE)
#' @method update bayesian
#' @rdname bayesian
#' @export
update.bayesian <-
  function(object,
           parameters = NULL,
           formula.override = NULL,
           family = NULL,
           prior = NULL,
           sample_prior = NULL,
           knots = NULL,
           stanvars = NULL,
           fit = NULL,
           inits = NULL,
           chains = NULL,
           iter = NULL,
           warmup = NULL,
           thin = NULL,
           cores = NULL,
           threads = NULL,
           algorithm = NULL,
           backend = NULL,
           stan_args = NULL,
           control = NULL,
           save_pars = NULL,
           save_model = NULL,
           file = NULL,
           file_refit = NULL,
           normalize = NULL,
           future = NULL,
           seed = NULL,
           silent = NULL,
           fresh = FALSE,
           ...) {
    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
      formula.override = rlang::enquo(formula.override),
      family = rlang::enquo(family),
      prior = rlang::enquo(prior),
      sample_prior = rlang::enquo(sample_prior),
      knots = rlang::enquo(knots),
      stanvars = rlang::enquo(stanvars),
      fit = rlang::enquo(fit),
      inits = rlang::enquo(inits),
      chains = rlang::enquo(chains),
      iter = rlang::enquo(iter),
      warmup = rlang::enquo(warmup),
      thin = rlang::enquo(thin),
      cores = rlang::enquo(cores),
      threads = rlang::enquo(threads),
      algorithm = rlang::enquo(algorithm),
      backend = rlang::enquo(backend),
      stan_args = rlang::enquo(stan_args),
      control = rlang::enquo(control),
      save_pars = rlang::enquo(save_pars),
      save_model = rlang::enquo(save_model),
      file = rlang::enquo(file),
      file_refit = rlang::enquo(file_refit),
      normalize = rlang::enquo(normalize),
      future = rlang::enquo(future),
      seed = rlang::enquo(seed),
      silent = rlang::enquo(silent)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    eng_args <- parsnip::update_engine_parameters(object$eng_args, ...)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- purrr::map_lgl(args, parsnip::null_value)
      if (any(null_args)) {
        args <- args[!null_args]
      }
      if (length(args) > 0) {
        object$args[names(args)] <- args
      }
      if (length(eng_args) > 0) {
        object$eng_args[names(eng_args)] <- eng_args
      }
    }

    parsnip::new_model_spec(
      "bayesian",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# -------------------------------------------------------------------------

check_args.bayesian <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$chains) && args$chains < 1L) {
    rlang::abort("`chains` should be positive integer.")
  }

  if (is.numeric(args$iter) && args$iter < 1L) {
    rlang::abort("`iter` should be positive integer.")
  }

  invisible(object)
}

# -------------------------------------------------------------------------

#' Fit Bayesian models
#' @inheritParams brms::brm
#  @param fit An instance of S3 class \code{brmsfit}; If \code{fit} is
#     of class \code{brmsfit}, the fitted model will be updated using
#     \code{\link[brms]{update.brmsfit}}.
#  @param formula. Optional formula giving a template which specifies
#    how to update the formula. See \code{\link{update.formula}} and
#    \code{\link[brms]{brmsformula}} for more details.
#  @param newdata Optional \code{data.frame} to update the model with
#    new data. Data-dependent priors will not be updated automatically.
#  @param recompile Logical, indicating whether the \pkg{Stan} model
#    should be recompiled from the stored or updated \code{C++} code.
#    If \code{NULL} (the default), \code{\link[brms]{update.brmsfit}}
#    will try to figure out internally, if recompilation is necessary.
#    Setting it to \code{FALSE} will cause all \pkg{Stan} code changing
#    arguments to be ignored.
#  @param ... Other arguments passed to \code{\link[brms]{brm}}.
#'
#  @return An object of class \code{brmsfit}, which contains the posterior
#    samples along with many other useful information about the model. Use
#    \code{methods(class = "brmsfit")} for an overview on available methods.
#'
#' @seealso \code{\link[brms]{brm}},
#'   \code{\link[brms]{brmsfit}},
#'   \code{\link[brms]{update.brmsfit}},
#'   \code{\link[brms]{predict.brmsfit}},
#'   \code{\link[brms]{posterior_epred.brmsfit}},
#'   \code{\link[brms]{posterior_predict.brmsfit}},
#'   \code{\link[brms]{brmsformula}},
#'   \code{\link[brms]{brmsformula-helpers}},
#'   \code{\link[brms]{brmsterms}},
#'   \code{\link[brms]{brmsfamily}},
#'   \code{\link[brms]{customfamily}},
#'   \code{\link[stats]{family}},
#'   \code{\link[stats]{formula}},
#'   \code{\link[stats]{update.formula}}.
#'
#' @rdname bayesian
#' @export
bayesian_fit <- function(formula, data, ...) {
  dots <- list(formula = formula, data = rlang::enquo(data), ...)

  # Override the formula, if needed
  if (!is.null(dots$formula.override)) {
    if (inherits(
      dots$formula.override,
      c("formula", "list")
    )) {
      dots$formula <- dots$formula.override
    } else {
      rlang::abort("Unsupported or invalid formula.override!")
    }
  }
  dots$formula.override <- NULL

  # Simplify and expose the family call
  if (inherits(dots$family, c("family", "brmsfamily"))) {
    family_chr <- purrr::map_lgl(dots$family, is.character)
    family_func <- ifelse(
      inherits(dots$family, "brmsfamily"),
      "brmsfamily",
      dots$family$family
    )
    family_fmls <- rlang::fn_fmls_names(
      get(
        family_func,
        mode = "function"
      )
    )
    family_chrs <- intersect(names(family_chr[family_chr]), family_fmls)
    family_args <- dots$family[family_chrs]
    dots$family <- rlang::call2(family_func, !!!family_args)
  }

  # Pass extra arguments to Stan
  dots <- append(dots, dots$stan_args)
  dots$stan_args <- NULL

  # Create the fit call
  if (brms::is.brmsfit(dots$fit)) {
    dots$object <- dots$fit
    dots$fit <- NULL
    dots$data <- NULL

    fitcall <- rlang::call2("update", !!!dots, .ns = "stats")
  } else {
    dots$formula. <- NULL
    dots$newdata <- NULL
    dots$recompile <- NULL

    fitcall <- rlang::call2("brm", !!!dots, .ns = "brms")
  }

  # Evaluate the fit call
  rlang::eval_tidy(fitcall)
}

# -------------------------------------------------------------------------

#' Setup a formula for Bayesian models
#' @inheritParams brms::brmsformula
#  @param ... Other arguments passed to \code{\link[brms]{brmsformula}}.
#'
#  @return An object of class \code{brmsformula}, which
#    is essentially a \code{list} containing all model
#    formulas as well as some additional information.
#'
#' @rdname bayesian
#' @export
bayesian_formula <- function(formula, ...) {
  formula <- brms::brmsformula(formula, ...)
  class(formula) <- union(class(formula), "formula")
  return(formula)
}

# -------------------------------------------------------------------------

#' Parse a formula for Bayesian models
#' @inheritParams brms::brmsterms
#  @param ... Other arguments passed to \code{\link[brms]{brmsterms}}.
#'
#  @return An object of class \code{brmsterms} or \code{mvbrmsterms}
#    (for multivariate models), which is a \code{list} containing all
#    required information initially stored in \code{formula}
#    in an easier to use format, basically a list of formulas
#    (not an abstract syntax tree).
#'
#' @rdname bayesian
#' @export
bayesian_terms <- function(formula, ...) {
  brms::brmsterms(formula, ...)
}

# -------------------------------------------------------------------------

#' Special distribution families for \pkg{brms} models
#' @inheritParams brms::brmsfamily
#  @param ... Other arguments passed to \code{\link[brms]{brmsfamily}}.
#'
#  @return An \code{brmsfamily} object, which inherits from \code{family}.
#'
#' @rdname bayesian
#' @export
bayesian_family <- function(family, ...) {
  brms::brmsfamily(family, ...)
}

# -------------------------------------------------------------------------

#' Samples from the posterior predictive distribution
#' @inheritParams brms:::predict.brmsfit
#  @param object An instance of S3 class \code{brmsfit}; If \code{fit} is
#     of class \code{brmsfit}, the fitted model will be updated using
#     \code{\link[brms]{update.brmsfit}}.
#  @param ... Other arguments passed to \code{\link[brms]{predict.brmsfit}}.
#'
#  @return An \code{array} of predicted response values.
#    If \code{summary = FALSE} the output resembles those of
#    \code{\link{posterior_predict.brmsfit}}.
#    If \code{summary = TRUE} the output depends on the family: For categorical
#    and ordinal families, the output is an N x C matrix, where N is the number
#    of observations, C is the number of categories, and the values are
#    predicted category probabilities. For all other families, the output is a N
#    x E matrix where E = \code{2 + length(probs)} is the number of summary
#    statistics: The \code{Estimate} column contains point estimates (either
#    mean or median depending on argument \code{robust}), while the
#    \code{Est.Error} column contains uncertainty estimates (either standard
#    deviation or median absolute deviation depending on argument
#    \code{robust}). The remaining columns starting with \code{Q} contain
#    quantile estimates as specified via argument \code{probs}.
#'
#' @rdname bayesian
#' @export
bayesian_predict <- function(object, ...) {
  predict.brmsfit <- utils::getFromNamespace("predict.brmsfit", "brms")

  if (brms::is.brmsfit(object)) {
    predict.brmsfit(object, ...)
  } else if (brms::is.brmsfit(object$fit)) {
    predict.brmsfit(object$fit, ...)
  } else if (brms::is.brmsfit(object$fit$fit)) {
    predict.brmsfit(object$fit$fit, ...)
  } else if (brms::is.brmsfit(object$fit$fit$fit)) {
    predict.brmsfit(object$fit$fit$fit, ...)
  } else {
    rlang::abort("Unsupported or invalid model fit!")
  }
}

# -------------------------------------------------------------------------

#' Write an \code{brmsfit} object to a file
#  @param object An object of class \code{brmsfit}, which contains the posterior
#    samples along with many other useful information about the model. Use
#    \code{methods(class = "brmsfit")} for an overview on available methods.
#' @param file A character string of the file path to \code{brmsfit} object
#'   saved via \code{\link{saveRDS}}.
#'
#  @return NULL
#'
#' @rdname bayesian
#' @export
bayesian_write <- function(object, file) {
  write_brmsfit <- utils::getFromNamespace("read_brmsfit", "brms")
  write_brmsfit(object, file)
}

# -------------------------------------------------------------------------

#' Read an \code{brmsfit} object from a file
#' @param file A character string of the file path to \code{brmsfit} object
#'   saved via \code{\link{saveRDS}}.
#'
#  @return An object of class \code{brmsfit}, which contains the posterior
#    samples along with many other useful information about the model. Use
#    \code{methods(class = "brmsfit")} for an overview on available methods.
#'
#' @rdname bayesian
#' @export
bayesian_read <- function(file) {
  read_brmsfit <- utils::getFromNamespace("read_brmsfit", "brms")
  read_brmsfit(file)
}
