#' General Interface for Bayesian TidyModels
#'
#' `bayesian()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  \pkg{Stan} via \pkg{brms} package in R. The main
#'  arguments for the model are:
#' \itemize{
#'   \item \code{prior} One or more \code{brmsprior} objects created by
#'   \code{\link[brms]{set_prior}} or related functions and combined using
#'   the \code{c} method or the \code{+} operator. See also
#'   \code{\link[brms]{get_prior}} for more help.
#'   \item \code{inits} Either \code{"random"} or \code{"0"}. If inits is
#'   \code{"random"} (the default), Stan will randomly generate initial values
#'   for parameters. If it is \code{"0"}, all parameters are initialized to
#'   zero. This option is sometimes useful for certain families, as it happens
#'   that default (\code{"random"}) inits cause samples to be essentially
#'   constant. Generally, setting \code{inits = "0"} is worth a try, if chains
#'   do not behave well. Alternatively, \code{inits} can be a list of lists
#'   containing the initial values, or a function (or function name) generating
#'   initial values. The latter options are mainly implemented for internal
#'   testing but are available to users if necessary. If specifying initial
#'   values using a list or a function then currently the parameter names must
#'   correspond to the names used in the generated Stan code (not the names
#'   used in \R). For more details on specifying initial values you can consult
#'   the documentation of the selected \code{backend}.
#'   \item \code{chains} Number of Markov chains (defaults to 4).
#'   \item \code{iter} Number of total iterations per chain (including warmup;
#'   defaults to 2000).
#'   \item \code{warmup} A positive integer specifying number of warmup
#'   (aka burnin) iterations. This also specifies the number of iterations
#'   used for stepsize adaptation, so warmup samples should not be used for
#'   inference. The number of warmup should not be larger than \code{iter}
#'   and the default is \code{iter/2}.
#'   \item \code{thin} Thinning rate. Must be a positive integer. Set
#'   \code{thin > 1} to save memory and computation time if \code{iter} is
#'   large.
#'   \item \code{cores} Number of cores to use when executing the chains in
#'   parallel, which defaults to 1 but we recommend setting the \code{mc.cores}
#'   option to be as many processors as the hardware and RAM allow (up to the
#'   number of chains). For non-Windows OS in non-interactive \R sessions,
#'   forking is used instead of PSOCK clusters.
#'   \item \code{algorithm} Character string naming the estimation approach
#'   to use. Options are \code{"sampling"} for MCMC (the default),
#'   \code{"meanfield"} for variational inference with independent normal
#'   distributions, \code{"fullrank"} for variational inference with
#'   a multivariate normal distribution, or \code{"fixed_param"} for sampling
#'   from fixed parameter values. Can be set globally for the current \R session
#'   via the \code{"brms.algorithm"} option (see \code{\link{options}}).
#'   \item \code{backend} Character string naming the package to use as the
#'   backend for fitting the Stan model. Options are \code{"rstan"} (the
#'   default) or \code{"cmdstanr"}. Can be set globally for the current \R
#'   session via the \code{"brms.backend"} option (see \code{\link{options}}).
#'   Details on the \pkg{rstan} and \pkg{cmdstanr} packages are available at
#'   \url{https://mc-stan.org/rstan/} and \url{https://mc-stan.org/cmdstanr/},
#'   respectively.
#'   \item \code{control} A named \code{list} of parameters to control the
#'   sampler's behavior. It defaults to \code{NULL} so all the default values
#'   are used.
#'   The most important control parameters are discussed in the 'Details'
#'   section below. For a comprehensive overview see
#'   \code{\link[rstan]{stan}}.
#'   \item \code{future} Logical; If \code{TRUE}, the
#'   \pkg{\link[future]{future}} package is used for parallel execution of
#'   the chains and argument \code{cores} will be ignored. Can be set
#'   globally for the current \R session via the \code{future} option.
#'   The execution type is controlled via \code{\link[future]{plan}}
#'   (see the examples section below).
#'   \item \code{seed} The seed for random number generation to make results
#'   reproducible. If \code{NA} (the default), \pkg{Stan} will set the seed
#'   randomly.
#'   \item \code{refresh} (non-negative integer) The number of iterations
#'   between printed screen updates. If `refresh = 0`, only error messages
#'   will be printed.
#'   \item \code{silent} Logical; If \code{TRUE} (the default), most of the
#'   informational messages of compiler and sampler are suppressed. The actual
#'   sampling progress is still printed. Set \code{refresh = 0} to turn this off
#'   as well. If using \code{backend = "rstan"} you can also set
#'   \code{open_progress = FALSE} to prevent opening additional progress bars.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param prior One or more \code{brmsprior} objects created by
#'   \code{\link[brms]{set_prior}} or related functions and combined using the
#'   \code{c} method or the \code{+} operator.
#'   See also \code{\link[brms]{get_prior}} for more help.
#' @param inits Either \code{"random"} or \code{"0"}. If inits is
#'   \code{"random"} (the default), Stan will randomly generate initial values
#'   for parameters. If it is \code{"0"}, all parameters are initialized to
#'   zero. This option is sometimes useful for certain families, as it happens
#'   that default (\code{"random"}) inits cause samples to be essentially
#'   constant. Generally, setting \code{inits = "0"} is worth a try, if chains
#'   do not behave well. Alternatively, \code{inits} can be a list of lists
#'   containing the initial values, or a function (or function name) generating
#'   initial values. The latter options are mainly implemented for internal
#'   testing but are available to users if necessary. If specifying initial
#'   values using a list or a function then currently the parameter names must
#'   correspond to the names used in the generated Stan code (not the names
#'   used in \R). For more details on specifying initial values you can consult
#'   the documentation of the selected \code{backend}.
#' @param chains Number of Markov chains (defaults to 4).
#' @param iter Number of total iterations per chain (including warmup; defaults
#'   to 2000).
#' @param warmup A positive integer specifying number of warmup (aka burnin)
#'   iterations. This also specifies the number of iterations used for stepsize
#'   adaptation, so warmup samples should not be used for inference. The number
#'   of warmup should not be larger than \code{iter} and the default is
#'   \code{iter/2}.
#' @param thin Thinning rate. Must be a positive integer. Set \code{thin > 1} to
#'   save memory and computation time if \code{iter} is large.
#' @param cores Number of cores to use when executing the chains in parallel,
#'   which defaults to 1 but we recommend setting the \code{mc.cores} option to
#'   be as many processors as the hardware and RAM allow (up to the number of
#'   chains). For non-Windows OS in non-interactive \R sessions, forking is used
#'   instead of PSOCK clusters.
#' @param algorithm Character string naming the estimation approach to use.
#'   Options are \code{"sampling"} for MCMC (the default), \code{"meanfield"}
#'   for variational inference with independent normal distributions,
#'   \code{"fullrank"} for variational inference with a multivariate normal
#'   distribution, or \code{"fixed_param"} for sampling from fixed parameter
#'   values. Can be set globally for the current \R session via the
#'   \code{"brms.algorithm"} option (see \code{\link{options}}).
#' @param backend Character string naming the package to use as the backend for
#'   fitting the Stan model. Options are \code{"rstan"} (the default) or
#'   \code{"cmdstanr"}. Can be set globally for the current \R session via the
#'   \code{"brms.backend"} option (see \code{\link{options}}). Details on the
#'   \pkg{rstan} and \pkg{cmdstanr} packages are available at
#'   \url{https://mc-stan.org/rstan/} and \url{https://mc-stan.org/cmdstanr/},
#'   respectively.
#' @param control A named \code{list} of parameters to control the sampler's
#'   behavior. It defaults to \code{NULL} so all the default values are used.
#'   The most important control parameters are discussed in the 'Details'
#'   section below. For a comprehensive overview see
#'   \code{\link[rstan]{stan}}.
#' @param future Logical; If \code{TRUE}, the \pkg{\link[future]{future}}
#'   package is used for parallel execution of the chains and argument
#'   \code{cores} will be ignored. Can be set globally for the current \R
#'   session via the \code{future} option. The execution type is controlled via
#'   \code{\link[future]{plan}} (see the examples section below).
#' @param seed The seed for random number generation to make results
#'   reproducible. If \code{NA} (the default), \pkg{Stan} will set the seed
#'   randomly.
#' @param refresh (non-negative integer) The number of iterations between
#'   printed screen updates. If `refresh = 0`, only error messages will be
#'   printed.
#' @param silent Logical; If \code{TRUE} (the default), most of the
#'   informational messages of compiler and sampler are suppressed. The actual
#'   sampling progress is still printed. Set \code{refresh = 0} to turn this off
#'   as well. If using \code{backend = "rstan"} you can also set
#'   \code{open_progress = FALSE} to prevent opening additional progress bars.
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `bayesian()`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{brms}:  `"stan"`
#' }
#'
#' @includeRmd man/rmd/bayesian-engine.Rmd details
#'
#' For prediction, the `stan` engine can compute posterior
#'  intervals analogous to confidence and prediction intervals. In
#'  these instances, the units are the original outcome and when
#'  `std_error = TRUE`, the standard deviation of the posterior
#'  distribution (or posterior predictive distribution as
#'  appropriate) is returned.
#'
#' @examples
#' bayesian()
#'
#' show_model_info("bayesian")
#'
#' bayesian(mode = "classification")
#' bayesian(mode = "regression")
#' \dontrun{
#' bayesian_mod <-
#'   bayesian() %>%
#'   set_engine("stan") %>%
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
           prior = NULL,
           inits = NULL,
           chains = NULL,
           iter = NULL,
           warmup = NULL,
           thin = NULL,
           cores = NULL,
           algorithm = NULL,
           backend = NULL,
           control = NULL,
           future = NULL,
           seed = NULL,
           refresh = NULL,
           silent = NULL) {
    args <- list(
      prior = rlang::enquo(prior),
      inits = rlang::enquo(inits),
      chains = rlang::enquo(chains),
      iter = rlang::enquo(iter),
      warmup = rlang::enquo(warmup),
      thin = rlang::enquo(thin),
      cores = rlang::enquo(cores),
      algorithm = rlang::enquo(algorithm),
      backend = rlang::enquo(backend),
      control = rlang::enquo(control),
      future = rlang::enquo(future),
      seed = rlang::enquo(seed),
      refresh = rlang::enquo(refresh),
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
    message("Used `engine = 'stan'` for translation.")
    engine <- "stan"
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
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
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
           prior = NULL,
           inits = NULL,
           chains = NULL,
           iter = NULL,
           warmup = NULL,
           thin = NULL,
           cores = NULL,
           algorithm = NULL,
           backend = NULL,
           control = NULL,
           future = NULL,
           seed = NULL,
           refresh = NULL,
           silent = NULL,
           fresh = FALSE,
           ...) {
    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
      prior = rlang::enquo(prior),
      inits = rlang::enquo(inits),
      chains = rlang::enquo(chains),
      iter = rlang::enquo(iter),
      warmup = rlang::enquo(warmup),
      thin = rlang::enquo(thin),
      cores = rlang::enquo(cores),
      algorithm = rlang::enquo(algorithm),
      backend = rlang::enquo(backend),
      control = rlang::enquo(control),
      future = rlang::enquo(future),
      seed = rlang::enquo(seed),
      refresh = rlang::enquo(refresh),
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

#' @inherit brms:::predict.brmsfit
#' @rdname bayesian
#' @export
bayesian_predict <- utils::getFromNamespace("predict.brmsfit", "brms")

# -------------------------------------------------------------------------

#' read a brmsfit object from a file

#' @param file A character string of the file path to \code{brmsfit} object
#'   saved via \code{\link{saveRDS}}.
#' @return An object of class \code{brmsfit}, which contains the posterior
#'   samples along with many other useful information about the model. Use
#'   \code{methods(class = "brmsfit")} for an overview on available methods.
#' @rdname bayesian
#' @export
bayesian_read <- utils::getFromNamespace("read_brmsfit", "brms")

# -------------------------------------------------------------------------

# write a brmsfit object to a file
#' @param x An object of class \code{brmsfit}, which contains the posterior
#'   samples along with many other useful information about the model. Use
#'   \code{methods(class = "brmsfit")} for an overview on available methods.
#' @param file A character string of the file path to \code{brmsfit} object
#'   saved via \code{\link{saveRDS}}.
#' @return NULL
#' @rdname bayesian
#' @export
bayesian_write <- utils::getFromNamespace("write_brmsfit", "brms")
