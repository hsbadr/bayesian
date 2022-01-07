#' @import brms
#' @import parsnip
#' @importFrom dplyr between bind_cols
#' @importFrom purrr map_lgl
#' @importFrom rlang !!! abort call2 enquo eval_tidy expr fn_fmls_names warn
#' @importFrom stats sd update
#' @importFrom tibble as_tibble tibble
#' @importFrom utils globalVariables getFromNamespace

# -------------------------------------------------------------------------

utils::globalVariables(
  c(".pred", "group", "level", "new_data", "object")
)

# -------------------------------------------------------------------------
# The generic for predict_raw is not exported so make one here (if needed)

if (!any(getNamespaceExports("parsnip") == "predict_raw")) {
  predict_raw <- function(object, ...) {
    UseMethod("predict_raw")
  }
}
