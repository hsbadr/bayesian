#' @import brms
#' @import parsnip
#' @importFrom dplyr bind_cols
#' @importFrom purrr map_lgl
#' @importFrom rlang enquo expr
#' @importFrom stats sd
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
