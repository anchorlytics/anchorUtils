# convert factors into multiple indicators

#' Dummy-code (one-hot) factors
#'
#' Dummy coding expands a polytomous categorical factor into several variables,
#' each of which is 0/1.
#' The one-hot contrast expands a factor with `L` levels into `L` indicator
#' variables.  The result is not a full-rank parameterisation,
#' however it avoids needing to select a reference level.
#'
#' Ordinal (ordered categorical) and dichotomous (only 2 levels) variables are
#' not modified; nor are non-factors (numeric, logical, etc.).
#' Additional arguments are passed to dplyr::select() to further filter the
#' list of variables eligible for dummy coding.
#'
#' The new indicator variables are appended to the end of the variable list,
#' and the original factors are removed.
#'
#' @param .data data frame or tibble
#' @param ... One or more unquoted expressions separated by commas,
#'   as passed to dplyr::select().
#'   If omitted, all variables are considered.
#'
#' @return A tibble with selected factors converted to 0/1 dummy variables
#' @importFrom dplyr %>% quos select select_if one_of as_tibble
#' @export
#'
#' @examples
#' mutate_at(mtcars, c("cyl", "gear"), as.factor) %>% dummy_code()
#'
#' mutate_at(mtcars, c("cyl", "gear"), as.factor) %>% dummy_code(-cyl)
dummy_code <- function(.data, ...) {
  sel = quos(...)
  if (length(sel) < 1) {
    sel = quos(everything())
  }

  noms <-
    .data %>%
    select(!!!sel) %>%
    select_if(~is.factor(.) & !is.ordered(.)) %>%
    select_if(~length(na.omit(unique(.))) > 2) %>%
    names()

  .data %>%
    select(-one_of(noms)) %>%
    cbind(
      select(.data, one_of(noms)) %>%
        model.matrix(
          ~.-1, data = .,
          contrasts.arg = lapply(., contrasts, contrasts = FALSE)
        )
    ) %>%
    as_tibble()
}
