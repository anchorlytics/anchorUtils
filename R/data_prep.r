# Helpers for data munging

#' @title Dummy-code (one-hot) factors into multiple indicators
#'
#' @description
#' Dummy coding expands a polytomous categorical factor into several variables,
#' each of which is 0/1.
#' The one-hot contrast expands a factor with $L$ levels into $L$ indicator
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
#'
#' @importFrom dplyr %>% quos select select_if one_of as_tibble mutate_all
#' @family data
#' @author Sean Ho `<anchor@seanho.com>`
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
    bind_cols(
      select(.data, one_of(noms)) %>%
        model.frame(~., ., na.action = "na.pass") %>%
        model.matrix(
          ~.-1, data = .,
          contrasts.arg = lapply(., contrasts, contrasts = FALSE)
        ) %>%
        as_tibble() %>%
        mutate_all(as.integer)
    )
}

#' Compress ordinal factors with too many levels
#'
#' Polychoric correlation routines often limit the number of levels an
#' ordinal (ordered categorical) factor can have.
#' This function is like `forcats::fct_collapse` but preserves the ordering
#' of levels.
#'
#' @param .data data frame with proper column types
#' @param maxlev apply level collapsing only if `nlevels` is greater than this
#' @param newlev if collapsing levels, how many levels should the factor have.
#'   Default is `maxlev`.
#' @return data frame with same columns
#' @importFrom dplyr mutate_if
#' @family data
#' @examples
#' library(dplyr)
#' as_tibble(mtcars) %>%
#'   mutate_at(c("cyl", "vs", "am", "gear", "carb"), as.ordered) %>%
#'   mutate_at("disp", as.ordered) %>%
#'   ord_collapse() %>%
#'   summarise_all(nlevels)
ord_collapse <- function(.data, maxlev = 10, newlev = maxlev) {
  mutate_if(
    .data,
    ~is.factor(.) & is.ordered(.) & nlevels(.) > maxlev,
    ~as.integer(levels(.))[.] %>%
      cut(newlev, labels = 1:newlev, ordered_result = TRUE))
}

#' Near-Zero Variance filter
#'
#' Removes numeric variables with very little variability.
#' This can, for instance, aid stability of inverting covariance matrices for
#' factor analysis.
#' @param .data data frame
#' @param ... other options passed through to `caret::nearZeroVar``
#' @return data frame, potentially with fewer columns
#' @importFrom dplyr select one_of
#' @importFrom caret nearZeroVar
#' @family data
#' @examples
#' drop_nzv(mtcars, freqCut = 1.4)
drop_nzv <- function(.data, ...) {
  nzv <- nearZeroVar(.data, names = TRUE, ...)
  select(.data, -one_of(nzv))
}

