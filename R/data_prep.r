# Helpers for data munging

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

