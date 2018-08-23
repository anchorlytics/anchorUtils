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
#' @importFrom dplyr mutate_if
ord_collapse <- function(.data, maxlev = 8, newlev = maxlev) {
  mutate_if(
    .data,
    ~is.factor(.) & is.ordered(.) & nlevels(.) > maxlev,
    ~as.integer(levels(.))[.] %>% 
      cut(newlev, labels = 1:newlev, ordered_result = TRUE))
}

