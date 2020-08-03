# Helpers for data munging

#' Dummy-code (one-hot) factors
#'
#' Dummy coding expands a polytomous categorical factor into several
#' indicator variables, each of which is logical (Boolean, TRUE/FALSE).
#'
#' The one-hot contrast expands a factor with `L` levels into `L`
#' indicator variables.  The result is not a full-rank parameterisation,
#' however it avoids needing to select a reference level.
#'
#' Ordinal (ordered categorical) and dichotomous (only 2 levels) variables are
#' not modified; nor are non-factors (numeric, logical, etc.).
#'
#' Note that the number of levels is determined from the unique values in the
#' actual data, rather than the factor levels.
#'
#' The new indicator variables are appended to the end of the variable list,
#' and the original factors are removed.
#'
#' @details # TODO
#' + if .data has only one factor, it is renamed "data"
#'
#' @param .data data frame
#' @return data frame with selected factors converted to logical dummy variables
#'
#' @export
#' @family data munging
#'
#' @examples
#' library(dplyr)
#' mtcars %>% mutate(across(c("cyl", "gear"), as.factor)) %>% dummy_code()
#'
dummy_code <- function(.data) {
  # logical vector
  noms <-
    sapply(.data, function(col) {
      is.factor(col) & !is.ordered(col) &
        length(stats::na.omit(unique(col))) > 2
    })

  if (!any(noms)) {
    return(.data)
  }

  # add terms attribute to orig data frame
  nom_frame <- stats::model.frame(~., .data[, noms], na.action = "na.pass")

  # data frame with only dummy vars
  nom_df <-
    sapply(
      as.data.frame(stats::model.matrix(
        ~.-1, data = nom_frame,
        contrasts.arg = lapply(nom_frame, stats::contrasts, contrasts = FALSE)
      )),
      as.logical
    )

  cbind(.data[, !noms], nom_df)
}

#' Compress integer ordinal factors with too many levels
#'
#' Polychoric correlation routines often limit the number of levels an
#' ordinal (ordered categorical) factor can have.
#' This function is like [forcats::fct_collapse()]
#' but preserves the ordering of levels.
#' Modified factors are releveled to 1:n.
#'
#' @param .data data frame with ordinal factors coded as integers
#' @param maxlev apply level collapsing only if `nlevels` is greater than this
#' @param newlev if collapsing levels, how many levels should the factor have.
#'   Default is `maxlev`.
#' @return data frame with same columns
#'
#' @import dplyr
#'
#' @export
#' @family data munging
#'
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   mutate(across(c("cyl", "vs", "am", "gear", "carb", "disp"), as.ordered)) %>%
#'   ord_collapse() %>%
#'   summarise_all(nlevels)
#'
ord_collapse <- function(.data, maxlev = 10, newlev = maxlev) {
  .data %>%
    mutate(across(tidyselect::vars_select_helpers$where(
      ~is.factor(.) & is.ordered(.) & nlevels(.) > maxlev),
      ~cut(as.integer(levels(.))[.], newlev, labels = 1:newlev,
           ordered_result = TRUE)
    ))
}

#' Near-Zero Variance filter
#'
#' Removes numeric variables with very little variability.
#' This can, for instance, aid stability of inverting covariance matrices for
#' factor analysis.
#'
#' @details # TODO
#' + remove dependency on caret
#'
#' @param .data data frame
#' @param ... other options passed through to [caret::nearZeroVar()]
#' @return data frame, potentially with fewer columns
#'
#' @export
#' @family data munging
#'
#' @examples
#' drop_nzv(mtcars, freqCut = 1.4)
#'
drop_nzv <- function(.data, ...) {
  nzv <- caret::nearZeroVar(.data, ...)
  .data[-1*nzv]
}
