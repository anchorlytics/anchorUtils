#' List all ordered factors in a data frame
#'
#' @param data data frame
#' @return character vector of column names
#'
#' @import dplyr
#'
#' @export
#' @family Amelia imputation
#'
#' @examples
#' get_ords(data.frame(
#'   x = factor(LETTERS[1:5]),
#'   y = factor(LETTERS[6:10], ordered = TRUE)
#' ))
#'
get_ords <- function(data) {
  names(select(data, tidyselect::vars_select_helpers$where(is.ordered)))
}

#' List all unordered factors in a data frame
#'
#' @param data data frame
#' @return character vector of column names
#'
#' @import dplyr
#'
#' @export
#' @family Amelia imputation
#'
#' @examples
#' get_noms(data.frame(
#'   x = factor(LETTERS[1:5]),
#'   y = factor(LETTERS[6:10], ordered = TRUE)
#' ))
#'
get_noms <- function(data) {
  names(select(data, tidyselect::vars_select_helpers$where(
    ~is.factor(.) & !is.ordered(.))))
}

#' Run Amelia imputation on data with factors
#'
#' Wrapper around [Amelia::amelia()] filling in the `ords` and `noms` args.
#'
#' @param x matrix, data.frame, "amelia" object, or "molist" object: input data
#' @param boot.type "ordinary" to enable bootstrap stderr estimates
#' @param ... other args passed to [Amelia::amelia()]
#' @return "amelia" object containing imputed datasets
#'
#' @export
#' @family Amelia imputation
#'
run_amelia <- function(x, boot.type = "none", ...) {
  Amelia::amelia(as.data.frame(x), boot.type = boot.type, ...,
                 ords = get_ords(x), noms = get_noms(x))
}
