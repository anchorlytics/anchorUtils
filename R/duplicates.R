#' Show only duplicated rows
#'
#' Whereas [dplyr::distinct()] removes duplicates, this function
#' removes unique rows and only shows rows whose values in the specified
#' columns are duplicates.
#'
#' @param .data data frame
#' @param ... tidyselect specification of columns
#' @return grouped tibble of duplicate rows
#'
#' @import dplyr
#' @export
#'
#' @examples
#' duplicates(data.frame(x = 1:5, y = c(1, 2, 1, 4, 5)), dplyr::everything())
#'
duplicates <- function(.data, ...) {
  .data %>%
    # dplyr-1.0: group_by(across(!!!enquos(...)))
    group_by_at(vars(!!!enquos(...))) %>%
    filter(n() > 1)
}
