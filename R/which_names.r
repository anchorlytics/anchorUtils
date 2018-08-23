# Find indices of TRUE entries in an array

#' Find indices of TRUE entries in an array
#' 
#' `base::which(arr.ind = TRUE)` returns numeric indices of TRUE entries
#' in an array.
#' This wrapper uses `dimnames` to return pairs of row/column names.
#'
#' @param .a array (could be matrix)
#' @param dimnames list of character vectors.
#'   E.g., if `.a` is a matrix, then `length(dimnames) == 2`, 
#'   `dimnames[[1]]` is a vector of row names, and 
#'   `dimnames[[2]]` is a vector of column names.
#' @param ... Additional options passed to `base::which()``.
#'
#' @return a tibble of named indices
#' @importFrom dplyr %>% as_tibble()
#' @importFrom purrr map2
#' @export
#'
#' @examples
which_names <- function(.a, dimnames = NULL, ...) {
  if (!is.null(dimnames)) {
    dimnames(.a) <- dimnames
  }
  which(.a, arr.ind = TRUE, ...) %>% 
    dplyr::as_tibble() %>% 
    purrr::map2(seq_along(.), ~dimnames(.a)[[.y]][.x]) %>% 
    dplyr::as_tibble()
}

