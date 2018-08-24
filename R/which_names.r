#' Find indices of TRUE entries in an array
#'
#' [base::which()] returns numeric indices of TRUE entries in an array.
#' This wrapper uses `dimnames` to return pairs of row/column names.
#'
#' @param .a array
#' @param dimnames list of character vectors.
#'   E.g., if `.a` is a matrix, then `length(dimnames) == 2`,
#'   `dimnames[[1]]` is a vector of row names, and
#'   `dimnames[[2]]` is a vector of column names.
#' @param ... Additional options passed to [base::which()].
#'
#' @export
#' @return a data frame of named indices
#'
#' @examples
#' which_names(mtcars[1:6, c("vs", "am")] == 1)
#' which_names(matrix(1:20, nrow = 4) %% 3 == 0)
which_names <- function(.a, dimnames = NULL, ...) {
  if (is.null(dimnames)) {
    dimnames <- dimnames(.a)
  }

  wh <- as.data.frame(which(.a, arr.ind = TRUE, ...))
  if (is.null(dimnames)) {
    return(wh)
  }

  as.data.frame(mapply(`[`, dimnames, wh))
}
