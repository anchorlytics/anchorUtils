# Helpers for creating Mplus input

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This function takes a character vector, concatenates it with spaces,
#' and word-wraps the result, returning a single string.
#'
#' @param .list a character vector
#' @param width passed to [stringi::stri_wrap()]
#' @param exdent passed to [stringi::stri_wrap()]
#' @param whitespace_only passed to [stringi::stri_wrap()]
#' @param ... passed to [stringi::stri_wrap()]
#' @return a single string, potentially with newlines
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(mp_wordwrap(rownames(mtcars)))
mp_wordwrap <- function(.list, width = 80, exdent = 4, whitespace_only = TRUE,
                        ...) {
  # stringr::str_wrap doesn't expose 'whitespace_only' option
  paste(
    stringi::stri_wrap(
      paste(.list, collapse = " "),
      width = width, exdent = exdent, whitespace_only = whitespace_only, ...),
    collapse = "\n")
}

#' Specify ordinal variables to Mplus
#'
#' This creates an Mplus input syntax line indicating which
#' variables are ordered categorical.
#'
#' @param .data data frame with proper column types
#' @param ... additional options passed to [mp_wordwrap()]
#' @return a single string that can be passed to
#'   [MplusAutomation::mplusObject()] in the `VARIABLE` argument
#'
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(mp_ordinal(data.frame(
#'   Number_of_Cylinders = ordered(mtcars$cyl),
#'   Miles_per_Gallon = mtcars$mpg,
#'   Engine_V_shaped_or_Straight = ordered(mtcars$vs),
#'   Transmission_Automatic_or_Manual = ordered(mtcars$am))))
mp_ordinal <- function(.data, ...) {
  mp_wordwrap(c("CATEGORICAL =", names(which(sapply(.data, is.ordered))), ";"),
              ...)
}
