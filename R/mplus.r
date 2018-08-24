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
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' cat(mp_wordwrap(rownames(mtcars)))
mp_wordwrap <- function(.list, width = 80, exdent = 4, whitespace_only = TRUE,
                        ...) {
  paste(.list, collapse = " ") %>%
    # stringr::str_wrap doesn't expose 'whitespace_only' option
    stringi::stri_wrap(width = width, exdent = exdent,
                       whitespace_only = whitespace_only, ...) %>%
    paste(collapse = "\n")
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
#' @importFrom dplyr select_if
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' library(dplyr)
#' as_tibble(mtcars) %>%
#'   mutate_at(c("cyl", "vs", "am"), as.ordered) %>%
#'   rename(`Number of Cylinders` = cyl,
#'          `Engine V-shaped or Straight` = vs,
#'          `Transmission Automatic or Manual` = am) %>%
#'   mp_ordinal() %>%
#'   cat()
mp_ordinal <- function(.data, ...) {
  mp_wordwrap(c("CATEGORICAL =", names(select_if(.data, is.ordered)), ";"),
              ...)
}
