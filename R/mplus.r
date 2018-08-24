# Helpers for creating Mplus input

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This wrapper around `stringi::stri_wrap()` takes a character vector,
#' concatenates it with spaces, and word-wraps the result.
#'
#' @param .list a character vector
#' @param width passed to `stringi::stri_wrap`
#' @param exdent passed to `stringi::stri_wrap`
#' @param whitespace_only passed to `stringi::stri_wrap`
#' @param ... passed to `stringi::stri_wrap`
#' @return a single string, potentially with newlines
#' @importFrom stringi stri_wrap
#' @family mplus
#'
#' @examples
#' cat(mp_wordwrap(rownames(mtcars)))
mp_wordwrap <- function(.list, width = 80, exdent = 4, whitespace_only = TRUE,
			                        ...) {
	  paste(.list, collapse = " ") %>%
		      # stringr::str_wrap doesn't expose 'whitespace_only' option
		      stringi::stri_wrap(
					       width = width, exdent = exdent, whitespace_only = whitespace_only,
					             ...) %>%
	    paste(collapse = "\n")
}

#' @title Specify ordinal variables to Mplus
#' @description This creates an Mplus input syntax line indicating which
#' variables are ordered categorical.
#' @param .data data frame with proper column types
#' @param ... additional options passed to `mp_wordwrap()``
#' @return a single string that can be passed to
#'   `MplusAutomation::mplusObject()` in the `VARIABLE` argument
#' @author Sean Ho `<anchor@seanho.com>``
#' @importFrom dplyr %>% select_if
#' @family mplus
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
  .data %>%
    select_if(is.ordered) %>%
    names() %>%
    c("CATEGORICAL =", ., ";") %>%
    mp_wordwrap(...)
}
