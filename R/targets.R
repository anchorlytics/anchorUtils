#' Wrapper around [targets::tar_meta()]
#'
#' @param names char vec names of targets
#' @param ... other args passed to [targets::tar_meta()]
#' @return single char string, formatted duration
#'
#' @export
#' @family target helpers
#'
tgt_time <- function(names = NULL, ...) {
  targets::tar_meta(any_of(names), ...)$seconds %>%
    lubridate::dseconds() %>%
    format()
}
