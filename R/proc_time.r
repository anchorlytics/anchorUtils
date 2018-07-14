#' Pretty-print a proc_time object
#'
#' @param pt A proc_time object, e.g., from system.time()
#'
#' @return A formatted string.
#' @export
#'
#' @examples
#' fmt.proc_time(system.time(rnorm(1e7)))
fmt.proc_time <- function(pt) {
  pt_per <- pt %>%
    as.list() %>%
    .$elapsed %>%
    seconds_to_period()

  c("day", "hour", "minute", "second") %>%
    set_names() %>%
    map(do.call, list(pt_per)) %>%
    .[.>0] %>%
    imap(~sprintf("%d %ss", floor(.x), .y)) %>%
    paste(collapse = " ")
}
