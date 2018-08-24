#' Pretty-print a proc_time object
#'
#' @param .pt A proc_time object, e.g., from [base::system.time()]
#'
#' @return A formatted string.
#' @importFrom purrr map
#' @importFrom lubridate seconds_to_period
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' fmt.proc_time(system.time(rnorm(1e7)))
fmt.proc_time <- function(.pt) {
  pt_sec = as.list(.pt)$elapsed
  pt_per = lubridate::seconds_to_period(pt_sec)

  units = c("day", "hour", "minute", "second")
  names(units) = units
  units_funs = purrr::map(units, get, asNamespace("lubridate"))
  units_call = purrr::map(units_funs, do.call, list(pt_per))
  units_pos = units_call[ units_call > 0 ]
  units_fmt = purrr::imap(units_pos, ~sprintf("%d %ss", floor(.x), .y))
  paste(units_fmt, collapse = " ")
}
