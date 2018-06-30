# save and load to/from RDS

#' Save multiple objects as RDS files
#'
#' @param .list character vector of names of objects in environment
#' @param ... path components relative to project root
#'
#' @return
#' @export
#'
#' @examples
#' # Creates files "data/rds/mtcars.rds" and "data/rds/letters.rds"
#' # relative to project root
#' save_all(c("mtcars", "letters"), "data", "rds")
save_all <- function(.list, ...) {
  path = list(...)
  .list %>%
    map(~list(get(.), here(path, paste0(., ".rds")))) %>%
    transpose() %>%
    pwalk(saveRDS)
}

#' Load multiple RDS files into current environment
#'
#' @param .list character vector of names
#' @param ... path components relative to project root
#'
#' @return
#' @export
#'
#' @examples
#' # Reads from "data/rds/mtcars.rds" and "data/rds/letters.rds"
#' # relative to project root
#' read_all(c("mtcars", "letters"), "data", "rds")
read_all <- function(.list, ...) {
  path = list(...)
  .list %>%
    set_names %>%
    map(~here(path, paste0(., ".rds") )) %>%
    map(readRDS) %>%
    iwalk(~assign(.y, .x, .GlobalEnv))
}
