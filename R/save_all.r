# save and load to/from RDS

#' @title Save multiple objects as RDS files
#'
#' @param .list character vector of names of objects in environment
#' @param ... path components relative to project root
#' @return None, global environment is modified
#'
#' @family io
#'
#' @examples
#' # Creates files "data/rds/mtcars.rds" and "data/rds/letters.rds"
#' # relative to project root
#' save_all(c("mtcars", "letters"), "data", "rds")
save_all <- function(.list, ...) {
  rdsdir = here::here(...)
  vars = .list
  objs = purrr::map(vars, get)
  paths = purrr::map(vars, ~file.path(rdsdir, paste0(., ".rds")))
  purrr::walk2(objs, paths, saveRDS)
}

#' @title Load multiple RDS files into current environment
#'
#' @param .list character vector of names
#' @param ... path components relative to project root
#' @return None, global environment is modified
#'
#' @family io
#'
#' @examples
#' # Reads from "data/rds/my_mtcars.rds" and "data/rds/my_letters.rds"
#' # relative to project root
#' read_all(c("my_mtcars", "my_letters"), "data", "rds")
read_all <- function(.list, ...) {
  rdsdir = here::here(...)
  vars = .list
  paths = purrr::map(vars, ~file.path(rdsdir, paste0(., ".rds")))
  objs = purrr::map(paths, readRDS)
  purrr::walk2(vars, objs, assign, .GlobalEnv)
}
