# save and load to/from RDS

#' @title Save multiple objects as RDS files
#'
#' @details # DEPRECATED
#' Obsoleted by drake caching
#'
#' @param .list character vector of names of objects in environment
#' @param rdsdir path to directory, passed to [base::file.path()].
#'   If not specified, [base::tempdir()].
#' @param ... additional options passed to [base::saveRDS()]
#' @return None, global environment is modified
#'
#' @export
#' @family saving/loading data
#'
#' @examples
#' # Saves/loads files "mtcars.rds" and "letters.rds" in tmpdir
#' my_mtcars <- mtcars; my_letters <- letters
#' save_all(c("my_mtcars", "my_letters"))
#' rm(my_mtcars, my_letters)
#' read_all(c("my_mtcars", "my_letters"))
save_all <- function(.list, rdsdir = NULL, ...) {
  if (is.null(rdsdir)) {
    rdsdir <- tempdir()
  }
  sapply(.list, function(name) {
    saveRDS(get(name), file.path(rdsdir, paste0(name, ".rds")), ...)
  })
  invisible()
}

#' Load multiple RDS files into current environment
#'
#' @details # DEPRECATED
#' Obsoleted by [drake::loadd()]
#'
#' @param .list character vector of names
#' @param rdsdir path to directory, passed to [base::file.path()].
#'   If not specified, [base::tempdir()].
#' @param ... additional options passed to [base::readRDS()]
#' @return None, global environment is modified
#'
#' @export
#' @family saving/loading data
#' @author Sean Ho <anchor@seanho.com>
#'
#' @examples
#' # Saves/loads files "mtcars.rds" and "letters.rds" in tmpdir
#' my_mtcars <- mtcars; my_letters <- letters
#' save_all(c("my_mtcars", "my_letters"))
#' rm(my_mtcars, my_letters)
#' read_all(c("my_mtcars", "my_letters"))
read_all <- function(.list, rdsdir = NULL, ...) {
  if (is.null(rdsdir)) {
    rdsdir <- tempdir()
  }
  sapply(.list, function(name) {
    assign(name, readRDS(file.path(rdsdir, paste0(name, ".rds")), ...),
           envir = .GlobalEnv)
  })
  invisible()
}
