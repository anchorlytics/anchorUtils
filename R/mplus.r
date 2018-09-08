# Helpers for creating Mplus input

#' Word wrap to accommodate Mplus line length limitations.
#'
#' Mplus input syntax is limited to 90 characters on a line.
#' This function takes a character vector, concatenates it with spaces,
#' and word-wraps the result, returning a single string.
#'
#' @param .list a character vector
#' @param width passed to [base::strwrap()]
#' @param exdent passed to [base::strwrap()]
#' @param ... passed to [base::strwrap()]
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
  paste(
    strwrap(paste(.list, collapse = " "), width = width, exdent = exdent, ...),
    collapse = "\n")
}

#' Specify categorical variables to Mplus
#'
#' This creates an Mplus input syntax line indicating which
#' variables are categorical (ordinal or binary/dichotomous).
#' A variable is deemed dichotomous if it has at most two unique non-NA values,
#' regardless of its type.
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
#' cat(mp_cat(data.frame(
#'   Number_of_Cylinders = ordered(mtcars$cyl),
#'   Miles_per_Gallon = mtcars$mpg,
#'   Engine_V_shaped_or_Straight = ordered(mtcars$vs),
#'   Transmission_Automatic_or_Manual = ordered(mtcars$am))))
mp_cat <- function(.data, ...) {
  mp_wordwrap(c(
    "CATEGORICAL =",
    names(which(sapply(.data, function(col) {
      is.ordered(col) | length(na.omit(unique(col))) <= 2
    }))),
    ";"), ...)
}

#' Parse Mplus TECH4 output
#'
#' Mplus' `TECH4` output contains first- and second-order estimated **moments**
#' for the latent factors.
#' Unfortunately, the `MplusAutomation` library is still buggy
#' in parsing this section; hence this function.
#'
#' For **single-group** models, the TECH4 section has one subsection,
#' containing the following moments estimated for the latent factors:
#'
#' 1. **Means**
#' 2. **Covariance** matrix
#' 3. **Correlation** matrix
#'
#' If **standard errors** are available, a second subsection is added with
#' the following for **each** of the means, covariance, and correlation:
#'
#' 1. **Standard error**
#' 2. **Ratio** of SE/estimate
#' 3. **P-value** on estimate differing from zero
#'
#' Hence, there are 9 items within this second subsection.
#'
#' For **multi-group** models, each group has its own subsection.
#' If standard errors are available, there are **twice** as many subsections as
#' there are groups: first all the moments for each of the groups, then all the
#' standard errors for each of the groups.
#'
#' @param .text contents of an Mplus output file, as read by readLines()
#' @return a list with one entry for each group.
#'   Each entry has members 'cov' and 'cor',
#'   each of which has members 'est', 'se', 'ratio', and 'p',
#'   each of which is a matrix.
#' @export
#' @family Mplus helpers
#' @author Sean Ho <anchor@seanho.com>
#' @examples
#' # Not run
#' parse_tech4(readLines("mplus.out"))
parse_tech4 <- function(.text) {
  output <- MplusAutomation:::parse_into_sections(.text)
  tech4 <- MplusAutomation:::getSection("^TECHNICAL 4 OUTPUT$", output)
  t4sec <- MplusAutomation:::getMultilineSection(
    ".* DERIVED .*", tech4, allowMultiple = TRUE)

  # TODO: if no groups, this paragraph produces one group named "X"
  headers <- tech4[attr(t4sec, "matchlines")]
  headers <- trimws(headers)
  groups <- sapply(headers, function(h) {
    sub("^ESTIMATES DERIVED FROM THE MODEL( FOR )?", "", h)
  })
  groups <- make.names(groups)

  st <- list(
    est = "",
    se = "s.e. for",
    ratio = "est./s.e. for",
    p = "two-tailed p-value for"
  )
  moments <- list(
    #    mean = "means", # TODO: vector, not matrixExtract()
    cov = "covariance matrix",
    cor = "correlation matrix"
  )
  suffix <- "for the latent variables"

  # initialise result
  result <- list()
  for (gp in unique(groups)) {
    for (m in names(moments)) {
      result[[gp]][[m]] <- st
    }
  }

  # given a subsection within TECH4, extract all known matrices
  store_matrices <- function(sec, gp) {
    # fix est/SE ratio on diagonal of cov
    sec <- gsub("***********", "0.000", sec, fixed = TRUE)
    # iterate over possible entries
    for (m in names(moments)) {
      for (s in names(st)) {
        tag <- paste("^\\s*", st[[s]], "estimated", moments[[m]], suffix)
        # check if entry exists in current subsection
        if (any(grepl(tag, sec, ignore.case = TRUE))) {
          mat <- MplusAutomation:::matrixExtract(sec, tag, ignore.case = TRUE)
          mat <- ifelse(is.na(mat), t(mat), mat)
          result[[gp]][[m]][[s]] <<- mat
        }
      }
    }
  }

  # iterate over subsections
  mapply(store_matrices, t4sec, groups)
  result
}
