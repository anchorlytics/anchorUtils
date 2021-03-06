% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_prep.R
\name{dummy_code}
\alias{dummy_code}
\title{Dummy-code (one-hot) factors}
\usage{
dummy_code(.data)
}
\arguments{
\item{.data}{data frame}
}
\value{
data frame with selected factors converted to logical dummy variables
}
\description{
Dummy coding expands a polytomous categorical factor into several
indicator variables, each of which is logical (Boolean, TRUE/FALSE).
}
\details{
The one-hot contrast expands a factor with \code{L} levels into \code{L}
indicator variables.  The result is not a full-rank parameterisation,
however it avoids needing to select a reference level.

Ordinal (ordered categorical) and dichotomous (only 2 levels) variables are
not modified; nor are non-factors (numeric, logical, etc.).

Note that the number of levels is determined from the unique values in the
actual data, rather than the factor levels.

The new indicator variables are appended to the end of the variable list,
and the original factors are removed.
}
\section{TODO}{
\itemize{
\item if .data has only one factor, it is renamed "data"
}
}

\examples{
library(dplyr)
mtcars \%>\% mutate(across(c("cyl", "gear"), as.factor)) \%>\% dummy_code()

}
\seealso{
Other data munging: 
\code{\link{drop_nzv}()},
\code{\link{ord_collapse}()}
}
\concept{data munging}
