% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryText.R
\name{summaryText}
\alias{summaryText}
\alias{summaryText.default}
\alias{summaryText.factor}
\alias{summaryText.ordered}
\alias{summaryText.character}
\alias{summaryText.logical}
\title{Summary Text}
\usage{
summaryText(x, fmt, ...)

\method{summaryText}{default}(x, fmt = "\%.2f", ...)

\method{summaryText}{factor}(x, fmt = "\%.1f", useNA = c("no", "always"), ...)

\method{summaryText}{ordered}(x, fmt = "\%.1f", useNA = c("no", "always"), ...)

\method{summaryText}{character}(x, ...)

\method{summaryText}{logical}(x, fmt = "\%.1f", ...)
}
\arguments{
\item{x}{an R object}

\item{fmt}{\link[base]{character} scalar, format string, see \link[base]{sprintf}}

\item{...}{additional parameters, currently not in use}

\item{useNA}{\link[base]{character} scalar, \code{'no'} (default) or \code{'always'},
see \link[base]{table}}
}
\value{
Function \link{summaryText} returns a \link[base]{character} scalar.
}
\description{
Provide the summary text of an R object
}
\examples{
x = rpois(n = 20L, lambda = 2)
x[sample.int(length(x), 3L)] = NA_integer_
summaryText(x)

# factor 
x = state.region
x[2L] = NA_integer_
summaryText(x)

# binary
summaryText(c(TRUE, FALSE, TRUE, NA))
summaryText(c(TRUE, FALSE, TRUE))
summaryText(c(FALSE, FALSE, NA))
summaryText(c(FALSE, FALSE, FALSE))
summaryText(c(NA, NA, NA))

}
