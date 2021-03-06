% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_types.R
\name{random_string}
\alias{random_string}
\title{Generate random string}
\usage{
random_string(
  length = NULL,
  min_length = 1,
  max_length = 15,
  pattern = "[A-Za-z0-9]"
)
}
\arguments{
\item{length}{integer or NULL (default), output string length.
If NULL, length will be random}

\item{min_length}{integer, minimum length if length is random.
Default: 1.}

\item{max_length}{integer, maximum length if length is random.
Default: 15.}

\item{pattern}{string, pattern for string to follow.
Check \code{\link[stringi]{stringi-search-charclass}} for details.}
}
\value{
random string
}
\description{
Generate random string
}
\examples{
random_string(length = 5)
}
