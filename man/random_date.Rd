% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_types.R
\name{random_date}
\alias{random_date}
\title{Get random date from an interval}
\usage{
random_date(min_date, max_date, format = NULL)
}
\arguments{
\item{min_date}{character or date, beginning of the time interval to sample from}

\item{max_date}{character or date, ending of the time interval to sample from}

\item{format}{character, check \code{\link[base]{strptime}} for details}
}
\description{
Get random date from an interval
}
\examples{
random_date("2012-12-04", "2020-10-31")
}
