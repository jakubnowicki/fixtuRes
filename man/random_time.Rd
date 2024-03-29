% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_types.R
\name{random_time}
\alias{random_time}
\title{Get random time from an interval}
\usage{
random_time(
  min_time = "00:00:00",
  max_time = "23:59:59",
  resolution = "seconds"
)
}
\arguments{
\item{min_time}{character, beginning of the time interval to sample from}

\item{max_time}{character, ending of the time interval to sample from}

\item{resolution}{character, one of "seconds", "minutes", "hours", time resolution}
}
\description{
Get random time from an interval
}
\examples{
random_time("12:23:00", "15:48:32")
}
