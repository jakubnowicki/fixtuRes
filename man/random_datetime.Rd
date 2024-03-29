% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_types.R
\name{random_datetime}
\alias{random_datetime}
\title{Get random datetime}
\usage{
random_datetime(
  min_date,
  max_date,
  date_format = NULL,
  min_time = "00:00:00",
  max_time = "23:59:59",
  time_resolution = "seconds",
  tz = "UTC"
)
}
\arguments{
\item{min_date}{character or date, beginning of the dates interval to sample from}

\item{max_date}{character or date, ending of the dates interval to sample from}

\item{date_format}{character, check \code{\link[base]{strptime}} for details}

\item{min_time}{character, beginning of the time interval to sample from}

\item{max_time}{character, ending of the time interval to sample from}

\item{time_resolution}{character, one of "seconds", "minutes", "hours", time resolution}

\item{tz}{character, time zone to use}
}
\description{
Get random datetime
}
\examples{
random_datetime("2012-12-04", "2020-10-31", min_time = "7:00:00", max_time = "17:00:00")
}
