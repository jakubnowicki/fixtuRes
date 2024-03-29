% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mock_data_generator.R
\name{MockDataGenerator}
\alias{MockDataGenerator}
\title{MockDataGenerator}
\description{
Object that stores mock data configurations and generated datasets
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{MockDataGenerator$new()}}
\item \href{#method-get_data}{\code{MockDataGenerator$get_data()}}
\item \href{#method-get_all_data}{\code{MockDataGenerator$get_all_data()}}
\item \href{#method-clone}{\code{MockDataGenerator$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a new MockDataGenerator object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MockDataGenerator$new(configuration)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{configuration}}{list or path to YAML file with datasets configurations.
Check
\href{https://github.com/jakubnowicki/fixtuRes/blob/master/vignettes/configuration.Rmd}{configuration}
for details.
For a sample YAML check
\href{https://github.com/jakubnowicki/fixtuRes/tree/master/examples}{examples}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new MockDataGenerator object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_data}{}}}
\subsection{Method \code{get_data()}}{
Get a dataset (if does not exist, generate it)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MockDataGenerator$get_data(data_name, size = NULL, refresh = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_name}}{string, data set name to retrieve}

\item{\code{size}}{integer, size of dataset (if provided, will refresh dataset)}

\item{\code{refresh}}{boolean, refresh existing data?}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
mock dataset
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_all_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_all_data}{}}}
\subsection{Method \code{get_all_data()}}{
Get all datasets
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MockDataGenerator$get_all_data(refresh = FALSE, sizes = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{refresh}}{boolean, refresh existing data?}

\item{\code{sizes}}{integer, or vector of integers with data sizes}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
list with all datasets
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MockDataGenerator$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
